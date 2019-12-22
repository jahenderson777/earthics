(ns earthics.db
  (:require
   [earthics.config :as config :refer [world-size-x world-size-y]]
   [earthics.util :as util :refer [hsl clamp]]))

(def default-db
  {:mode :terrain/water
   :reflection 0
   :co2 100
   :methane 10
   :particulates 10
   :rainfall 0

   :history {:co2 []
             :methane []
             :rainfall []
             :food []
             :population []
             :temp []
             :unnecessary-death []}

   :time 1500.0
   :food 0
   :temp 15
   :money 10
   :population 2
   :unnecessary-death 0
   :fulfilment 2})

(def modes
  {:surface {:label "surface"
             :modes {:trees {:label "trees" :color (hsl 100 70 20) :reflect 0.1}
                     :civilisation {:label "civilisation" :color (hsl 0 0 50) :reflect 0.1}
                     :desert {:label "desert" :color (hsl 30 80 60) :reflect 0.2}
                     :pasture {:label "pasture" :color (hsl 130 60 40) :reflect 0.1}
                     :agriculture {:label "agriculture" :color (hsl 70 60 40) :reflect 0.1}
                     :ice {:label "ice" :color (hsl 23 64 90) :reflect 1}}}
   :terrain {:label "terrain"
             :modes {:land {:label "land" :color (hsl 23 34 20) :reflect 0.1}
                     :water {:label "water" :color (hsl 213 64 27) :reflect 0}}}})

(def tile-paths
  (for [x (range world-size-x)
        y (range world-size-y)]
    (list :world x y)))


(defn step-temp [db]
  (let [{:keys [co2 methane reflection temp]} db]
    (assoc db :temp (+ temp
                       (* -0.002 temp) ; heat radiation
                       (* co2 0.0002)
                       (* reflection -0.0001)
                       (* methane 0.0005)))))

(defn spread-trees [db [_ x y]]
  (let [x (+ x (rand-int 3) -1)
        y (+ y (rand-int 3) -1)
        {:keys [surface terrain] :as new-tile} (get-in db [:world x y])]
    (if (and (not= surface :trees)
             (not= surface :civilisation)
             (= terrain :land)
             (< y world-size-y)
             (< x world-size-x)
             (> y 0)
             (>= x 0))
      (-> db
          (assoc-in [:world x y :surface] :trees)
          (assoc-in [:world x y :surface-q] 0))
      db)))

(defn spread-civilisation [db [_ x y]]
  (let [x (+ x (rand-int 3) -1)
        y (+ y (rand-int 3) -1)
        {:keys [surface terrain] :as new-tile} (get-in db [:world x y])]
    (if (and (not= surface :civilisation)
             (= terrain :land)
             (< y world-size-y)
             (< x world-size-x)
             (> y 0)
             (>= x 0))
      (-> db
          (assoc-in [:world x y :surface] :civilisation)
          (assoc-in [:world x y :surface-q] 0))
      db)))


(def latitude-temp-bias
  (vec (for [y (range world-size-y)]
         (/ (- (/ world-size-y 4)
               (js/Math.abs (- y (/ world-size-y 2))))
            2.5))))

(defn set-surface-if-true [db tile-path new-surface test]
  (if test
     (assoc-in db (concat tile-path [:surface]) test)
     db))


(defn step-tile [db tile-path]
  (let [{:keys [co2 temp food population rainfall]} db
        temp (+ temp (nth latitude-temp-bias (nth tile-path 2)))

        {:keys [underworld terrain surface troposphere stratosphere]
         :as tile} (get-in db tile-path)

        maxed-out (volatile! false)
        dead (volatile! false)

        {surface :surface q :surface-q :as new-tile}
        (update tile :surface-q
                (fn [q]
                  (let [new-q ((if (or
                                    (and (= surface :civilisation)
                                         (< temp 22)
                                         (> temp 8)
                                         (< population food))
                                    (and (= surface :trees)
                                         (> co2 20)
                                         (< temp 30)
                                         (> temp 2))
                                    (and (= surface :agriculture)
                                         (< temp 18)
                                         (> temp 10)
                                         (> rainfall 16))
                                    (and (= surface :ice)
                                         (< temp 13))
                                    (and (= surface :pasture)
                                         (< temp 22)
                                         (> temp 8)))
                                 +
                                 -)
                               q
                               0.1)]
                    (when (> new-q 10)
                      (vreset! maxed-out true))
                    (when (< new-q 0)
                      (vreset! dead true))
                    (clamp new-q 0 10))))]
    (cond-> (assoc-in db tile-path new-tile)
      true (update :reflection + (get-in modes [:surface :modes surface :reflect]))

      (= terrain :water) (update :new-rainfall + 0.003)

      (and @dead
           (= 0 (rand-int 10))) (assoc-in (concat tile-path [:surface]) :none)

      (and (not= surface :ice)
           (< temp 11)
           (or (and (= 0 (rand-int 800)) (= terrain :land))
               (and (= 0 (rand-int 140)) (= terrain :water))))
      (-> (assoc-in (concat tile-path [:surface]) :ice)
          (assoc-in (concat tile-path [:surface-q]) 3))

      (and (= surface :civilisation) @maxed-out) (spread-civilisation tile-path)

      (= surface :civilisation) (-> (update :new-population + q)
                                    (update :co2 + (* q 0.01)))

      (and (= surface :trees) @maxed-out) (spread-trees tile-path)

      (= surface :trees) (update :co2 + (* q -0.01))

      (or (= surface :agriculture)
          (= surface :pasture)) (update :new-food + q)

      (= surface :pasture) (-> (update :methane + (* q 0.006))
                               (update :co2 + (* q 0.005)))
      )))

(defn update-history [db k]
  (update-in db [:history k]
             (fn [history]
               (let [new-history (conj history (get db k))]
                 (if (> (count new-history) 100)
                   (subvec new-history 1)
                   new-history)))))

(defn update-histories [db]
  (-> db
      (update-history :co2)
      (update-history :methane)
      (update-history :rainfall)
      (update-history :food)
      (update-history :population)
      (update-history :temp)
      (update-history :unnecessary-death)))

(defn step [db]
  (let [db (assoc db
                  :reflection 0
                  :new-rainfall 0
                  :rainfall (or (:new-rainfall db) 0)
                  :new-food 0
                  :food (or (:new-food db) 0)
                  :new-population 0
                  :population (or (:new-population db) 0))
        next-db
        (-> (reduce step-tile db tile-paths)
            (update :time + 1)
            (update :co2 (fn [co2] (clamp (* 0.9993 co2) 0 500)))
            (update :methane * 0.97)
            (update :new-rainfall (fn [r] (clamp (* r (:temp db))
                                            0 100)))
            step-temp)
        pre-population (:population db)
        next-population (:new-population next-db)
        time (:time next-db)
        decade? (zero? (int (mod time 10)))]
    (cond-> next-db
      (< next-population pre-population)
      (update :unnecessary-death + (- pre-population next-population))

      decade?
      (update-histories))))

