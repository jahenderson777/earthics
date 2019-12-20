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

   :time 1800.0
   :food 0
   :temp 15
   :money 10
   :population 2
   :unnecessary-death 0
   :fulfilment 2})

(def modes
  {:surface {:label "surface"
             :modes {:trees {:label "trees" :color (hsl 100 70 20) :reflect 1}
                     :civilisation {:label "civilisation" :color (hsl 0 0 50) :reflect 1}
                     :desert {:label "desert" :color (hsl 30 80 60) :reflect 1.4}
                     :pasture {:label "pasture" :color (hsl 130 60 40) :reflect 2}
                     :agriculture {:label "agriculture" :color (hsl 70 60 40) :reflect 2}}}
   :terrain {:label "terrain"
             :modes {:land {:label "land" :color (hsl 23 34 20) :reflect 1}
                     :water {:label "water" :color (hsl 213 64 27) :reflect 0}
                     :ice {:label "ice" :color (hsl 23 64 90) :reflect 3}}}})

(def tile-paths
  (for [x (range world-size-x)
        y (range world-size-y)]
    (list :world x y)))

(defn calc-reflection-tile [tile]
  (let [{:keys [underworld terrain surface troposphere stratosphere]} tile]
    (assoc tile :reflect (get-in modes [:terrain :modes terrain :reflect]))))

(defn calc-reflection [db]
  (let [db (volatile! db)
        r (volatile! 0)]
    (doall (for [x (range world-size-x)
                 y (range world-size-y)
                 :let [tile (calc-reflection-tile (get-in @db [:world x y]))]]
             (do
               (vswap! db assoc-in [:world x y] tile)
               (vswap! r + (:reflect tile)))))
    (assoc @db :reflection @r)))

(defn calc [db]
  (-> db
      calc-reflection))

(defn step-temp [db]
  (let [{:keys [co2 methane reflection temp]} db]
    (assoc db :temp (+ temp
                       (* -0.001 temp) ; heat radiation
                       (* co2 0.0002)
                       (* reflection -0.0001)
                       (* methane 0.001)))))


(defn rand-replace [db from to]
  (loop [x 0
         y 0]
    (let [terrain (get-in db [:world x y :terrain])]
      (if (and (= x (dec world-size-x))
               (= y world-size-y))
        db
        (if (and (= 1 (rand-int 6))
                 (= terrain from))
          (assoc-in db [:world x y :terrain] to)
          (recur (if (= world-size-x (inc x))
                   0
                   (inc x))
                 (if (= world-size-x (inc x))
                   (inc y)
                   y)))))))

(defn ice [db]
  (let [{:keys [temp]} db]
    (cond (and (> temp 15)
               (= 1 (rand-int 5)))
          (-> (rand-replace db :ice :water) calc)

          (and (< temp 8)
               (= 1 (rand-int 5)))
          (-> (rand-replace db :water :ice) calc)

          :else
          db)))

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

(defn step-tile [db tile-path]
  (let [{:keys [co2 temp food population]} db

        {:keys [underworld terrain surface troposphere stratosphere]
         :as tile} (get-in db tile-path)

        maxed-out (volatile! false)
        dead (volatile! false)

        {surface :surface q :surface-q :as new-tile}
        (update tile :surface-q
                (fn [q]
                  (let [new-q ((if (or
                                    (and (= surface :civilisation)
                                         (< temp 20)
                                         (> temp 8)
                                         (< population food))
                                    (and (= surface :trees)
                                         (> co2 20))
                                    (and (= surface :agriculture)
                                         (< temp 17)
                                         (> temp 11))
                                    (and (= surface :pasture)
                                         (< temp 20)
                                         (> temp 8)))
                                 +
                                 -)
                               q
                               0.1)]
                    (when (> new-q 10)
                      (vreset! maxed-out true))
                    (when (< new-q 0)
                      (vreset! dead true))

                    (clamp new-q 0 10))))
        db (assoc-in db tile-path new-tile)]
    (case surface
      :civilisation (-> (if @maxed-out
                          (spread-civilisation db tile-path)
                          db)
                        (update :new-population + q)
                        (update :co2 + (* q 0.01)))
      :trees (-> (if @maxed-out
                   (spread-trees db tile-path)
                   db)
                 (update :co2 + (* q -0.01)))
      :agriculture (update db :new-food + q)
      :pasture (-> (update db :new-food + q)
                   (update :methane + (* q 0.01)))
      db)))

(defn step [db]
  (let [db (assoc db
                  :new-food 0
                  :food (or (:new-food db) 0)
                  :new-population 0
                  :population (or (:new-population db) 0))
        next-db
        (-> (reduce step-tile db tile-paths)
            (update :time + 1)
            (update :co2 (fn [co2] (clamp co2 0 500)))
            (update :methane * 0.989)
            step-temp
            ice)
        pre-population (:population db)
        next-population (:new-population next-db)]
    (if (< next-population pre-population)
      (update next-db :unnecessary-death + (- pre-population next-population))
      next-db)))
