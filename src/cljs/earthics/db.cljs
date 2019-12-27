(ns earthics.db
  (:require
   [earthics.config :as config :refer [world-size-x world-size-y]]
   [earthics.util :as util :refer [hsl clamp]]
   [clojure.string :as str]))

(def default-db
  {:mode :surface/agriculture
   :reflection 0
   :co2 200
   :methane 10
   :particulates 1
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
   :money 100
   :population 2
   :unnecessary-death 0
   :fulfilment 2})

(def modes
  {:surface {:label "surface"
             :modes {:trees {:label "trees" :color (hsl 100 70 20) :reflect 0.1}
                     :civilisation {:label "civilisation" :color (hsl 0 0 40) :reflect 0.1}
                     :civilisation-2 {:label "civilisation 2" :color (hsl 232 10 60) :reflect -0.2 :level 2}
                     :desert {:label "desert" :color (hsl 30 80 60) :reflect 0.2}
                     :pasture {:label "pasture" :color (hsl 130 60 40) :reflect 0.1}
                     :agriculture {:label "agriculture" :color (hsl 70 60 40) :reflect 0.1}
                     :ice {:label "ice" :color (hsl 23 64 90) :reflect 1}
                     :volcano {:label "volcano" :color (hsl 0 74 40) :reflect 0.1}}}
   :terrain {:label "terrain"
             :modes {:land {:label "land" :color (hsl 23 34 20) :reflect 0.1}
                     :water {:label "water" :color (hsl 213 64 27) :reflect 0}}}})

(def tile-paths
  (for [x (range world-size-x)
        y (range world-size-y)]
    (list :world x y)))


(defn step-temp [db]
  (let [{:keys [co2 methane reflection temp particulates]} db]
    (assoc db :temp (+ temp
                       (* 0.7
                          (+ (* -0.002 temp) ; heat radiation
                             (* -0.011 (clamp (- temp 16) 0 100))
                             (* co2 0.0002)
                             (* reflection -0.0001)
                             (* particulates -0.01)
                             (* methane 0.0005)))))))

(defn spread-tile [db [_ x y] to-spread & avoiding]
  (let [x (+ x (rand-int 3) -1)
        y (+ y (rand-int 3) -1)
        {:keys [surface terrain] :as new-tile} (get-in db [:world x y])]
    (if (and (not (contains? (conj (set avoiding)
                                   to-spread)
                             surface))
             (= terrain :land)
             (< y world-size-y)
             (< x world-size-x)
             (> y 0)
             (>= x 0))
      (-> db
          (assoc-in [:world x y :surface] to-spread)
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
                                    (and (= surface :civilisation-2)
                                         (< temp 23)
                                         (> temp 8)
                                         (< population food))
                                    (and (= surface :trees)
                                         (> co2 20)
                                         (< temp 30)
                                         (> temp 2))
                                    (and (= surface :agriculture)
                                         (< temp 18)
                                         (> temp 9)
                                         (> rainfall 16))
                                    (and (= surface :ice)
                                         (< temp 10))
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
      (and (= terrain :water) (= surface :none)) (update :co2 + -0.001)

      (and @dead
           (= 0 (rand-int 30))) (assoc-in (concat tile-path [:surface]) :none)

      (and (not= surface :ice)
           (< temp 11)
           (or (and (= 0 (rand-int 800)) (= terrain :land))
               (and (= 0 (rand-int 140)) (= terrain :water))))
      (-> (assoc-in (concat tile-path [:surface]) :ice)
          (assoc-in (concat tile-path [:surface-q]) 3))

      (and (= surface :civilisation) @maxed-out) (spread-tile tile-path :civilisation)

      (= surface :civilisation) (-> (update :new-population + q)
                                    (update :co2 + (* q 0.01)))
      (= surface :civilisation-2) (-> (update :new-population + (* 2 q))
                                      (update :co2 + (* q 0.014)))

      (= surface :volcano) (-> (update :co2 + (* 0.25 q))
                               (update :particulates + (* 0.1 q)))

      ;(and (= surface :trees) @maxed-out) (spread-tile tile-path :trees :civilisation :civilisation-2)

      (= surface :trees) (update :co2 + (* q -0.013))

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

(defn add-volcano [db]
  (update-in db[:world (rand-int world-size-x) (rand-int world-size-y)]
             assoc
             :surface :volcano
             :surface-q (inc (rand-int 6))))

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
            (update :co2 (fn [co2] (clamp (* 1 ;0.9993
                                            co2) 0 800)))
            (update :methane * 0.97)
            (update :particulates (fn [p] (clamp (* 0.8 (- p 0.16)) 0 500)))
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

      true
      (update :money #(clamp (+ % 0.1 (- next-population pre-population)) 0 1000000))

      (> next-population pre-population)
      (update :unnecessary-death + (* 0.01 (- next-population pre-population)))

      (> next-population 2000) (update :level-2-counter inc)

      (> (:level-2-counter next-db) 50) (assoc :level-2 true)

      (= 0 (rand-int 50)) add-volcano

      decade? update-histories)))


(defn print-world [db]
  (doall (for [y (range world-size-y)]
           (println (apply str (for [x (range world-size-x)]
                                 (let [{:keys [terrain surface surface-q]} (get-in db [:world x y])]
                                   (str (cond (= surface :agriculture) "a"
                                              (= surface :pasture) "p"
                                              (= surface :civilisation) "c"
                                              (= surface :trees) "t"
                                              (= surface :ice) "i"
                                              (= terrain :water) "."
                                              (= surface :none) "n"
                                              :else " ")
                                        (let [q (int surface-q)]
                                          (if (= q 10) "A" q))))))))))


(def real-world "iAiAiAiAiAiAiAiAiAiAiAiAiAtAtAtAtAtAtAiAiAiAiAiAiAiAn0n0iAiAn0iAiAiAiAiAiAiAiAiA
.0.0.0.0.0.0.0.0tAt0t0tAtAtAtAtAtAtAn0n0n0tAtA.0.0.0.0.0.0.0.0n0.0.0.0n0.0.0.0.0
.0.0.0tAtAtAtAtAtAn0aAtA.0.0tAtAtAtA.0.0.0.0.0.0tAaAtAtAtA.0.0.0tAtAtA.0.0.0.0.0
.0tAtAtAtAtAtAtAtAaAaAn0tA.0.0tAtA.0.0.0.0.0tAaAtApAaAtAtAaAaAcAtAaAaAcAtAcAcAtA
tAtAtAtAtAtAtAtAtAaA.0.0cAtAtA.0.0aA.0.0tA.0tApA.0pAaAcAcAcAtAcAtAaAaAcAaAcAtAtA
tAtAtAtAtAtAtAaAaAaApAcApApApAtA.0.0.0.0cA.0aA.0cApApAtAtAtAaAtAaAaAaAcA.0tAtA.0
.0.0.0cApAcAaAaAaAaApApApApAcA.0.0.0.0pAcA.0.0cApApApApAtAtAtAtAaAaAcApAaA.0.0.0
.0.0.0cAaApAaAaAaAaAaAcAcAcA.0.0.0.0.0.0.0cAcAcAcAcAaAaAaAtAaAaAaAaAcAaA.0.0.0.0
.0.0.0n0pAtApApAtApApAcAcA.0.0.0.0.0.0cAcAcAtAcAaAaApAtAtAaAaAaAaAaAaA.0aA.0.0.0
.0.0.0.0pAaAcAtApAcApAcA.0.0.0.0.0.0cAcAaApAcAcAcAcAcAcAcAcAcAaAaAcA.0.0aA.0.0.0
.0.0.0cAcAaAaAaAcAcAcA.0.0.0.0.0.0cAcAcAcAaAcAcAcApAaAaAtAtAtAaAaAcAcA.0cA.0.0.0
.0.0.0.0cAcAcAcAaAcA.0.0.0cA.0.0.0.0cAcA.0.0.0cA.0pA.0aAtAtAn0aAcAcApA.0aA.0.0.0
.0.0.0.0.0cAcAcAcAcA.0cA.0.0aA.0.0.0.0.0cAaA.0.0.0pAaA.0tAtAaApAcAcAcA.0cA.0.0.0
.0cA.0.0.0.0.0cA.0cA.0.0aA.0.0aA.0.0tAcAtAaAaAcAaAaAtAtAtAtAcApAaAcA.0.0.0.0.0.0
.0.0aA.0.0.0.0.0tAaAtA.0.0aA.0.0.0aAtAtAtAtAaAaAaA.0tAtAtAn0cApAaAcA.0.0.0.0.0.0
.0.0.0.0.0.0.0tAtAtAtAcAcA.0.0.0.0cAcAn0n0pApAn0.0n0.0cAcAn0tAcAcAcA.0.0.0.0.0.0
.0.0.0.0.0.0tAtAtAtAaAaAaAaA.0.0.0tAcAtAtAtAtAtAtA.0.0aAcAcAtAaAcAcA.0.0.0.0.0.0
.0.0.0.0.0.0cAcAtAtAt9aAaAcA.0.0.0.0tAcAtAtAtAtAtAtA.0tAcAcAaA.0cA.0aA.0.0.0.0.0
.0.0.0.0.0.0cAcAtAtAaAaAaAcAcA.0.0.0.0tAtAtAtAtAtA.0.0.0tAaA.0.0.0aAaA.0.0.0.0.0
.0.0.0.0.0.0tAtAtAtAaAaAaAcA.0.0.0.0.0tAcAtAaAtAtA.0.0.0tAaA.0.0.0aA.0cA.0cA.0cA
.0.0.0.0.0.0cAtAtAt3aAaAaAcA.0.0.0.0.0cAtAtAaAaAtA.0.0.0.0aA.0.0.0aA.0.0aA.0aA.0
.0.0.0.0.0.0.0c4tAaAaAaAcA.0.0.0.0.0.0.0cAcAaAcApA.0.0.0.0.0.0.0.0.0cA.0.0cA.0cA
.0.0.0.0.0.0.0c4tAaAaAaAcA.0.0.0.0.0.0.0c4aApAaA.0.0.0.0.0.0.0.0.0aAcAcA.0.0.0.0
.0.0.0.0.0.0.0c4tAaAcAcA.0.0.0.0.0.0.0.0c4c4aAaAcA.0aA.0.0.0.0c4c4a4aAaAcA.0.0.0
.0.0.0.0.0.0.0.0tAaAcA.0.0.0.0.0.0.0.0.0.0aAaAaAcA.0tA.0aA.0.0c4aAn0pAaAaAcA.0.0
.0.0.0.0.0.0.0.0tAaAcA.0.0.0.0.0.0.0.0.0.0aAaAaA.0tAaA.0.0.0.0cAn0n0n0pAaAcA.0.0
.0.0.0.0.0.0.0.0tAn0.0.0.0.0.0.0.0.0.0.0.0cAcAaA.0.0.0.0.0.0.0pAaAaAaApAaAcA.0pA
.0.0.0.0.0.0.0.0t9aAtA.0.0n0iAiAiAiA.0.0.0pAcA.0.0.0.0.0.0.0.0.0.0.0.0n0pA.0.0pA
n0i0i0.0.0.0.0.0.0.0.0.0iAiAiAiAiAiA.0.0.0.0.0.0.0iAiAiAiAiAiA.0.0.0.0.0.0.0.0.0
IAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiAiA")


(defn read-world [db s]
  (let [x (volatile! 0)
        y (volatile! 0)
        new-world (volatile! (get db :world))]
    (doall (for [l (str/split-lines s)]
             (do (vreset! x 0)
               (doall (for [[t q] (partition 2 l)]
                          (do (vswap! new-world assoc-in [@x @y]
                                      {:surface-q (if (= q "A")
                                                    10
                                                    (js/parseInt q))
                                       :underworld :foo
                                       :terrain (if (= t ".") :water :land)
                                       :surface (cond (= t "a") :agriculture
                                                      (= t "t") :trees
                                                      (= t "c") :civilisation
                                                      (= t "i") :ice
                                                      (= t "p") :pasture
                                                      (= t "n") :none
                                                      :else :none)
                                       :troposphere :foo
                                       :stratosphere :foo})
                              (vswap! x inc))))
               (vswap! y inc))))
    (assoc db :world @new-world)))

