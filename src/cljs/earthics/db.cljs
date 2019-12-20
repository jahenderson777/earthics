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

   :time 2020.0
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

(defn step-tile [db tile]
  (let [{:keys [co2 temp food population]} db
        {:keys [underworld terrain surface troposphere stratosphere]} tile]
    (update tile :surface-q (fn [q] (clamp ((if (or
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
                                           0.1)
                                          0 10)))))

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
        (if (and (= 1 (rand-int 3))
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
               (= 1 (rand-int 10)))
          (-> (rand-replace db :ice :water) calc)

          (and (< temp 6)
               (= 1 (rand-int 10)))
          (-> (rand-replace db :water :ice) calc)

          :else
          db)))

(defn step [pre-db]
  (let [{:keys [reflection]} pre-db
        db (volatile! pre-db)
        trees (volatile! 0)
        population (volatile! 0)
        food (volatile! 0)
        more-methane (volatile! 0)]
    (doall (for [x (range world-size-x)
                 y (range world-size-y)
                 :let [tile (step-tile @db (get-in @db [:world x y]))
                       {:keys [surface surface-q]} tile]]
             (do (vswap! db assoc-in [:world x y] tile)
                 (when (= surface :civilisation)
                   (vswap! population + surface-q))
                 (when (= surface :trees)
                   (vswap! trees + surface-q))
                 (when (= surface :agriculture)
                   (vswap! food + surface-q))
                 (when (= surface :pasture)
                   (vswap! more-methane + surface-q)
                   (vswap! food + surface-q)))))
    (let [next-db
          (-> @db
              (update :time + 1)
              (assoc :population @population)
              (update :co2 + (* @trees -0.01) (* @population 0.01))
              (update :co2 (fn [co2] (clamp co2 0 500)))
              (update :methane * 0.994)
              (update :methane + (* @more-methane 0.01))
              (assoc :food @food)
              step-temp
              ice)
          pre-population (:population pre-db)
          next-population @population]
      (if (< next-population pre-population)
        (update next-db :unnecessary-death + (- pre-population next-population))
        next-db))))
