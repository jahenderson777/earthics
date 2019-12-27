(ns earthics.events
  (:require
   [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx]]
   [earthics.db :as db]
   [earthics.config :as config :refer [world-size-x world-size-y]]))

(reg-event-db :init-db
              (fn [_ _]
                (assoc db/default-db :world
                       (vec (for [x (range world-size-x)]
                              (vec (for [y (range world-size-y)]
                                     {:surface-q 0
                                      :underworld :foo
                                      :terrain :water
                                      :surface (if (or (>= y (- world-size-y 1))
                                                       (= y 0))
                                                 :ice :none)
                                      :troposphere :foo
                                      :stratosphere :foo})))))))

(reg-event-db :assoc
              (fn [db [_ & kvs]]
                (apply assoc db kvs)))

(reg-event-fx :tile-click
              (fn [{db :db} [_ x y]]
                (let [{:keys [mode money last-tile-click]} db
                      mode-group (keyword (namespace mode))
                      mode (keyword (name mode))
                      current-surface (get-in db [:world x y :surface])
                      current-terrain (get-in db [:world x y :terrain])
                      q (get-in db [:world x y :surface-q])
                                        ;tile (get-in db :world x y)
                      db (assoc db :last-tile-click [x y])]
                  (when (and (pos? money)
                             (not= [x y] last-tile-click))
                    {:db (cond-> db
                           (= mode-group :terrain)
                           (-> (update :money dec)
                               (assoc-in [:world x y :terrain] mode)
                               (assoc-in [:world x y :surface] :none)
                               (assoc-in [:world x y :surface-q] 0))

                           (and (= current-terrain :land)
                                (= mode-group :surface))
                           (-> (update :money dec)
                               (assoc-in [:world x y :surface] mode)
                               (assoc-in [:world x y :surface-q] (if (= mode :volcano)
                                                                   (inc (rand-int 10))
                                                                   0))
                               (update :unnecesary-death (fn [x] (if (= current-surface :civilisation)
                                                                  (+ x q)
                                                                  x)))))}))))

(reg-event-db :step
              (fn [db _]
                (db/step db)))

(reg-event-fx :mouse-over
              (fn [{db :db} [_ x y]]
                (let [{:keys [mouse-down]} db]
                  (when mouse-down
                    {:dispatch [:tile-click x y]}))))

(reg-event-db :load-default
              (fn [db _]
                (db/read-world db/default-db db/real-world)))
