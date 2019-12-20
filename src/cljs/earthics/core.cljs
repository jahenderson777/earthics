(ns earthics.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [earthics.events :as events]
   [earthics.views :as views]
   [earthics.config :as config]
   ))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn init []
  (re-frame/dispatch-sync [:init-db])
  (js/setInterval (fn []
                    (re-frame/dispatch [:step])
                    true)
                  500)
  (js/window.addEventListener "mouseup" #(re-frame/dispatch [:assoc :mouse-down false]))
  (dev-setup)
  (mount-root))
