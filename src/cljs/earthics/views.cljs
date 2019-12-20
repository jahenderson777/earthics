(ns earthics.views
  (:require
   [re-frame.core :as re-frame :refer [dispatch subscribe]]
   [earthics.subs]
   [earthics.db :as db]
   [earthics.util :as util :refer [hsl]]
   [earthics.config :as config :refer [world-size-x world-size-y]]
   [goog.string :as gstring]
   [goog.string.format]
   ))

(defn <- [& v]
  (deref (subscribe (vec v))))

(defn tile [x y]
  (let [{:keys [underworld terrain surface troposphere stratosphere surface-q]} (<- :get :world x y)]
    [:div {:style {:display "inline-block"
                   :width 40
                   :height 30
                   :margin-right 1
                   :margin-bottom 1
                   :text-align "center"
                   :background (get-in db/modes [:terrain :modes terrain :color])}
           :on-mouse-down #(do (dispatch [:assoc :mouse-down true])
                               (dispatch [:mouse-over x y]))
           :on-mouse-over #(dispatch [:mouse-over x y])}
     [:div {:style {:width 34
                    :height 24
                    :margin 3
                    :padding-top 0
                    :line-height 2
                    :color "black"
                    :background (get-in db/modes [:surface :modes surface :color])}}
      (if (and surface-q (not (zero? surface-q)))
        (int surface-q)
        "0")]]))

(defn header []
  (let [current-mode (<- :get :mode)]
    [:div {:style {:top 0
                   :height 100
                   :position "fixed"
                   :background (hsl 0 0.4 0.4 0.4)}}
     [:h1 "Riverford Earth Sim"]
     [:div
      [:div {:style {:display "inline-block"}}
       [:div "reflection: " (<- :get :reflection)]
       [:div "CO2: " (gstring/format "%.2f" (<- :get :co2))]
       [:div "methane: " (gstring/format "%.2f" (<- :get :methane))]
       ;[:div "particulates: " (<- :get :particulates)]
       ]

      [:div {:style {:display "inline-block" :margin-left 10}}
       [:div "temp: " (gstring/format "%.2f" (<- :get :temp))]
       [:div "food: " (gstring/format "%.2f" (<- :get :food))]
       [:div "population: " (int (<- :get :population))]
       [:div "unnecessary death: " (int (<- :get :unnecessary-death))]
       ;[:div "fulfilment: " (<- :get :fulfilment)]
       ]

      [:div {:style {:display "inline-block" :margin-left 10 :font-size 20 :vertical-align "top"}}
       "date: " (gstring/format "%.2f" (<- :get :time))]

      [:div {:style {:display "inline-block" :text-align "center" :margin-left 10 :vertical-align "top"}}
       (doall (for [[mode-group {:keys [modes label]}] db/modes]
                ^{:key mode-group}
                [:div {:style {:display "block" :text-align "center"}}
                 [:div label]
                 (for [[mode {:keys [label]}] modes
                       :let [mode (keyword mode-group mode)]]
                   ^{:key mode}
                   [:div {:on-click #(dispatch [:assoc :mode mode])
                          :style {:padding 4
                                  :margin 3
                                  :display "inline-block"
                                  :background (if (= mode current-mode)
                                                "#339"
                                                "#555")}}
                    label])]))]]]))

(defn main-panel []
  [:<>
   [header]
   [:div.noselect {:style {:margin-top 100
                  :text-align "center"}}
    (doall
      (for [y (range world-size-y)]
        ^{:key y}
        [:div {:style {:white-space "nowrap"}}
         (for [x (range world-size-x)]
           ^{:key [x y]}
           [tile x y])]))]])
