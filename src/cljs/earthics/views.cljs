(ns earthics.views
  (:require
   [re-frame.core :as re-frame :refer [dispatch subscribe]]
   [earthics.subs]
   [earthics.db :as db]
   [earthics.util :as util :refer [hsl]]
   [earthics.config :as config :refer [world-size-x world-size-y]]
   [goog.string :as gstring]
   [goog.string.format]
   [reagent.core :as reagent]
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
           :on-click #(dispatch [:tile-click x y])
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
     [:h1 "Riverford Earth Sim v2"]
     [:div
      [:div {:style {:display "inline-block"}}
       [:div "reflection: " (gstring/format "%.2f" (<- :get :reflection))]
       [:div "CO2: " (gstring/format "%.2f" (<- :get :co2))]
       [:div "methane: " (gstring/format "%.2f" (<- :get :methane))]
       [:div "rainfall: " (gstring/format "%.2f" (<- :get :rainfall))]
       [:div "particulates: " (gstring/format "%.2f" (<- :get :particulates))]
       ]

      [:div {:style {:display "inline-block" :margin-left 10}}
       [:div "temp: " (gstring/format "%.2f" (<- :get :temp))]
       [:div "food: " (gstring/format "%.2f" (<- :get :food))]
       [:div "population: " (int (<- :get :population))]
       [:div "unnecessary death: " (int (<- :get :unnecessary-death))]
       [:div "credits: " (gstring/format "%.2f" (<- :get :money))]
       ]


      [:div {:style {:display "inline-block"}}
       [:div {:on-click #(dispatch [:load-default])
              :style {:padding 4
                      :margin 4
                      :display "inline-block"
                      :background "#474"}}
        "reset"]
       [:div {:style {:display "inline-block" :margin-left 10 :font-size 20 :vertical-align "top"}}
        "date: " (gstring/format "%.2f" (<- :get :time))]]

      [:div {:style {:display "inline-block" :text-align "center" :margin-left 10 :vertical-align "top"}}
       (doall (for [[mode-group {:keys [modes label]}] db/modes]
                ^{:key mode-group}
                [:div {:style {:display "block" :text-align "center"}}
                 [:div label]
                 (doall (for [[mode {:keys [label level]}] modes
                              :let [mode (keyword mode-group mode)]
                              :when (or (nil? level)
                                        (and (= 2 level) (<- :get :level-2)))]
                          ^{:key mode}
                          [:div {:on-click #(dispatch [:assoc :mode mode])
                                 :style {:padding 4
                                         :margin 3
                                         :display "inline-block"
                                         :background (if (= mode current-mode)
                                                       "#339"
                                                       "#555")}}
                           label]))]))]]]))

(defn draw-canvas-contents [canvas props]
  (let [{:keys [data color scale]} props
        ctx (.getContext canvas "2d")
        w (.-clientWidth canvas)
        h (.-clientHeight canvas)
        vx (volatile! 0)]
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (set! (.-strokeStyle ctx) color)
    (.beginPath ctx)
    (.moveTo ctx 0 (- 200 (* scale (first data))))
    (doseq [y data]
      (.lineTo ctx @vx (- 200 (* scale y)))
      (vswap! vx + 6))
    (aset ctx "lineWidth" 1)
    (.stroke ctx)))

(defn html-canvas [data]
  (let [dom-node (reagent/atom nil)]
    (reagent/create-class
     {:component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this)))

      :component-did-update
      (fn [this]
        (draw-canvas-contents (.-firstChild @dom-node) (second (reagent/argv this))))

      :reagent-render
      (fn []
        [:div [:canvas {:width 1000 :height 200}]])})))

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
           [tile x y])]))]
   [html-canvas {:color (hsl 100 80 50)
                 :scale 0.3
                 :data (<- :get :history :co2)}]
   [html-canvas {:color (hsl 180 80 50)
                 :scale 3
                 :data (<- :get :history :temp)}]
   [html-canvas {:color (hsl 250 80 50)
                 :scale 0.1
                 :data (<- :get :history :population)}]
   [html-canvas {:color (hsl 50 80 50)
                 :scale 0.1
                 :data (<- :get :history :unnecessary-death)}]])



