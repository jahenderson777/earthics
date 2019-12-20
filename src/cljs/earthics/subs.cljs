(ns earthics.subs
  (:require
   [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub
  :get
  (fn [db [_ & path]]
    (get-in db path)))
