(ns hyperfiddle.popover
  #?(:cljs (:require-macros hyperfiddle.popover))
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.electric-ui5 :as ui5]
            [missionary.core :as m]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.history :as router]))

; data PopoverState = Closed | Open request | Pending request
; data BodyState = Idle | Request command | Pending command
; data Command = Commit tx | Discard

(e/defn BranchWrap [Body-client] ; todo colorless p/fns
  (let [return (m/dfv)
        x (Body-client. (fn commit!discard! [cmd]
                          (case cmd 
                            :commit (return x)
                            :discard (return nil))))]
    (dom/hr)
    (ui4/button (e/fn [] (return x)) (dom/text "commit!"))
    (ui4/button (e/fn [] (return nil)) (dom/text "discard"))
    (ui4/edn x nil (dom/props {::dom/disabled true
                              ::dom/style {:display "block" :width "100%" :height "3rem"}}))
    (new (e/task->cp return)))) ; popovers are pending until committed

(e/defn PopoverBody [Body-client]
  (dom/div (dom/props {:class    "hyperfiddle popover-body"
                       :tabIndex "1"}) 
    (new (m/reductions {} nil
           (e/listen> dom/node "click"
             (fn [e]
               (when (= (.-target e) (.-currentTarget e)) ; click on self
                 (.focus (.-currentTarget e)))))))
    (BranchWrap. (e/fn [] (Body-client)))))

(e/defn Popover [label Form-client]
  (let [!open (atom false)
        latch (m/dfv)]
    (dom/div (dom/props {:class "hyperfiddle popover-wrapper"})
      (let [open0 (e/watch !open)
            open (new (ui5/Button. label ; popover anchor
                        (fn [acc x] (not acc)) open0))]
        (when open
          (latch ; this is symetrical with relieving dom events to latest-input
            (doto (PopoverBody. Form-client) ; pending until commit
              (case (reset! !open false)))))))
    (new (e/task->cp latch))))

(defmacro staged [& body] `(new BranchWrap (e/fn [] ~@body)))
(defmacro popover [label & body] `(new Popover ~label (e/fn [] ~@body)))
(defmacro popover-staged [label & body] `(~'popover ~label (~'staged ~@body)))
