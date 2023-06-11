(ns hyperfiddle.popover
  #?(:cljs (:require-macros hyperfiddle.popover))
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.history :as router]))

; data PopoverState = Closed | Open request | Pending request
; data BodyState = Idle | Request command | Pending command
; data Command = Commit tx | Discard

(e/defn BranchWrap [Body-client] ; todo colorless p/fns
  (e/server
    (let [x (hf/branch
              (e/client (Body-client.)))]
      (e/client
        (dom/hr)
        (let [return (m/dfv)]
          (ui/button (e/fn [] (return x)) (dom/text "commit!"))
          (ui/button (e/fn [] (return nil)) (dom/text "discard"))
          (ui/edn x nil (dom/props {::dom/disabled true
                                    ::dom/style {:display "block" :width "100%" :height "3rem"}}))
          (new (e/task->cp return))))))) ; popovers are pending until committed

(e/defn PopoverBody [Body-client]
  (dom/div (dom/props {:class    "hyperfiddle popover-body"
                       :tabIndex "1"}) 
    (new (m/reductions {} nil
           (e/listen> dom/node "click"
             (fn [e]
               (when (= (.-target e) (.-currentTarget e)) ; click on self
                 (.focus (.-currentTarget e)))))))
    (BranchWrap. (e/fn [] (Body-client)))))

(e/defn Popover [label Body-client]
  (let [!open? (atom false), open? (e/watch !open?)
        return (m/dfv)]
    (dom/div (dom/props {:class "hyperfiddle popover-wrapper"})
      (ui/button (e/fn [] (swap! !open? not)) (dom/text label)) ; popover anchor
      (when open?
        (return ; latch result, this is symetrical with relieving dom events to latest-input
          (doto (PopoverBody. Body-client) ; nil until commit then blinks result, can this be untangled to remove the blink?
            (case (swap! !open? not)))))) ; close popover when not pending (is that right? it should be optimistic, never pending)
    (new (e/task->cp return))))

(defmacro staged [& body] `(new BranchWrap (e/fn [] ~@body)))
(defmacro popover [label & body] `(new Popover ~label (e/fn [] ~@body)))
(defmacro popover-staged [label & body] `(~'popover ~label (~'staged ~@body)))
