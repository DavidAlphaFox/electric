(ns dustin.y2023.electric-tuple
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]))


(e/defn Cons$ [x y]
  (e/fn [Getter] (Getter. x y)))

(e/defn Car [Data]
  (Data. (e/fn [x y] x)))

(e/defn Cdr [Data]
  (Data. (e/fn [x y] y)))

; ---

#?(:cljs (def !x (m/dfv)))
#?(:clj (def !y (m/dfv)))

(e/defn Example []
  (e/client
    (dom/h1 (dom/text "Electric Tuple"))
    (let [x' (new (e/task->cp !x)) ; dom input
          z (Cons$.
              (e/server {:txn (new (e/task->cp !y)) #_(e/offload #(do (Thread/sleep 500) x'))})
              (e/client {:opt x'}))]
      (dom/div (dom/text (pr-str x')))
      (dom/div (dom/text (pr-str (Car. z))))
      (dom/div (dom/text (pr-str (Cdr. z)))))))

(comment
  (!x 1)
  (!y 2)
  )