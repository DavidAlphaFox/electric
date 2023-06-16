(ns hyperfiddle.electric-data
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]))

; a client-side data structure that can inspect x without waiting for dx when it is pending
(e/defn Triple [a b c] (e/fn [Getter] (Getter. a b c)))
(e/defn First [a b c] a)
(e/defn Second [a b c] b)
(e/defn Third [a b c] c)

; ---

;; data Tuple = First x | Second x
;;
;; match (Tuple 1 2)
;;   First x => (print x)
;;   Second x => (print x)

(e/defn Tuple [x y] ; access x while y is pending
  (e/fn [Getter] (Getter. x y)))

(e/defn First [Data]
  (Data. (e/fn [x y] x)))

(e/defn Second [Data]
  (Data. (e/fn [x y] y)))

(First. (Tuple. (e/client 1) (e/server 2))) := 2

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
  (!y 2))
