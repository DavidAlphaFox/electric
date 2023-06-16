(ns dustin.y2023.electric-vector2
  (:require [hyperfiddle.electric :as e]
            [missionary.core :as m]))

(declare Cons$ Cdr Car)

(e/defn CheckboxField [{:keys [v txn optimistic]}]
  (e/client
    (let [v' (new (m/reductions (m/relieve {} (m/observe (fn [!] (! v) ...)))))]
      (Cons$.
        (e/client (optimistic v'))
        (e/server (txn v'))))))

(e/defn Page [db]
  (e/client
    (let [X|Dx (e/for-by [record records] ; streaming actually
                 (CheckboxField.
                   {:v (e/server (:task/status (d/entity 1 db)))
                    :txn (fn [x'] [[:db/add e :task/status x']]) ; todo busy loop
                    :optimistic (fn [x'] {:task/status x'})}))] ; todo busy loop
      X|Dx)))

(e/defn App [db]
  (e/server
    (let [X|Dx (Page. db)]
      (e/client (dom/pre (dom/text (pprint-str (Car. X|Dx)))))
      (e/server (transact! (Cdr. X|Dx))))))

; if e/for-by-streaming maintains a reactive vector, 
; there is no diffing on either side