(ns dustin.y2023.electric-data-problem-statement 
  (:require [contrib.datomic-m :as d]
            [contrib.datomic-contrib-2020 :as dx]
            [hyperfiddle.electric :as e]))


(e/defn CheckboxField [{:keys [v txn optimistic]}]
  (e/client
    (let [v'-client (new (m/reductions {} nil (m/relieve {} (m/observe (fn [!] ...)))))] ; dom input signal
      (vector ; foreign call – CLJS vector constructed on client, AFTER pending resolves
        (e/client (optimistic v'-client)) ; no latency
        (e/server (txn v'-client)))))) ; pending

(e/defn App [db]
  (e/client
    (let [[optimistic txn] ; destructure CLJS vector on client
          (CheckboxField.
            {:v (e/server (:task/status (d/entity 1 db)))
             :txn (fn [v'] [[:db/add 1 :task/status v']])
             :optimistic (fn [v'] {:task/status v'})})]
      (e/server (d/transact! txn)) ; accidental roundtrip :(
      (e/client (dom/pre (dom/text (pprint-str optimistic))))))) ; accidental latency :(

; --- Should we just start building Electric data structures?

(e/defn App []
  (e/client
    (Cdr. ; extract 2 without waiting on 1
      (Cons$. ; must not delay the 2 to wait for the 1 (today, Electric waits, compiler not yet smart enough)
        (e/server 1) ; pending
        (e/client 2)))))
