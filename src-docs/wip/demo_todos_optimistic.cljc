(ns wip.demo-todos-optimistic
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.clojurex :refer [bindx]]
            [contrib.data :refer [merge-unordered]]
            [contrib.debug :as dbg]
            [contrib.identity :refer [tempid?]]
            [contrib.missionary-contrib :as mx]
            #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-data :refer [Cons$ Car Cdr]]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.electric-ui5 :as ui5 :refer [CreateController MasterList Field]]
            [hyperfiddle.stage :refer [transact!_]]
            [missionary.core :as m]
            [wip.demo-datomic-helpers :refer [Latency FailRate slow-transact! db tx-report]]))

#?(:clj
   (def schema
     [{:db/ident :task/status,      :db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
      {:db/ident :task/description, :db/valueType :db.type/string,  :db/cardinality :db.cardinality/one}
      {:db/ident :hf/stable-id,     :db/valueType :db.type/uuid,    :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}]))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id
                                       :task/description
                                       :task/status
                                       :hf/stable-id
                                       #_:task/order]) ...]
                      :where [?e :task/status]] db)
            #_(sort-by :task/order #(compare %2 %1)))))

(e/defn TodoItem [{:keys [db/id] :as ?record}] ; pre-pulled, todo entity api
  (e/client
    (dom/div (dom/style {:display "flex", :align-items "center"})
     (amb
       (ui5/Field.
         :record {:task/status (get ?record :task/status)} ; todo entity api
         :Control ui5/Checkbox
         :parse {:done true, :active false}
         :unparse {true :done, false :active}
         :edit (fn [v] [{:db/id (:db/id ?record) :task/status v}
                        [{:db/id (:db/id ?record) :task/status v}]]))
       (ui5/Field.
         :record  {:task/description (get ?record :task/description)}
         :Control ui5/Input
         :edit (fn [v] [{:task/description v}
                        [{:db/id (:db/id ?record) :task/description v}]]))))))

(e/defn TodoItemCreate "just another form, the caller will branch and deal with genesis
on submit"
  []
  ; How can we move the genesis edit to here? So we don't have to deconstruct
  ; the form monitor?
  [(ui5/Field.
     :record (e/server {:task/description nil})
     :Control ui5/Input
     :edit (fn [v] (fn [e] [{:db/id e
                             :task/description v
                             :task/status :active
                             :task/order next-order-id}
                            [[:db/add e :task/description v]
                             [:db/add e :task/status :active]
                             [:db/add e :task/order next-order-id]]]))
     (dom/props {:placeholder "Buy milk"}))])

(e/defn Page []
  (e/client
    (let [!return (m/mbx)]
      (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
      (dom/p (dom/text "it's multiplayer, try two tabs"))
      (dom/div (dom/props {:class "todo-list"})
        #_(dom/div {:class "todo-items"})

        (!return
          (let [stable-kf (contrib.identity/Entity-id-locally-stabilzied!. ui5/>tx-report)] ; fix
            (as-> (e/client (MasterList. stable-kf TodoItem TodoItemCreate)) 
              Curried (e/server (Curried. (todo-records hf/db))))))

        (dom/p (dom/props {:class "counter"})
          (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count hf/db))))
          (dom/text " items left")))
      (mx/poll-task !return)))) ; accidental transfer

(e/defn AdvancedTodoList []
  (e/server
    (bindx [ui5/!tx-report (atom nil)
            
            ; guarantee views can see each tx-report if needed
            ui5/>tx-report (m/stream (m/watch !tx-report))
            
            ; most views just want latest
            hf/db (:db-after (new (m/relieve {}
                                    (m/ap 
                                      (m/amb (d/db !conn) 
                                        (m/?< ui5/>tx-report))))))]
      (e/client
        (Latency. 300 2000)
        (FailRate. 3 10))

      (let [edits (Page.)] ; accidental transfer
        ; careful, rules of pending need to be stable to not repeat txns. e.g. x -> pending -> x can repeat
        (e/server
          (new (m/ap
                 (let [tx-report (m/?< ui5/>tx-report)]
                   (reset! ui5/!tx-report
                     (m/?< (transact!_ tx-report (e/fn [] edits))))))))

        (e/client
          (dom/hr)
          (ui4/edn edits nil (dom/props {:disabled true :class (css-slugify `staged)})))))))
