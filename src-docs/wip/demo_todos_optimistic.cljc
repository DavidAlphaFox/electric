(ns wip.demo-todos-optimistic
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.clojurex :refer [do1]]
            [contrib.data :refer [merge-unordered]]
            [contrib.debug :as dbg]
            [contrib.identity :refer [tempid?]]
            #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-data :refer [Cons$ Car Cdr]]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.electric-ui5 :as ui5 :refer [CreateController MasterList Field]]
            [hyperfiddle.stage :refer [aggregate-edits promote-edits]]
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
      (aggregate-edits
        ; single optimistic document, multiple txns?
        ; send up the best possible viewstate
        (ui5/Field.
          :record (get ?record :task/status) ; todo entity api
          :Control ui5/Checkbox
          :parse {:done true, :active false}
          :unparse {true :done, false :active}
          :optimistic (fn [v] {:db/id (:db/id ?record) :task/status x})
          :txn (fn [x] [{:db/id (:db/id ?record) :task/status x}]))
        (ui5/Field.
          :record  (get ?record :task/description)
          :Control ui5/Input
          :parse identity
          :unparse identity
          :optimistic (fn [v] ...)
          :txn (fn [tx] [{:db/id (:db/id ?record) :task/description v}]))))))

(e/defn TodoItemCreate "just another form, the caller will branch and deal with genesis
on submit"
  []
  (aggregate-edits
    (ui5/Field.
      :record (e/server {:task/description nil})
      :Control ui5/Input ; todo esc to revert
; how can Submit be wired directly to commit-stage in DT?
      :parse identity
      :unparse identity

      ; no ID yet for create
      :optimistic (fn [v] (fn [e] {:db/id e
                                   :task/description v
                                   :task/status :active
                                   :task/order next-order-id}))
      :txn (fn [v] (fn [e] [[:db/add e :task/description v]
                            [:db/add e :task/status :active]
                            [:db/add e :task/order next-order-id]]))
      ; on submit, latch branch
      (dom/props {:placeholder "Buy milk"}))))

(e/defn Page []
  (e/client
    (let [!return (atom nil)]
      (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
      (dom/p (dom/text "it's multiplayer, try two tabs"))
      (dom/div (dom/props {:class "todo-list"})
        #_(dom/div {:class "todo-items"})

        (reset! !return
          (let [stable-kf (contrib.identity/Entity-id-locally-stabilzied!. hf/tx-report)]
            (MasterList. stable-kf (e/server (todo-records hf/db))
              TodoItem TodoItemCreate #_{:task/status :active})))

        (dom/p (dom/props {:class "counter"})
          (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count hf/db))))
          (dom/text " items left")))
      (e/watch !return)))) ; accidental transfer

(defn transact!_ [<edits]
  ; do this in discrete time, these effects must not be interrupted.
  ; correlate the dirty-txn with the tx-report; is edits = tx-report?
  (m/ap
    (let [{:keys [::hf/dirty] :as edits} (m/?> <edits)] ; non-preemptive
      (m/amb ; progressive states must be shown to user
        [tx-report
         (promote-edits edits dirty ::hf/pending)]
        (try
          [(m/? (slow-transact! conn dirty))
           (promote-edits edits dirty ::hf/synced)]
          (catch Exception e
            [tx-report
             (promote-edits edits dirty ::hf/failed)]))))))

(e/defn AdvancedTodoList []
  (e/server
    (binding [hf/tx-report (e/watch !tx-report)
              hf/db (:db-after hf/tx-report)]
      (e/client
        (Latency. 300 2000)
        (FailRate. 3 10))

      (let [edits (Page.)] ; accidental transfer
        (e/server
          (reset! !tx-report
            ; a stream of progress reports, not just final reports
            (transact!_ (e/fn [] edits))))
        
        (e/client
          (dom/hr)
          (ui4/edn edits nil (dom/props {:disabled true :class (css-slugify `staged)})))))))
