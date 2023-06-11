(ns wip.demo-todos-optimistic
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.identity :refer [tempid?]]
            #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.electric-ui5 :as ui5 :refer [CreateController MasterList]]
            [contrib.data :refer [merge-unordered]]
            [contrib.debug :as dbg]
            [hyperfiddle.api :as hf]
            [wip.demo-datomic-helpers :refer [Latency FailRate transact! db tx-report]]))

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

(e/defn TodoItem [{:keys [db/id] :as ?record} submit!] ; pre-pulled, todo entity api
  (e/client
    (dom/div (dom/style {:display "flex", :align-items "center"}) 
      (e/server
        ; strategy = commit asap, no reason to delay.
        ; this means create-new records are imediately synced, which is correct
        ; this means edits are isolated, which is correct
        ; if edits should be batched, that's what hf/branch provides, so this is correct. 
        
        (ui5/Field.
          :record ?record ; should be lazy loaded - entity api. This is over-fetched. Thereby ensure right here?
          :a :task/status
          :Control ui5/Checkbox
          :parse {:done true, :active false}
          :unparse {true :done, false :active}
          :txn (fn [x] [{:db/id (:db/id ?record) :task/status x}]))
        
        (ui5/Field.
          :record ?record
          :a :task/description
          :Control ui5/Input
          :parse identity
          :unparse identity
          :txn (fn [tx] [{:db/id (:db/id ?record) :task/description v}]))))))

(e/defn TodoItemCreate "just another form, the caller will branch and deal with genesis 
on submit"
  []
  ; its a colored input but perhaps we suppress the styles as the popover is inlined?
  ; This input has no colors actually due to being a glorified popover submit button?
  (e/server
    (ui5/Field. ; local optimistic updates via side channel on client
      :record (e/server {})
      :a :task/description
      :Control ui5/Input ; todo esc to revert
; how can Submit be wired directly to commit-stage in DT? 
      :parse identity
      :unparse identity
      :txn (fn [v]
             {#_#_:txn [[:db/add ?x :task/description v] ; no ID yet! Cannot transact, have local view repr only
                        [:db/add ?x :task/status :active]
                        [:db/add ?x :task/order (e/server (swap! !order-id inc))]]
              :optimistic {:task/description v
                           :task/status :active
                           :task/order (e/server (swap! !order-id inc))}})
      (dom/props {:placeholder "Buy milk"})))
  #_(e/client v'-client) ; return optimistic client value as local-index for the masterlist
  )

(e/defn Page []
  (e/client
    (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
    (dom/p (dom/text "it's multiplayer, try two tabs"))
    (dom/div (dom/props {:class "todo-list"})
      #_(dom/div {:class "todo-items"})
      (e/server 
        (let [stable-kf (contrib.identity/Entity-id-locally-stabilzied!. hf/tx-report)
              tx (MasterList. stable-kf (todo-records hf/db)
                   TodoItem TodoItemCreate #_{:task/status :active})]
          tx))
      (dom/p (dom/props {:class "counter"})
        (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count hf/db))))
        (dom/text " items left")))))

(e/defn AdvancedTodoList []
  (e/server
    (binding [hf/db (e/watch !db)
              hf/into-tx' hf/into-tx
              hf/with (fn [db tx] ; inject datomic dependency 
                        (try
                          (let [{:keys [db-after tx-report]} (new (e/task->cp (transact! db tx)))]
                            db-after)
                          (catch Exception e 
                            (println "...failure, e: " e)
                            db)))]
      (e/client
        (Latency. 300 2000)
        (FailRate. 3 10))
      (let [vdv (hf/branch
                  (Page.))]
        ; vdv is on the client actually, its optimistic. dv is also concurrently on the server.
        ; perhaps those parallel code flows should be made explicit! Can Electric compiler solve it?
        (e/client
          (dom/hr)
          (dom/element "style" (str "." (css-slugify `staged) " { display: block; width: 100%; height: 10em; }"))
          ; optimistic 
          (ui/edn vdv nil (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)}))
          #_(ui/edn (e/server .) nil (dom/props {::dom/disabled true ::dom/class (css-slugify `staged)})))))))
