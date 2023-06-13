(ns hyperfiddle.api
  (:import [hyperfiddle.electric Pending]
           #?(:cljs [goog.math Long]))
  (:require clojure.edn
            [contrib.clojurex :refer [bindx]]
            [contrib.dynamic :refer [call-sym]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.electric :as e]
            [missionary.core :as m]
            hyperfiddle.electric-dom2
            [hyperfiddle.api :as hf]))

(def ^:dynamic *$*) ; dbval, for REPL usage. Available in cljs for HFQL datascript tests
(e/def db "inject database value for hyperfiddle stage and HFQL")
(s/def ::ref? any?)
(e/def secure-db "database value excluding stage, so that user can't tamper")
(e/def with "inject datomic.api/with or equivalent, used by stage")
(e/def into-tx')
(def -read-edn-str-default (partial clojure.edn/read-string
                                    {:readers #?(:cljs {'goog.math/Long goog.math.Long/fromString} ; datomic cloud long ids
                                                :clj {})}))
(e/def read-edn-str "inject app-specific edn extensions" -read-edn-str-default) ; avoid Electric warning about goog.math.Long
(e/def ^:dynamic *nav!*)

;;; Database

(def db-state #?(:clj (atom nil))) ; Server side only
(e/def db-name "$")

(e/def schema "pre-fetched schema for explorer")
(e/def ^{:dynamic true, :doc "To be bound to a function [db attribute] -> schema"} *schema*)
(e/def ^{:dynamic true, :doc "To be bound to a function schema -> ::hf/one | ::hf/many"} *cardinality*
  (fn cardinality [schemaf db attr]
    (let [card
          ({:db.cardinality/one ::one
            :db.cardinality/many ::many} (:db/cardinality (schemaf db attr)))]
      card)))

(defn entity [ctx] (or (::entity ctx) (::entity (::parent ctx))))
(defn attribute [ctx] (or (::attribute ctx) (::attribute (::parent ctx))))

(e/def validation-hints nil)

(e/defn tx "WIP, this default impl captures the essence" [v' props] ; meant to be called by a renderer
  ;; Does it return a tx or side-effect to the staging area?
  (assert false "TBD")
  #_(if-let [Txfn (::tx props)] ; provided by hfql (props ... {::hf/tx (p/fn [] ...)})
      (Txfn. v')
      (when v'
        (let [[E a _] (first context)] ; context is a stack of [[E a] ...] in dynamic scope ; MISSING today
          [[:db/add (E.) a v']]))))

(defmulti tx-meta (fn [schema tx] (if (map? tx) ::map (first tx))))

(s/def ::tx-cardinality (s/or :one :many))
(s/def ::tx-identifier map?)
(s/def ::tx-inverse fn?)
(s/def ::tx-special fn?)
(s/def ::transaction-meta (s/keys :req [::tx-identifier]
                                  :opt [::tx-cardinality ::tx-inverse ::tx-special
                                        ::tx-conflicting?]))
(s/fdef tx-meta :ret ::transaction-meta)

; resolve cycle - hyperfiddle.txn needs hf/tx-meta
#?(:clj (require 'hyperfiddle.txn)) ; [rosie] before rcf turns on due to test/seattle undefined
#?(:clj (defn expand-hf-tx [tx] (call-sym 'hyperfiddle.txn/expand-hf-tx tx)))
;#?(:clj
;   (defmacro into-tx
;     ([tx tx'] `(call-sym ~'hyperfiddle.txn/into-tx ~hyperfiddle.api/schema ~tx ~tx')) ; Electric call can infer schema
;     ([schema tx tx'] `(call-sym ~'hyperfiddle.txn/into-tx ~schema ~tx ~tx'))) ; clojure compatible call
;   :cljs (def into-tx nil))
#?(:clj (defn into-tx
          ;([tx tx'] (into-tx schema tx tx')) -- needs Electric->Clojure binding conveyance
          ([schema tx tx'] (call-sym 'hyperfiddle.txn/into-tx schema tx tx'))))

(e/defn Transact!* [!t tx] ; colorless, !t on server
  ; need the flattening be atomic?
  #_(when-some [tx (seq (hyperfiddle.txn/minimal-tx hyperfiddle.api/db tx))]) ; stabilize first loop (optional)
  (new (e/task->cp
         ;; workaround: Datomic doesn't handle a thread interrupt correctly
         (m/compel
           (m/via m/blk
             ;; return basis-t ?
             (swap! !t (fn [[db tx0]]
                         [(with db tx) ; injected datomic dep, to avoid hardcoding a specific Datomic prodcut line
                          (into-tx' schema tx0 tx)]))))))) ; datascript is different

(e/def Transact!) ; server -- rename to With
(e/def ClearStage!) ; server
(e/def stage) ; server
(e/def loading) ; client

(e/defn Load-timer []
  (e/client
    (let [[x] (e/with-cycle [[elapsed start :as s] [0 nil]]
                (case hyperfiddle.api/loading
                  true [(some->> start (- e/system-time-ms))
                             (js/Date.now)]
                  s))]
      x)))

(e/def !status) (e/def status (e/watch !status))
(e/def !x) (e/def x (e/watch !x))
(e/def !dx) (e/def dx (e/watch !dx))
(e/def !tx-report) (e/def tx-report (e/watch !tx-report))

(e/defn Branch [Body-server] ; todo colorless
  (e/client
    (binding [!status (atom ::init) status (e/watch !status)
              !x (atom (e/server (get record a))) x (e/watch !x)] ; fixme record
      (e/server
        (bindx [!dx (atom nil) dx (e/watch !dx)
                !tx-report (atom nil) tx-report (e/watch !tx-report)
                hyperfiddle.api/db (or (:db-after tx-report) ; get from queue instead?
                                     hyperfiddle.api/db)
                hyperfiddle.api/Transact! (e/fn [tx]
                                            #_(println "Transact! " (hash !t) "committing: " tx)
                                            (reset! !tx-report (Transact!*. !t tx))
                                            #_(println "Transact! " (hash !t) "commit result: " tx-report))] 

          (e/client
            (e/with-cycle [loading false]
              (binding [hyperfiddle.api/loading loading]
                #_(dom/div (name loading) " " (str (Load-timer.)) "ms")
                (try
                  (e/server
                      ; today, Body must return by side channel to avoid accidental transfer
                    (let [_xdx (Body-server.)])) ; cycle xdx ?
                  false (catch Pending e true)))))
          
          #_[x dx] nil))
      ; there's really no point to returning anything, you need a nested branch for the side-channel call convention
      [status x]))) ; return optimistic view for client display, txn is never seen on client anyway.

(defmacro branch [& body] `(new Branch (e/fn [] ~@body)))

(e/def page-drop -1)
(e/def page-take -1)

(e/defn Paginate [xs]
  (if (coll? xs)
    (cond->> xs
      (pos-int? page-drop) (drop page-drop)
      (pos-int? page-take) (take page-take))
    xs))
