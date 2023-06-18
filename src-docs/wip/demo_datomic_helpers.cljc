(ns wip.demo-datomic-helpers
  (:require [missionary.core :as m]
            [hyperfiddle.electric :as e]))

(defn conn< [uri]
  (m/signal (m/observe (fn [!]
                         (let [!conn (d/connect uri)]
                           (! !conn)
                           #(d/release !conn))))))

(defn tx-reports> [conn] ; don't call twice
  (m/stream
    (m/ap
      (let [!q (m/?< (m/observe (fn [!] (! (d/tx-report-queue conn))
                                  #(d/remove-tx-report-queue conn))))]
        (loop []
          (m/amb (m/? (m/via m/blk (.take ^LinkedBlockingQueue !q)))
            (recur)))))))

(defn db< [conn]
  (m/signal (m/relieve {} (m/ap
                            (m/amb (d/db (m/?< conn))
                              (:db-after (m/?< (tx-reports> conn))))))))

(e/def txrq (new (tx-reports> conn)))

(e/defn Db [conn]
  (d/db conn)
  (:db-after txrq)
  )

#_
(e/defn Conn [uri]
  (let [conn (d/connect uri)]
    (e/on-unmount #(d/release conn))
    conn))

(e/def conn (Conn. uri))

; userland
(e/def conn (new (conn< uri)))
(e/def db (new (db< conn)))

;; user configurable latency and tx fail rate
#?(:clj (def !latency (atom 200)))
(e/def latency (e/server (e/watch !latency)))

#?(:clj (def !fail-rate (atom 1)))
(e/def fail-rate (e/server (e/watch !fail-rate)))

(e/defn Latency [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Latency: " latency "ms"))
    (ui/range latency (e/fn [v] (e/server (reset! !latency v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

(e/defn FailRate [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Fail Rate: " fail-rate " out of " max))
    (ui/range fail-rate (e/fn [v] (e/server (reset! !fail-rate v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

#?(:clj (defn slow-transact! "tx with configured latency and fail rate" [db tx]
          (m/sp
            (m/? (m/sleep @!latency))
            (if (< (rand-int 10) @!fail-rate)
              (throw (ex-info "tx failed" {:tx tx}))
              (d/with db (dbg/dbg :tx tx))
              #_@(d/transact !conn (dbg/dbg :tx tx))))))

#?(:clj (defn init-conn []
          (let [uri "datomic:mem://db"]
            (d/delete-database uri)
            (d/create-database uri)
            (let [conn (d/connect uri)]
              (d/transact conn schema)
              conn))))

(defonce !conn #?(:clj (init-conn) :cljs nil)) ; database on server
#?(:clj (comment (alter-var-root #'!conn (fn [_] (init-conn)))))
(e/def db) ; injected database ref; Electric defs are always dynamic
(e/def tx-report) ; for promoted tempids

(defonce !db #?(:clj (atom nil) :cljs nil))
(defonce !taker #?(:clj (future
                          (reset! !db (d/db !conn))
                          (let [q (d/tx-report-queue !conn)]
                            (loop []
                              (reset! !db (:db-after (.take ^java.util.concurrent.LinkedBlockingQueue q)))
                              (recur))))
                   :cljs nil))