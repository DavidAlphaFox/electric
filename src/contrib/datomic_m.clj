(ns contrib.datomic-m
  (:require [contrib.data :refer [omit-keys-ns]]
            [clojure.core.protocols :as ccp :refer [nav]]
            [clojure.datafy :refer [datafy]]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests tap %]]))

(defn dep-available? [qualified-sym]
  (some? (try @(requiring-resolve qualified-sym)
              (catch Exception e nil))))
(comment
  (dep-available? 'datomic.api/connect)
  (dep-available? 'datomic.client.api/connect)
  (dep-available? 'datomic.client.api.async/connect))

(defn detect-datomic-products []
  (->> ['datomic.api/connect
        'datomic.client.api/connect
        'datomic.client.api.async/connect]
       (filter dep-available?)
       (map (comp symbol namespace))
       set))

(comment
  (detect-datomic-products) := #{'datomic.api}
  ('datomic.api #{'datomic.api}) := datomic.api
  ('datomic.client.api #{'datomic.api}) := nil)

(def datomic-products (detect-datomic-products))

; if you have more than one on the classpath, you'll need to set this from userland
;(def ^:dynamic datomic-product (if (= 1 (count datomic-products)) (first datomic-products) nil))

(def tempid?)
(def client)
(def connect)
(def db)
(def db-stats)
(def entity)
(def touch)
(def pull)
(def pull-sorted)
(def datoms>)
(def tx-range>)
(def q)
(def query)
(def qseq)
(def history)

(defn install-datomic-onprem []
  (require 'contrib.datomic-peer-m)
  (alter-var-root #'tempid?     (constantly (eval 'contrib.datomic-peer-m/tempid?)))
  ; client
  (alter-var-root #'connect     (constantly (eval 'contrib.datomic-peer-m/connect)))
  (alter-var-root #'db          (constantly (eval 'contrib.datomic-peer-m/db)))
  (alter-var-root #'db-stats    (constantly (eval 'contrib.datomic-peer-m/db-stats)))
  (alter-var-root #'entity      (constantly (eval 'contrib.datomic-peer-m/entity)))
  (alter-var-root #'touch       (constantly (eval 'contrib.datomic-peer-m/touch)))
  (alter-var-root #'pull        (constantly (eval 'contrib.datomic-peer-m/pull)))
  (alter-var-root #'pull-sorted (constantly (eval 'contrib.datomic-peer-m/pull-sorted)))
  (alter-var-root #'datoms>     (constantly (eval 'contrib.datomic-peer-m/datoms>)))
  (alter-var-root #'tx-range>   (constantly (eval 'contrib.datomic-peer-m/tx-range>)))
  (alter-var-root #'q           (constantly (eval 'contrib.datomic-peer-m/q)))
  (alter-var-root #'query       (constantly (eval 'contrib.datomic-peer-m/query)))
  (alter-var-root #'qseq        (constantly (eval 'contrib.datomic-peer-m/qseq)))
  (alter-var-root #'history     (constantly (eval 'contrib.datomic-peer-m/history))))

(defn install-datomic-cloud []
  (require 'contrib.datomic-cloud-m)
  (alter-var-root #'tempid?     (constantly (eval 'contrib.datomic-cloud-m/tempid?)))
  (alter-var-root #'connect     (constantly (eval 'contrib.datomic-cloud-m/connect)))
  (alter-var-root #'client      (constantly (eval 'contrib.datomic-cloud-m/client)))
  (alter-var-root #'db          (constantly (eval 'contrib.datomic-cloud-m/db)))
  (alter-var-root #'db-stats    (constantly (eval 'contrib.datomic-cloud-m/db-stats)))
  ; entity
  ; touch
  (alter-var-root #'pull        (constantly (eval 'contrib.datomic-cloud-m/pull)))
  (alter-var-root #'pull-sorted (constantly (eval 'contrib.datomic-cloud-m/pull-sorted)))
  (alter-var-root #'datoms>     (constantly (eval 'contrib.datomic-cloud-m/datoms>)))
  (alter-var-root #'tx-range>   (constantly (eval 'contrib.datomic-cloud-m/tx-range>)))
  (alter-var-root #'q           (constantly (eval 'contrib.datomic-cloud-m/q)))
  (alter-var-root #'query       (constantly (eval 'contrib.datomic-cloud-m/query)))
  (alter-var-root #'qseq        (constantly (eval 'contrib.datomic-cloud-m/qseq)))
  (alter-var-root #'history     (constantly (eval 'contrib.datomic-cloud-m/history))))

(defn install-defs! []
  (cond
    (datomic-products 'datomic.api)
    (install-datomic-onprem)

    (datomic-products 'datomic.client.api.async)
    (install-datomic-cloud)))

(install-defs!)

(tests
  "datomic facade is installed"
  (take 3 (keys (m/? (pull user/db {:eid 50 :selector '[*]}))))
  := [:db/id :db/ident :db/valueType])
