(ns hyperfiddle.stage
  (:require [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]
            [wip.demo-datomic-helpers :refer [slow-transact!]]))

(defn transact!_ [old-tx-report <edit-monitor] ; Flow (List (x+Tx | nil))
  (m/ap
    (let [edits (->> (m/?< <edit-monitor) ; sparse
                  (remove nil?)) ; most fields are not firing concurrently
          [x tx] (m/?> 1 #_##Inf (m/seed edits))] ; serialize them?
      (m/amb
        #_(assoc old-tx-report ::pending tx) ; for what purpose? & do on client?
        (try
          (let [tx-report (m/? (slow-transact! conn dirty))]
            (assoc tx-report ::accepted tx))
          (catch Exception e
            (assoc old-tx-report ::rejected tx ::rejection e)))))))

(defn conj-edit [edits x] (conj edits x))

(defn cons-edit [x edits] (cons x edits))
