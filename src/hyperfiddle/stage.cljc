(ns hyperfiddle.stage
  (:require [datascript.core :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]
            [wip.demo-datomic-helpers :refer [slow-transact!]]))


; Edits happen one at a time
; Backpressure is relieved, which means edits can be merged/batched
; If sampling rate is low, edits are so batched
; If a popover branches, edits can be batched (merged into single txn)

; Edits start dirty, which renders the field as dirty
; Dirty edits are transacted by the entrypoint
; Before transacting, dirty edits are promoted to pending
; Pending edits become either ::completed or ::failed per transaction result

; Dirty edits are locally optimistic (field-local)
; Pending edits are desired to be globally optimistic, but list-local in practice

; Complex edits
; edits may span multiple records, this is hard to apply optimistically?


; In Google Sheets mode, transactions are isolated and race.
; In CRUD mode, popovers are introduced to manage batching.

; are they isolated or should we batch them here?
; the popover merges them; this is parallel edits.
; By the time they are seen here they are already batched
; which means there is only ever one edit in this list
; if there are two it means the system is lagging or the user
; is a robot and can type in multiple widgets simultaneously, or
; faster than the system's sampling rate

(defn transact!_ [old-tx-report <txs] ; Flow (List Tx)
  (m/ap
    ; what if tx reports are seen out of order due to the race?
    (let [tx (m/?> 1 #_##Inf (m/seed (m/?< <txs)))] ; serialize them?
      (m/amb
        #_(assoc old-tx-report ::pending tx) ; for what purpose? & do on client?
        (try
          (let [tx-report (m/? (slow-transact! conn dirty))]
            (assoc tx-report ::accepted tx))
          (catch Exception e
            (assoc old-tx-report ::rejected tx)))))))
