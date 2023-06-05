(ns contrib.identity
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn ->Object "allocate a process-unique identity that cannot collide" []
  #?(:clj (Object.) :cljs (js/Object.)))

(def tempid? (some-fn nil? string?))

(defn genesis-tempid! [db] (str "hyperfiddle-tempid-" (random-uuid)))

(defn upsert-id! 
  ([!seen e] ; todo concurrency?
   (let [heap @!seen]
     (if (contains? heap e)
       (get heap e)
       (let [ref (->Object)]
         (swap! !seen assoc e ref)
         ref))))
  ([!seen e e-prev]
   (let [heap @!seen
         ref-prev (get heap e-prev)]
     (assert ref-prev)
     (or (get heap e)
       (do (swap! !seen assoc e ref-prev) ref-prev)))))

(tests
  (def !seen (atom {}))
  (upsert-id! !seen "tempid-1") := ?a
  (upsert-id! !seen "tempid-1") := ?a
  (upsert-id! !seen "tempid-2") := ?b
  (upsert-id! !seen 123) := ?c
  (upsert-id! !seen 123) := ?c
  (upsert-id! !seen 124) := ?d
  (upsert-id! !seen "tempid-1") := ?a
  (upsert-id! !seen "tempid-2") := ?b)

(defn entity-id-locally-stabilzied!
  "manifest a unified entity identity which is stabilized across tempid promotions, 
i.e. make sure promoted tempids reuse the same identity if it had been 
previously seen."
  [!ids tx-report record]
  (let [e (:db/id record)]
    (case (tempid? e)
      true (upsert-id! !ids e)
      false (if-some [e' ((:ids->tempids tx-report) e)] ; tempid was promoted 
              (upsert-id! !ids e e') ; unify with previously seen identity 
              (upsert-id! !ids e)))))

(tests
  (def !ids (atom {}))
  (entity-id-locally-stabilzied! !ids db "tempid-1") := ?a
  (entity-id-locally-stabilzied! !ids db "tempid-1") := ?a
  (entity-id-locally-stabilzied! !ids db "tempid-2") := ?b
  (entity-id-locally-stabilzied! !ids db 123) := ?c
  ; transact, tempid-1 promoted to 124
  (entity-id-locally-stabilzied! !ids db 124) := ?a
  (entity-id-locally-stabilzied! db "tempid-1") := ?a ; for how long is this valid? Can the tempid be reused?
  (entity-id-locally-stabilzied! !ids db 123) := ?b)