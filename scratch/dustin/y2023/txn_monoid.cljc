(ns dustin.y2023.txn-monoid 
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui5 :as ui5]
            [hyperfiddle.api :as hf]
            [contrib.datomic-m :as d]))

; Goals:
; combine two txns with good syntax (dom, do, let)
; txn value available on both client and server in parallel, no accidental transfer

; Constraint: Assume Electric is able to eliminate dumb transfers, what does it look like?
; Problem: no vectors, must use separate bindings for tuples to avoid constructing the vector on the client only

; Simplify: we can eliminate the tx notation, just use the values.

; Proposition: can we assume all events flow to the server asap, therefore txn concat is always on server?
; client doesn't need to see the txn, only the optimistic value. Is the optimistic value the edge case?

; Optimistic value is create-new, it is indeed coordinated on the client.

; apply concat vdv
;   v is on the client, MUST be for optmism in current design
;   dv is on the server, never needed on client

; Idea - rewrite to Field into CPS to control return value locality without using Electric's return channel at all.

(e/defn Field []
  (e/server
    (let [v-domain (get record a)
          v-physical (parse v-domain)

            ; this transfers v'-physical twice, but it's likely not noticable 
            ; (but the point here is rigorous data safety, which this violates)
          v'-physical (e/client (e/with-cycle [status [::init (e/server v-physical)]]
                                  (let [v'-physical (Control. v-physical status)]
                                    (try [::ok (e/server v'-physical)] ; never even seen, as the db will update and shadow this with the same value
                                      (catch Pending _ [::pending v'-physical])))))

          v'-domain (unparse v'-physical)
          {:keys [txn optimistic] :as vdv} (txn v'-domain)]

      (if-not txn ; in create-new case, we don't have a txn (due to no ID until popover submit)
        vdv ; but we have the perfect optimistic record already!
        (let [{:keys [db tx-report]} (hf/Transact!. field-tx)] ; impacts this branch only; even needed?
          vdv))

        ; still send both v and dv up to top? why?
        ; because at higher levels we may concat in more txn at form level e.g. create-new-todo
        ; they will have branched in this case, causing the above to be local.
      vdv)))

(e/defn Field
  [{:keys [record ; record on server except create-new
           a Control parse unparse txn]}]
  (e/server
    (binding [!v'-server (atom nil)] ; setup side channel
      (let [v-domain (get record a)
            v-physical (parse v-domain)
            _v'-physical (Control. v-physical) ; optimistic, can it rollback? ; monitor progress here? 
            v'-physical (e/watch !v'-server) ; weird trick - the InputController owns the sync
            ; otherwise, progress is monitored right here, which is the wrong place. (or is it? - yes due
            ; to stabilizing concurrent edits inside the control.)
            v'-domain (unparse v'-physical)
            {:keys [txn optimistic] :as vdv} (txn v'-domain)]

        (if-not txn ; in create-new case, we don't have a txn (due to no ID until popover submit)
          vdv ; but we have the perfect optimistic record already!
          (let [{:keys [db tx-report]} (hf/Transact!. field-tx)] ; impacts this branch only; even needed?
            vdv))

        ; still send both v and dv up to top? why?
        ; because at higher levels we may concat in more txn at form level e.g. create-new-todo
        ; they will have branched in this case, causing the above to be local.
        vdv))))

(e/defn Page [[a b]] ; server bias
  ; (e/server (let [xs (todo-records db)]))
  (e/client
    (dom/div
      (apply concat-vdv ; not sure if on server or client
        (dom/div (dom/text "markup above"))
        (e/server (let [v (ui5/Checkbox. a)
                        dv [:db/add 1 :task/status v]]
                    ))
        (e/server (let [v (ui5/Input. b) ; client, used for create new
                        dv [:db/add 2 :task/description v]] ; server only
                    ))
        (dom/div (dom/text "markup below"))))))

#?(:clj (def !db (atom [true true])))

(e/defn App []
  (e/server
    (binding [db (e/watch !db)]
      (let [vdv (Page. db)] ; vdv on server and client
        (e/client (println vdv))
        (e/server (println vdv))))))

(e/def return-client)
(e/def return-server)

(e/client
  (binding [return-client .]
    (e/server
      (binding [return-server .]
        (e/client
          (do
            (DomInputController.) ; return by side channel effect
            (let [a return-client]
              )
            (e/server (let [b return-server]
                ))))))))