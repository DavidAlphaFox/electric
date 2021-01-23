(ns dustin.trace29
  (:require [dustin.trace28 :refer [amb= from-trace! gen-trace let-bindings trace! translate-binding]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(def extended-fmap-ast '(let [>a (input)
                              >b (input)
                              >c (fmap clojure.core/+ >a >b)]))


;; * Junction points
;;
;;   |                  |                  | Compute where   | Trace  |             |
;;   |------------------+------------------+-----------------+--------+-------------|
;;   | Server active    | Client active    | compute both    | none   |             |
;;   | Server active    | Client passive   | compute server  | server |             |
;;   | Server passive   | Client active    | compute client  | client |             |
;;   | +Server passive+ | +Client passive+ | +don’t compute+ | +none+ | <- Nonsense |
;;
;;   We suppose inputs and outputs got pre-computed (analysed from ast) before
;;   the compilation phase. The compiler receives:
;;   - ast
;;   - list of passive nodes (inputs)
;;   - list of traced nodes (outputs)

(defn compile-functor [ast input-registry passives traced >replayer tracef]
  (let [bindings     (let-bindings ast)
        syms         (distinct (map first bindings)) ;; shadowing
        new-bindings (mapcat (partial translate-binding input-registry passives >replayer) bindings)]
    `(fn [~input-registry ~>replayer ~tracef]
       (let [~@new-bindings]
         ~(gen-trace tracef (filter (set (keys traced)) syms))))))

(tests
 (def server (compile-functor extended-fmap-ast
                              'inputs
                              {'>a ['>a]
                               '>b ['>b]}
                              {'>c ['>c]}
                              '>replayer
                              'tracef))
 server
 :=
 `(fn [~'inputs ~'>replayer ~'tracef]
    (let [~'>a (from-trace! '[~'>a] ~'>replayer)
          ~'>b (from-trace! '[~'>b] ~'>replayer)
          ~'>c (m/signal! (m/latest + ~'>a ~'>b))]
      (trace! ~'tracef (m/stream! (m/relieve merge (m/ap (amb= {[~'>c] (m/?? ~'>c)})))))))

 (def client (compile-functor extended-fmap-ast
                              'inputs
                              {'>c ['>c]}
                              {'>a ['>a]
                               '>b ['>b]}
                              '>replayer
                              'tracef))
 client
 :=
 `(fn [~'inputs ~'>replayer ~'tracef]
    (let [~'>a (m/signal! (m/watch (get ~'inputs ~'>a)))
          ~'>b (m/signal! (m/watch (get ~'inputs ~'>b)))
          ~'>c (from-trace! '[~'>c] ~'>replayer)]
      (trace! ~'tracef (m/stream! (m/relieve merge (m/ap (amb= {[~'>a] (m/?? ~'>a)}
                                                               {[~'>b] (m/?? ~'>b)})))))))
 )


;; * Questions [0/3]
;; ** TODO Are anonymous nodes traced in all cases?
;;    Or are they traced only if their ancestors are?
;; ** TODO Is the `(input)` indirection anemic?
;;    If some function `f` is `clj` or `cljs` only, then used as `(let [>x
;;    (f)]…)` implies `f` is an input. If it can be resolved in clj but not in
;;    cljs, then it’s active on the server and passive on the client.
;;
;;    #+begin_src clojure
;;    (def flux `(let [>needle (watch !needle)
;;                     >result (fmap search >needle)]))
;;    #+end_src
;;
;;    Then, we specify to the compiler that `>needle` is passive on the server,
;;    so it will come from the trace.
;;
;; ** TODO Are replay! and directive! the same?
;;    If `(input)` is an anemic indirection, which seems to be the case, then
;;    `directive!` is an external concept to the flow. If you declare an input
;;    as a DOM `<input>` for instance, the directive should talk about the DOM
;;    node, not the AST.
