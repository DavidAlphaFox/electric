(ns hyperfiddle.electric
  (:refer-clojure :exclude [eval def defn fn for empty? partial])
  (:require [clojure.core :as cc]
            [contrib.cljs-target :refer [do-browser]]
            [hyperfiddle.electric.impl.compiler :as c]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.impl.for :refer [map-by]]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])     ; todo remove
            [missionary.core :as m]
            #?(:cljs [hyperfiddle.electric-client])
            [hyperfiddle.electric.impl.io :as io]
            [hyperfiddle.electric.debug :as dbg])
  #?(:cljs (:require-macros [hyperfiddle.electric :refer [def defn fn boot for for-by local run debounce wrap on-unmount]]))
  (:import #?(:clj (clojure.lang IDeref IFn))
           (hyperfiddle.electric Pending Failure FailureInfo)
           (missionary Cancelled)))

;; Equality semantics for Failure and Pending
;; For JVM, defined in java class.
#?(:cljs
   (extend-type Pending
     IEquiv
     (-equiv [this other]
       (instance? Pending other))))

#?(:cljs
   (extend-type Failure
     IEquiv
     (-equiv [this other]
       (and (instance? Failure other)
            (= (.-error this) (.-error other))))))

#?(:cljs (set! (.. FailureInfo -prototype -__proto__) cljs.core/ExceptionInfo.prototype))
#?(:cljs
   (extend-type FailureInfo
     IEquiv
     (-equiv [this other]
       (and (instance? FailureInfo other)
            (= (.-cause this) (.-cause other))))))

#?(:clj
   (do
     ;; Optionally, tell RCF not to rewrite Electric programs.
     (defmethod ana/macroexpand-hook `hyperfiddle.electric/run [the-var form env args] (reduced `(hyperfiddle.electric/run ~@args))) ; optional
     ;;(defmethod ana/macroexpand-hook `hyperfiddle.electric/run2 [_the-var _form _env args] `(hyperfiddle.electric/run2 ~@args))

     ;; Don't expand cc/binding (prevent infinite loop). Explicit implicit do
     (defmethod ana/macroexpand-hook 'clojure.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))
     (defmethod ana/macroexpand-hook 'cljs.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))))

(def eval "Takes a resolve map and a program, returns a booting function.
  The booting function takes
  * as first argument a function Any->Task[Unit] returned task writes the value on the wire.
  * as second argument a flow producing the values read on the wire.
  and returning a task that runs the local reactor."
  r/eval)

(def hook r/hook)
(def bind r/bind) ; for when you want to spawn a e/fn without a new
(def with r/with)

(cc/defn pair [c s]
  (m/sp
    (let [s->c (m/dfv)
          c->s (m/dfv)]
      (m/?
        (m/join {}
          (s (cc/fn [x] (m/sp ((m/? s->c) x)))
            (cc/fn [!] (c->s !) #()))
          (c (cc/fn [x] (m/sp ((m/? c->s) x)))
            (cc/fn [!] (s->c !) #())
            #(throw %)))))))

(defmacro local
  "Single peer loopback system without whitelist. Returns boot task."
  [& body]
                                        ; use compiler (client) because no need for exports
  (let [[client server] (c/analyze &env `(do ~@body))]
    `(pair ~(r/emit (gensym) client) ~(r/emit (gensym) server))))

(defmacro run "test entrypoint without whitelist." [& body]
  `((local ~@body) (cc/fn [_#]) (cc/fn [_#])))

(cc/defn failure? [x] (instance? Failure x))

#?(:clj
   (cc/defn -offload [thunk executor]
     (->> (m/ap (m/? (m/via executor (thunk)))) ; run once
       (m/reductions {} (Failure. (Pending.)))
       (m/relieve {}))))

(defmacro offload
  "run a blocking function (i.e. query) on threadpool specified by `executor` (i.e. m/blk or m/cpu).
IO-bound fns should use m/blk, which is the default. Compute-bound fns should pass m/cpu. Custom
executors are allowed (i.e. to control max concurrency, timeouts etc). Currently JVM only."
  ([f! executor] `(new (-offload ~f! ~executor)))
  ; no varadic arity, user should explicitly state unit of work, so no ambiguity about concurrent tasks
  ([f!] `(new (-offload ~f! m/blk))))

(defmacro ^:deprecated wrap "Deprecated. Use `offload` instead." [& body] `(offload #(do ~@body)))

; Should these be in missionary?
;(def chan-read! contrib.missionary-contrib/chan-read!)
;(def chan->ap contrib.missionary-contrib/chan->ap)
;(def chan->task contrib.missionary-contrib/chan->task)
;(def chan->cp contrib.missionary-contrib/chan->cp)

(cc/defn task->cp ; leo to review
  ([!x] (task->cp !x (Failure. (Pending.)))) ; note Electric dependency
  ([!x pending] (->> (m/ap (m/? !x)) (m/reductions {} pending))))

; Moved to contrib.missionary-contrib
;(defmacro use-channel ;; TODO rename
;  ([chan] `(use-channel nil ~chan))
;  ([init chan] `(new (m/reductions {} ~init (chan->ap ~chan)))))

#?(:cljs
   (deftype Clock [^:mutable ^number raf
                   ^:mutable callback
                   terminator]
     IFn                                                    ; cancel
     (-invoke [_]
       (if (zero? raf)
         (set! callback nil)
         (do (.cancelAnimationFrame js/window raf)
             (terminator))))
     IDeref                                                 ; sample
     (-deref [_]
       ; lazy clock, only resets once sampled
       (if (nil? callback)
         (terminator)
         (set! raf (.requestAnimationFrame js/window callback))) ; RAF not called until first sampling
       ::tick)))

; cc def, must be above defmacro def
(def ^:no-doc <clock "lazy & efficient logical clock that schedules no work unless sampled"
  #?(:cljs (cc/fn [n t]
             (let [cancel (->Clock 0 nil t)]
               (set! (.-callback cancel)
                 (cc/fn [_] (set! (.-raf cancel) 0) (n)))
               (n) cancel))
     
     ; 120 hz server, careful this impacts bandwidth in demo-two-clocks
     ; typical UI animation rate is 60 or 120hz, no point in going higher 
     :clj (m/ap (loop [] (m/amb nil (do (m/? (m/sleep (/ 1000 120))) (recur)))))
     #_(m/ap (m/? (m/sleep 1 (m/?> (m/seed (repeat nil))))))))

;; --------------------------------------

(defmacro def
  ([symbol] `(hyperfiddle.electric/def ~symbol ::c/unbound))
  ([symbol docstring init]
   (assert (string? docstring))
   (#'def &form &env (vary-meta symbol assoc :doc docstring) init))
  ([symbol init]
   ;; GG: Expand to an unbound var with body stored in ::c/node meta.
   ;;     Clojure compiler will analyze vars metas, which would analyze form as clojure, so we quote it.
   ;;     ClojureScript do not have vars at runtime and will not analyze or emit vars meta. No need to quote.
   `(def ~(vary-meta symbol assoc ::c/node (if (:js-globals &env) init `(quote ~init))))))

(cc/defn -get-system-time-ms [& [_]] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))

; DOM event utilities promoted due to visibility-state being critical
#?(:cljs (cc/defn -listen [node typ f opts] (.addEventListener node typ f opts) #(.removeEventListener node typ f)))
#?(:cljs (cc/defn event* [node typ f! opts] ; f! is discrete
           (->> (m/observe (cc/fn [!] 
                             (! nil)
                             (-listen node typ #(-> % f! !) (clj->js opts))))
             (m/relieve {}))))

(def <dom-visibility-state #?(:cljs (do-browser (->> (event* js/document "visibilitychange" identity {})
                                                  (m/latest (cc/fn [_] (.-visibilityState js/document)))))))

(hyperfiddle.electric/def dom-visibility-state (client (new (identity <dom-visibility-state)))) ; starts Pending on server

(hyperfiddle.electric/def system-time-ms "ms since 1970 Jan 1" 
  (if (= "visible" dom-visibility-state)
    (new (m/sample -get-system-time-ms <clock))
    (throw (Pending.)))) ; tab is hidden, no clock. (This guards NPEs in userland)

(hyperfiddle.electric/def system-time-secs "seconds since 1970 Jan 1" (/ system-time-ms 1000.0))

(cc/defn -check-fn-arity! [name expected actual]
  (when (not= expected actual)
    (throw (ex-info (str "You called " (or name (pr-str ::unnamed-efn)) ", a " expected "-arg e/fn with " actual " arguments.")
             {:name name}))))

;; TODO self-refer
(defmacro fn [name? & [args & body]]
  (let [[name? args body] (if (symbol? name?) [name? args body]
                              [nil name? (cons args body)])]
    (if (bound? #'c/*env*)
      `(::c/closure
        ;; Beware, `do` is implemented with `m/latest`, which evaluates
        ;; arguments in parallel. The e/fn body will be called even if arity is
        ;; incorrect, then the arity exception will be thrown. This might be
        ;; confusing to users in presence of effects. Same as `(do (assert
        ;; false) (prn 42))`: 42 is printed anyway. This is a broader question
        ;; than "what should the semantics of e/fn should be", so we decided to
        ;; be consistent with the current model and to not introduce a specific
        ;; behavior for e/fn.
        (do (-check-fn-arity! '~name? ~(count args) c/%arity)
            (binding [c/rec (::c/closure (let [~@(interleave args c/arg-sym)] ~@body))]
              (new c/rec ~@(take (count args) c/arg-sym))))
        ~{::dbg/name name?, ::dbg/args args, ::dbg/type (or (::dbg/type (meta name?)) :reactive-fn)
          ::dbg/meta (merge (select-keys (meta &form) [:file :line])
                       (select-keys (meta name?) [:file :line]))})
      `(throw (ex-info "Invalid e/fn in Clojure code block (use from Electric code only)" ~(into {} (meta &form)))))))

; syntax quote doesn't qualify special forms like 'def
(defmacro defn [sym & fdecl]
  (let [[_defn sym' & _] (macroexpand `(cc/defn ~sym ~@fdecl))] ; GG: docstring support
    `(hyperfiddle.electric/def ~sym' (hyperfiddle.electric/fn ~(vary-meta sym' merge {::dbg/type :reactive-defn}
                                                                          (meta &form)
                                                                          (meta sym'))
                                                              ~@(if (string? (first fdecl)) ; GG: skip docstring
                                         (rest fdecl)
                                         fdecl)))))

(defmacro for-by [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    `(let [xs# ~v]
       (new (r/bind map-by ~kf
              ~(->> body
                 (list* `for-by kf bindings)
                 (list `let [s (first c/arg-sym)])
                 (list `fn [])
                 (list `cc/partial (list ::c/inject (first c/arg-sym))))
              (::c/lift xs#))))
    (cons `do body))) ; todo, buggy: (e/for [x []] (println 42)) should not print

(defmacro for [bindings & body]
  `(hyperfiddle.electric/for-by identity ~bindings ~@body))

(cc/defn ^:no-doc watchable? [x]
  #?(:clj (instance? clojure.lang.IRef x)
     :cljs (satisfies? IWatchable x)))

(cc/defn ^:no-doc checked-watch [!x]
  (assert (watchable? !x) "Provided argument is not Watchable.")
  (m/watch !x))

(def -invalid-watch-usage-message "Invalid e/watch (use from Electric code only, maybe you forgot a e/def?)")

(defmacro watch "for tutorials (to delay teaching constructor syntax); m/watch is also idiomatic"
  [!x]
  (if (bound? #'c/*env*)
    `(new (checked-watch ~!x))
    `(throw (ex-info -invalid-watch-usage-message ~(into {} (meta &form))))))

(cc/defn debounce-discreet
  ([delay flow] (debounce-discreet delay nil flow))
  ([delay init flow] (m/reductions {} init (m/ap (let [x (m/?< flow)]
                                                   (try (m/? (m/sleep delay x))
                                                        (catch Cancelled _ (m/amb))))))) )

(defmacro ^:deprecated debounce ; immoral? introduces avoidable delays
  "Debounce a continous flow by `delay` milliseconds."
  [delay flow]
  `(new (->> (fn [] ~flow)
             (debounce-discreet ~delay)
             (m/relieve {}))))

(cc/defn throttle [dur >in] ; in CLJ, wrong number of args (1) passed to: hyperfiddle.electric-ui4/long --- ?????
  (m/ap
    (let [x (m/?> (m/relieve {} >in))]
      (m/amb x (do (m/? (m/sleep dur)) (m/amb))))))

(defmacro ^:deprecated remote [& body]
  (if (= 1 (count body))
    `(unquote-splicing ~@body)
    `(unquote-splicing (do ~@body))))

(defmacro client [& body]
  (if (bound? #'c/*env*)
    `(::c/client (do ~@body) ~(assoc (meta &form) ::dbg/type :transfer, ::dbg/name ::client))
    `(throw (ex-info "Invalid e/client in Clojure code block (use from Electric code only)" ~(into {} (meta &form))))))

(defmacro server [& body]
  (if (bound? #'c/*env*)
    `(::c/server (do ~@body) ~(assoc (meta &form) ::dbg/type :transfer, ::dbg/name ::server))
    `(throw (ex-info "Invalid e/server in Clojure code block (use from Electric code only)" ~(into {} (meta &form))))))

(defmacro discard
  "Silence \"Unserializable reference transfer\"; inlining `(do ... nil)` is idiomatic as well"
  [& body] `(do ~@body nil))

(hyperfiddle.electric/def trace "In a `catch` block, bound by the runtime to the current stacktrace. An Electric stacktrace is an ExceptionInfo. Use `hyperfiddle.electric.debug/stack-trace` to get a string representation." nil)

(defmacro lexical-env "Return a map containing lexical bindings" []
  (let [env (if (:js-globals &env) (:locals &env) &env)]
    (with-meta
      `{:env ~(reduce-kv (cc/fn [r k v] (assoc r (list 'quote k) k)) (empty env) env)}
      (meta &form))))

(defmacro with-cycle
  "evaluates body with symbol s bound to the previous result of the body evaluation.
  the first evaluation binds s to i."
  [[s i] & body]
  `(let [a# (atom ~i) ~s (hyperfiddle.electric/watch a#)]
     (reset! a# (do ~@body))))

(defmacro partial-dynamic
  "Return a function calling given function `f` with given dynamic environment."
  [bindings f]
  `(cc/fn [& args#] (binding ~bindings (apply ~f args#))))

(defmacro partial
  "Like `cc/partial` for reactive functions. Requires the target function
  arity (`argc`) until reactive function supports variadic arguments.

  e.g. (new (partial 2 (e/fn [a b] [a b]) :a) :b) ;; => [:a :b]"
  [argc F & args]
  (if (= 0 argc)
    F
    (let [rest-args (map #(symbol (str "arg_" %)) (range (- argc (count args))))]
      `(let [F# ~F]
         (hyperfiddle.electric/fn ~@(when (symbol? F) [F]) [~@rest-args]
                                  (new F# ~@args ~@rest-args))))))

(defmacro on-unmount "Run clojure(script) thunk `f` during unmount.

  Standard electric code runs on mount, therefore there is no `on-mount`."
  [f] `(new (m/observe (cc/fn [!#] (!# nil) ~f)))) ; experimental

(cc/defn -on-cancel [f]
  (cc/fn [n t]
    (n)
    (reify
      IFn (#?(:clj invoke :cljs -invoke) [_] (f) (t))
      IDeref (#?(:clj deref :cljs -deref) [_]))))

(defmacro on-cancel [f] `(new (-on-cancel ~f)))

(defn ?PrintServerException [id]
  (try (server
         (when-some [ex (io/get-original-ex id)]
           (println ex)
           (try (client (println "server logged the root exception"))
                (catch Pending _ true)))) ; return true for `for-event`
       (catch Pending _ true)))           ; return true for `for-event`

(defmacro print-client-exception [ex]
  `(~(if (:ns &env) 'js/console.error 'prn) (str (ex-message ~ex) "\n\n" (dbg/stack-trace trace)) "\n\n"
    (-> trace dbg/ex-id io/get-original-ex (or ~ex))))

(defmacro with-zero-config-entrypoint [& body]
  `(try
     (do ~@body)
     (catch Pending _#)                 ; silently ignore
     (catch Cancelled e# (throw e#))    ; bypass catchall, app is shutting down
     (catch :default err#               ; note client bias
       (print-client-exception err#)
       (new ?PrintServerException (dbg/ex-id hyperfiddle.electric/trace)))))

(defmacro boot "
Takes an Electric program and returns a task setting up the full system with client part running locally and server part
running on a remote host.
" [& body]
  (assert (:js-globals &env))
  (let [[client server] (c/analyze
                          (assoc &env ::c/peers-config {::c/local :cljs ::c/remote :clj})
                          `(with-zero-config-entrypoint ~@body))]
    `(hyperfiddle.electric-client/reload-when-stale
      (hyperfiddle.electric-client/boot-with-retry
       ~(r/emit (gensym) client)
       (hyperfiddle.electric-client/connector (quote ~server))))))

;; WIP: user space socket reconnection

#_
(hyperfiddle.electric/def ^{:doc "
`true` if the main process was cancelled, `false` otherwise.
"} cancelled (new r/cancelled))

#_
(hyperfiddle.electric/def ^{:doc "
`true` if the link to the remote peer is up, `false` otherwise. May throw Pending during connection.
"} connected (new r/connected))

#_
(hyperfiddle.electric/defn Entrypoint [App]
  (or cancelled
    (not= :waiting
      (with-cycle [s {:status :connecting :delay 1000}]
        (try (App.) (catch :default e (.error js/console e)))
        (case (:status s)
          :waiting (let [{:keys [since delay]} s
                         remaining (-> since (+ delay) (- time))]
                     (if (pos? remaining)
                       (do (println (str "Retrying in " (int (/ remaining 1000)) "s.")) s)
                       (do (println "Connecting...")
                           (-> s
                             (dissoc :since)
                             (assoc :status :connecting)))))
          :connecting (try (if connected
                             (do (println "Connected.")
                                 (-> s
                                   (dissoc :delay)
                                   (assoc :status :connected)))
                             (do (println "Failed to reconnect.")
                                 (-> s
                                   (update :delay * 2)
                                   (assoc :status :waiting
                                          :since time))))
                           (catch Pending _ s))
          :connected (if connected
                       s (do (println "Connection reset.")
                             {:status :waiting
                              :since  time
                              :delay  1000})))))))

(def ^:dynamic *http-request* "Bound to the HTTP request of the page in which the current Electric program is running." nil)

(defmacro check-electric [fn form]
  (if (bound? #'c/*env*)
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))


;; https://github.com/weavejester/medley/blob/master/src/medley/core.cljc
;; https://clojure.atlassian.net/browse/CLJ-1451
(cc/defn take-upto [pred]
  (cc/fn [rf] (cc/fn ([] (rf))  ([ac] (rf ac))  ([ac nx] (cond-> (rf ac nx) (pred nx) ensure-reduced)))))

(cc/defn -snapshot [flow] (->> flow (m/eduction (take-upto (complement #{r/pending})))))
(defmacro snapshot "Snapshots the first non-pending value of `x` and stops updating afterwards."
  [x] `(check-electric snapshot (new (-snapshot (hyperfiddle.electric/fn [] ~x)))))

(cc/defn ->map [mbx]
  (->> mbx repeat m/seed m/?> m/? m/ap
    (m/reductions (cc/fn [ac [t k v]] (case t :assoc (assoc ac k v) :dissoc (dissoc ac k))) {})))

(cc/defn ->unique [] #?(:clj (Object.) :cljs (js/Object.)))

(defmacro for-event "Runs `body` for each value of missionary `>flow` bound to `sym`.

  When `body` returns a falsy value `for-event` unmounts it.
  Exceptions bubble up.
  Returns a vector of running `body` results.

  Useful to process a discrete event stream (e.g. DOM events) in Electric."
  [[sym >flow] & body]
  `(let [mbx# (m/mbx)]
     (new (m/reductions {} nil (m/eduction (map #(mbx# [:assoc (->unique) %])) ~>flow)))
     (for-by first [[k# ~sym] (new (->map mbx#))]
       (let [v# (do ~@body)] (if v# v# (do (mbx# [:dissoc k#]) nil))))))

(defmacro for-event-pending "Runs `body` for each value of missionary `>flow` bound to `sym`.

  `body` is running while it is Pending.
  Returns one of `[::idle]`, `[::ok value]`, `[::pending]` or `[::failed exception]`.

  Useful to process a discrete event stream (e.g. DOM events) in Electric."
  [[sym >flow] & body]
  `(let [mbx# (m/mbx), !state# (atom [::idle]), state# (e/watch !state#)]
     (new (m/reductions {} nil (m/eduction (map #(mbx# [:assoc (->unique) %])) ~>flow)))
     (if (seq (for-by first [[k# ~sym] (new (->map mbx#))]
                (try (case (reset! !state# [::ok (do ~@body)]) (mbx# [:dissoc k#]))
                     (catch hyperfiddle.electric.Pending ex# ::pending)
                     (catch missionary.Cancelled ex#)
                     (catch ~(if (:ns &env) :default `Throwable) ex# (reset! !state# [::failed ex#]) (mbx# [:dissoc k#])))))
       [::pending] state#)))

(defmacro for-event-pending-2 [bind & body]
  `(let [!state# (atom [::idle]), state# (e/watch !state#)]
     (if (seq (for-event ~bind
                (try (reset! !state# [::ok (do ~@body)]) false
                     (catch hyperfiddle.electric.Pending ex# true)
                     (catch missionary.Cancelled ex# false)
                     (catch ~(if (:ns &env) :default `Throwable) ex# (reset! !state# [::failed ex#]) false))))
       [::pending] state#)))

(defmacro for-event-pending-switch [[sym >flow] & body]
  `(let [!latest# (atom nil), latest# (e/watch !latest)]
     (e/for-event-pending [~sym (m/eduction (map #(reset! !latest# %)) ~>flow)]
       (when (= latest# e#) ~@body))))

;; (for1 [e (dom/listen "click")]
;;   ...)
(defmacro for-one-event [[sym >flow] & body]
  `(let [!e# (atom nil)]
     (if-some [~sym (e/watch !e#)]
       (if-let [v# (do ~@body)] v# (reset! !e# nil))
       (->> ~>flow (m/reductions #(reset! !e# %2) nil) new))))

(defmacro for-one-event-pending [bind & body]
  `(let [!state# (atom [::idle])]
     (for-one-event ~bind
       (try (reset! !state# [::ok (do ~@body)]) false
            (catch hyperfiddle.electric.Pending ex# (reset! !state# [::pending]) true)
            (catch missionary.Cancelled ex# false)
            (catch ~(if (:ns &env) :default `Throwable) ex# (reset! !state# [::failed ex#]) false)))
     (e/watch !state#)))
