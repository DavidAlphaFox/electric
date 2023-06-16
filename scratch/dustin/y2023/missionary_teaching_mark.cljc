(ns dustin.y2023.missionary-teaching-mark)

(def >x (m/observe (fn mount [emit!]
                     (emit! nil)
                     (.addEventListener node typ emit!)
                     (fn unmount []
                       (.removeEventListener node typ emit!)))))

(m/cp
  (if))

; Doc request - solid examples of js/electric bridging
; suspect 2-4 patterns for this, with nuance in each case

(e/defn foo []
  (let [event (new >x)
        value (-> e.-target .-value)]
    (if value
      (m/observe (fn mount [emit!]
                   (emit! nil)
                   (.addEventListener node typ emit!)
                   (fn unmount []
                     (.removeEventListener node typ emit!))))
      (m/observe (fn mount [emit!]
                   (emit! nil)
                   (.addEventListener node typ emit!)
                   (fn unmount []
                     (.removeEventListener node typ emit!)))))))

(def cancel! (>x println println))

(cancel!)




(defn Checkbox []
  (let [>count (m/reductions + 0 >v)

        >n (m/eduction (map inc) >v)

        <vs (m/relieve {} >v)



        #_#_vs (new (m/reductions conj [] >v))]))

({} :a 2)
(fn [acc v] v)