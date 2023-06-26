(ns . "retit example from @denik"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [stuffs.js-interop :as j]
            [reitit.core :as rr]
            [reitit.coercion :as rc]
            [reitit.frontend.easy :as rfe]
            [reitit.coercion.spec :as rss]))

(def router
  (/router
    [["/app"
      ["" :home]
      ["/sheet/:id" {:name       :sheet
                     :parameters {:path {:id int?}}}]
      ["/panel/:id" {:name       :panel
                     :parameters {:path {:id int?}}}]]]
    {:compile rc/compile-request-coercers
     :data    {:coercion rss/coercion}}))

(defn set-page-title! [route-match]
  (->> route-match :data :name (str "Tesserae ")
    (j/assoc! js/document :title)))

(e/def re-router
  (new (->> (m/observe (fn [!]
                         (rfe/start! router ! {:use-fragment false})
                         #()))
         (m/relieve {}))))

(def electric-main
  (e/boot
    (binding [dom/node (dom/by-id "root")]
      (let [{:as match :keys [data query-params path-params]} re-router]
        (binding [g/route-match match
                  g/route       (some-> data :name)]
          (set-page-title! match)
          (new views/App))))))

(p/defn Main []
  (binding [dom/node (dom/by-id "root")]
    (let [{:as match :keys [data query-params path-params]} re-router]
      (binding [d/route-match match
                d/route       (some-> data :name)]
        (set-page-title! match)
        (new views/App)))))

(ns . "retit example from @nakkaya")

#?(:cljs
   (def router
     (r/router
       [["/patients" ::patients]
        ["/add-patient" ::add-patient]
        ["/view-patient/{uuid}" ::view-patient]
        ["/view-radiology/{image-uuid}/{patient-uuid}" ::view-radiology]])))

#_:clj-kondo/ignore
(e/defn Page []
  (let [match (r/match-by-path router browser/path)]
    (if-let [view (get-in match [:data :name])]
      (let [params (:path-params match)]
        (condp = view
          ::patients       (Patients.)
          ::add-patient    (AddPatient.)
          ::view-patient   (ViewPatient. params)
          ::view-radiology (ViewRadiology. params)
          (Patients.)))
      (Patients.))))