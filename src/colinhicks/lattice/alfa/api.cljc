(ns colinhicks.lattice.alfa.api
  (:require [clojure.spec :as s]
            [colinhicks.lattice.alfa.impl :as l]
            [om.next :as om]))

(s/def ::tag keyword?)
(s/def ::opts (s/? (s/map-of keyword? any? :conform-keys true)))
(s/def ::children (s/* #(or (string? %)
                             (s/coll-of ::tree))))
(s/def ::tree
  (s/cat :tag ::tag
         :opts ::opts
         :children ::children))
(s/def ::tree-node-unresolved (s/keys :req-un [::tag ::opts ::children]))
(s/def ::component (s/and fn? om/iquery?))
(s/def ::factory fn?)
(s/def ::depends? fn?)
(s/def ::merge-query fn?)
(s/def ::region? boolean?)
(s/def ::ui-impl
  (s/keys :req-un [::component ::factory]
          :opt-un [::depends? ::merge-query ::region?]))
(s/def ::tree-node-resolved (s/keys :req-un [::tag ::opts ::children ::ui-impl]))

(def ui-impl l/ui-impl)

(s/fdef ui-impl
  :args (s/cat :tag ::tag
               :node ::tree-node-unresolved)
  :ret ::ui-impl)

(defn region [tree]
  (->> tree
       (l/normalize-tree)
       (l/resolve-implementations)
       (l/region*)))

(s/fdef region
  :args (s/cat :tree ::tree)
  :ret ::ui-impl)

(defmethod ui-impl :lattice/region [_ node]
  (l/region* (:children node)))

(comment
  (def sample-1
    [:div
     [:section
      [:span {:className "label"} "Editor label"]
      [:blueprint/editor {:lattice/id ::my-editor}]
      [:div {:className "arbitrary-nesting"}
       [:blueprint/graph {:lattice/id ::my-graph}
        [:strong "nested in component"]]]]
     [:footer {:className "static"}
      [:blueprint/auditor {:lattice/id ::my-auditor}]]])

  (def sample-2
    [:blueprint/auditor {:lattice/id ::my-auditor}])

  (def sample-3
    [:main
     [:lattice/region {:lattice/id ::sample-1} sample-1]
     [:lattice/region {:lattice/id ::sample-2} sample-2]])

  
  (->> sample-3 region :tree (l/rendering-tree {}))

  (->> sample-3 region :component om/get-query)

  (require '[om.dom :as dom])X
  
  (-> sample-3 region :factory (as-> f (dom/render-to-str (f))))

  (defmethod ui-impl :blueprint/auditor [tag _]
    (let [component (l/not-implemented-component tag)]
      {:dependent? (fn [ks props] (some ks [::my-editor]))
       :component component
       :factory (om/factory component)}))

  (l/include-dependent-keys
   '[(foo! {:bar false}) ::my-editor]
   {}
   (->> sample-1 region :child-ui-nodes))
 )
