(ns colinhicks.lattice.alfa.api
  (:require [clojure.spec :as s]
            [colinhicks.lattice.alfa.impl :as l]
            [om.next :as om]))

(s/def :lattice/tag keyword?)
(s/def :lattice/opts (s/? (s/map-of keyword? any? :conform-keys true)))
(s/def :lattice/children (s/* #(or (string? %)
                             (s/coll-of :lattice/tree))))
(s/def :lattice/tree
  (s/cat :tag :lattice/tag
         :opts :lattice/opts
         :children :lattice/children))
(s/def :lattice/tree-node-unresolved (s/keys :req-un [:lattice/tag :lattice/opts :lattice/children]))
(s/def :lattice/om-ui (s/and fn? om/iquery?))
(s/def :lattice/factory fn?)
(s/def :lattice/depends? fn?)
(s/def :lattice/merge-query fn?)
(s/def :lattice/region? boolean?)
(s/def :lattice/dom-impl (s/keys :req-un [:lattice/factory]))
(s/def :lattice/ui-impl
  (s/merge :lattice/dom-impl
           (s/keys :req-un [:lattice/om-ui :lattice/factory]
                   :opt-un [:lattice/depends? :lattice/merge-query :lattice/region?])))
(s/def :lattice/region-ui-impl
  (s/merge :lattice/ui-impl
           (s/keys :req-un [:lattice/region? :lattice/child-ui-nodes :lattice/tree])))
(s/def :lattice/tree-node-resolved (s/keys :req-un [:lattice/tag
                                             :lattice/opts
                                             :lattice/children
                                             :lattice/ui-impl]))

(def ui-impl l/ui-impl)

(s/fdef ui-impl
  :args (s/cat :tag :lattice/tag)
  :ret :lattice/ui-impl)

(defn region [tree]
  (->> tree
       (l/normalize-tree)
       (l/resolve-implementations)
       (l/region*)))

(s/fdef region
  :args (s/cat :tree :lattice/tree)
  :ret :lattice/region-ui-impl)

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

  (require '[om.dom :as dom])
  
  (-> sample-3 region :factory (as-> f (dom/render-to-str (f))))

  (defmethod ui-impl :blueprint/auditor [tag]
    (let [om-ui (l/not-implemented-ui tag)]
      {:dependent? (fn [ks props] (some ks [::my-editor]))
       :om-ui om-ui
       :factory (om/factory om-ui)}))

  (l/include-dependent-keys
   '[(foo! {:bar false}) ::my-editor]
   {}
   (->> sample-1 region :child-ui-nodes))
 )
