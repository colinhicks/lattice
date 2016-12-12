(ns specroll.lattice.alfa.engines.om-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.test.check.generators :refer [recursive-gen]]
            [clojure.spec.test :as stest]
            [om.next :as om :refer [defui ui]]
            [om.dom :as dom]
            [specroll.lattice.alfa.api :as api]
            [specroll.lattice.alfa.extensions :as extensions]
            [specroll.lattice.alfa.impl :as impl :refer [$->]]
            [specroll.lattice.alfa.engines.om]))


(require '[clojure.test.check]) ; clojure-emacs/cider#1841

(def dom-tag-gen
  (s/gen (set (map keyword dom/tags))))

(defn gen-tree
  ([] (gen-tree []))
  ([ui-tags]
   (recursive-gen
    (fn [inner]
      (gen/one-of
       [inner
        (gen/tuple dom-tag-gen (gen/one-of [inner (gen/string-ascii)]))
        (gen/tuple (gen/elements (into [:lattice/region] ui-tags))
                   (gen/fmap (fn [k]
                               {:lattice/id (keyword "test.gen.lattice.ui-id" k)})
                             (gen/such-that
                              #(not= % "")
                              (gen/string-alphanumeric)))
                   inner)]))
    (gen/tuple dom-tag-gen))))

(deftest region-gen-test
  (let [{:keys [total check-passed]}
        (stest/summarize-results
         (stest/check `api/region
                      {:gen {:specroll.lattice.specs/tree gen-tree}}))]
    (is (= total check-passed))))


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

  (->> sample-3 l/normalize-tree l/resolve-implementations)

  (->> sample-3 api/region)

  (->> sample-3
       (api/region {:lattice/id ::sample-3
                    :foo true})
       api/region-db)

  (->> sample-3 api/region :children (rendering-tree {}))

  (->> sample-3 api/region :impl :om-ui om/get-query)

  (-> sample-3 api/region :impl :factory (as-> f (dom/render-to-str (f))))

  (defmethod extensions/ui-impl :blueprint/auditor [tag]
    (let [om-ui (not-implemented-ui tag)]
      {:dependent? (fn [ks props] (some ks [::my-editor]))
       :om-ui om-ui
       :factory (om/factory om-ui)}))

  (include-dependent-keys
   '[(foo! {:bar false}) ::my-editor]
   {}
   (->> sample-1 api/region :impl :child-ui-nodes))


 )
