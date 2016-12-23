(ns specroll.lattice.alfa.engines.om-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen
                :cljs cljs.spec.impl.gen) :as gen]
            [clojure.spec.test :as stest]
            #?@(:clj [[om.dom :as dom]]
                :cljs [[cljsjs.react]
                       [goog.object :as gobj]])
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [om.next :as om :refer [defui ui]]
            [om.next.protocols :as p]
            [specroll.lattice.specs :as $]
            [specroll.lattice.alfa.api :as api]
            [specroll.lattice.alfa.extensions :as extensions]
            [specroll.lattice.alfa.impl :as l]
            [specroll.lattice.alfa.engines.om :as engines.om]))



(comment
  (s/conform ::$/tree (gen/generate (fgen-tree)))
  
  (gen/generate (s/gen ::$/tree))
  
  )

#?(:clj ;; todo: figure out why node runs out of memory
   (deftest region-gen-test
     (let [{:keys [total check-passed]}
           (stest/summarize-results
            (stest/check `api/region {:clojure.spec.test.check/opts {:max-size 2}}))]
       (is (= total check-passed)))))

(defn pkg [region]
  (let [id->opts (api/index-ui-opts region)
        #?@(:cljs [test-renderer (js/React.addons.TestUtils.createRenderer)])
        reconciler (om/reconciler {:state id->opts
                                   :parser (om/parser {:read engines.om/parser-easy-read})
                                   #?@(:cljs [:root-render (fn [c _]
                                                             (.render test-renderer c))])})
        root-ui (engines.om/root-ui region)]
    {:region region-gen-test
     :id->opts id->opts
     :root-ui root-ui
     :reconciler reconciler
     #?@(:cljs [:test-renderer test-renderer])}))

(defn pkg-render [{:keys [reconciler root-ui test-renderer] :as pkg}]
  (let [c (om/add-root! reconciler root-ui :target)
        rendered #?(:clj (p/-render c)
                    :cljs (.getRenderOutput test-renderer))]
    (assoc pkg :rendered rendered)))

(defn pkg-props [{:keys [rendered] :as pkg}]
  #?(:clj (om/props rendered)
     :cljs (-> rendered
               (.-props)
               (om/get-props)
               (om/unwrap))))


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

  
  
  (->> sample-3 l/normalize-tree)

  (->> sample-3 api/region)

  (->> sample-3
       (api/region {:lattice/id ::sample-3
                    :foo true})
       api/index-ui-opts)

  (->> sample-1
       (api/region)
       api/index-ui-opts)
  
  (->> sample-3 api/region :children (rendering-tree {}))

  (->> sample-3 api/region :impl :om-ui om/get-query)

  (->> sample-3 api/region engines.om/join-query)

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
