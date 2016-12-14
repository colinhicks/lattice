(ns specroll.lattice.alfa.engines.om
  (:require [clojure.spec :as s]
            [clojure.pprint :as pprint]
            [specroll.lattice.specs :as $]
            [specroll.lattice.alfa.impl :as l]
            [specroll.lattice.alfa.extensions :as extensions]
            [om.dom :as dom]
            [om.next :as om :refer [ui]]))


(s/def ::$/om-ui (s/and fn? om/iquery?))

(s/def ::$/depends? fn?)

(s/def ::$/merge-query fn?)

(s/def ::$/impl
  (s/merge ::$/base-impl
           (s/keys :req-un [::$/om-ui]
                   :opt-un [::$/depends? ::$/merge-query])))

(s/def ::$/region-ui-impl
  (s/merge ::$/impl
           (s/keys :req-un [::$/region?])))

(defn create-element
  ([tag]
   (create-element tag nil))
  ([tag opts & children]
   #?(:clj (dom/element {:tag tag
                         :attrs (dissoc opts :ref :key)
                         :react-key (:key opts)
                         :children children})
      :cljs (js/React.createElement tag opts children))))

(defmethod extensions/dom-impl :default [tag]
  {:factory (partial create-element (name tag))})

(defn not-implemented-ui [tag]
  (ui
    static om/IQuery
    (query [this]
      [:lattice/id])
    Object
    (render [this]
      (apply dom/div #js {:data-id (:lattice/id (om/props this))
                          :data-tag-not-implemented (str tag)}
             (om/children this)))))

(defmethod extensions/ui-impl :default [tag]
  (let [om-ui (not-implemented-ui tag)]
    {:om-ui om-ui
     :factory (om/factory om-ui)}))

(defn collect-query [nodes]
  (into [:lattice/id :lattice/resolved-tree]
        (map (fn [{:keys [tag opts impl]}]
               (let [{:keys [om-ui merge-query]} impl
                     q (if om-ui
                         (let [q* (om/get-query om-ui)]
                           (if merge-query
                             (merge-query q* opts)
                             q*))
                         [:lattice/id])]
                 {[(:lattice/id opts) '_] q})))
        nodes))

(defn rendering-tree [props tree]
  (map (fn [node]
         (if-not (map? node)
           node
           (let [{:keys [tag opts children impl]} node
                 {:keys [factory]} impl
                 {:keys [lattice/id]} opts]            
             (if id
               (factory #?(:clj (get props id)
                           :cljs (clj->js (get props id)))
                        (rendering-tree props children))
               (factory #?(:clj opts
                           :cljs (clj->js opts))
                        (rendering-tree props children))))))
       tree))

(defn include-dependent-keys [tx props components]
  (let [ast (om/query->ast tx)
        tx-reads (into #{}
                       (keep #(when (= :prop (:type %))
                                (:key %)))
                       (:children ast))
        dependent-reads (keep (fn [{:keys [opts impl]}]
                                (let [id (:lattice/id opts)
                                      dependent? (get impl :dependent? (constantly false))]
                                  (when (dependent? tx-reads (get props id))
                                    id)))
                              components)]
    (-> ast
        (update :children into
                (map (fn [k]
                       {:type :prop :dispatch-key k :key k})
                     dependent-reads))
        (om/ast->query))))

(defn region* [init-resolved-tree]
  (let [init-child-nodes (l/collect-ui-nodes init-resolved-tree)
        init-query (collect-query init-child-nodes)
        region-component
        (ui
          static om/Ident
          (ident [this props]
            [(:lattice/id props) '_])
          static om/IQuery
          (query [this]
            init-query)
          om/ITxIntercept
          (tx-intercept [this tx]
            (let [{:keys [lattice/resolved-tree] :as props} (om/props this)
                  child-nodes (if resolved-tree
                                (l/collect-ui-nodes resolved-tree)
                                init-child-nodes)]
              (include-dependent-keys tx (om/props this) child-nodes)))
          Object
          (render [this]
            (let [{:keys [lattice/resolved-tree] :as props} (om/props this)
                  resolved-tree' (or resolved-tree init-resolved-tree)]
              (first (rendering-tree props resolved-tree')))))]
    {:om-ui region-component
     :factory (om/factory region-component)
     :region? true
     :child-ui-nodes init-child-nodes}))

(defmethod extensions/region-ui-impl :lattice/region [_ node]
  (region* (:children node)))

(defn template [{:keys [opts children]}]
  (let [{:keys [lattice.tmpl/spec]} opts]
    {:region? true
     :spec spec}))

(defmethod extensions/region-ui-impl :lattice/tmpl [_ node]
  (template node))


(comment 
  (let [tree (-> [:lattice/tmpl {:lattice/id ::tmpl-one :lattice.tmpl/spec :z/foo}
                  '[:section
                    [foo/bar [:div {:className foo.bar/class-name} foo.bar/text]]
                    [foo/baz [:aside foo.baz/text1 [:span foo.baz/text2]]]
                    [foo/dne [:span "does not exist"]]]]
                 (l/normalize-tree)
                 (l/resolve-implementations))
        spec (-> tree first :opts :lattice.tmpl/spec)]
    (-> [{:foo.bar/class-name "hello" :foo.bar/text "world"}
         {:foo.baz/text1 "baz1" :foo.baz/text2 "baz2"}]
        (as-> x (s/conform spec x))
        (l/replace-tree-syms (:children (first tree)))))


  (s/def :foo.bar/class-name string?)
  (s/def :foo.bar/text string?)
  (s/def :foo.baz/text1 string?)
  (s/def :foo.baz/text2 string?)
  
  (s/def :z/foo
    (s/cat :foo/bar (s/keys :req [:foo.bar/class-name :foo.bar/text])
           :foo/baz (s/keys :req [:foo.baz/text1 :foo.baz/text2])))
  
  
  )
