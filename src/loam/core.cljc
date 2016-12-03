(ns loam.core
  (:require
   [clojure.spec :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [om.dom :as dom]
   [om.next :as om #?(:clj  :refer
                      :cljs :refer-macros) [defui ui invariant]]))


;; todo: expect om.dom/create-element in >alpha47
(defn create-element
  ([tag]
   (create-element tag nil))
  ([tag opts & children]
   #?(:clj (dom/element {:tag tag
                         :attrs (dissoc opts :ref :key)
                         :react-key (:key opts)
                         :children children})
      :cljs (js/React.createElement tag opts children))))

(defn ui-component? [tag]
  (boolean (namespace tag)))

(defn generic-dom-impl [tag]
  {:factory (partial create-element tag)})

(s/def ::component om/component?)
(s/def ::factory fn?)
(s/def ::depends? fn?)
(s/def ::merge-query fn?)
(s/def ::ui-info-impl (s/keys :req-un [::component ::factory]
                              :opt-un [::depends? ::merge-query?]))

(defmulti ui-impl (fn [tag] tag))

(defmethod ui-impl :default [tag]
  (let [not-implemented-ui
        (ui
          static om/IQuery
          (query [this]
            [:loam/id])
          Object
          (render [this]
            (apply dom/div #js {:data-id (:loam/id (om/props this))
                                :data-tag-not-implemented (str tag)}
                   (om/children this))))]
    {:component not-implemented-ui
     :factory (om/factory not-implemented-ui)}))

(defn normalize-tree [raw-tree]
  (map (fn [node]
         (if (string? node)
           node
           (let [[tag maybe-opts & children] node
                 opts (when (map? maybe-opts) maybe-opts)
                 children' (if-not opts
                             (into [maybe-opts] children)
                             children)]
             {:tag tag
              :opts opts
              :id (when (ui-component? tag) (:loam/id opts))
              :children (mapcat normalize-tree children')})))
       [raw-tree]))

(defn resolve-implementations [tree]
  (map #(walk/postwalk
         (fn [node]
           (if-let [tag (and (map? node)
                             (:tag node))]
             (if (ui-component? tag)
               (assoc node :impl (ui-impl tag))
               (assoc node :impl (generic-dom-impl tag)))
             node))
         %)
   tree))

(defn collect-ui-infos [tree]
  (->> tree
       (mapcat #(tree-seq (constantly true) :children %))
       (filter :id)))

(defn collect-query [ui-infos]
  (into []
        (map (fn [{:keys [id tag opts impl]}]
               (let [{:keys [component merge-query]} impl
                     q (if component
                         (let [q* (om/get-query component)]
                           (if merge-query
                             (merge-query q* opts)
                             q*))
                         [:id])]
                 {[id '_] q})))
        ui-infos))

(defn rendering-tree [props tree]
  (map (fn [node]
         (if-not (map? node)
           node
           (let [{:keys [tag id opts children impl]} node
                 {:keys [factory]} impl]
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
        dependent-reads (keep (fn [{:keys [id impl]}]
                                (when-let [dependent? (:dependent? impl)]
                                  (when (dependent? tx-reads (get props id))
                                    id)))
                              components)]
    (-> ast
        (update :children into
                (map (fn [k]
                       {:type :prop :dispatch-key k :key k})
                     dependent-reads))
        (om/ast->query))))

(defn region [raw-tree]
  (let [tree (->> raw-tree
                  (normalize-tree)
                  (resolve-implementations))
        child-uis (collect-ui-infos tree)
        query (collect-query child-uis)
        region-component
        (ui
          static om/IQuery
          (query [this]
            query)
          om/ITxIntercept
          (tx-intercept [this tx]
            (include-dependent-keys tx (om/props this) child-uis))
          Object
          (render [this]
            (let [props (om/props this)]
              (first (rendering-tree props tree)))))]
    {:component region-component
     :factory (om/factory region-component)
     :child-uis child-uis}))


(comment
  (def sample-1
    [:main {:id "my-region1"}
     [:section
      [:span {:className "label"} "Editor label"]
      [:blueprint/editor {:loam/id ::my-editor
                          :blueprint.editor/default-input [[:a :b] [:b :c]]
                          :blueprint.evaluations/init :blueprint.editor/default-input}]
      [:div {:className "arbitrary-nesting"}
       [:blueprint/graph {:loam/id ::my-graph
                          :loam/link {:editor [[:blueprint/evaluations ::my-editor]]}
                          :loam/query-params {:editor ::my-editor}}
        [:strong "nested in component"]]]]
     [:footer {:className "static"}
      [:blueprint/auditor {:loam/id ::my-auditor
                           :loam/link {::my-editor [[:blueprint/evaluations ::my-editor]]}}]]])

  (def sample-2
    [:blueprint/auditor {:loam/id ::my-auditor
                         :loam/merge-query {::my-editor [[:blueprint/evaluations ::my-editor]]}
                         :loam/query-params {:audit [::my-editor]}}])

  (normalize-tree sample-2)

  (normalize-tree sample-1)

  (->> sample-1 normalize-tree flatten-components)

  (->> sample-1 normalize-tree resolve-implementations)

  (->> sample-1 normalize-tree resolve-implementations flatten-components)

  (om/get-query (:component (region sample-1)))

  (rendering-tree
   {}
   (->> sample-1 normalize-tree resolve-implementations))

  (dom/render-to-str ((:factory (region sample-1))))

  (defmethod ui-impl :blueprint/auditor [tag]
    {:dependent? (fn [ks props] (some ks [::my-editor]))
     :factory (fn [props & children]
                (apply dom/pre #js {:data-not-implemented (str tag)} children))})

  (include-dependent-keys
   '[(foo! {:bar false}) ::my-editor]
   {}
   (->> sample-1 normalize-tree resolve-implementations flatten-components))

 )
