(ns loam.core
  (:require
   [clojure.spec :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [om.dom :as dom]
   [om.next :as om #?(:clj  :refer
                      :cljs :refer-macros) [defui ui invariant]])
  #?(:clj (:import [clojure.lang ExceptionInfo])))


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

(defn dom-impl [tag]
  {:factory (partial create-element tag)})

(s/def ::component (s/and fn? om/iquery?))
(s/def ::factory fn?)
(s/def ::depends? fn?)
(s/def ::merge-query fn?)
(s/def ::region? boolean?)
(s/def ::ui-impl-ret
  (s/keys :req-un [::component ::factory]
          :opt-un [::depends? ::merge-query ::region?]))

(defmulti ui-impl (fn [tag node] tag))

(s/fdef ui-impl
  :args (s/cat :tag keyword? :node vector?)
  :ret ::ui-impl-ret)

(defn not-implemented-component [tag]
  (ui
    static om/IQuery
    (query [this]
      [:loam/id])
    Object
    (render [this]
      (apply dom/div #js {:data-id (:loam/id (om/props this))
                          :data-tag-not-implemented (str tag)}
             (om/children this)))))

(defmethod ui-impl :default [tag node]
  (let [component (not-implemented-component tag)]
    {:component component
     :factory (om/factory component)}))

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

(defn update-ex-data [^ExceptionInfo ex f & args]
  (ex-info #?(:clj (.getMessage ex)
              :cljs (ex-message ex))
           (apply f (ex-data ex) args)
           #?(:clj (.getCause ^ExceptionInfo ex)
              :cljs (ex-cause ex))))

(defn resolve-implementations [tree]
  (map #(walk/postwalk
         (fn [node]
           (if-let [tag (and (map? node)
                             (:tag node))]
             (if-not (ui-component? tag)
               (assoc node :impl (dom-impl tag))
               (try
                 (assoc node :impl (s/assert ::ui-impl-ret (ui-impl tag node)))
                 (catch ExceptionInfo ex
                   (throw (update-ex-data ex assoc ::tag tag)))))
             node))
         %)
       tree))

(defn collect-ui-infos [tree]
  (->> tree
       (mapcat (fn [node]
                 (tree-seq #(not (get-in % [:impl :region?]))
                           :children
                           node)))
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
                         [:loam/id])]
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

(defn region [resolved-tree]
  (let [child-uis (collect-ui-infos resolved-tree)
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
              (first (rendering-tree props resolved-tree)))))]
    {:component region-component
     :factory (om/factory region-component)
     :region? true}))

(defmethod ui-impl ::region [_ node]
  (region (:children node)))

(comment
  (def sample-1
    [:div
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

  (def sample-3
    [:main
     [::region {:loam/id ::sample-1} sample-1]
     [::region {:loam/id ::sample-2} sample-2]])

  (s/check-asserts true)
  
  (normalize-tree sample-2)

  (normalize-tree sample-3)

  (->> sample-1 normalize-tree resolve-implementations)

  (->> sample-3 normalize-tree resolve-implementations)
  
  (->> sample-3 normalize-tree resolve-implementations collect-ui-infos #_(map (juxt :tag :id)))

  (->> sample-3 normalize-tree resolve-implementations region :component om/get-query)

  (->> sample-3 normalize-tree resolve-implementations (rendering-tree {}))

  (-> sample-3 normalize-tree resolve-implementations region :factory (as-> f (dom/render-to-str (f))))

  (defmethod ui-impl :blueprint/auditor [tag _]
    (let [component (not-implemented-component tag)]
      {:dependent? (fn [ks props] (some ks [::my-editor]))
       :component component
       :factory (om/factory component)}))

  (include-dependent-keys
   '[(foo! {:bar false}) ::my-editor]
   {}
   (->> sample-1 normalize-tree resolve-implementations collect-ui-infos))
 )
