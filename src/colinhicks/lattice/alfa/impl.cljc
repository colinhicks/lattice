(ns colinhicks.lattice.alfa.impl
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

(defn dom-impl [tag]
  {:factory (partial create-element tag)})


(defmulti ui-impl (fn [tag node] tag))

(defn not-implemented-component [tag]
  (ui
    static om/IQuery
    (query [this]
      [:lattice/id])
    Object
    (render [this]
      (apply dom/div #js {:data-id (:lattice/id (om/props this))
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
              :children (mapcat normalize-tree children')})))
       [raw-tree]))

(defn resolve-implementations [tree]
  (map #(walk/postwalk
         (fn [node]
           (if-let [tag (and (map? node)
                             (:tag node))]
             (if-not (ui-component? tag)
               (assoc node :ui-impl (dom-impl tag))
               (assoc node :ui-impl (ui-impl tag node)))
             node))
         %)
       tree))

(defn collect-ui-nodes [tree]
  (->> tree
       (mapcat (fn [node]
                 (tree-seq #(not (get-in % [:ui-impl :region?]))
                           :children
                           node)))
       (filter #(when-let [tag (:tag %)]
                  (ui-component? tag)))))

(defn collect-query [nodes]
  (into []
        (map (fn [{:keys [tag opts ui-impl]}]
               (let [{:keys [component merge-query]} ui-impl
                     q (if component
                         (let [q* (om/get-query component)]
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
           (let [{:keys [tag opts children ui-impl]} node
                 {:keys [factory]} ui-impl
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
        dependent-reads (keep (fn [{:keys [opts ui-impl]}]
                                (let [id (:lattice/id opts)
                                      dependent? (get ui-impl :dependent? (constantly false))]
                                  (when (dependent? tx-reads (get props id))
                                    id)))
                              components)]
    (-> ast
        (update :children into
                (map (fn [k]
                       {:type :prop :dispatch-key k :key k})
                     dependent-reads))
        (om/ast->query))))

(defn region* [resolved-tree]
  (let [child-nodes (collect-ui-nodes resolved-tree)
        query (collect-query child-nodes)
        region-component
        (ui
          static om/IQuery
          (query [this]
            query)
          om/ITxIntercept
          (tx-intercept [this tx]
            (include-dependent-keys tx (om/props this) child-nodes))
          Object
          (render [this]
            (let [props (om/props this)]
              (first (rendering-tree props resolved-tree)))))]
    {:component region-component
     :factory (om/factory region-component)
     :region? true
     :tree resolved-tree
     :child-ui-nodes child-nodes}))
