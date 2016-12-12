(ns specroll.lattice.alfa.impl
  (:require [clojure.spec :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [specroll.lattice.alfa.extensions :as extensions]))


(defmacro $-> [ns & forms]
  "In forms' keywords, replace placeholder $ with the supplied ns.
   ($-> foo.bar {:$/baz true}) ; => {:foo.bar/baz true}"
  (cons 'do
        (walk/postwalk
         (fn [x]
           (if (and (keyword? x)
                    (str/starts-with? (str x) ":$/"))
             (keyword (str ns "/" (name x)))
             x))
         forms)))

(defn ui-tag? [tag]
  (boolean (namespace tag)))

(defmethod extensions/region-ui-impl :default [tag _]
  (extensions/ui-impl tag))

(defn normalize-tree [raw-tree]
  (map (fn [node]
         (if (string? node)
           node
           (let [[tag maybe-opts & children] node
                 opts (when (map? maybe-opts) maybe-opts)
                 children' (if (and (not opts)
                                    (seq maybe-opts))
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
             (if-not (ui-tag? tag)
               (assoc node :impl (extensions/dom-impl tag))
               (assoc node :impl (extensions/region-ui-impl tag node)))
             node))
         %)
       tree))

(defn collect-ui-nodes [tree]
  (->> tree
       (mapcat (fn [node]
                 (tree-seq #(not (get-in % [:impl :region?]))
                           :children
                           node)))
       (filter #(when-let [tag (:tag %)]
                  (ui-tag? tag)))))

