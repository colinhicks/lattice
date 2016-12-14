(ns specroll.lattice.alfa.api
  (:require [clojure.spec :as s]
            [#?(:clj clojure.spec.gen
                :cljs cljs.spec.impl.gen) :as gen]
            [specroll.lattice.specs :as $]
            [specroll.lattice.alfa.impl :as l]
            [specroll.lattice.alfa.extensions :as extensions]))


(defn region
  ([tree] (region {:lattice/id (keyword "gen.lattice" (str (gensym "region")))} tree))
  ([opts tree]
   (-> (if-not (= :lattice/region (first tree))
         [:lattice/region tree]
         tree)
       (l/normalize-tree)
       (l/resolve-implementations)
       (first)
       (update :opts merge opts))))

(defn region-db [region]
  (->> region
       (tree-seq #(-> % :impl :region?)
                 #(-> % :impl :child-ui-nodes))
       (into {}
             (keep (fn [{:keys [opts]}]
                     (when-let [id (:lattice/id opts)]
                       [id opts]))))))


(s/def ::$/tag keyword?)

(s/def ::$/opts (s/nilable (s/map-of keyword? any? :conform-keys true)))

(s/def ::$/tree
  (s/cat :tag ::$/tag
         :opts (s/? (s/spec ::$/opts))
         :children (s/* (s/spec (s/or :str string?
                                      :ctree ::$/tree)))))

(s/def ::$/children (s/coll-of (s/or :node ::$/node-unresolved
                                    :str string?)))

(s/def ::$/factory fn?)

(s/def ::$/base-impl (s/keys :req-un [::$/factory]))

(s/def ::$/impl (s/merge ::$/base-impl))

(s/def ::$/node-unresolved (s/keys :req-un [::$/tag ::$/opts ::$/children]))

(s/def ::$/node-resolved
  (s/merge ::$/node-unresolved
           (s/keys :req-un [::$/impl])))

(s/fdef extensions/ui-impl
  :args (s/cat :tag ::$/tag)
  :ret ::$/impl)

(s/def ::$/region? boolean?)

(s/def ::$/region-ui-impl
  (s/merge ::$/impl
           (s/keys :req-un [::$/region?])))

(s/fdef extensions/region-ui-impl
  :args (s/cat :tag ::$/tag
               :node (s/spec ::$/node-unresolved))
  :ret ::$/region-ui-impl)

(s/def ::$/ui-id
  (s/with-gen #(and (keyword? %) (namespace %))
    (fn []
      (gen/fmap #(keyword "gen.lattice.ui-id" %)
                (gen/such-that #(not= % "")
                               (gen/string-alphanumeric))))))

(s/def :lattice/id ::$/ui-id)

(s/def ::$/ui-opts
  (s/keys :req [:lattice/id]))

(s/def ::$/region ::$/node-resolved)

(s/fdef region
  :args (s/cat :opts (s/? (s/nilable (s/spec ::$/ui-opts)))
               :tree (s/spec ::$/tree))
  :ret ::$/region)

(s/fdef region-db
  :args (s/cat :region ::$/region)
  :ret (s/every-kv ::$/ui-id ::$/ui-opts))

