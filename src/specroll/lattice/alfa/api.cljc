(ns specroll.lattice.alfa.api
  (:require [clojure.spec :as s]
            [specroll.lattice.alfa.extensions :as extensions]
            [specroll.lattice.alfa.impl :as l #?(:clj :refer
                                                 :cljs :refer-macros) [$->]]))


(defn region
  ([tree] (region {} tree))
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

($-> specroll.lattice.specs
  (s/def :$/ui-id #(and (keyword? %) (namespace %)))

  (s/def :$/ui-opts
    (s/keys :req-un [:$/ui-id]))

  (s/def :$/region-ui-impl
    (s/merge :$/impl
             (s/keys :req-un [:$/region? :$/child-ui-nodes])))

  (s/fdef region
    :args (s/cat :opts :$/ui-opts
                 :tree :$/tree)
    :ret :$/region-ui-impl)

  (s/fdef region-db
    :args (s/cat :region :$/region-ui-impl)
    :ret (s/every-kv :$/ui-id :$/ui-opts)))
