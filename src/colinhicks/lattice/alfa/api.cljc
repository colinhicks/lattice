(ns colinhicks.lattice.alfa.api
  (:require [clojure.spec :as s]
            [colinhicks.lattice.alfa.extensions :as extensions]
            [colinhicks.lattice.alfa.impl :as l #?(:clj :refer
                                                   :cljs :refer-macros) [$->]]))


(defn region
  ([tree] (region {} tree))
  ([opts tree]
   (-> (if-not (= :lattice/region (first tree))
         [:lattice/region tree]
         tree)
       (l/normalize-tree)
       (as-> t (into [] t))
       (update-in [0 :opts] merge opts)
       (l/resolve-implementations)
       (first))))

(defn region-db [region]
  (->> region
       (tree-seq #(-> % :ui-impl :region?)
                 #(-> % :ui-impl :child-ui-nodes))
       (into {}
             (keep (fn [{:keys [opts]}]
                     (when-let [id (:lattice/id opts)]
                       [id opts]))))))

($-> colinhicks.lattice.specs
  (s/def :$/ui-id #(and (keyword? %) (namespace %)))

  (s/def :$/ui-opts
    (s/keys :req-un [:$/ui-id]))

  (s/def :$/region-ui-impl
    (s/merge :$/ui-impl
             (s/keys :req-un [:$/region? :$/child-ui-nodes])))

  (s/fdef region
    :args (s/cat :opts :$/ui-opts
                 :tree :$/tree)
    :ret :$/region-ui-impl)

  (s/fdef region-db
    :args (s/cat :region :$/region-ui-impl)
    :ret (s/every-kv :$/ui-id :$/ui-opts)))
