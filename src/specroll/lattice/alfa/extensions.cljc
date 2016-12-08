(ns specroll.lattice.alfa.extensions)


(defmulti dom-impl (fn [tag] tag))

(defmulti region-ui-impl (fn [tag node] tag))

(defmulti ui-impl (fn [tag] tag))
