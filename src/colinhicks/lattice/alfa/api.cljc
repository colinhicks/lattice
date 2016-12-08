(ns colinhicks.lattice.alfa.api
  (:require [clojure.spec :as s]
            [colinhicks.lattice.alfa.extensions :as extensions]
            [colinhicks.lattice.alfa.impl :as l #?(:clj :refer
                                                   :cljs :refer-macros) [$->]]))


(defn region [tree]
  (-> tree
      (l/normalize-tree)
      (l/resolve-implementations)
      (as-> resolved-tree
          (extensions/region-ui-impl :lattice/region
                                     {:children resolved-tree}))))

(s/fdef region
  :args (s/cat :tree :colinhicks.lattice.specs/tree)
  :ret :colinhicks.lattice.specs/region-ui-impl)
