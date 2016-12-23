(ns specroll.lattice.alfa.api
  (:require [clojure.spec :as s]
            #?@(:clj [[clojure.spec.gen :as gen]]
                :cljs [[cljs.spec.impl.gen :as gen :refer [dynaload]]])
            [clojure.string :as str]
            [specroll.lattice.specs :as $]
            [specroll.lattice.alfa.impl :as l]
            [specroll.lattice.alfa.extensions :as extensions]))


(defn region
  ([tree] (region {} tree))
  ([opts tree]
   (-> (if-not (= :lattice/region (first tree))
         [:lattice/region tree]
         tree)
       (l/normalize-tree)
       (as-> t (into [] t))
       (update-in [0 :opts] #(merge {:lattice/id (keyword "gen.lattice" (str (gensym "region")))}
                                 %
                                 opts))
       (l/resolve-implementations)
       (first))))

(defn index-ui-opts [region]
  (->> region
       (tree-seq #(-> % :impl :region?)
                 #(-> % :impl :child-ui-nodes))
       (into {}
             (keep (fn [{:keys [opts]}]
                     (when-let [id (:lattice/id opts)]
                       [id opts]))))))


;; specs

(s/def ::$/tag (s/with-gen ident? gen/keyword-ns)) ; Allow symbols (for templates), but don't gen them.

(s/def ::$/opts (s/nilable (s/map-of keyword? any? :gen-max 2)))

(declare fgen-lorem-ipsum fgen-tree)

(s/def ::$/text (s/with-gen string? #(fgen-lorem-ipsum)))

(s/def ::$/tree
  (s/with-gen
    (s/cat :tag ::$/tag
           :opts (s/? (s/spec ::$/opts))
           :children (s/* (s/spec (s/or :text string?
                                        :sym symbol?
                                        :ctree ::$/tree))))
    #(fgen-tree)))

(s/def ::$/children
  (s/coll-of (s/or :node ::$/node-unresolved
                   :str string?)
             :gen-max 2))

(s/def ::$/factory (s/fspec :args (s/* any?)
                            :ret any?))

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


;; generator implementations

(def ^:private dom-tags
  #{:a :abbr :address :area :article :aside :b :big :blockquote :body :br :button
    :canvas :cite :code :col :colgroup :data :datalist :details :dialog :div :em
    :embed :fieldset :figure :footer :form :h1 :h2 :h3 :h4 :h5 :h6 :head :header
    :i :iframe :img :ins :label :legend :li :link :main :map :mark :menu :menuitem
    :nav :noscript :object :ol :optgroup :output :p :param :picture :pre :progress
    :script :section :small :source :span :strong :style :sub :summary :sup :table
    :tbody :td :tfoot :th :thead :time :title :tr :u :ul :video})

(defn fgen-tree []
  (gen/fmap
   (fn [vecs]
     (reduce #(conj %2 %1)
             (reverse vecs)))
   (gen/bind
    (gen/large-integer* {:min 1 :max 5})
    (fn [depth]
      (gen/vector (gen/one-of
                   [(gen/tuple (s/gen dom-tags)
                               (s/gen ::$/opts))
                    (gen/tuple (gen/fmap #(keyword "gen.ui-tag" %)
                                         (gen/such-that #(not= % "")
                                                        (gen/string-alphanumeric)))
                               (s/gen ::$/ui-opts))])
                  depth)))))

(def ^:private lorem-ipsum
  "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?")

(def ^:private lorem-sentences
  (delay (str/split lorem-ipsum #" (?=[A-Z])")))

(def ^:private shuffle-ref
  #?(:clj (delay (#'gen/dynaload 'clojure.test.check.generators/shuffle))
     :cljs (dynaload 'clojure.test.check.generators/shuffle)))

(defn fgen-lorem-ipsum []
  (gen/fmap #(str/join \space (take 2 %))
            (@shuffle-ref @lorem-sentences)))
