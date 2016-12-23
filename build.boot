(set-env!
 :source-paths #{"src"}
 :resource-paths #{"public" "src"}
 :dependencies '[[org.clojure/clojurescript "1.9.293"]
                 [org.omcljs/om "1.0.0-alpha47" :scope "provided"
                  :exclusions [cljsjs/react]]
                 [cljsjs/react-with-addons "15.3.1-0"]
                 [cljsjs/react-dom "15.3.1-0"
                  :exclusions [cljsjs/react]]
                 [org.clojure/test.check "0.9.0" :scope "test"]
                 [com.gfredericks/test.chuck "0.2.7" :scope "test"]
                 [adzerk/boot-cljs "1.7.228-2" :scope "test"]
                 [adzerk/boot-reload "0.4.13" :scope "test"]
                 [adzerk/boot-test "1.1.2" :scope "test"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 [org.clojure/tools.nrepl "0.2.12" :scope "test"]
                 [pandeiro/boot-http "0.7.6" :scope "test"]
                 [crisptrutski/boot-cljs-test "0.3.0" :scope "test"]])

(require '[adzerk.boot-cljs :refer [cljs]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[adzerk.boot-test :as clj-test])
(require '[adzerk.bootlaces :refer [build-jar]])
(require '[pandeiro.boot-http :refer [serve]])
(require '[crisptrutski.boot-cljs-test :as cljs-test])

(task-options!
 pom {:project 'com.specroll/lattice
      :version "alfa"
      :license {"name" "Eclipse Public License"
                "url" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask testing []
  (set-env! :source-paths #(conj % "test"))
  identity)

(deftask test-clj []
  (comp (testing)
        (clj-test/test)))

(deftask test-cljs
  [e exit? bool "Exit after results?"]
  (let [exit? (cond-> exit?
                (nil? exit?) not)]
    (comp
      (testing)
      (cljs-test/test-cljs
       :js-env :node
       :cljs-opts {:parallel-build true}
       :exit? exit?))))

(deftask auto-test-cljs []
  (comp
    (watch)
    (speak)
    (test-cljs :exit? false)))

(deftask dev []
  (comp
   (notify)
   (serve)
   (watch)
   (reload)
   (cljs)))
