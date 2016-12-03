(set-env!
 :source-paths #{"src"}
 :resource-paths #{"public"}
 :dependencies '[[org.clojure/clojurescript "1.9.293"]
                 [org.omcljs/om "1.0.0-alpha47"]
                 [adzerk/boot-cljs "1.7.228-2" :scope "test"]
                 [adzerk/boot-reload "0.4.13" :scope "test"]
                 [org.clojure/tools.nrepl "0.2.12" :scope "test"]
                 [pandeiro/boot-http "0.7.6" :scope "test"]])

(require '[adzerk.boot-cljs :refer [cljs]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[pandeiro.boot-http :refer [serve]])

(deftask dev []
  (comp
   (notify)
   (serve)
   (watch)
   (reload)
   (cljs)))
