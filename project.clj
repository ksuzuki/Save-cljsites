(defproject save-cljsites "2.1.6"
  :description "Save Clojure web sites"
  :main save-cljsites
  :jar-name "save-cljsites.jar"
  :uberjar-name "save-cljsites-standalone.jar"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.apache.httpcomponents/httpclient "4.0.1"]]
  :aot [save-cljsites])
