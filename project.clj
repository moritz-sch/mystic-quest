(defproject mystic-quest "1.0-SNAPSHOT"
  :description "An editor for Mystic Quest"
  :url ""
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [seesaw "1.5.1-SNAPSHOT"]]
  :main ^:skip-aot mystic-quest.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
