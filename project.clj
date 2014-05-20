(defproject clj-asteroids "1.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [quil "1.7.0"]]
  :main clj-asteroids.core
  :profiles {:uberjar {:aot :all}})
