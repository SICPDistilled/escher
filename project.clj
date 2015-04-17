(defproject escher "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-tuple "0.2.1"]
                 [quil "2.2.4"]]
  :main ^:skip-aot escher.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.6.3"]]}}
  :plugins [[lein-midje "3.1.3"]]
  )
;; To start a development session, in a terminal window, do:
;;
;;     cd /Users/gr/GitHub/escher
;;     lein midje :autotest
