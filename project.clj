(defproject fset "0.1.0-SNAPSHOT"
  :description "Finite Set removal"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[com.rpl/specter "1.1.3"]
                 [org.clojure/core.match "1.0.0"]
                 [potemkin "0.4.5"]
                 [org.clojars.pkoerner/lisb "0.0.2-SNAPSHOT"]
                 [org.clojure/clojure "1.10.1"]
                 [net.mikera/vectorz-clj "0.48.0"]]
  :plugins [[lein-cloverage "1.2.2"]]
  :repl-options {:init-ns fset.core})
