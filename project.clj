(defproject parse-ez "0.3.1"
  :description "Clojure Parser Library"
  :url "https://github.com/protoflex/parse-ez"
  :license {:name "Eclipse Public License"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :dev-dependencies [[lein-autodoc "0.9.0"]
                     [lein-clojars "0.6.0"]]
  :jvm-opts ["-Xmx512m"] 
  :warn-on-reflection true
  :autodoc { :name "Parse Library", :page-title "Parse API Documentation"
             :copyright "Protoflex Software"}
  )
