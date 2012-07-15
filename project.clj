(defproject parse-ez "0.2.0"
  :description "Clojure Parser Library"
  :dependencies [
                 [org.clojure/clojure "1.3.0"]]
  :dev-dependencies [
                     [lein-autodoc "0.9.0"]
                     [lein-clojars "0.6.0"]]
  :jvm-opts ["-Xmx512m"] 
  :warn-on-reflection true
  :autodoc { :name "Parse Library", :page-title "Parse API Documentation"
             :copyright "Protoflex Software"}
  )
