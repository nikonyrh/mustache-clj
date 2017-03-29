(defproject mustache-clj "0.1.0"
  :description "Mustache templating algorithm"
  :url         "https://github.com/nikonyrh/mustache-clj"
  :license {:name "Apache License, Version 2.0"
            :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :scm {:name "git"
        :url  "https://github.com/nikonyrh/mustache-clj"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [mustache-clj.core]
  :main mustache-clj.core)
