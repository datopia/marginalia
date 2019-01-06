(defproject io.datopia/marginalia "0.9.2-SNAPSHOT"
  :description "Lightweight literate programming for clojure -- inspired by [docco](http://jashkenas.github.com/docco/)"
  :main        marginalia.core
  :aot         [marginalia.core]
  :dependencies
  [[org.clojure/clojure             "1.10.0"]
   [org.clojure/clojurescript       "1.10.439"]
   [org.clojure/tools.namespace     "0.2.11"]
   [org.clojure/tools.cli           "0.3.3"]
   [org.markdownj/markdownj         "0.3.0-1.0.2b4"]
   [hiccup                          "1.0.5"]]

  :resource-paths ["vendor"]

  ;;Needed for testing Latex equation formatting. You must download
  ;;and install MathJax in you doc directory.
  :marginalia {:javascript ["mathjax/MathJax.js"]})
