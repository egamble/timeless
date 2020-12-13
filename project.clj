(defproject egamble/timeless "0.1.0"
  :description "Timeless interpreter"
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [instaparse "1.4.10"]]
  :min-lein-version "2.9.0"
  :url "https://github.com/egamble/timeless"
  :main timeless.ast.load)
