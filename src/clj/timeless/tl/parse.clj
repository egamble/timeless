(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix]]
            [instaparse.core :as insta]
            [clojure.string :as str]))



;;; Leave grouping parens exposed after parsing, i.e. all parens except those that denote sections, prefixized operators, and tuples. The exposed parens allow (1) correct post-processing of comparison chains, and (2) recognizing that comparison operations not surrounded by grouping parens that are in places allowed for embedded assertions become embedded assertions. It isn't easy to do this during parsing while still allowing operations lower in precedence than comparisons in those places.

;;; In post-processing, extract the assertions from the top level guard expressions.

;;; Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing.

(def parser
  (insta/parser (slurp "src/clj/timeless/tl/grammar.txt")))


;; Returns: <assertions>
(defn parse [path declarations source]
  (let [pr-matrix (build-pr-matrix declarations)
        ;; Add a newline in case the last line is a comment without a newline.
        parsed (parser (str "missing " source "\n"))]
    (if (insta/failure? parsed)
      (println parsed)
      parsed)))
