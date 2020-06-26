(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix]]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;;; Don't prefixize comparison expressions, including comparison chains and embedded assertions. Just surround them with a "comparison" type. In post-processing, expand embedded assertions if they're not within (original) parens.
;;; Don't insert "missing" in front of the source. Just check in post-processing that the top level expression is a section of a guard.
;;; In post-processing, extract the assertions from the top leval guard section and the prefixized guard expressions within it.
;;; Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing.

;;; When parsing a chain of operators (other than all comparisons), an error is produced if (a) the operators don't have a relative precedence, or if (b) the operators have equal precedence but one is left assoc and another is right assoc. Equal precedence, with one left or right and another simply assoc, should be okay, but how can that be written as a parse rule?


(def parser
  (insta/parser (slurp "src/clj/timeless/tl/grammar.txt")))


;; Returns: <assertions>
(defn parse [path declarations source]
  (let [pr-matrix (build-pr-matrix declarations)
        ;; Add a newline in case the last line is a comment without a newline.
        parsed (parser (str source "\n"))]
    (if (insta/failure? parsed)
      (println parsed)
      parsed)))
