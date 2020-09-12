(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-pr-matrix]]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;;; All operations except guard operations are prefixized during parsing, including comparison chains, embedded assertions, and arrow operations. The operations may be reformed and expanded in post-processing. Comparisons must be associative (probably right-associative) to allow comparison chains, even if they are formally non-associative.

;;; Guard operations are not prefixized because they are left-associative, which makes checking for partial comparisons on their left sides difficult if they were prefixized.

;;; Leave grouping parens exposed after parsing, i.e. all parens except those that denote sections, prefixized operators, and tuples. The exposed parens allow correct post-processing of comparison chains, and abbreviated arrow and guard operations.

;;; In post-processing, extract the assertions from the top level guard expressions.

;;; Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing.

;;; When parsing a chain of operators (other than all comparisons), an error is produced if (a) the operators don't have a relative precedence, or if (b) the operators have equal precedence but one is left assoc and another is right assoc. Equal precedence, with one left or right and another simply assoc, should be okay, but how can that be written as a parse rule?


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
