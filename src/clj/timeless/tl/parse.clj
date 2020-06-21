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
  (insta/parser
    "
<S> = exp
<exp> = non-appl-exp | application
<non-appl-exp> = guard-op | seq | tup | str | name
application = non-appl-exp non-appl-exp+


element = (epsilon | exp)

(* If a seq ends with a comma, an empty element is produced, for a seq section. *)
(* There is no way to make a seq section with a single empty element. *)

seq = (seq-left seq-right) | (seq-left (element comma)+ element seq-right)

<seq-left> = ws <'['> ws
<seq-right> = ws <']'> ws


tup = (tup-left (element comma)+ element tup-right)

<tup-left> = ws <'('> ws
<tup-right> = ws <')'> ws


str = ws #'\".*\"' ws
guard-op = ws <'|'> ws
name = ws #'[a-zA-Z_]\\w*' ws

<comma> = ws <','> ws

<ws> = (ws-only | comment)*
<ws-only> = <#'\\s+'>
<comment> = <#'#.*\n'>"))


;; Returns: <assertions>
(defn parse [path declarations source]
  (let [pr-matrix (build-pr-matrix declarations)
        parsed (parser source)]
    (if (insta/failure? parsed)
      (println parsed)
      parsed)))
