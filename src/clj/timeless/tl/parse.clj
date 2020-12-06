(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.grammar :refer [build-operator-grammar]]
            [timeless.tl.utils :refer :all]
            [instaparse.core :as insta]))


;;; TODO:
;;; - Find chains.
;;; - Make grammar for the rest of the language, e.g. .., quote, etc.
;;; - For instaparse errors, find a way to suppress the "Expected one of:" part, which is unhelpful.
;;; - Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing.
;;; - Write command line script for generating TLS.


;; Returns: <assertions>
(defn extract-assertions [parsed]
  (loop [exp (first parsed)
         assertions ()]
    (let [[_ left-exp op right-exp] exp]
      (if (= left-exp [:name "_"])
        (if (= right-exp [:name "_"])
          assertions
          (error "no top-level guard operation"))
        (recur left-exp
               (cons right-exp
                     assertions))))))


(def comparison-op-names
  #{"=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@"})

(defn comparison->embedded [exp]
  (if (and (vector? exp)
           (let [[key _ op] exp]
             (and (= key :operation)
                  (comparison-op-names (second op)))))
    (apply vector :embedded (rest exp))
    exp))

(defn is-arrow-op [[_ op-name]]
  (#{"->" "→"} op-name))

(defn is-guard-op [[_ op-name]]
  (= "|" op-name))

(defn transform-left [exp op]
  (if (or (is-arrow-op op)
          (is-guard-op op))
    (comparison->embedded exp)
    exp))

(defn transform-right [exp op]
  (if (is-arrow-op op)
    (comparison->embedded exp)
    exp))

(defn transform-operation-nnn [left-exp op right-exp]
  [:operation
   (transform-left left-exp op)
   op
   (transform-right right-exp op)])

(defn transform-op-nnn [op-name]
  [:op op-name])

(defn transform-left-section [left-exp op]
  [:left-section
   (transform-left left-exp op)
   op])

(defn transform-right-section [op right-exp]
  [:right-section
   op
   (transform-right right-exp op)])

(defn transform-truncated-embedded [& rest]
  (apply vector :embedded [:name "_"] rest))

(defn transform-empty-element []
  :empty-element)


;;; do-transformations:
;;; (1) removes the precedence suffix from :operation-nnn and :op-nnn
;;; (2) finds and marks embedded assertions
;;; (3) adds "_" as the left side of truncated embedded assertions
;;; (4) makes :empty-element a single keyword rather than a vector
;;; (5) replaces :number vectors with literal numbers
;;; (6) replaces :str vectors with literal strings

(defn do-transformations [precedences assertions]
  (let [transform-map (-> {}
                          (into (mapcat (fn [pr]
                                          [[(keyword (str "operation-" pr))
                                            transform-operation-nnn]
                                           [(keyword (str "op-" pr))
                                            transform-op-nnn]])
                                        precedences))
                          (into [[:clause-maybe-embedded comparison->embedded]
                                 [:element-maybe-embedded comparison->embedded]
                                 [:left-section transform-left-section]
                                 [:right-section transform-right-section]
                                 [:truncated-embedded transform-truncated-embedded]
                                 [:empty-element transform-empty-element]
                                 [:number read-string]
                                 [:str read-string]]))]
    (map (partial insta/transform transform-map) assertions)))


(defn comparison->chain [left-exp op right-exp]
  ;; TODO
  [:operation left-exp op right-exp])

;;; find-chains is done after embedded assertions are found and marked, so that
;;; excluding embedded assertions from consideration as chains is simpler.

(defn find-chains [assertions]
(let [transform-map {:operation comparison->chain}]
    (map (partial insta/transform transform-map) assertions)))


;;; remove-groups removes :group markers.
;;; A :group expression is used to mark all parens except those that denote sections,
;;; prefixized operators, and tuples. The :group markers can only be removed after:
;;; (1) comparison operations that are not within a :group expression, and that are
;;;     in places allowed for embedded assertions, are identified and marked by
;;;     changing from :operation to :embedded, and
;;; (2) comparison chains have been identified and marked.
;;; It isn't easy to identify embedded and chain comparisons during parsing while
;;; still allowing operations lower in precedence than comparisons in those places.

(defn remove-groups [assertions]
  (let [transform-map {:group identity}]
    (map (partial insta/transform transform-map) assertions)))


;; Returns: <assertions>
(defn post-process [parsed precedences]
  (let [assertions (extract-assertions parsed)]
    (if (seq assertions)
      (->> assertions
           (do-transformations precedences)
           find-chains
           remove-groups)
      (error "no expressions"))))


;; Returns: <assertions>
(defn parse [declarations source]
  (let [predefined-grammar (slurp  "src/clj/timeless/tl/grammar.txt")
        [op-grammar precedences] (build-operator-grammar declarations)
        grammar (str predefined-grammar op-grammar)
        _ (spit "generated-grammar.txt" grammar)
        parser (insta/parser grammar)
        ;; A simple guard operation is prepended to make parsing the top-level guard
        ;; operations easier, and to provide better error messages when the top-level
        ;; guards are missing or malformed. This makes column numbers incorrect for
        ;; parsing errors in the first line. Need to find a way to fix this.
        ;; A newline is added at the end in case the last line is a comment without a newline.
        parsed (parser (str "_|_ " source "\n"))]
    (if (insta/failure? parsed)
      (println parsed)
      (post-process parsed precedences))))
