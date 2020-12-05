(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.grammar :refer [build-operator-grammar]]
            [timeless.tl.utils :refer :all]
            [instaparse.core :as insta]))


;;; Next:
;;; - Find comparisons not in a :group to determine if they should be in an :embedded.
;;; - Find chains.
;;; - Remove :groups.
;;; - For instaparse errors, find a way to suppress the "Expected one of:" part, which is unhelpful.
;;; - Make grammar for the rest of the language, e.g. .., quote, etc.
;;; - Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing.
;;; - Write command line script for generating TLS.


;;; Leave grouping parens exposed after parsing, i.e. all parens except those that denote sections, prefixized operators, and tuples. The exposed parens allow (1) correct post-processing of comparison chains, and (2) recognizing that comparison operations not surrounded by grouping parens that are in places allowed for embedded assertions become embedded assertions. It isn't easy to do this during parsing while still allowing operations lower in precedence than comparisons in those places.


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




(defn transform-left [exp op]
  exp)

(defn transform-right [exp op]
  exp)



;; Used for terminals of the form :operation-nnn.
(defn transform-operation [left-exp op right-exp]
  (vector :operation
          (transform-left left-exp op)
          op
          (transform-right right-exp op)))

;; Used for terminals of the form :op-nnn.
(defn transform-op [& rest]
  (apply vector :op rest))

(defn transform-left-section [left-exp op]
  (vector :left-section
          (transform-left left-exp op)
          op))

(defn transform-right-section [op right-exp]
  (vector :right-section
          op
          (transform-right right-exp op)))

(defn transform-embedded [& rest]
  (apply vector :embedded [:name "_"] rest))


;; Returns: <assertions>
(defn post-process [parsed precedences]
  (let [assertions (extract-assertions parsed)
        f (fn [& rest]
            (apply vector :operation rest))
        transform-map (-> {}
                          (into (mapcat (fn [pr]
                                          [[(keyword (str "operation-" pr))
                                            transform-operation]
                                           [(keyword (str "op-" pr))
                                            transform-op]])
                                        precedences))
                          (into [[:embedded transform-embedded]]))]
    (if (seq assertions)
      (map (partial insta/transform transform-map) assertions)
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
