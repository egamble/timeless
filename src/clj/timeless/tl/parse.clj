(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.ops :refer [build-operator-grammar]]
            [timeless.tl.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))

;; TODO: a declared operator, e.g. #opl 78 foo, is parsed correctly in an operation, but not in a right or left section or prefix op.

;; TODO: write decode-op for _ ops.


;;; Leave grouping parens exposed after parsing, i.e. all parens except those that denote sections, prefixized operators, and tuples. The exposed parens allow (1) correct post-processing of comparison chains, and (2) recognizing that comparison operations not surrounded by grouping parens that are in places allowed for embedded assertions become embedded assertions. It isn't easy to do this during parsing while still allowing operations lower in precedence than comparisons in those places.

;;; Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing.

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

(defn decode-op [[op]]
  (let [op-str (str op)
        name (subs op-str
                1
                (- (count op-str) 3))]
    (if (= \_ (nth name 0))
      [:op-to-be-decoded name]
      [:op name])))

(defn transform-operation [left-exp op right-exp]
  [:operation left-exp (decode-op op) right-exp])

(defn transform-left-section [exp op]
  [:left-section exp (decode-op op)])

(defn transform-right-section [op exp]
  [:right-section (decode-op op) exp])

(defn transform-prefix-op [op]
  [:prefix-op (decode-op op)])

(defn transform-embedded [op exp]
  [:embedded (decode-op op) exp])

(defn transform-set-element [& exps]
  (apply vector :set-element (map (fn [exp]
                                    (if (or (= exp [:_7c-op]) ; guard operator
                                            (= exp [:_2d3e-op])) ; arrow operator
                                      (decode-op exp)
                                      exp))
                                  exps)))

;; Returns: <assertions>
(defn post-process [parsed precedences]
  (let [assertions (extract-assertions parsed)
        f (fn [& rest]
            (apply vector :operation rest))
        transform-map (-> {}
                          (into (map (fn [pr]
                                       [(keyword (str "operation-" pr))
                                        transform-operation])
                                     precedences))
                          (into [[:right-section transform-right-section]
                                 [:left-section transform-left-section]
                                 [:prefix-op transform-prefix-op]
                                 [:embedded transform-embedded]
                                 [:set-element transform-set-element]]))]
    (if assertions
      (map (partial insta/transform transform-map) assertions)
      (error "no expressions"))))

;; Returns: <assertions>
(defn parse [path declarations source]
  (let [predefined-grammar (slurp  "src/clj/timeless/tl/grammar.txt")
        [op-grammar precedences] (build-operator-grammar declarations)
        grammar (str predefined-grammar op-grammar)
        _ (spit "generated-grammar.txt" grammar)
        parser (insta/parser grammar)
        ;; Prepending a simple guard operation to make parsing the top-level guard
        ;; operations easier, and to provide better error messages when the top-level
        ;; guards are missing or malformed. This makes column numbers incorrect for
        ;; parsing errors in the first line. Need to find a way to fix this.
        ;; Adding a newline at the end in case the last line is a comment without a newline.
        parsed (parser (str "_|_ " source "\n"))]
    (if (insta/failure? parsed)
      (println parsed)
      (post-process parsed precedences))))
