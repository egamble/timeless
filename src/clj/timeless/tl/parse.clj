(ns timeless.tl.parse
  "Reform tokens to produce TLS S-expressions."
  (:require [timeless.tl.grammar :refer [build-operator-grammar]]
            [timeless.tl.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;;; TODO:
;;; - Use insta/add-line-and-column-info-to-metadata so line/column info is available to generate errors when post-processing. Also write line/column info to the TLS file for use in run-time errors.
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


(defn is-application [exp]
  (and (vector? exp)
       (= :application (first exp))))

(defn transform-application [& exps]
  (apply vector
         :application
         (mapcat (fn [exp]
                   (if (is-application exp)
                     (rest exp)
                     (list exp)))
                 exps)))

;;; do-transformations:
;;; (1) removes the precedence suffix from :operation-nnn and :op-nnn, except for :op-0, :op-1 and :op-10
;;; (2) changes [:op-0 ...] to :arrow-op and [:op-1 ...] to :guard-op
;;; (3) changes :op-10 to :comparison-op to simplify searching for embedded assertions and chains
;;;     (This also allows user-defined comparison ops to form embedded assertions and chains.)
;;; (4) makes :empty-element and :range single keywords rather than vectors
;;; (5) replaces :number vectors with literal numbers
;;; (6) replaces :str vectors with literal strings
;;; (7) collapses nested :applications

(defn do-transformations [encoded-precedences assertions]
  (let [transform-operation-nnn (fn [& exps] (apply vector :operation exps))
        transform-op-nnn (fn [op-name] [:op op-name])
        transform-map (-> {}
                          (into (mapcat (fn [pr]
                                          (if (#{"0" "1" "10"} pr)
                                            [[(keyword (str "operation-" pr))
                                              transform-operation-nnn]]

                                            [[(keyword (str "operation-" pr))
                                              transform-operation-nnn]
                                             [(keyword (str "op-" pr))
                                              transform-op-nnn]]))
                                        encoded-precedences))
                          (into [[:op-0 (constantly :arrow-op)]
                                 [:op-1 (constantly :guard-op)]
                                 [:op-10 (fn [op-name] [:comparison-op op-name])]
                                 [:empty-element (constantly :empty-element)]
                                 [:range (constantly :range)]
                                 [:number read-string]
                                 [:str read-string]
                                 [:application transform-application]]))]
    (map (partial insta/transform transform-map) assertions)))


(defn is-comparison-op [op]
  (and (vector? op)
       (= :comparison-op (first op))))

(defn is-comparison [exp]
  (and (vector? exp)
       (= :operation (first exp))
       (is-comparison-op (third exp))))

(defn comparison->embedded [exp]
  (if (is-comparison exp)
    (let [[_ _ _ right-exp] exp]
      (if (is-comparison right-exp)
        (error "an embedded assertion can't be a comparison chain")
        (apply vector :embedded (rest exp))))
    exp))

(defn transform-left [exp op]
  (if (#{:arrow-op :guard-op} op)
    (comparison->embedded exp)
    exp))

(defn transform-right [exp op]
  (if (= :arrow-op op)
    (comparison->embedded exp)
    exp))

(defn transform-operation [left-exp op right-exp]
  [:operation
   (transform-left left-exp op)
   op
   (transform-right right-exp op)])

(defn transform-left-section [left-exp op]
  [:left-section
   (transform-left left-exp op)
   op])

(defn transform-right-section [op right-exp]
  [:right-section
   op
   (transform-right right-exp op)])

(defn transform-truncated-embedded [& exps]
  (apply vector :embedded [:name "_"] exps))


;;; find-embedded-assertions:
;;; (1) finds and marks embedded assertions
;;; (2) adds "_" as the left side of truncated embedded assertions

(defn find-embedded-assertions [assertions]
  (let [transform-map  {:clause-maybe-embedded comparison->embedded
                        :element-maybe-embedded comparison->embedded
                        :operation transform-operation
                        :left-section transform-left-section
                        :right-section transform-right-section
                        :truncated-embedded transform-truncated-embedded}]
      (map (partial insta/transform transform-map) assertions)))


;;; find-chains is called after embedded assertions are found and marked, so that
;;; excluding embedded assertions from consideration as chains is simpler.

(defn is-comparison-or-chain [exp]
  (or (is-comparison exp)
      (and (vector? exp)
           (= :chain (first exp)))))

(defn comparison-operation->chain [left-exp op right-exp]
  (if (and (is-comparison-op op)
           (is-comparison-or-chain right-exp))
    (apply vector :chain left-exp op (rest right-exp))
    [:operation left-exp op right-exp]))

(defn find-chains [assertions]
  (let [transform-map {:operation comparison-operation->chain}]
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

;;; remove-groups also changes [:comparison-op ...] to [:op ...] since
;;; the :comparison-op marker is no longer needed.

(defn remove-groups [assertions]
  (let [transform-map {:group identity
                       :comparison-op (fn [op-name] [:op op-name])}]
    (map (partial insta/transform transform-map) assertions)))


;; Returns: <assertions>
(defn post-process [parsed encoded-precedences]
  (let [assertions (extract-assertions parsed)]
    (if (seq assertions)
      (->> assertions
           (do-transformations encoded-precedences)
           find-embedded-assertions
           find-chains
           remove-groups)
      (error "no expressions"))))


;; Returns: <assertions>
(defn parse [declarations source]
  (let [predefined-grammar (slurp  "src/clj/timeless/tl/grammar.txt")
        [op-grammar encoded-precedences] (build-operator-grammar declarations)
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
      (-> parsed
          pr-str
          (str/split #"\nExpected one of:")
          first
          println)
      (post-process parsed encoded-precedences))))
