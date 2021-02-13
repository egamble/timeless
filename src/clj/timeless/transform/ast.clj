(ns timeless.transform.ast
  "Parse and post-process TL code to produce an AST."
  (:require [timeless.transform.grammar :refer [build-operator-grammar]]
            [timeless.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; Simplify the metadata map.
(defn simplify-metadata [prefix-len exp]
  (let [m (meta exp)
        line (:instaparse.gll/start-line m)
        col (:instaparse.gll/start-column m)]
    ;; Adjust the column number for expressions in the first line to subtract the
    ;; characters for the extra guard operation that was prepended to the first line.
    {:l line
     :c (if (= line 1)
          (- col prefix-len)
          col)}))

;; Move a simplified version of the metadata map of each vector into the vector, as the second element.
(defn alter-metadata [prefix-len exp]
  (if (sequential? exp)
    (with-meta
      (into [] (map (partial alter-metadata prefix-len)
                    exp))
      (simplify-metadata prefix-len exp))
    exp))


;; Returns: <assertions>
(defn extract-assertions [parsed]
  (let [f (fn [[k n]]
            (and (= k :name)
                 (= n "_")))]
    (loop [exp parsed
           assertions ()]
      (let [[_ left-exp op right-exp] exp]
        (if (f left-exp)
          (if (f right-exp)
            assertions
            (error "no top-level guard operation"))
          (recur left-exp
                 (cons right-exp
                       assertions)))))))


(defn transform-apply [exp]
  (with-meta
    (into [:apply]
          (mapcat (fn [subexp]
                    (if (has-type :apply subexp)
                      (rest subexp)
                      (list subexp)))
                  (rest exp)))
    (meta exp)))


(defn change-op-keys [k]
  (or ({:op-0 :arrow-op
        :op-1 :guard-op
        :op-10 :comparison-op} k)

      (when (re-find #"^:op-" (str k))
        :op)

      (when (re-find #"^:operation-" (str k))
        :operation)

      k))


;;; do-transformations:
;;; (1) removes the precedence suffix from :operation-nnn and :op-nnn, except for :op-0, :op-1 and :op-10
;;; (2) changes :op-0 to :arrow-op and :op-1 to :guard-op, also removing the op-name string
;;; (3) changes :op-10 to :comparison-op to simplify searching for embedded assertions and chains
;;;     (This also allows user-defined comparison ops to form embedded assertions and chains.)
;;; (5) replaces the exp of :num with a literal number
;;; (6) replaces the exp of :str with a literal string
;;; (7) collapses nested :applys

(defn do-transformations [assertions]
  (let [trans-map (-> {:operation identity
                       :op identity
                       :arrow-op (fn [exp] (with-meta
                                            [:arrow-op]
                                            (meta exp)))
                       :guard-op (fn [exp] (with-meta
                                            [:guard-op]
                                            (meta exp)))
                       :comparison-op identity
                       :num (partial change-arg read-string)
                       :str (partial change-arg read-string)
                       :apply transform-apply})]
    (map (partial transform-with-change-key-fn trans-map change-op-keys)
         assertions)))


(defn is-comparison [exp]
  (and (has-type :operation exp)
       (has-type :comparison-op (second-arg exp))))

(defn comparison->embedded [exp]
  (if (is-comparison exp)
    (if (is-comparison (third-arg exp))
      (error-at "an embedded assertion can't be a comparison chain"
                exp)
      (change-key :embedded exp))
    exp))

(defn transform-left [exp op]
  (if (has-types #{:arrow-op :guard-op} op)
    (comparison->embedded exp)
    exp))

(defn transform-right [exp op]
  (if (has-type :arrow-op op)
    (comparison->embedded exp)
    exp))

(defn transform-operation [exp]
  (let [[_ left-exp op right-exp] exp]
    (with-meta
      [:operation
       (transform-left left-exp op)
       op
       (transform-right right-exp op)]
      (meta exp))))

(defn transform-left-section [exp]
  (let [[_ left-exp op] exp]
    (with-meta
      [:left-section
       (transform-left left-exp op)
       op]
      (meta exp))))

(defn transform-right-section [exp]
  (let [[_ op right-exp] exp]
    (with-meta
      [:right-section
       op
       (transform-right right-exp op)]
      (meta exp))))

(defn transform-truncated-embedded [exp]
  (with-meta
    (into [:embedded (with-meta
                       [:name "_"]
                       (meta exp))]
          (rest exp))
    (meta exp)))


;;; find-embedded-assertions:
;;; (1) finds and marks embedded assertions
;;; (2) adds "_" as the left side of truncated embedded assertions

(defn find-embedded-assertions [assertions]
  (let [trans-map  {:clause-maybe-embedded (fn [exp] (comparison->embedded (first-arg exp)))
                    :element-maybe-embedded (fn [exp] (comparison->embedded (first-arg exp)))
                    :operation transform-operation
                    :left-section transform-left-section
                    :right-section transform-right-section
                    :truncated-embedded transform-truncated-embedded}]
      (map (partial transform trans-map) assertions)))


;;; find-chains is called after embedded assertions are found and marked, so that
;;; excluding embedded assertions from consideration as chains is simpler.

(defn is-comparison-or-chain [exp]
  (or (is-comparison exp)
      (has-type :chain exp)))

(defn comparison-operation->chain [exp]
  (let [[_ left-exp op right-exp] exp]
    (with-meta
      (if (and (has-type :comparison-op op)
               (is-comparison-or-chain right-exp))
        (into [:chain left-exp op] (all-args right-exp))
        [:operation left-exp op right-exp])
      (meta exp))))

(defn find-chains [assertions]
  (let [trans-map {:operation comparison-operation->chain}]
    (map (partial transform trans-map) assertions)))


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
  (let [trans-map {:group (fn [exp] (first-arg exp))
                   :comparison-op (partial change-key :op)}]
    (map (partial transform trans-map) assertions)))


;; Returns: <assertions>
(defn post-process-assertions [parsed prefix-len]
  (let [assertions (->> parsed
                        first
                        (alter-metadata prefix-len)
                        extract-assertions)]
    (if (seq assertions)
      (->> assertions
           do-transformations
           find-embedded-assertions
           find-chains
           remove-groups)
      (error "no expressions"))))


;; If there's a parse error in the first line, adjust the instaparse error message
;; to remove the extra guard operation that was prepended to the first line.
(defn adjust-error-in-first-line [msg prefix-len]
  (if (str/index-of msg "at line 1,")
    (let [[first-line second-line third-line] (str/split-lines msg)
          col (->> first-line
                   (re-find #"column (\d+)\:")
                   second
                   read-string)]
      (str "Parse error at line 1, column " (- col prefix-len) ":\n"
           (subs second-line prefix-len) "\n"
           (subs third-line prefix-len)))
    msg))


;; Returns: <assertions>
(defn tl->ast [declarations source out-path-grammar]
  (let [predefined-grammar (slurp  "src/clj/timeless/transform/grammar.txt")
        grammar (str predefined-grammar
                     (build-operator-grammar declarations))
        _ (spit out-path-grammar grammar)
        parser (insta/parser grammar :start :S1)

        ;; A simple guard operation is prepended to the source to make parsing the top-level
        ;; guard operations easier, and to provide better error messages when the top-level
        ;; guards are missing or malformed.

        ;; A newline is added at the end in case the last line is a comment without a newline.
        prefix "_|_ "
        prefix-len (count prefix)
        source (str prefix source "\n")

        parsed (insta/add-line-and-column-info-to-metadata
                source
                (parser source))]
    (if (insta/failure? parsed)
      (-> parsed
          pr-str
          (str/split #"\nExpected one of:")
          first
          (adjust-error-in-first-line prefix-len)
          println)
      (post-process-assertions parsed prefix-len))))

                
;; Returns: <exp>
(defn post-process-exp [parsed]
  (->> parsed
       first
       (alter-metadata 0)
       list
       do-transformations
       find-embedded-assertions
       find-chains
       remove-groups))


(defn make-tl-exp-parser [grammar]
  (insta/parser grammar :start :S0))


;; Returns: <exps>
(defn tl-exp->ast [parser tl-exp]
  (let [parsed (insta/add-line-and-column-info-to-metadata
                tl-exp
                (parser tl-exp))]
    (if (insta/failure? parsed)
      (-> parsed
          pr-str
          (str/split #"\nExpected one of:")
          first
          println)
      (post-process-exp parsed))))
