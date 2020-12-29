(ns timeless.transform.ast
  "Parse and post-process TL code to produce an AST."
  (:require [timeless.transform.grammar :refer [build-operator-grammar]]
            [timeless.transform.utils :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as str]))


;; Extract and simplify the metadata map.
(defn extract-meta [prefix-len exp]
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
(defn move-metadata [prefix-len exp]
  (if (sequential? exp)
    (apply vector
           (first exp)
           (extract-meta prefix-len exp)
           (map (partial move-metadata prefix-len) (rest exp)))
    exp))


;; Returns: <assertions>
(defn extract-assertions [parsed]
  (let [f (fn [[k _ n]]
            (and (= k :name)
                 (= n "_")))]
    (loop [exp parsed
           assertions ()]
      (let [[_ _ left-exp op right-exp] exp]
        (if (f left-exp)
          (if (f right-exp)
            assertions
            (error "no top-level guard operation"))
          (recur left-exp
                 (cons right-exp
                       assertions)))))))


(defn transform-apply [m & exps]
  (apply vector
         :apply
         m
         (mapcat (fn [exp]
                   (if (has-type :apply exp)
                     (all-args exp)
                     (list exp)))
                 exps)))

;;; do-transformations:
;;; (1) removes the precedence suffix from :operation-nnn and :op-nnn, except for :op-0, :op-1 and :op-10
;;; (2) changes :op-0 to :arrow-op and :op-1 to :guard-op, also removing the op-name string
;;; (3) changes :op-10 to :comparison-op to simplify searching for embedded assertions and chains
;;;     (This also allows user-defined comparison ops to form embedded assertions and chains.)
;;; (5) replaces the exp of :num with a literal number
;;; (6) replaces the exp of :str with a literal string
;;; (7) collapses nested :applys

(defn do-transformations [encoded-precedences assertions]
  (let [transform-operation-nnn (fn [m & exps] (apply vector :operation m exps))
        transform-op-nnn (fn [m op-name] [:op m op-name])
        trans-map (-> {}
                      (into (mapcat (fn [pr]
                                      (if (#{"0" "1" "10"} pr)
                                        [[(keyword (str "operation-" pr))
                                          transform-operation-nnn]]
                                        
                                        [[(keyword (str "operation-" pr))
                                          transform-operation-nnn]
                                         [(keyword (str "op-" pr))
                                          transform-op-nnn]]))
                                    encoded-precedences))
                      (into [[:op-0 (fn [m _] [:arrow-op m])]
                             [:op-1 (fn [m _] [:guard-op m])]
                             [:op-10 (fn [m op-name] [:comparison-op m op-name])]
                             [:num (fn [m exp] [:num m (read-string exp)])]
                             [:str (fn [m exp] [:str m (read-string exp)])]
                             [:apply transform-apply]]))]
    (map (partial insta/transform trans-map) assertions)))


(defn is-comparison-op [op]
  (= :comparison-op (first op)))

(defn is-comparison [exp]
  (and (= :operation (first exp))
       (is-comparison-op (fourth exp))))

(defn comparison->embedded [exp]
  (if (is-comparison exp)
    (let [[_ m _ _ right-exp] exp]
      (if (is-comparison right-exp)
        (error-meta m "an embedded assertion can't be a comparison chain")
        (apply vector :embedded (rest exp))))
    exp))

(defn transform-left [exp op]
  (if (#{:arrow-op :guard-op} (first op))
    (comparison->embedded exp)
    exp))

(defn transform-right [exp op]
  (if (= :arrow-op (first op))
    (comparison->embedded exp)
    exp))

(defn transform-operation [m left-exp op right-exp]
  [:operation
   m
   (transform-left left-exp op)
   op
   (transform-right right-exp op)])

(defn transform-left-section [m left-exp op]
  [:left-section
   m
   (transform-left left-exp op)
   op])

(defn transform-right-section [m op right-exp]
  [:right-section
   m
   op
   (transform-right right-exp op)])

(defn transform-truncated-embedded [m & exps]
  (apply vector :embedded m [:name m "_"] exps))


;;; find-embedded-assertions:
;;; (1) finds and marks embedded assertions
;;; (2) adds "_" as the left side of truncated embedded assertions

(defn find-embedded-assertions [assertions]
  (let [trans-map  {:clause-maybe-embedded (fn [_ exp] (comparison->embedded exp))
                    :element-maybe-embedded (fn [_ exp] (comparison->embedded exp))
                    :operation transform-operation
                    :left-section transform-left-section
                    :right-section transform-right-section
                    :truncated-embedded transform-truncated-embedded}]
      (map (partial insta/transform trans-map) assertions)))


;;; find-chains is called after embedded assertions are found and marked, so that
;;; excluding embedded assertions from consideration as chains is simpler.

(defn is-comparison-or-chain [exp]
  (or (is-comparison exp)
      (= :chain (first exp))))

(defn comparison-operation->chain [m left-exp op right-exp]
  (if (and (is-comparison-op op)
           (is-comparison-or-chain right-exp))
    (apply vector :chain m left-exp op (rest (rest right-exp))) ; the two rests skip the keyword and map
    [:operation m left-exp op right-exp]))

(defn find-chains [assertions]
  (let [trans-map {:operation comparison-operation->chain}]
    (map (partial insta/transform trans-map) assertions)))


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
  (let [trans-map {:group (fn [_ exp] exp)
                   :comparison-op (fn [m op-name] [:op m op-name])}]
    (map (partial insta/transform trans-map) assertions)))


;; Returns: <assertions>
(defn post-process-assertions [parsed encoded-precedences prefix-len]
  (let [assertions (->> parsed
                        first
                        (move-metadata prefix-len)
                        extract-assertions)]
    (if (seq assertions)
      (->> assertions
           (do-transformations encoded-precedences)
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
        [op-grammar encoded-precedences] (build-operator-grammar declarations)
        grammar (str predefined-grammar op-grammar)
        _ (when out-path-grammar
            (spit out-path-grammar grammar))
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
      (post-process-assertions parsed encoded-precedences prefix-len))))

                
(defn find-encoded-precedences [grammar]
  (map #(subs % 3)
       (str/split
        (second (re-find #"(?m)^\<op\> = (.*)$" grammar))
        #" \| ")))


;; Returns: <exps>
(defn post-process-exps [parsed encoded-precedences]
  (if (seq parsed)
    (->> parsed
         (do-transformations encoded-precedences)
         find-embedded-assertions
         find-chains
         remove-groups)
    (error "no expressions")))


;; Returns: <exps>
(defn tl-exp->ast [source in-path-grammar]
  (let [grammar (slurp in-path-grammar)
        parser (insta/parser grammar :start :S0)
        parsed (insta/add-line-and-column-info-to-metadata
                source
                (parser source))]
    (if (insta/failure? parsed)
      (-> parsed
          pr-str
          (str/split #"\nExpected one of:")
          first
          println)
      (post-process-exps parsed
                         (find-encoded-precedences grammar)))))
