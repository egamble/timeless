(ns timeless.tl.ops
  "Build a precedence matrix from declarations."
  (:require [clojure.string :as str]))


;;; Construct the transitive closure of the precedence matrix.

(defn throw-pr-error [op1 op2 type1 type2]
  (throw (Exception. (str "Conflicting precedence for ops \""
                          op1 "\" and \"" op2
                          "\": " type1 " vs. " type2 "."))))

(declare recur-on-row)

;; TODO: describe the logic of this function in comments.
(defn maybe-add-op [pr-matrix start-op curr-type next-op next-type]
  (if (= start-op next-op)
    pr-matrix
    (if curr-type
      (let [type (cond (= curr-type :eq)
                       next-type

                       (or (= next-type :eq)
                           (= next-type type))
                       curr-type
                       
                       :default
                       nil)]
        (if type
          (let [start-row-type ((pr-matrix start-op) next-op)]
            (if start-row-type ; already added
              (if (= type start-row-type)
                pr-matrix
                (throw-pr-error start-op next-op type start-row-type))
              (recur-on-row (update-in pr-matrix [start-op next-op] (constantly type))
                            start-op
                            next-op
                            type)))
          pr-matrix))
      (recur-on-row pr-matrix
                    start-op
                    next-op
                    next-type))))

(defn recur-on-row [pr-matrix start-op op type]
  (let [op-row (pr-matrix op)]
    (reduce (fn [pr-matrix [next-op next-type]]
              (maybe-add-op pr-matrix
                            start-op
                            type
                            next-op
                            next-type))
            pr-matrix
            op-row)))

(defn transitive-closure-of-pr-matrix [pr-matrix]
  (reduce (fn [pr-matrix [start-op _]]
            (recur-on-row pr-matrix
                          start-op
                          start-op
                          nil))
          pr-matrix
          pr-matrix))


;;; Build the precedence matrix.

(def predefined-precedence-declarations
  '((:declare "#pr>" "*" "+" "=")
    (:declare "#pr>" ";" ":" "++" "∩" "∪" "=" "|" "->")
    (:declare "#pr=" "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@")
    (:declare "#pr=" "*" "/")
    (:declare "#pr=" "+" "-")
    (:declare "#pr=" "∩" "<>")
    (:declare "#pr=" "∪" "><")
    (:declare "#pr=" "→" "->")))

(defn add-pair-to-pr-matrix [pr-matrix type pair]
  (update-in pr-matrix pair
             (fn [prev-type]
               (if (or (not prev-type)
                       (= prev-type type))
                 type
                 (throw-pr-error (first pair) (second pair) prev-type type)))))

(defn build-pr-matrix-from-pair [type pr-matrix pair]
  (add-pair-to-pr-matrix
   (add-pair-to-pr-matrix pr-matrix
                          type
                          pair)
   (case type
     :gt :lt
     :lt :gt
     :eq :eq)
   (reverse pair)))

(defn build-pr-matrix-from-declaration [pr-matrix declaration]
  (let [type (case (second declaration)
               "#pr>" :gt
               "#pr<" :lt
               "#pr=" :eq
               nil)]
    (if type
      (let [pairs (partition 2 1 (rest (rest declaration)))]
        (reduce (partial build-pr-matrix-from-pair
                         type)
                pr-matrix
                pairs))
      pr-matrix)))

(defn build-pr-matrix [declarations]
  (let [pr-matrix (reduce build-pr-matrix-from-declaration
                          {}
                          (concat predefined-precedence-declarations
                                  declarations))]
    (transitive-closure-of-pr-matrix pr-matrix)))
