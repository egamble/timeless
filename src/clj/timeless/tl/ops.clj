(ns timeless.tl.ops
  "Process operator declarations."
  (:require [clojure.string :as str]))


;;;

(defn follow-col-ops [row-op prev-type pr-matrix [col-op next-type]]
  ;; TODO:
  ;; If prev-type is nil, don't try to add a pair, just recur.
  ;; If prev-type is non-nil:
  ;; - If col-op is already in the row of row-op:
  ;;   - Check that the already-added type is the same as the new type. If not, throw an error.
  ;;   - Don't recur.
  ;; - If prev-type is :eq, let type be next-type.
  ;; - If prev-type is not :eq, and next-type is the same or is :eq, let type be prev-type.
  ;; - If prev-type is not :eq, and next-type is the opposite, throw an error.
  ;; - Add col-op with type to the row of row-op, and recur.

  (reduce (partial follow-col-ops row-op type)
          pr-matrix
          (pr-matrix col-op)))

(defn transitive-closure-of-pr-matrix [pr-matrix]
  (reduce (fn [pr-matrix [row-op col-map]]
            (reduce (partial follow-col-ops row-op nil)
                    pr-matrix
                    col-map))
          pr-matrix
          pr-matrix))


;;; Build precedence matrix.

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
                 (throw (Exception. (str "Conflicting precedence for ops \""
                                         (first pair) "\" and \"" (second pair)
                                         "\": " prev-type " vs. " type ".")))))))

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


;;; Annotate operator tokens.

(def predefined-op-declarations
  '((:declare "#opa" "*" "+" "++" "∩" "∪" "<>" "><")
    (:declare "#op" "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@")
    (:declare "#opl" "/" "-" "|")
    (:declare "#opr" ":" "->" "→" ";")))

(defn make-template-ops-from-declaration [declaration]
  (let [assoc-keyword (case (second declaration)
                        "#op" :none
                        "#opr" :right
                        "#opl" :left
                        "#opa" :assoc
                        nil)]
    (when assoc-keyword
      (map #(do {:token %
                 :type :op
                 :assoc assoc-keyword})
           (rest (rest declaration))))))

;; Returns:
;; <list of
;;  {:token <op token>
;;   :type :op
;;   :assoc :assoc|:left|:right|:none}>
(defn make-template-ops [declarations]
  (mapcat make-template-ops-from-declaration
          (concat predefined-op-declarations
                  declarations)))

(defn annotate-op-token [template-ops annotated-token]
  (if (= (:type annotated-token) :string)
    annotated-token
    (let [token (:token annotated-token)
          annotated-op (some #(when (= (:token %) token)
                                %)
                             template-ops)]
      (into annotated-token annotated-op))))

(defn annotate-ops [declarations annotated-tokens]
  (map (partial annotate-op-token
                (make-template-ops declarations))
       annotated-tokens))
