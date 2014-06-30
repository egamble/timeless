(ns timeless.rewrite
  "Restructure a Timeless AST in preparation for interpretation or code generation."
  (:require [timeless
             [common      :refer :all]
             [parser      :refer [parse]]]
            [clojure.walk :refer [postwalk]]
            [clojure.set  :refer [union]]
            [let-else     :refer [let?]]))


;;; predefined names
;;;;;;;;;;;;;;;;;;;;

(def predefined
  #{"Obj" "Num" "Int" "Bool" "Char" "Str" "Set" "Vec" "Fn" "Dom" "Cod" "SetOf" "VecOf" "X" "not" "size" "char" "stdin"})


;;; make map of top-level defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-top-map [top-defs]
  (let [r
        (fn [m def]
          (let [name-node (-> def :val second)
                name (:val name-node)
                expr (-> def :val third)
                e #(error (str "Attempt to bind " % "\"" name "\" at top level")
                          name-node)]
            (condf name
                   predefined        (e "predefined name ")
                   m                 (e "previously bound ")

                   (assoc m name expr))))]
    (reduce r {} top-defs)))


;;; check for stdout binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-stdout [top-map]
  (if (top-map "stdout")
    top-map
    (error "No top-level binding for stdout")))


;;; restructure clause nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn shatter-st
  "Break up 'such that' into a seq of nodes, one for each &&-separated sub-assertion."
  [clause]
  (update-in clause [:st]
             (fn split-and [node]
               (when node
                 (if (is-op-node? "&&" node)
                   (mapcat split-and (rest (:val node)))
                   [node])))))

(defn move-key
  "Replace the key expression with a gensym and add an equality assertion
  for the key expression and the new name to 'such that'."
  [clause]
  (let [k (:key clause)]
    (let [k-name (merge k {:type :name :val (str (gensym "__"))})
          eq-op (merge k {:type :op :val "="})
          eq-assert (merge k {:type :apply :val [eq-op k-name k]})]
      (-> clause
          (update-in [:st] (partial cons eq-assert))
          (assoc :key k-name)))))

(defn move-val
  "Replace the val expression with a gensym and add an equality assertion
  for the val expression and the new name to 'such that'."
  [clause]
  (let [v (:val clause)]
    (if v
      (let [v-name (merge v {:type :name :val (str (gensym "__"))})
            eq-op (merge v {:type :op :val "="})
            eq-assert (merge v {:type :apply :val [eq-op v-name v]})]
        (-> clause
            (update-in [:st] (partial cons eq-assert))
            (assoc :val v-name)))
      clause)))

(defn extract-member-of-asserts
  "Find member-of assertions, replace them with their left side expressions,
  and add them to 'such that'."
  [clause]
  (let [f (fn [form]
            (if (is-op-node? "<-" form)
             (let [[_ pattern setish] (:val form)]
               [pattern [form]])
             [form []]))

        [changed-st member-of-asserts] (postwalk-collect f (:st clause))]

    (-> clause
        (assoc :st (concat changed-st member-of-asserts)))))

(defn rewrite-clauses [top-map]
  (let [f (fn [form]
            (if (node? form :clause)
              (-> form
                  shatter-st
                  move-key
                  move-val
                  extract-member-of-asserts)
              form))]

    (postwalk f top-map)))


;;; restructure and analyze AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rewrite [top-defs]
  (-> top-defs
      make-top-map
      check-stdout
      rewrite-clauses))

(defn parse-and-rewrite [src-str]
  (-> src-str
      parse
      rewrite))

;; TODO: restructure embedded assertions for both = and <-
;; TODO: for embedded assertions (with left expr not missing) replace a non-name LHS with a gensym, then add an = assertion for the left expr and a = or <- assertion for the right expr to the such that
