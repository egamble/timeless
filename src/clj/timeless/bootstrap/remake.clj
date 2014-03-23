(ns timeless.bootstrap.remake
  "Restructure a Timeless AST in preparation for interpretation or code generation."
  (:require [timeless.bootstrap
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
                   (partial = "_")   (e nil)

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

(defn replace-underscores
  "Replace underscores with gensymed names."
  [clause]
  (let [f (fn [node]
            (if (and (node? node :name)
                     (= "_" (:val node)))
              (assoc node :val (str (gensym "__")))
              node))]

    (update-in clause [:st] (partial postwalk f))))

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

(defn remake-clauses [top-map]
  (let [f (fn [form]
            (if (node? form :clause)
              (-> form
                  shatter-st
                  move-key
                  move-val
                  extract-member-of-asserts
                  replace-underscores)
              form))]

    (postwalk f top-map)))


;;; restructure and analyze AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remake [top-defs]
  (-> top-defs
      make-top-map
      check-stdout
      remake-clauses))

(defn parse-and-remake [src-str]
  (-> src-str
      parse
      remake))
