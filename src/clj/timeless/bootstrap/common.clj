(ns timeless.bootstrap.common
  "Generally useful defs for the Timeless bootstrap compiler."
  (:require [clojure.walk :refer [walk postwalk]]))


;;; node predicates
;;;;;;;;;;;;;;;;;;;

(defn node?
  "Returns true for a node of the given type(s) (a keyword or a set of keywords) and,
   optionally, with the given value or one of a set of values."
  [n type & [val]]
  (let [t (:type n)
        v (:val n)]
    (and (if (set? type) (type t) (= type t))
         (or (not val)
             (if (set? val) (val v) (= val v))))))

(defn is-op-node? [op node]
  (and (node? node :apply)
       (node? (first (:val node)) :op op)))


;;; error reporting
;;;;;;;;;;;;;;;;;;;

(defn error
  ([msg]
     (throw (Exception. msg)))
  ([msg pos-map]
     (error (str msg " at (" (:line pos-map) "," (:col pos-map) ")"))))


;;; condf: the missing cond macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro condf
  "Variant of cond that takes an expr and several clauses, where the test in
  each clause is a pred to be applied to the expr value. A single default
  expression can follow the clauses, as in condp."
  [expr & clauses]
  `(let [expr# ~expr]
     (condp (fn [pred# _#] (pred# expr#)) nil
       ~@clauses)))


;;; map fns
;;;;;;;;;;;

(defn update-vals
  "\"Update\" all values in m by calling f on every value to get the new value."
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-vals
  "Returns a map excluding entries in `m` for which `(pred v)` is logical false"
  [pred m]
  (into {} (remove (fn [[_ v]] (pred v)) m)))


;;; miscellany
;;;;;;;;;;;;;;

(defn third [s] (second (rest s)))

(defn remove-map-nils [m]
  (into {} (remove (comp nil? second) m)))

(defn wrap-if-not-seq [x]
  (if (or (not x) (seq? x)) x (list x)))


;;; reformat nodes for display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn show
  [nodes & [wrap-ops?]]
  "Reformat nodes for display.

  By default ops and names are displayed the same way.
  If wrap-ops? is truthy, display ops in parens."

  (let [f (fn [form]
            (let [t (:type form)
                  v (:val form)]
              (case t
                (:apply :num :str)  v
                (:vec :set :fn) `[~t ~@v]

                :name (symbol v)
                :op (let [s (symbol v)]
                      (if wrap-ops? (list s) s))

                :clause
                (let [key-if-only #(if (= (keys %) '(:key)) (:key %) %)]
                  (-> form
                      (select-keys [:key :st :val])
                      (update-in [:st] (comp seq wrap-if-not-seq))
                      remove-map-nils
                      key-if-only))

                form)))]

    (postwalk f nodes)))


;;; tree walking
;;;;;;;;;;;;;;;;

(defn walk-collect
  "Like clojure.walk/walk, except concats seqs from second of each
  inner and outer application."
  [inner outer form]
  (if (coll? form)
    (let [s (map inner form)
          fs (map first s)
          inner-seq (mapcat second s)
          [form new-seq] (outer
                          (condf form
                                 list? (apply list fs)
                                 (partial instance? clojure.lang.IMapEntry) (vec fs)
                                 seq? (doall fs)
                                 (into (empty form) fs)))]
      [form (concat new-seq inner-seq)])
    (outer form)))

(defn postwalk-collect
  "Like clojure.walk/postwalk, except f must return [<new form> <a seq>].
  Concats all the seqs together and returns [<new form> <concat of seqs>]."
  [f form]
  (walk-collect (partial postwalk-collect f) f form))

(defn postwalk-ancestors
  "Like clojure.walk/postwalk, except the first arg of f is a list of all
  ancestor nodes in the tree. The first ancestor is the parent of form,
  the second ancestor is the grandparent of form, etc.
  The second argument of f is the form."
  [f form]
  (letfn [(g [f ancestors form]
            (walk (partial g f (cons form ancestors))
                  (partial f ancestors)
                  form))]
    (g f nil form)))
