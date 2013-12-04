(ns timeless.bootstrap.remake
  "Restructure a Timeless AST in preparation for interpretation or code generation."
  (:require [timeless.bootstrap
             [common      :refer :all]
             [parser      :refer [parse]]]
            [clojure.walk :refer [postwalk walk]]
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


;;; change setish applications to member-of ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remake-setish-applications [top-map]
  (let [f (fn [form]
            (or
             (let? [v (:val form) :when (node? form :apply)
                    name-node (first v)
                    :when (and (node? name-node :name)
                               (re-find #"^[A-Z]" (:val name-node)))

                    op      (merge form {:type :op :val "<-"})
                    setish  (vec (butlast v))
                    setish  (if (second setish)
                              (merge form {:type :apply :val setish})
                              (first setish))
                    pattern (last v)]
               (assoc form :val [op pattern setish]))
             form))]

    (postwalk f top-map)))


;;; restructure clause nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-op-node? [op node]
  (and (node? node :apply)
       (node? (first (:val node)) :op op)))

(defn shatter-st
  "Break up 'such that' into a seq of nodes, one for each &&-separated sub-assertion."
  [clause]
  (update-in clause [:st]
             (fn split-and [node]
               (when node
                 (if (is-op-node? "&&" node)
                   (mapcat split-and (rest (:val node)))
                   [node])))))

(defn replace-underscores
  "Replace underscores in the key with gensymed names."
  [clause]
  (let [f (fn [node]
            (if (and (node? node :name)
                     (= "_" (:val node)))
              (assoc node :val (str (gensym "__")))
              node))]

    (update-in clause [:key] (partial postwalk f))))

(defn extract-key-asserts
  "Find member-of key assertions, replace them with their left side expressions,
  and move them to 'such that'."
  [clause]
  (let [f (fn [form]
            (if (is-op-node? "<-" form)
             (let [[_ pattern setish] (:val form)]
               [pattern [form]])
             [form []]))

        [k key-asserts] (postwalk-collect f (:key clause))]

    (-> clause
        (update-in [:st] concat key-asserts)
        (assoc :key k))))

(defn move-key
  "If the key expression is not a name, gensym a name to be the key and add
  a binding assertion for the key expression to 'such that'."
  [clause]
  (let [k (:key clause)]
    (if (node? k :name)
      clause
      (let [k-name (merge k {:type :name :val (str (gensym "__"))})
            bind-op (merge k {:type :op :val ":="})
            bind-assert (merge k {:type :apply :val [bind-op k k-name]})]
        (-> clause
            (update-in [:st] (partial cons bind-assert))
            (assoc :key k-name))))))

(defn move-val
  "If the val expression is not a name, gensym a name to be the val and add
  a binding assertion for the new name to 'such that'."
  [clause]
  (let [v (:val clause)]
    (if (or (not v) (node? v :name))
      clause
      (let [v-name (merge v {:type :name :val (str (gensym "__"))})
            bind-op (merge v {:type :op :val ":="})
            bind-assert (merge v {:type :apply :val [bind-op v-name v]})]
        (-> clause
            (update-in [:st] (partial cons bind-assert))
            (assoc :val v-name))))))

(defn remake-clauses [top-map]
  (let [f (fn [form]
            (if (node? form :clause)
              (-> form
                  shatter-st
                  replace-underscores
                  extract-key-asserts
                  move-key
                  move-val)
              form))]

    (postwalk f top-map)))


;;; restructure and analyze AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remake [top-defs]
  (-> top-defs
      make-top-map
      check-stdout
      remake-setish-applications
      remake-clauses))

(defn parse-and-remake [src-str]
  (-> src-str
      parse
      remake))
