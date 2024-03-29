(ns timeless.utils
  "Useful functions."
  (:require [clojure.string :as str]))


(defn p [msg x]
  (println msg x)
  x)

(defn error [msg]
  (throw (Exception. msg)))

(defn error-at [msg exp]
  (let [m (meta exp)]
    (error (str msg
                (when (and m
                           (or (:l m) (:path m)))
                  (str
                   "\n("
                   (when (:l m)
                     (str "line " (:l m)
                          ", column " (:c m)))
                   (when (:path m)
                     (str (when (:l m) ", ")
                          "path " (:path m)))
                   ")"))))))

(defn third [s]
  (nth s 2))

(defn fourth [s]
  (nth s 3))

(defn fifth [s]
  (nth s 4))

(defn third-on [s]
  (rest (rest s)))

(defn fourth-on [s]
  (rest (rest (rest s))))

(defn has-type [type exp]
  (and (vector? exp)
       (= type (first exp))))

(defn has-types [type-set exp]
  (and (vector? exp)
       (type-set (first exp))))

(defn all-args [exp]
  (rest exp))

(def first-arg second)

(def second-arg third)

(def third-arg fourth)

(defn uuid []
  (.toString (java.util.UUID/randomUUID)))

(defn cons-at-end [s x]
  (concat s (list x)))

(defn interleave-newlines [s]
  (interleave s (repeat "\n")))

(defn strip-tl-filepath [path]
  (str/replace path #"\.tls?$" ""))

(defn transform-with-change-key-fn [transform-map
                                    change-key-fn
                                    exp]
  (if (and (vector? exp) (seq exp))
    (let [k (if change-key-fn
              (change-key-fn (first exp))
              (first exp))
          exp-recur (with-meta 
                      (into [k]
                            (map (partial transform-with-change-key-fn
                                          transform-map
                                          change-key-fn) 
                                 (rest exp)))
                      (meta exp))]
      (if-let [transform-fn (transform-map k)]
        (transform-fn exp-recur)
        exp-recur))
    exp))

(defn transform [transform-map exp]
  (transform-with-change-key-fn transform-map nil exp))

(defn change-key [new-key exp]
  (with-meta
    (into [new-key]
          (rest exp))
    (meta exp)))

(defn change-arg [f exp]
  (with-meta
    [(first exp)
     (f (first-arg exp))]
    (meta exp)))

(defn has-no-value [exp]
  (and (has-type :vals exp)
       (empty? (all-args exp))))

(defn is-empty-set [exp]
  (and (has-type :set exp)
       (empty? (all-args exp))))

(defn has-name [name exp]
  (and (has-type :name exp)
       (= name (first-arg exp))))

(def predefined-operators
  #{"->" "→"
    "|"
    "=" "≠" "<" ">" "≤" "≥" "⊂" "⊃" "∈" "∉" "!=" "<=" ">=" "<<" ">>" "@" "!@"
    "∪" "><"
    "∩" "<>"
    ":" "++"
    ";"
    "+" "-"
    "*" "/"})

(def predefined-names
  (into predefined-operators
        ["Any" "Num" "Int" "Bool" "Sym" "Tag" "Arr" "Set" "Fn" "Seq" "Str" "Char"
         "Dm" "Im"
         "size" "infinity" "∞"
         "true" "false"
         "null"
         "flip"
         "_"]))

(defn set-meta-key [exp k v]
  (if (vector? exp)
    (with-meta
      exp
      (into (or (meta exp) {})
            {k v}))
    exp))

(defn reset-meta-key [exp k]
  (if (and (vector? exp)
           (meta exp))
    (with-meta exp (dissoc (meta exp) k))
    exp))

(defn set-evaled [exp]
  (set-meta-key exp :evaled true))

(defn set-type-set [exp type-set]
  (set-meta-key exp :type-set type-set ))
