(ns timeless.transform.utils
  "Useful functions.")


(defn error [msg]
  (throw (Exception. msg)))

(defn error-meta [m msg]
  (error (str msg " at line " (:l m) ", column " (:c m))))

(defn third [s]
  (nth s 2))

(defn fourth [s]
  (nth s 3))

(defn fifth [s]
  (nth s 4))

(defn third-on [s]
  (rest (rest s)))

(defn p [s x]
  (println s x)
  x)

(defn get-meta [exp]
  (second exp))

(defn has-type [type exp]
  (and (vector? exp)
       (= type (first exp))))

(defn has-types [type-set exp]
  (and (vector? exp)
       (type-set (first exp))))

(defn all-args [exp]
  (third-on exp))

(defn first-arg [exp]
  (third exp))

(defn second-arg [exp]
  (fourth exp))

(defn third-arg [exp]
  (fifth exp))

(defn v [k m args]
  (apply vector k m args))

(defn uuid []
  (.toString (java.util.UUID/randomUUID)))

(defn cons-at-end [s x]
  (concat s (list x)))
