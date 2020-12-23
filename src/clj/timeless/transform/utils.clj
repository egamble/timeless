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

(defn third-on [s]
  (rest (rest s)))

(defn p [s x]
  (println s x)
  x)

(defn get-meta [exp]
  (second exp))

(defn has-type [type exp]
  (= type (first exp)))

(defn has-types [type-set exp]
  (type-set (first exp)))

(defn exp-args [exp]
  (third-on exp))

(defn make-exp [k m args]
  (apply vector k m args))
