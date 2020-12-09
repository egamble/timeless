(ns timeless.tl.utils
  "Useful functions.")


(defn error [msg]
  (throw (Exception. msg)))

(defn error-meta [m msg]
  (error (str msg " at line " (:l m) ", column " (:c m))))

(defn third [s]
  (nth s 2))

(defn fourth [s]
  (nth s 3))

(defn p [s x]
  (println s x)
  x)
