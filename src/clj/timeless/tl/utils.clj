(ns timeless.tl.utils
  "Useful functions.")


(defn error [msg]
  (throw (Exception. msg)))

(defn third [s]
  (nth s 2))

(defn p [s x]
  (println s x)
  x)
