(ns timeless.tl.utils
  "Useful functions.")


(defn error [msg]
  (throw (Exception. msg)))

(defn third [s]
  (nth s 2))

(defn p [s x]
  (println s x)
  x)

(defn hexify [s]
  (->> (.getBytes s "UTF-8")
       (map (partial format "%02x"))
       (apply str)))

(defn unhexify [s]
  (let [bytes (->> (partition 2 s)
                   (map (fn [[x y]]
                          (unchecked-byte (Integer/parseInt (str x y) 16))))
                   (into-array Byte/TYPE))]
    (String. bytes "UTF-8")))
