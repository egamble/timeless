(ns timeless.tls.run
  "Run TLS code from the command line.")

;; (defn read-tl-file
;;   [file]
;;   (with-open [r (java.io.PushbackReader. (clojure.java.io/reader file))]
;;     (read-tl r)))

(defn lazy-input
  "Returns a lazy sequence of characters from an input stream or Reader."
  [in]
  (let [step (fn step []
               (let [c (.read in)]
                 (when-not (== c -1)
                   (cons (char c) (lazy-seq (step))))))]
    (lazy-seq (step))))


(defn lazy-input
  "Returns a lazy sequence of characters from an input stream or Reader."
  [in]
  (let [step (fn step []
               (let [c (.read in)]
                 (when-not (== c -1)
                   (cons (char c) (lazy-seq (step))))))]
    (let [c (.read in)]
      (when-not (== c -1)
        (cons (char c) (lazy-seq (step)))))
    (lazy-seq (step))))
