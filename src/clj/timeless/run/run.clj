(ns timeless.tls.run
  "Run TLS code from the command line."
  (:require [timeless.tls.eval :refer [eval-for]]
            [timeless.common :refer :all]))

(defn lazy-input
  "Returns a lazy sequence of characters from an input stream or Reader."
  [in]
  (lazy-seq
   (let [c (.read in)]
     (when-not (== c -1)
       (cons (char c) (lazy-input in))))))

(defn error-top []
  (error "The TLS top-level expression must eval to a sequence of chars."))

(defn run [expr in out]
  (let [context (or (get-context expr) {})
        a (atom nil)
        context (assoc context 'stdin a)
        stdin (set-context (cons :seq (lazy-input in))
                           context)
        expr (set-context expr context) ; override original context on expr, if any
        ]
    (reset! a stdin)
    (if-let [s (eval-for :seq expr)]
      (let [cs (map #(or (eval-for :char % context)
                         (error-top))
                    (rest s))]
        (doseq [c cs]
          (.write out (int c))
          (when (= c \newline)
            (.flush out))))
      (error-top))))

(defn test-run [expr in-str]
  (run expr (java.io.StringReader. in-str) *out*))

(defn run-file [file in out]
  (run (read-string (slurp file)) in out))

(defn test-run-file [file in-str]
  (run-file file (java.io.StringReader. in-str) *out*))

(defn tl->tls [in-file out-file]
  (spit out-file (slurp in-file)))

(defn -main [file]
  (run-file file *in* *out*))
