(ns timeless.tls.run
  "Run TLS code from the command line."
  (:require [timeless.tls.eval :refer [eval']]
            [timeless.common :refer [third error get-context set-context]]))

(defn lazy-input
  "Returns a lazy sequence of characters from an input stream or Reader."
  [in]
  (lazy-seq
   (let [c (.read in)]
     (when-not (== c -1)
       (cons (char c) (lazy-input in))))))

(defn top-error []
  (error "The TLS top-level expression must eval to a sequence of chars."))

(defn eval1
  "Eval expr. Use the given context if expr doesn't already have a :context metatag.
  If multiple values are returned, use the first one. Error if there are no values."
  [expr context]
  (let [v (eval' expr context)]
    (if (and (list? v) (= (first v) :values))
      (if (seq (rest v))
        (second v)
        (top-error))
      v)))

(defn lazy-eval
  "Returns a lazy sequence of characters from evaluation of a TLS expression."
  [expr context]
  (lazy-seq
   (let [context (or (get-context expr) context)
         s (eval1 expr context)]
     (cond
       (list? s)
       (case (first s)
         :cons
         (let [c (eval1 (second s) context)]
           (if (char? c)
             (cons c (lazy-eval (third s) context))
             (top-error)))

         ++
         (concat (lazy-eval (first s) context)
                 (lazy-eval (second s) context))

         :seq
         (let [[_ x xs] s]
           (when x
             (lazy-eval (list :cons x (cons :seq xs)) context)))

         (top-error))

       (string? s) (seq s)

       :else (top-error)))))

(defn run [expr in out]
  (let [context (or (get-context expr) {})
        a (atom nil)
        context (assoc context 'stdin a)
        stdin (set-context
               (cons :seq (lazy-input in))
               context)
        expr (set-context expr context) ; override original context on expr, if any
        ]
    (reset! a stdin)
    (doseq [c (lazy-eval expr nil)]
      (.write out (int c))
      (when (= c \newline)
        (.flush out)))))

(defn test-run [expr in-str]
  (run expr (java.io.StringReader. in-str) *out*))

(defn run-file [file in out]
  (run (read-string (slurp file)) in out))

(defn test-run-file [file in-str]
  (run-file file (java.io.StringReader. in-str) *out*))

(defn -main [file]
  (run-file file *in* *out*))
