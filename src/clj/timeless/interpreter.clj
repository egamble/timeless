(ns timeless.interpreter
  "Interpreter for an S-expression form of Timeless."
  (:require [timeless.common :refer :all]
            [timeless.transform
             [misc   :refer [pre-eval-walk post-eval-walk]]
             [clause :refer [reorder-assertions-walk]]]
            [timeless.eval :refer [eval']]
            [clojure.set :as set]))

(defn read-tl
  "Reads top-level assertions from stream, which defaults to *in*.
  Ignores assertions other than equality assertions, because this interpreter can't use them.
  Returns a context map."
  ([]
   (read-tl *in*))
  ([stream]
   (let [asserts (->> (repeatedly #(read stream false nil))
                      (take-while not-nil?)
                      (filter (par op-isa? '=)) ;; keep only equality assertions
                      (map pre-eval-walk)
                      (doall))
         names (into #{} (map second asserts))
         asserts (map (par reorder-assertions-walk
                           (set/union predefined names))
                      asserts)]
     (into {} (map (fn [[_ nam expr]]
                     [nam expr])
                   asserts)))))

(defn read-tl-str
  [s]
  (with-in-str s
    (read-tl)))

(defn read-tl-file
  [file]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader file))]
    (read-tl r)))

(defn read-tl-files
  [& files]
  (apply merge (map read-tl-file files)))

(defn transform
  ([expr]
   (transform expr {}))
  ([expr context]
   (reorder-assertions-walk (set/union predefined (keys context))
                            (pre-eval-walk expr))))

(def t transform)

(defn transform-and-eval
  ([expr]
   (transform-and-eval expr {}))
  ([expr context]
   (post-eval-walk
    (eval' (transform expr context) context))))

(def e transform-and-eval)
