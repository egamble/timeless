(ns timeless.run.eval
  "Evaluate TLS expressions."
  (:require [timeless.utils :refer :all]))


;; TODO:
;; - :seq and :str applied to number. :str applied to a number yields the Unicode code point of the character.
;;   Use the Clojure int fn to convert a character to a code point.
;; - Convert tagged form of string to :str.


(defn eval-++ [exp1 exp2]
  (cond
    (and (has-type :str exp1)
         (has-type :str exp2))
    (with-meta
      [:str (str (first-arg exp1)
                 (first-arg exp2))]
      (meta exp1))

    (and (has-type :seq exp1)
         (has-type :seq exp2))
    (with-meta
      [:seq (concat (first-arg exp1)
                    (first-arg exp2))]
      (meta exp1))

    :else [:vals]))


(defn eval-+ [exp1 exp2]
  (cond
    (and (has-type :num exp1)
         (has-type :num exp2))
    (with-meta
      [:num (+ (first-arg exp1)
               (first-arg exp2))]
      (meta exp1))

    :else [:vals]))


;; https://stackoverflow.com/questions/18246549/cartesian-product-in-clojure
(defn cart [colls]
  (if (empty? colls)
    '(())
    (let [c1 (first colls)]
      (for [more (cart (rest colls))
            x c1]
        (cons x more)))))


(defn spread-vals [exps]
  (let [valss (map (fn [exp]
                     (if (has-type :vals exp)
                       (first-arg exp)
                       (list exp)))
                   exps)]
    (cart valss)))


(defn eval-tls [ctx exp]
  (cond
    (has-type :name exp)
    (if-let [v (ctx (first-arg exp))]
      (eval-tls ctx v)
      exp)

    (has-type :apply exp)
    (let [exps (map (partial eval-tls ctx)
                    (first-arg exp))]
      (if (and (= 3 (count exps))
               (has-type :name (first exps)))
        (if-let [f ({"+" eval-+
                     "++" eval-++}
                    (first-arg (first exps)))]
          (let [vals (map (partial apply f)
                          (spread-vals (rest exps)))]
            (if (next vals)
              (with-meta
                [:vals vals]
                (meta (first vals)))
              (first vals)))
          exp)))

    :else
    exp))
