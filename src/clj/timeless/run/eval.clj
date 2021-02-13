(ns timeless.run.eval
  "Evaluate TLS expressions."
  (:require [timeless.utils :refer :all]))


;; TODO:
;; - :seq and :str applied to number. :str applied to a number yields the Unicode code point of the character.
;;   Use the Clojure int fn to convert a character to a code point.
;; - Convert tagged seq form of string to :str.


(declare eval-tls)


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


(defn eval-apply [ctx exp]
  (let [exps (map (partial eval-tls ctx)
                  (all-args exp))]
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
        exp)
      exp)))


(defn eval-in [ctx exp]
  (let [v (eval-tls ctx (first-arg exp))]
    (if (has-type :set v)
      (with-meta
        (if (has-type :set v)
          (into [:vals] (all-args v))
          [:in v])
        (meta exp)))))


(defn eval-name [ctx exp]
  (if-let [v (ctx (first-arg exp))]
      (eval-tls ctx v)
      exp))


(defn eval-vals [ctx exp]
  (with-meta
    (into [:vals]
          (mapcat (fn [arg]
                    (let [v (eval-tls ctx arg)]
                      (if (has-type :vals v)
                        (all-args v)
                        (list v))))
               (all-args exp)))
    (meta exp)))


(defn eval-tls [ctx exp]
  (if (vector? exp)
    ((case (first exp)
       :apply eval-apply
       :in eval-in
       :name eval-name
       :vals eval-vals
       (fn [ctx exp] exp))
     ctx exp)
    exp))
