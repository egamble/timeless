(ns timeless.run.eval
  "Evaluate TLS expressions."
  (:require [timeless.run.set :refer [eval-set eval-set* eval-union eval-inter]]
            [timeless.run.apply :refer [eval-apply]]
            [timeless.utils :refer :all]))


;; Meta tags:
;; :evaled is a boolean, set to true to avoid re-evaluation, and removed or set to false to allow re-evaluation.
;; :type-set is a TLS expression evaluating to a set that contains the tagged expression. To be useful, the set should be neither too specific nor too general. :num, :str and predefined :name expressions don't need a :type-set tag.
;; :clj= is a boolean that is true when Clojure equality testing of the tagged object is the same as Timeless equality. :set expressions must not have :clj= true. Some expressions for which :clj= can be set to true are:
;; - A :set* expression whose single argument is a Clojure set or map containing elements which have :clj= true. A Clojure map represents a set of arrows.
;; - A :seq expression whose elements have :clj= true.
;; - :num and :str expressions don't set :clj=, as it is implicit.


(defn eval-name [ctx exp eval-fn]
  (let [name (first-arg exp)]
    (if (predefined-names name)
      exp
      (or (ctx (first-arg exp))
          (with-meta
            [:in (with-meta
                   [:name "Any"]
                   (meta exp))]
            (meta exp))))))


(defn eval-in [ctx exp eval-fn]
  (let [v (eval-fn ctx (first-arg exp))]
    (with-meta
      (if (has-type :set v)
        (into [:vals] (all-args v))
        [:in v])
      (meta exp))))


(defn eval-vals [ctx exp eval-fn]
  (let [exps (->> exp
                  all-args
                  (mapcat (fn [arg]
                            (let [v (eval-fn ctx arg)]
                              (if (has-type :vals v)
                                (all-args v)
                                (list v)))))
                  ;; Deduplicate, ignoring differences in metadata.
                  ;; This can be done for values with and without :clj= true.
                  set
                  seq)]
    (cond
      (and (first exps)
           (empty? (rest exps)))
      (first exps)
      
      :else
      (with-meta
        (into [:vals] exps)
        (meta exp)))))


(defn eval-tls [ctx exp]
  (if (and (vector? exp)
           (or (not (meta exp))
               (not ((meta exp) :evaled))))
    (set-evaled
     ((case (first exp)
        :apply eval-apply
        :in eval-in
        :inter eval-inter
        :name eval-name
        :set eval-set
        :set* eval-set*
        :union eval-union
        :vals eval-vals
        (fn [_ exp _] exp))
      ctx exp eval-tls))
    exp))


(defn eval-context [ctx]
  (reduce (fn [prev-ctx name]
            (into prev-ctx
                  {name (eval-tls
                         prev-ctx
                         (prev-ctx name))}))
          ctx
          (keys ctx)))
