(ns timeless.run.eval
  "Evaluate TLS expressions."
  (:require [timeless.utils :refer :all]))


(declare eval-tls)


(defn set-evaled [exp]
  (if (vector? exp)
    (with-meta
      exp
      (into (or (meta exp) {})
            {:evaled true}))
    exp))

(defn reset-evaled [exp]
  (if (and (vector? exp)
           (meta exp))
    (with-meta exp (dissoc (meta exp) :evaled))
    exp))


(defn calc-++ [exp]
  (let [[_ _ exp1 exp2] exp
        disallowed-types #{:num :set}]
    (cond
      (or (has-types disallowed-types exp1)
          (has-types disallowed-types exp2))
      (with-meta
        [:vals]
        (meta exp))
      
      (and (has-type :str exp1)
           (has-type :str exp2))
      (with-meta
        [:str (str (first-arg exp1)
                   (first-arg exp2))]
        (meta exp))

      (and (has-type :seq exp1)
           (has-type :seq exp2))
      (with-meta
        (into [:seq] (concat (all-args exp1)
                             (all-args exp2)))
        (meta exp))

      :else [:vals])))

(defn calc-arith [exp]
  (let [[_ op exp1 exp2] exp
        disallowed-types #{:set :seq :str}]
    (cond
      (or (has-types disallowed-types exp1)
          (has-types disallowed-types exp2))
      (with-meta
        [:vals]
        (meta exp))

      (and (= "/" (first-arg op))
           (has-type :num exp2)
           (= 0 (first-arg exp2)))
      (with-meta
        [:vals]
        (meta exp))
      
      (and (has-type :num exp1)
           (has-type :num exp2))
      (with-meta
        [:num (({"+" +
                 "-" -
                 "*" *
                 "/" /}
                (first-arg op))
               (first-arg exp1)
               (first-arg exp2))]
        (meta exp))

      :else nil)))

;; Returns nil when can't calculate, so unchanged.
(defn calc-predefined [exp]
  (when-let [f ({"+" calc-arith
                 "-" calc-arith
                 "*" calc-arith
                 "/" calc-arith
                 "++" calc-++}
                (first-arg (first-arg exp)))]
    (f exp)))

(def predefined-operators #{"+" "-" "*" "/" "++"})

(def predefined-names
  (into predefined-operators
        ["Any" "Num" "Int" "Bool" "Sym" "Tag" "Arr" "Set" "Fn" "Seq" "Str" "Char"]))

(defn calc-neg [exp]
  (let [arg (second-arg exp)]
    (cond
      (has-types #{:set :seq :str} arg)
      (with-meta
        [:vals]
        (meta exp))
      
      (has-type :num arg)
      (with-meta
        [:num (- (first-arg arg))]
        (meta exp))

      :else nil)))


(defn eval-apply [ctx exp]
  (let [exps (map (partial eval-tls ctx)
                  (all-args exp))
        exp (with-meta
              (into [:apply] exps)
              (meta exp))]
    (cond
      (empty? (rest exps))
      (first exps)

      (has-type :apply (first exps))
      (eval-tls ctx
                (with-meta
                  (into [:apply] (concat (all-args (first exps))
                                         (rest exps)))
                  (meta exp)))

      (has-type :vals (first exps))
      (eval-tls ctx
                (with-meta
                  (into [:vals] (map (fn [v]
                                       (with-meta
                                         (into [:apply v]
                                               (rest exps))
                                         (meta exp)))
                                     (all-args (first exps))))
                  (meta (reset-evaled
                         (first exps)))))

      (has-type :vals (second exps))
      (eval-tls ctx
                (with-meta
                  (into [:vals] (map (fn [v]
                                       (with-meta
                                         (into [:apply (first exps) v]
                                               (third-on exps))
                                         (meta exp)))
                                     (all-args (second exps))))
                  (meta (reset-evaled
                         (second exps)))))

      (and (>= (count exps) 3)
           (has-type :vals (third exps)))
      (eval-tls ctx
                (with-meta
                  (into [:vals] (map (fn [v]
                                       (with-meta
                                         (into [:apply (first exps) (second exps) v]
                                               (fourth-on exps))
                                         (meta exp)))
                                     (all-args (third exps))))
                  (meta (reset-evaled
                         (third exps)))))

      (and (>= (count exps) 3)
           (has-type :name (first exps))
           (predefined-operators (first-arg (first exps))))
      (let [v (calc-predefined exp)]
        (if v
          (eval-tls ctx
                    (with-meta
                      (into [:apply v] (fourth-on exps))
                      (meta exp)))
          exp))

      (has-type :neg (first exps))
      (let [v (calc-neg exp)]
        (if v
          (eval-tls ctx
                    (with-meta
                      (into [:apply v] (third-on exps))
                      (meta exp)))
          exp))

      :else exp)))

(defn eval-in [ctx exp]
  (let [v (eval-tls ctx (first-arg exp))]
    (with-meta
      (if (has-type :set v)
        (into [:vals] (all-args v))
        [:in v])
      (meta exp))))

;; TODO: rewrite this like eval-union
(defn eval-inter [ctx exp]
  (let [exps (->> exp
                  all-args
                  (mapcat (fn [subexp]
                            (let [v (eval-tls ctx subexp)]
                              (if (has-type :inter v)
                                (all-args v)
                                (list v)))))
                  (remove (partial has-name "Any")))]
    (cond
      (empty? exps)
      (with-meta
        [:name "Any"]
        (meta exp))

      (some (partial has-type :num)
            exps)
      (with-meta
        [:vals]
        (meta exp))

      (empty? (rest exps))
      (first exps)
      
      :else
      (with-meta
        (into [:inter] exps)
        (meta exp)))))

(defn merge-union-pair [ctx exp1 exp2]
  (cond
    (has-no-value exp1)
    exp1
        
    (has-no-value exp2)
    exp2

    (is-empty-set exp1)
    exp2

    (is-empty-set exp2)
    exp1

    (has-name "Any" exp1)
    exp1

    (has-name "Any" exp2)
    exp2

    (and (has-name "Int" exp1)
         (has-name "Num" exp2))
    exp2

    (and (has-name "Num" exp1)
         (has-name "Int" exp2))
    exp1

    (and (has-type :set exp1)
         (has-type :set exp2))
    (with-meta
      (into [:set] (set (concat (all-args exp1)
                                (all-args exp2)))))

    ;; TODO: more cases

    :else nil))

;; TODO: reduce on (rest exps), each time returning two values: boolean whether the pairwise merge of (first exps) and the next exp worked, and all the exps checked, whether merged or not. Then call this function recursively on (rest exps), and either cons (first exps) on the head or not, depending on the boolean.
(defn pairwise-reduce-union [exps]
)

(defn eval-union [ctx exp]
  (let [exps (->> exp
                  all-args
                  (mapcat (fn [subexp]
                            (let [v (eval-tls ctx subexp)]
                              (if (has-type :union v)
                                (all-args v)
                                (list v))))))]
    (cond
      (empty? exps)
      (with-meta
        [:set]
        (meta exp))

      (empty? (rest exps))
      (first exps)

      :else
      (pairwise-reduce-union exps))))

(defn eval-name [ctx exp]
  (let [name (first-arg exp)]
    (if (predefined-names name)
      exp
      (or (ctx (first-arg exp))
          (with-meta
            [:in (with-meta
                   [:name "Any"]
                   (meta exp))]
            (meta exp))))))

(defn eval-set [ctx exp]
  (let [exps (->> exp
                  all-args
                  (map (partial eval-tls ctx))
                  set ; Deduplicate, ignoring differences in metadata.
                  seq)
        {in-exps true other-exps false}
        (group-by (partial has-type :in)
                  exps)
        
        new-set (with-meta
                  (into [:set] other-exps)
                  (meta exp))]
    (if in-exps
      (eval-tls ctx
                (with-meta
                  (into [:union new-set]
                        (map first-arg in-exps))
                  (meta exp)))
      new-set)))

(defn eval-vals [ctx exp]
  (let [exps (->> exp
                  all-args
                  (mapcat (fn [arg]
                            (let [v (eval-tls ctx arg)]
                              (if (has-type :vals v)
                                (all-args v)
                                (list v)))))
                  set ; Deduplicate, ignoring differences in metadata.
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
        :union eval-union
        :vals eval-vals
        (fn [ctx exp] exp))
      ctx exp))
    exp))


(defn eval-context [ctx]
  (reduce (fn [prev-ctx name]
            (into prev-ctx
                  {name (eval-tls
                         prev-ctx
                         (prev-ctx name))}))
          ctx
          (keys ctx)))
