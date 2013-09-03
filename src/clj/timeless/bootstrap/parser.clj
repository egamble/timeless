(ns timeless.bootstrap.parser
  "Parser for Timeless. Mainly intended to parse a self-hosting Timeless compiler."
  (:require [name.choi.joshua.fnparse :as p]))


;;; memoized rule generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro complex-m
  [steps & product-expr]
  `(memoize (p/complex ~steps ~@product-expr)))

(defmacro alt-m
  [& subrules]
  `(memoize (p/alt ~@subrules)))


;;; lexer character rules
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nb-char-rule [rule]
  (p/invisi-conc (p/except rule (p/lit \newline))
                 (p/update-info :col inc)))

(defn nb-char [c] (nb-char-rule (p/lit c)))

(def any-nb-char (nb-char-rule p/anything))

(def newline-lex
  (p/invisi-conc (p/lit \newline)
                 (p/set-info :col 1)
                 (p/update-info :line inc)))

(def digit-lex (p/lit-alt-seq "0123456789" nb-char))

(def sign-lex (p/alt (nb-char \+) (nb-char \-)))

(def letter-lex
  (p/lit-alt-seq (map char (concat (range (int \A) (inc (int \Z)))
                                   (range (int \a) (inc (int \z)))))
                 nb-char))

(def single-quote-lex (nb-char \'))
(def double-quote-lex (nb-char \"))

(def comment-lex (p/conc (nb-char \#)
                         (p/rep* any-nb-char)
                         newline-lex))

(def ws (p/rep* (p/alt comment-lex newline-lex (p/lit-alt-seq " \t" nb-char))))


;;; error reporting
;;;;;;;;;;;;;;;;;;;

(defn error [msg pos-map]
  (throw (RuntimeException.
          (str msg " at (" (:line pos-map) "," (:col pos-map) ")"))))

(defn failpoint-error-fn [msg pos-map]
  (fn [_ _] (error msg pos-map)))


;;; node builders
;;;;;;;;;;;;;;;;;

(defn make-node [type pos-map val]
  {:type type
   :val val
   :line (:line pos-map)
   :col (:col pos-map)})

(defmacro deflex
  "Node builder for the lex phase.
   Binds the unqualified name 'pos-state' to the initial state.
   The name 'pos-state' may be used within the step forms.
   Returns a node of the given type and value."
  [name type steps val]
  `(def ~name
     (complex-m [~'pos-state p/get-state ~@steps]
                (make-node ~type ~'pos-state ~val))))


;;; lexer token rules
;;;;;;;;;;;;;;;;;;;;;

(deflex name-lex :name
  [c letter-lex
   cs (p/rep* (p/alt letter-lex digit-lex))]
  (apply str c cs))

(deflex quoted-name-lex :name
  [_ single-quote-lex
   cs (p/rep*
       (p/alt
        (p/lit-conc-seq "\\\\" nb-char )
        (p/lit-conc-seq "\\'" nb-char)
        (p/except (p/alt any-nb-char newline-lex) (nb-char \'))))
   _ (p/failpoint single-quote-lex
                  (failpoint-error-fn "Unmatched single quote"
                                      pos-state))]

  (let [f #(if (seq? %) (second %) %)]
    (apply str (map f cs))))

(deflex underscore-name-lex :name
  [c (nb-char \_)]
  (str c))

(deflex string-lit :str
  [_ double-quote-lex
   cs (p/rep*
       (p/alt
        (p/lit-conc-seq "\\\\" nb-char)
        (p/lit-conc-seq "\\\"" nb-char)
        (p/except (p/alt any-nb-char newline-lex) double-quote-lex)))
   _ (p/failpoint double-quote-lex
                  (failpoint-error-fn "Unmatched double quote"
                                      pos-state))]

  (let [f #(if (seq? %) (second %) %)]
    (apply str (map f cs))))

(deflex char-lit :char
  [_ (nb-char \\)
   cs (p/alt (p/lit-conc-seq "tab" nb-char)
             (p/lit-conc-seq "space" nb-char)
             (p/lit-conc-seq "newline" nb-char)
             (p/invisi-conc any-nb-char (p/not-followed-by letter-lex))
             newline-lex)]
  (if (seq? cs)
    (read-string (apply str (cons \\ cs)))
    cs))

(def num-magnitude (p/conc (p/rep+ digit-lex)
                           (p/opt (p/conc (nb-char \.) (p/rep+ digit-lex)))))

(deflex signed-number-lit :num
  [_ ws
   sn (p/opt sign-lex)
   d num-magnitude
   _ ws]
  (read-string
   (apply str (flatten [sn d]))))

(deflex unsigned-number-lit :num
  [_ ws
   d num-magnitude
   _ ws]
  (read-string
   (apply str (flatten d))))

(def non-num-atom-lex
  (complex-m
   [_ ws
    a (p/alt name-lex quoted-name-lex underscore-name-lex string-lit char-lit)
    _ ws]
   a))

(deflex left-lex :bracket
  [_ ws
   c (p/lit-alt-seq "([{" nb-char)
   _ ws]
  (str c))

(deflex right-lex :bracket
  [_ ws
   c (p/lit-alt-seq ")]}" nb-char)
   _ ws]
  (str c))

(defmacro alt-lex
  "strs is a collection of op strings, or of tuples of op str and unicode alternative.
   Expands into a rule whose value on success is the matched op str."
  [strs]
  (let [rules (map (fn [s]
                     (let [s (if (coll? s) s [s])
                           [s1 c] s
                           r `(p/lit-conc-seq ~s1 nb-char)
                           r (if c
                               `(p/alt ~r (nb-char ~c))
                               r)]
                       `(p/semantics ~r (constantly ~s1))))
                   strs)]
    `(p/alt ~@rules)))

(deflex sep-lex :sep
  [_ ws
   s (alt-lex [["->" \u2192] ","])
   _ ws]
  s)

(deflex op-lex :op
  [_ ws
   s (alt-lex
      [["/=" \u2260] ["<=" \u2264] [">=" \u2265]
       ["&&" \u2227] ["||" \u2228] ["&" \u2229] ["|" \u222A]
       "++" "+" "-\\" "-" "*" "/" "<" ">" "=" ":" "..." ".."])
   _ ws]
  s)

(def tokens
  (p/semantics
   (p/rep*
    (p/alt (p/conc (p/alt left-lex sep-lex op-lex) (p/opt signed-number-lit))
           unsigned-number-lit
           non-num-atom-lex
           right-lex))
   (comp (partial remove nil?) flatten)))


;;; tokenize
;;;;;;;;;;;;

(defn tokenize [src]
  (let [e (fn [state]
            (error "Failure to tokenize" state))]
    (p/rule-match tokens
                  (fn [s] (e s))
                  (fn [_ s] (e s))
                  {:remainder src, :line 1, :col 1})))


;;; node detector
;;;;;;;;;;;;;;;;;

(defn node
  "Detects a node of the given type(s) (a keyword or a set of keywords) and,
   optionally, with the given value or one of a set of values."
  [type & [val]]
  (p/term
   (fn [n]
     (let [t (:type n)
           v (:val n)]
       (and (if (set? type) (type t) (= type t))
            (or (not val)
                (if (set? val) (val v) (= val v))))))))


;;; shatter tokens into groups, one group for each top-level assertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare expr-shatter)

(defmacro defunit-shatter
  [name left right]
  `(def ~name
     (complex-m
      [n1# (node :bracket ~left)
       s# (p/rep* expr-shatter)
       n2# (p/failpoint
            (node :bracket ~right)
            (failpoint-error-fn (str "Unmatched \"" ~left "\"")
                                n1#))]
      [n1# s# n2#])))

(defunit-shatter paren-shatter   "(" ")")
(defunit-shatter bracket-shatter "[" "]")
(defunit-shatter brace-shatter   "{" "}")

(def expr-shatter (p/alt (node #{:name :str :num :char :sep :op})
                         paren-shatter
                         bracket-shatter
                         brace-shatter))

(defn shatter
  "Split tokens into groups corresponding to top-level assertions."
  [tokens]
  (let [e (fn [state]
            (error "Failure to shatter into top-level assertions"
                   (first (:remainder state))))
        m (p/rule-match
           (p/rep+ (p/conc (node :name)
                           (node :op "=")
                           (p/rep+ (p/invisi-conc expr-shatter
                                                  (p/not-followed-by (node :op "="))))))
           (fn [s] (e s))
           (fn [_ s] (e s))
           {:remainder tokens})]
    ;; remove nils because p/rep* can return nil
    (map (comp (partial remove nil?) flatten) m)))


;;; seq literal
;;;;;;;;;;;;;;;

(declare expr)

(def empty-seq-lit
  (p/semantics (p/conc (node :bracket "[")
                       (node :bracket "]"))
               #(make-node :seq (first %) [])))

(def non-empty-seq-lit
  (complex-m
   [pos-node (node :bracket "[")
    es (p/rep* (p/invisi-conc expr (node :sep ",")))
    e expr
    _ (node :bracket "]")]
   (make-node :seq pos-node (vec (conj es e)))))

(def seq-lit (p/alt empty-seq-lit non-empty-seq-lit))


;;; function and set literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-set-lit
  (p/semantics (p/conc (node :bracket "{")
                       (node :bracket "}"))
               #(make-node :set (first %) [])))

(declare non-union-expr)

(def clause
  (complex-m
   [k non-union-expr
    st (p/opt
        (complex-m [_ (node :op "|")
                    st expr]
                   st))
    v (p/opt
       (complex-m [_ (node :sep "->")
                   v expr]
                  v))]
   (make-node :clause k {:key k, :st st, :val v})))

(def fn-lit
  (complex-m
   [pos-node (node :bracket "{")
    es (p/rep* (p/invisi-conc clause (node :sep ",")))
    e clause
    _ (node :bracket "}")]

   (let [clauses (vec (conj es e))
         has-val #(:val (:val %))
         type (if (not-any? has-val clauses)
                :set
                (if (every? has-val clauses)
                  :fn
                  (error "Mixed set and fn clauses" pos-node)))]
     (make-node type pos-node clauses))))


;;; units (atoms and bracketed exprs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def parens
  (complex-m
   [_ (node :bracket "(")
    e (p/alt (node :op) expr)
    _ (node :bracket ")")]
   e))

(def unit (p/alt (node #{:name :str :char :num})
                 parens seq-lit fn-lit empty-set-lit))


;;; function and set application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def application
  (p/semantics
   (p/rep+ unit)

   #(let [e (first %)
          r (rest %)]
      (if (seq r)
        (make-node :apply e
                   (vec
                    (if (= :apply (:type e))
                      (concat (:val e) r)
                      (cons e r))))
        e))))


;;; infix operations
;;;;;;;;;;;;;;;;;;;;

(defn right-assoc [pairs e1]
  (let [[o e2] (first pairs)
        r (rest pairs)
        e2 (if (seq r)
             (right-assoc r e2)
             e2)]
    (make-node :apply e1 [o e1 e2])))

(defmacro defop-right
  "Makes right associative infix parse rule."
  [op-name op-str higher-rule]
  `(def ~op-name
     (complex-m
      [e# ~higher-rule
       s# (p/rep* (p/conc (node :op ~op-str) ~higher-rule))]
      (if (seq s#)
        (right-assoc s# e#)
        e#))))


(defn left-assoc [reversed-pairs e2 pos-node]
  (let [[e1 o] (first reversed-pairs)
        r (rest reversed-pairs)
        e1 (if (seq r)
             (left-assoc r e1 pos-node)
             e1)]
    (make-node :apply pos-node [o e1 e2])))

(defmacro defop
  "Makes left associative infix parse rule. op-strs can be a str or coll of strs."
  [op-name op-strs higher-rule]
  `(def ~op-name
     (alt-m
      (complex-m
       [s# (p/rep+ (p/conc ~higher-rule (node :op ~op-strs)))
        :let [pos-node# (-> s# first first)]
        e# (p/failpoint ~higher-rule
                        (failpoint-error-fn
                         (str "Invalid infix op \""  (-> s# last second :val) "\"")
                         pos-node#))]
       (left-assoc (reverse s#) e# pos-node#))
      ~higher-rule)))


;; op-+     is higher than op-range so that  (a+1 .. b)  works
;; op-range is higher than op-cons  so that  (a : 1..10) works
;; op-+     is higher than op-cons  so that  (a+b : s)   works
;; op-cons  is higher than op-++    so that  (a:b:[] ++ [c,d]) is concat of two seqs
;; op-++    is higher than op-|     so that  (a++b) can be unioned with other fns

(defop        op-*            #{"*" "/"}    application)
(defop        op-+            #{"+" "-"}    op-*)
(defop        op-range        #{".." "..."} op-+)
(defop-right  op-cons         ":"           op-range)
(defop        op-++           "++"          op-cons)
(defop        op-set-diff     "-\\"         op-++)
(defop        op-&            "&"           op-set-diff)
(defop        op-|            "|"           op-&)

(let [eq-strs #{"=" "/=" "<=" ">=" "<" ">"}]

  (defop      op-=            eq-strs       op-|)
  (defop      op-&&           "&&"          op-=)
  (defop      op-||           "||"          op-&&)

  (defop      non-union-op-=  eq-strs       op-&)
  (defop      non-union-op-&& "&&"          non-union-op-=)
  (defop      non-union-op-|| "||"          non-union-op-&&))

(def non-union-expr non-union-op-||)

(def expr op-||)


;;; parse
;;;;;;;;;

(defn parse
  [src-str]
  (let [e (fn [state]
            (error "Failure to parse"
                   (first (:remainder state))))]
    (map (fn [token-group]
           (p/rule-match expr
                         (fn [s] (e s))
                         (fn [_ s] (e s))
                         {:remainder token-group}))
         (shatter (tokenize src-str)))))