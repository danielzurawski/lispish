(ns #^{:author "Daniel Zurawski"
       :doc "A simple Lisp to JavaScript transcompiler written in Clojure."}
  lispish.core
  [:require [clojure.string :as str]]
  [:use [clojure.walk]
        [clojure.tools.trace]])

(def op (set ['+ '- '* '/ '> '< '=]))
(def forms (set ['let 'if 'fn 'recur]))

;;
;; dogdogdog
;; godgodgod
;;

;; Clojure is a single pass compiler, thus we have to use forward declaration
;; if we need to use a function before it's declared
(declare emit-list)

(defn emit [expressions]
  "Take an s-expression and emit its corresponding JavaScript form"
  (do
    (println "type of expression: " (type expressions))
    (cond
      (nil? expressions) "null"
      (symbol? expressions) (do (println "emit: symbol")  (str expressions))
      (list? expressions) (do (println "emit: list") (emit-list expressions))
      (integer? expressions) (do (println "emit: integer") (str expressions))
      (float? expressions) (do (println "emit: float") (str expressions))
      (string? expressions) (do (println "emit: string") (str expressions)) )))

;; Abstract Structural Binding - + falls in type, + in op and 2 2 in tail
(defn emit-op [type [op & tail]]
  "Emit s-expression with single operators and two arguments"
  (do (println "emit-op, type: " type ", op: " (str " " op " ") ", tail: " tail ", tail first type: " (type (first tail)))
      ;; Interlace the arguments with the operator
      (str "(" (clojure.string/join
                (str op)
                (map emit tail))
           ")")))

(defn emit-let [type [let [x y]]]
  (str "var " x ";"
       x "=" (emit y) ";"))

(defn emit-if [type [if condition true-form & false-form]]
  (str "if"
       (emit condition)
       " {"
       (emit true-form)
       "} else {"
       (emit false-form)
       "}"))

(defn emit-fn [type [fn [arg] & rest]]
  (str "function(" arg ") {"
       "return "
       (emit rest)
       "}"))

(defn emit-recur [head expression]
  (println "emit recur, head: " head ", expression: " expression))

(defn emit-forms [head expression]
  (do (println "emit-forms, head: " head ", expression: " expression)
      (cond (= head 'let) (emit-let head expression)
            (= head 'if) (emit-if head expression)
            (= head 'fn) (emit-fn head expression)
            (= head 'recur) (emit-recur head expression))
      )
  )

(comment
  (defn emit-forms [type [bracket-or-x? [x y]]]
  (println "printing type: " (type type))
  (cond (= type 'let) (emit-let x y)
        (= type 'if) ())))

(defn emit-list [expressions]
  (do (println "emit-list expressions: " expressions)
      (if (symbol? (first expressions))

        (let [head (symbol (first expressions))
              expressions (conj
                           (rest expressions) head)]

          (println "emit list head: " head
                   ", expression: rest " (rest expressions) ", conj: "
                   ", symbol first:" (symbol (first expressions)))
          (cond
            (contains? op head) (emit-op head expressions)
            (contains? forms head) (emit-forms head expressions)

            :else "cos nowego w liscie"))
        ;; Not safe, may run into stack overflow if this will be a list or not-recognized
        (emit (first expressions)))))

(defn detect-recur [forms]
  (map #(if (= % 'recur) (print true) false) forms))

;; Macro not to evaluate the forms directly
(defmacro lisp-to-js [forms]
  "Convert a Lispy expression to its equivalent JavaScript expression.
   Returns JavaScript code."
  (emit forms)
;;  (detect-recur forms)
  )

;; TODO: TDD-quality test coverage!!

;; Simple arighmetic expression
;; (lisp-to-js (+ 2 2))
;; (lisp-to-js (- 2 2))

;; All of the forms ( including (let ) ) form (at the moment it only takes 1 argument and does not have implicit DO form)
;; (lisp-to-js (let [x 20]))

;; If form
;; (lisp-to-js (if (> x 10) 1 0))

;; Fn form with let
;; (lisp-to-js (let [x (fn [x] (* x x))]))

;; If with fn and let
;; (lisp-to-js (let [x (fn [x] (if (> x 5) 1 0))]))

;; Inner anonymous functions with scope issues (var declared inside of the if test)
;; (lisp-to-js (if (= x "test") (let [b (fn [x] (* x x))]) (let [c (fn [x] (/ x x))])))

;; Let doesn't have an implicit do form
;; (lisp-to-js (let [x 10] (- 15 x)))
;; An alternative is to define a function assigned to variable that takes an X as an arugment
;; !! (lisp-to-js (let [x (fn [x] (- 15 x))])) !!

;; TODO:
;; 1. Let with arithmetic - fails as there are multiple arguments to let
;; 2. Recur function
;; 3. Annonymous inner functions are locally scoped within for e.g. if statements
