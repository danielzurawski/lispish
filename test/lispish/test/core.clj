(ns lispish.test.core
  (:use [lispish.core])
  (:use [clojure.test]))

(deftest plus
  (is (= "(2+2)" (lisp-to-js "(+ 2 2)"))))

(deftest minus
  (is (= "(2-2)" (lisp-to-js "(- 2 2)"))))

(deftest multiply
  (is (= "(2*2)" (lisp-to-js "(* 2 2)"))))

(deftest divide
  (is (= "(2/2)" (lisp-to-js "(/ 2 2)"))))

(deftest if-form
  (is (= "((5>10) ? (true):(false))" (lisp-to-js "(if (> 5 10)\"true\" \"false\")"))))

(deftest fn-form
  (is (= "function(x) {return (x*x)}" (lisp-to-js "(fn [x] (* x x))"))))

(deftest let-form
  (is (= "var x;x=5;" (lisp-to-js "(let [x 5])"))))

(deftest let-lambda-function
  (is (= "var x;x=function(x) {return (x*5)};" (lisp-to-js "(let [x (fn [x] (* x 5))])"))))

(deftest defn-form
  (is (= "function square(x) {return (x*x)}" (lisp-to-js "(defn square [x] (* x x))"))))

(deftest fibonacci-example
  (is (= "function fib(n) {return ((n<2) ? (1):((fib(((n-1)))+fib(((n-2))))))}"
         (lisp-to-js "(defn fib [n] (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))" ))))

(deftest factorial-example
  (is (= "function factorial(n) {return ((n<2) ? (1):((n*factorial(((n-1))))))}"
         (lisp-to-js "(defn factorial [n] (if (< n 2) 1 (* n (factorial (- n 1)))))"))))

(deftest ackermann-function
  (is (= "function ackermann(m, n) {return ((m==0)?(n+1):((n==0)?ackermann((m-1), 1):ackermann((m-1), ackermann(m, (n-1)))))}"
         (lisp-to-js "(defn ackermann [m n]
                       (cond (= m 0) (+ n 1)
                             (= n 0) (ackermann (- m 1) 1)
                             :else (ackermann (- m 1) (ackermann m (- n 1)))))"))))

;; Test na let z wartoscia

;; Test z is_prime z temp.cl
