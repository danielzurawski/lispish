(ns lispish.test.core
  (:use [lispish.core])
  (:use [clojure.test]))

(deftest plus
  (is (= "(2+2)" (lisp-to-js (+ 2 2)))))

(deftest minus
  (is (= "(2-2)" (lisp-to-js (- 2 2)))))

(deftest multiply
  (is (= "(2*2)" (lisp-to-js (* 2 2)))))

(deftest divide
  (is (= "(2/2)" (lisp-to-js (/ 2 2)))))
;; Lisp (/ 2 2) = 1; JavaScript (2/2) = 1

(deftest if-form
  (is (= "if(5>10) {true} else {false}" (lisp-to-js (if (> 5 10) "true" "false")))))

(deftest fn-form
  (is (= "function(x) {return (x*x)}" (lisp-to-js (fn [x] (* x x))))))

(deftest fn-form-with-let
  (is (= "var x;x=function(x) {return (x*x)};" (lisp-to-js (let [x (fn [x] (* x x))])))))

(deftest recursion
  (is (= "function fib(n) {if(n<2) {return 1;} else { return (fib(n-1)+fib(n-2)); }}"
         (lisp-to-js (defn fib [n] (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))))
