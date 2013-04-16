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

(deftest logical-or
  (is (= "((5>10)||(10>5))" (lisp-to-js "(or (> 5 10) (> 10 5))"))))

(deftest logical-and
  (is (= "((5>10)&&(10>5))" (lisp-to-js "(and (> 5 10) (> 10 5))"))))

(deftest logical-and
  (is (= "(!(5>10))" (lisp-to-js "(not (> 5 10))"))))

(deftest if-form
  (is (= "((5>10) ? (true):(false))" (lisp-to-js "(if (> 5 10)\"true\" \"false\")"))))

(deftest fn-form
  (is (= "function (x) {return (x*x)}" (lisp-to-js "(fn [x] (* x x))"))))

(deftest let-form
  (is (= "(function(x) { return (x*x) })(2)" (lisp-to-js "(let [x 2] (* x x))"))))

(deftest let-lambda-function
  (is (= "(function(times-five) { return times-five((5)) })(function (x) {return (x*5)})" (lisp-to-js "(let [times-five (fn [x] (* x 5))] (times-five 5))"))))

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

(deftest primality-checking-program
  (is (= "function is_prime(num) {return (function(prime_over_two) { return ((num<2)?false:((2==num)?true:((0==(num%2))?false:prime_over_two(num, 3)))) })(function (num, factor) {return ((factor>Math.sqrt((num))) ? (true):(((0==(num%factor)) ? (false):(arguments.callee(num, (2+factor))))))})}"
         (lisp-to-js "(defn is_prime [num]
        (let [prime_over_two
                (fn [num factor]
                        (if (> factor (Math.sqrt num))
                                true
                                (if (= 0 (mod num factor))
                                        false
                                        (recur num (+ 2 factor)))))]
        (cond
                (< num 2) false
                (= 2 num) true
                (= 0 (mod num 2)) false
                :else (prime_over_two num 3))))"))))
