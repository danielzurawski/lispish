(ns lispish.test.core
  (:use [lispish.core])
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(deftest single-arithmetic
  (is (= "(2+2)" (lisp-to-js (+ 2 2)))))
