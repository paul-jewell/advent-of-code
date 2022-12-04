;;;; tests/main.lisp
;; note: answers in this test suite relate to the data provided to me
;;       by the advent of code website. Different data sets are provided
;;       to different users - your data set and answers therefore will be
;;       different.

(defpackage #:advent2022/test
  (:use #:cl
        #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package #:advent2022/test)

(def-suite advent2022
  :description "test suite - ensure refactoring doesn't break anything!")

(in-suite advent2022)

(test day1-test
  (is (= (day1:test1) 24000))
  (is (= (day1:solution1) 75501))
  (is (= (day1:test2) 45000))
  (is (= (day1:solution2) 215594)))

(test day2-test
  (is (= (day2:test1)     15))
  (is (= (day2:solution1) 15337))
  (is (= (day2:test2)     12))
  (is (= (day2:solution2) 11696)))
