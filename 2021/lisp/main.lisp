;;;; tests/main.lisp
;; note: answers in this test suite relate to the data provided to me
;;       by the advent of code website. Different data sets are provided
;;       to different users - your data set and answers therefore will be
;;       different.

(defpackage #:advent2021/test
  (:use #:cl
        #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package #:advent2021/test)

(def-suite advent2021
  :description "test suite - ensure refactoring doesn't break anything!")

(in-suite advent2021)

(test day1-test
  (is (= (day1:test1) 7))
  (is (= (day1:solution1) 1215)))
