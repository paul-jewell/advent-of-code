;;;; tests/main.lisp
;; note: answers in this test suite relate to the data provided to me
;;       by the advent of code website. Different data sets are provided
;;       to different users - your data set and answers therefore will be
;;       different.

(defpackage #:advent2024/test
  (:use #:cl
        #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package #:advent2024/test)

(def-suite advent2024
  :description "test suite - ensure refactoring doesn't break anything!")

(in-suite advent2024)

(test day1-test
      (is (day1:test1) 11))


;; (test day1
;;       (is (= (day1:solution1) 440979))
;;       (is (= (day1:solution2) 82498112)))

