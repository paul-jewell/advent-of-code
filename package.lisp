;;;; package.lisp

(defpackage #:advent2020
  (:use #:cl
        #:cl-ppcre)
  (:export day1/solution1
           day1/solution2

           day2/solution1
           day2/solution2

           day3/solution1
           day3/solution2

           day4/test1
           day4/solution1
           day4/test2
           day4/solution2

           day5/test1
           day5/solution1
           day5/solution2

           day6/test1
           day6/solution1
           day6/solution2

           day7/test1
           day7/solution1
           day7/test2
           day7/solution2))



(defpackage #:advent2020/test
  (:use #:cl
        #:advent2020
        #:fiveam)
  (:export #:run!
           #:all-tests))

