;;;; advent2021.asd

(defsystem #:advent2022
  :description "Advent of Code 2022"
  :author "Paul Jewell <paul@teulu.org>"
  :license "GNU3"  ;; Check proper license attribution
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:fiveam)
  :components ((:file "package")
               (:file "day1"))
  :in-order-to ((test-op (test-op #:advent2022/test))))

(defsystem #:advent2022/test
  :serial t
  :depends-on (#:advent2022
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op _) (symbol-call :fiveam :run-all-tests)))
