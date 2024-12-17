;;;; advent2021.asd

(defsystem #:advent2024
  :description "Advent of Code 2024"
  :author "Paul Jewell <paul@teulu.org>"
  :license "GNU3"  ;; Check proper license attribution
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:fiveam)
  :components ((:file "package")
               (:file "day1"))
  :in-order-to ((test-op (test-op #:advent2024/test))))

(defsystem #:advent2024/test
  :serial t
  :depends-on (#:advent2024
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op _) (symbol-call :fiveam :run-all-tests)))
