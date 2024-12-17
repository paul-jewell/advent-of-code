(in-package #:day1)

;; Read input into list
(defparameter day1-input "~/Projects/advent-of-code-2024/input/day1.txt")
(defparameter day1-test-input "~/Projects/advent-of-code-2024/input/day1-test.txt")

(defun depths (file)
  (mapcar #'parse-integer (uiop:read-file-lines file)))

(defun test1 ()
  (count-deeper 0 (depths day1-test-input)))

(defun solution1 ()
  (count-deeper 0 (depths day1-input)))

(defun count-deeper (qty depths)
  (if (>= 1 (length depths))                    ;; No more depths to process
      qty
      (if (< (car depths) (cadr depths))        ;; Getting deeper...
          (count-deeper (+ qty 1) (cdr depths)) ;; increment count
          (count-deeper qty (cdr depths)))))    ;; or not, as required



