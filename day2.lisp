(in-package :day2)

(defparameter day2-test-input "~/Projects/advent-of-code-2022/input/day2-test.txt")
(defparameter day2-input "~/Projects/advent-of-code-2022/input/day2.txt")

;;---------------------------------------------
;; Part 1 - score for different combinations
;; A  X  Rock
;; B  Y  Paper
;; C  Z  Scissors
;; Combination | Play | Result | Total |
;;     A X     |  1   |   3    |   4   |
;;     A Y     |  2   |   6    |   8   |
;;     A Z     |  3   |   0    |   3   |
;;     B X     |  1   |   0    |   1   |
;;     B Y     |  2   |   3    |   5   |
;;     B Z     |  3   |   6    |   9   |
;;     C X     |  1   |   6    |   7   |
;;     C Y     |  2   |   0    |   2   |
;;     C Z     |  3   |   3    |   6   |
;;-------------+------+--------+-------+



(defparameter +results+ '((AX 4)
                          (AY 8)
                          (AZ 3)
                          (BX 1)
                          (BY 5)
                          (BZ 9)
                          (CX 7)
                          (CY 2)
                          (CZ 6)))

(defparameter *results* (make-hash-table))

(defun process-input (input-file)
  (mapcar (lambda (a) (intern (remove #\Space a))) (uiop:read-file-lines input-file)))

(defun init (input-file)
  "Load initial data structures"
  (loop :for value in +results+ :collect (destructuring-bind (a . score) value (setf (gethash a *results*) (car score))))
  (process-input input-file))


(defun part1 (input-file)
  "Advent of code 2022 - day 2 part 1"
  (let ((input (init input-file)))
    (loop :for match
            :in input
          :sum (gethash match *results*))))


(defun test1 ()
  (part1 day2-test-input))

(defun solution1 ()
  (part1 day2-input))

;;---------------------------------------------
;; Part 2 - score for different combinations
;; A Rock             X Lose
;; B Paper            Y Draw
;; C Scissors         Z Win
;; Combination | Play | Play | Result | Total |
;;     A X     |  C   |  3   |   0    |   3   |
;;     A Y     |  A   |  1   |   3    |   4   |
;;     A Z     |  B   |  2   |   6    |   8   |
;;     B X     |  A   |  1   |   0    |   1   |
;;     B Y     |  B   |  2   |   3    |   5   |
;;     B Z     |  C   |  3   |   6    |   9   |
;;     C X     |  B   |  2   |   0    |   2   |
;;     C Y     |  C   |  3   |   3    |   6   |
;;     C Z     |  A   |  1   |   6    |   7   |
;;-------------+------+------+--------+-------+

(defparameter +results2+ '((AX 3)
                           (AY 4)
                           (AZ 8)
                           (BX 1)
                           (BY 5)
                           (BZ 9)
                           (CX 2)
                           (CY 6)
                           (CZ 7)))

(defparameter *results2* (make-hash-table))

(defun init2 (input-file)
  "Load data structures for part 2"
  (loop :for value in +results2+ :collect (destructuring-bind (a . score) value (setf (gethash a *results2*) (car score))))
  (process-input input-file))


(defun part2 (input-file)
  (let ((input (init2 input-file)))
    (loop :for match
            :in input
          :sum (gethash match *results2*))))

(defun test2 ()
  (part2 day2-test-input))

(defun solution2 ()
  (part2 day2-input))
