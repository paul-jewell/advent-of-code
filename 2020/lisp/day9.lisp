(in-package :day9)

(defparameter day9-test-input "input/day9-test-input.txt")
(defparameter day9-input "input/day9-input.txt")

(defun sums-in-preamble (list)
  (loop :for values on list
        :for x := (first values)
        :nconc (loop :for y :in (rest values)
                  :when (/= x y)
                    :collect (+ x y))))

(defun part1 (filename preamble)
  (let ((data (mapcar #'parse-integer (uiop:read-file-lines filename))))
    (loop :for i :from preamble
            :to (1- (length data))
          :when (null (find (nth i data) (sums-in-preamble (subseq data (- i preamble) i))))
            :collect (nth i data))))

(defun test1 ()
  (car (part1 day9-test-input 5)))

(defun solution1 ()
  (car (part1 day9-input 25)))

(defun find-value (value live-list remaining-list)
  (let ((sum (apply #'+ live-list)))
    (cond ((null live-list) (find-value value (list (car remaining-list)) (cdr remaining-list)))
          ((>  sum value) (find-value value (butlast live-list) remaining-list)) ; drop 1st value
          ((= sum value) (+ (apply #'min live-list) (apply #'max live-list)))   ; successful completion
          (t (find-value value (cons (car remaining-list) live-list) (cdr remaining-list))))))

(defun test2 ()
  (find-value (test1) nil (mapcar #'parse-integer (uiop:read-file-lines day9-test-input))))

(defun solution2 ()
  (find-value (solution1) nil (mapcar #'parse-integer (uiop:read-file-lines day9-input))))
