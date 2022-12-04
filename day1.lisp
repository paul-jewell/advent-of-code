(in-package :day1)

;; Read input into list
(defparameter day1-input "~/Projects/advent-of-code-2022/input/day1.txt")
(defparameter day1-test-input "~/Projects/advent-of-code-2022/input/day1-test.txt")



(defun find-max-calories (input-file)
  (reduce #'max
          (mapcar (lambda (l) (apply #'+ (mapcar #'parse-integer l)))
                  (mapcar (lambda (s) (split "\\n" s)) (split "\\n\\n" (uiop:read-file-string input-file))))))

(defun list-of-values (file)
          (mapcar (lambda (l) (apply #'+ (mapcar #'parse-integer l)))
                  (mapcar (lambda (s) (split "\\n" s)) (split "\\n\\n" (uiop:read-file-string file)))))

(defun sum-top-three-calories (input-file)
  (loop :for i :in (sort (list-of-values input-file) #'>)
        :for j :below 3
        :sum i))

(defun test1 ()
  (find-max-calories day1-test-input))

(defun solution1 ()
  (find-max-calories day1-input))

(defun test2 ()
  (sum-top-three-calories day1-test-input))

(defun solution2 ()
  (sum-top-three-calories day1-input))
