(in-package :day10)

(defparameter day10-test-input1 "input/day10-test-input-1.txt")
(defparameter day10-test-input2 "input/day10-test-input-2.txt")
(defparameter day10-input "input/day10-input.txt")

(defun part1 (filename)
  (let* ((adaptors (sort (mapcar #'parse-integer
                                 (uiop:read-file-lines filename))
                         #'<))
         (max-jolt (apply #'max adaptors))
         (data (append '(0) adaptors (list (+ max-jolt 3))))
         (one-jolt 0)
         (three-jolt 0))
    (loop :for x from 0 upto (- (length data) 2) 
          :for y from 1 upto (1- (length data))
          :do (if (= (- (nth  y data)
                        (nth  x data)) 1)
                  (setf one-jolt (1+ one-jolt))
                  (setf three-jolt (1+ three-jolt))))
    (* one-jolt three-jolt)))

(defun test1 ()
  (part1 day10-test-input1))

(defun test2 ()
  (part1 day10-test-input2)) ;; process day10-test-input-2


(defun solution1 ()
  (part1 day10-input))

(defun find-combinations (remaining-adaptors)
  (when (= 1 (length remaining-adaptors))
    (return-from find-combinations 1))
  (loop :with start := (first remaining-adaptors)
        :for (val . rest) :on (rest remaining-adaptors)
        :while (<= (- val start) 3)
        :sum (find-combinations (cons val rest))))

(unmemoize-functions)
(memoize-function 'find-combinations :test #'equal)

(defun read-data (filename)
  (cons 0 (sort (mapcar #'parse-integer (uiop:read-file-lines filename)) #'<)))

(defun test3 ()
  (find-combinations (read-data day10-test-input1)))

(defun test4 ()
  (find-combinations (read-data day10-test-input2)))


(defun solution2 ()
  (find-combinations (read-data day10-input)))






