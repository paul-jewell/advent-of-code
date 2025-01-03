(in-package #:day3)

(defparameter day3-input "input/day3-input.txt")

(defun parse-line (tree-line)
  (coerce tree-line 'list))

(defparameter tree-lines (mapcar #'parse-line (uiop:read-file-lines day3-input)))

(defparameter trees (make-array (list (length tree-lines) (length (first tree-lines)))
                                :initial-contents tree-lines))

(defun tree-count (dr dc)
  (loop :for r :below (first (array-dimensions trees)) :by dr
        :for c :from 0 :by dc
        :count (char= (aref trees r (mod c 31)) #\#)))

(defun solution1 ()
  (tree-count 1 3))

(defun solution2 ()
  (* (tree-count 1 1)
     (tree-count 1 3)
     (tree-count 1 5)
     (tree-count 1 7)
     (tree-count 2 1)))
