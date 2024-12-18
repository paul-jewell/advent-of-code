(in-package :day8)

(defparameter day8-test-input "input/day8-test-input.txt")
(defparameter day8-input "input/day8-input.txt")

(defstruct console
  code
  (acc 0)
  (pc 0))

(defmacro create-console (name code)
  `(setq ,name (make-console :code ,code)))

;; This is my refactored from my version
;; - following advice from zulu.inuoe (discord: Lisp channel)

(defparameter *opcodes* '(jmp nop acc))

(defun opcode-fn-or-error (opcode)
  (let ((op (find opcode *opcodes* :test #'string-equal)))
    (unless op (error "unknown opcode ~A" opcode))
    op))

(defun parse-program (program-file)
  (let ((code (uiop:read-file-lines program-file)))
    (loop :for line :in code
          collect (destructuring-bind (opcode operand)
                      (split "\\s" line)
                    (list (opcode-fn-or-error opcode) (parse-integer operand))))))


(defun run-console1 (name visitlist)
  (cond ((null (find (console-pc name) visitlist))
         (setf visitlist (cons (console-pc name) visitlist))
         (funcall (car (nth (console-pc name) (console-code name)))
                  name (cdr (nth (console-pc name) (console-code name))))
         (run-console1 name visitlist))
        (t (console-acc name))))

(defun nop (name in)
  (declare (ignore in))
  (setf (console-pc name) (+ 1 (console-pc name))))

(defun acc (name in)
  (setf (console-acc name) (+ (car in) (console-acc name)))
  (setf (console-pc name) (+ 1 (console-pc name))))

(defun jmp (name in)
  (setf (console-pc name) (+ (car in) (console-pc name))))

(defun test1 ()
  (let ((name 'prog))
    (run-console1 (create-console name (parse-program day8-test-input)) nil)))

(defun solution1 ()
  (let ((name 'prog))
    (run-console1 (create-console name (parse-program day8-input)) nil)))

;; Part b specific code below

(defun replace-code (code old-opcode new-opcode)
  (loop for i from 0 to (- (length code) 1)
        if (equal old-opcode (car (nth i code)))
          collect (nconc (subseq code 0 i) (list (cons new-opcode (cdr (nth i code)))) (nthcdr (+ i 1) code))))

(defun run-console2 (name visitlist)
  (cond ((null (nth (console-pc name) (console-code name))) (console-acc name))
        ((null (find (console-pc name) visitlist))
         (setf visitlist (cons (console-pc name) visitlist))
         (funcall (car (nth (console-pc name) (console-code name))) name (cdr (nth (console-pc name) (console-code name))))
         (run-console2 name visitlist))
        (t nil)))

(defun part2 (program-file)
  (let ((input (parse-program program-file))
        (name 'prog))
       (car (remove nil (loop :for program :in (nconc (replace-code input 'jmp 'nop)
                                                      (replace-code input 'nop 'jmp))
                              :collect (run-console2 (create-console name program) nil))))))

(defun test2 ()
  (part2 day8-test-input))

(defun solution2 ()
  (part2 day8-input))


