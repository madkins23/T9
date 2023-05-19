(in-package :cl-user)

;; Recursive solution to T9 problem.
(defun recursive-core (starting remaining)
  "Recursive core of solution to T9 problem."
  (let* ((focus (subseq remaining 0 1))
         (tail (if (> (length remaining) 1) (subseq remaining 1) ""))
         (tail-p (> (length tail) 0))
         (chars (cdr (assoc focus t9 :test #'string=))))
    (mapcan #'(lambda (ch)
                (let ((stem (concatenate 'string starting ch)))
                  (if tail-p (recursive-core stem tail) (list stem))))
      (if (> (length chars) 0) chars (list focus)))))

;; Recursive solution to T9 problem.
;; Outer call configures starting state for inner recursive call.
(defun recursive (item)
  "Recursive solution to T9 problem."
  (recursive-core "" item))
