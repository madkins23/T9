(in-package :cl-user)

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

(defun recursive (item)
  "Recursive solution to T9 problem."
  (recursive-core "" item))

(defun test-recursive ()
  "Test with known file stream."
  (with-open-file (stream "../test/digits")
    (main #'recursive stream)))
