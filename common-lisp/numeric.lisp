(in-package :cl-user)

;; Generate list of list of characters for each digit.
(defun make-columns (digits)
  (mapcar #'(lambda (digit)
              (cdr (assoc digit t9 :test #'string=)))
    (map 'list #'string digits)))

;; Use modulo arithmetic to generate a single solution from a number.
(defun numeric-solution-for (number columns)
  (format nil "~{~A~}"
    (reverse
      (mapcar #'(lambda (column)
                  (multiple-value-bind
                      (new-number modulo) (floor number (length column))
                    (setf number new-number)
                    (elt column modulo)))
        columns))))

;; Numeric solution to T9 problem.
(defun numeric (digits)
  (let* ((columns (reverse (make-columns digits)))
         (maximum (apply #'* (mapcar #'length columns))))
    (loop for i from 0 to (1- maximum)
          collect (numeric-solution-for i columns))))
