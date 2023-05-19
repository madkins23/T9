(in-package :cl-user)

;; Define class for a single 'wheel' in a mechanical odometer.
(defclass wheel nil
    ((index :initform 0 :accessor index)
     (chars :initarg :chars :reader chars)))

;; Click a wheel to its next character.
;; Returns true if the wheel returns to its first character,
;; indicating carry-over to the next wheel.
(defmethod click ((whl wheel))
  (if (>= (index whl) (1- (length (chars whl))))
      (progn (setf (index whl) 0) t)
      (progn (setf (index whl) (1+ (index whl))) nil)))

;; Return the current character showing on the wheel.
(defmethod current ((whl wheel))
  (elt (chars whl) (index whl)))

;; Collect wheels appropriate to the specified digit string.
(defun collect-wheels (digits)
  (mapcar #'(lambda (digit)
              (make-instance 'wheel
                :chars (cdr (assoc digit t9 :test #'string=))))
    (map 'list #'string digits)))

;; Define class for an 'odometer' made up of wheels.
(defclass meter nil
    ((wheels :accessor wheels)))

;; Construct an odometer and configure its wheels for the specified digit string.
(defun make-meter (digits)
  (let ((mtr (make-instance 'meter)))
    (setf (wheels mtr) (collect-wheels digits))
    mtr))

;; Return a string representing the current settings of odometer's wheels.
(defmethod current ((mtr meter))
  ; Generally accepted way to concatenate list of strings
  ; since the (concatenate 'string) form only accepts them individually.
  (format nil "~{~A~}"
    (mapcar #'(lambda (whl) (current whl))
      (wheels mtr))))

;; Click odometer to its next setting.
;; Click wheels from end, continuing as long as
;; individual wheels signal carry-over by returning true.
;; Returns true if the last (leftmost) wheel clicked returns true,
;; indicating all wheels have carried over.
(defmethod click ((mtr meter))
  (mapc #'(lambda (whl)
            (unless (click whl)
              (return-from click nil)))
    (reverse (wheels mtr)))
  t)

;; Odometer solution to T9 problem.
(defun odometer (digits)
  (let ((mtr (make-meter digits)))
    (loop collect (current mtr)
          until (click mtr))))
