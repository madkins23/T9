(in-package :cl-user)

;; Define class for a single 'wheel' in a mechanical odometer.
(defclass wheel nil
    ((index :initform 0 :accessor index)
     (chars :initarg :chars :reader chars)))

;; Click a wheel to its next character.
;; Returns true if the wheel goes to a new character,
;; or nil if the wheel repeats to the first character indicating carry-over.
(defmethod click ((whl wheel))
  (if (>= (index whl) (1- (length (chars whl))))
      (progn (setf (index whl) 0) nil)
      (progn (setf (index whl) (1+ (index whl))) t)))

;; Return the current character showing on the wheel.
(defmethod current ((whl wheel))
  (elt (chars whl) (index whl)))

;; Display image for wheel.
(defmethod print-object ((whl wheel) out)
  (format out "wheel:~d~s" (index whl) (chars whl)))

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

;; Display image for odometer.
(defmethod print-object ((mtr meter) out)
  (format out "Meter:")
  (mapc #'(lambda (whl) (format out " ~a" whl))
    (slot-value mtr 'wheels)))

;; Click odometer to its next setting.
;; Click wheels from end, continuing as long as
;; individual wheels signal carry-over by returning true.
;; Returns nil if the first wheel returns nil
;; indicating all wheels have carried over.
(defmethod click ((mtr meter))
  (mapc #'(lambda (whl)
            (when (click whl)
                  (return-from click t)))
    (reverse (wheels mtr)))
  nil)

;; Odometer solution to T9 problem.
(defun odometer (item)
  (let ((mtr (make-meter item)))
    (loop collect (current mtr)
          while (click mtr))))

(let ((mtr (make-meter "4")))
  (format t "~a~%" mtr)
  (format t "current: ~a~%" (current mtr))
  (format t "click:   ~a~%" (click mtr))
  (format t "current: ~a~%" (current mtr))
  (format t "click:   ~a~%" (click mtr))
  (format t "current: ~a~%" (current mtr))
  (format t "click:   ~a~%" (click mtr))
  (format t "current: ~a~%" (current mtr))
  (format t "click:   ~a~%" (click mtr))
  (format t "current: ~a~%" (current mtr))
  nil)

(odometer "206")