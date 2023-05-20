(in-package :cl-user)

(defvar
  t9
  '(("0" "0")
    ("1" "1")
    ("2" "A" "B" "C")
    ("3" "D" "E" "F")
    ("4" "G" "H" "I")
    ("5" "J" "K" "L")
    ("6" "M" "N" "O")
    ("7" "P" "Q" "R" "S")
    ("8" "T" "U" "V")
    ("9" "W" "X" "Y" "Z")))

(defun seq-split (seq n &optional acc orig-n)
  "Recursive core to split a sequence into sub-sequences of the specified size"
  (cond ((zerop (length seq)) (nreverse acc))
        ((zerop n) (seq-split seq
                              orig-n
                              (cons (subseq seq 0 0) acc)
                              orig-n))
        (t (seq-split (subseq seq 1)
                      (1- n)
                      (cons (concatenate (class-of seq)
                              (if acc (car acc) (subseq seq 0 0))
                              (list (elt seq 0)))
                            (cdr acc))
                      orig-n))))

(defun seq-split-by-n (seq n)
  "Split a sequence into sub-sequences of the specified size"
  (seq-split seq n nil n))

(defun format-results (digits results)
  "Format results for comparison with known solution."
  (let ((count (length results)))
    (format t "~25a~7d result~a~%" digits count (if (= count 1) "" "s"))
    (mapc #'(lambda (some-results)
              (format t " ")
              (mapc #'(lambda (single-result)
                        (format t " ~a" single-result))
                some-results)
              (format t "~&"))
      (seq-split-by-n results (ceiling (/ 79 (1+ (length digits))))))))

(defun process (fn line)
  "Process a number using the specified function and format results."
  (let ((digits (string-trim '(#\Space #\e #\t) line)))
    (when (not (string= digits ""))
          (format-results digits (funcall fn digits)))))

(defun main (fn &optional stream)
  "Read lines from specified stream and process each line."
  (let* ((str (format nil "~a" fn))
         (fn-name (subseq str 11 (1- (length str)))))
    (format *error-output* "### Starting ~a~%" fn-name)
    (let ((stream (if stream stream *standard-input*)))
      (loop for line = (read-line stream nil nil)
            while line
            do (process fn line)))
    (format *error-output* "### Finished ~a~%" fn-name)))


(defun test (fn)
  "Test with known file stream."
  (with-open-file (stream "../test/digits")
    (main fn stream)))
