#lang racket/base

(require
  racket/contract
  racket/format
  racket/function
  racket/list
  racket/string)

(provide main digits? t9)

(module+ test
  (require rackunit))

(define
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

(define/contract (digits? str)
  (any/c . -> . boolean?)
  (and
   (string? str)
   ; Can be empty string in some cases, but if not empty must be all digits.
   (regexp-match? #px"^\\d*$" str)))

(module+ test
  (check-exn exn:fail? (thunk (digits?)))
  (check-true (digits? "123"))
  (check-false (digits? "abc"))
  (check-false (digits? 123)))

;; TODO: more unit tests to do below

; Format as many results as will fit in one line.
; Return the remaining results.
(define/contract (format-result-line-core results column)
  ((listof string?) integer? . -> . (listof string?))
  (if (empty? results)
      results
      (let* ([current (car results)]
             [new-column (+ column (string-length current) 1)])
        (if (> new-column 80)
            results
            (begin
              (printf " ~a" current)
              (format-result-line-core (cdr results) new-column))))))

; Format a single line of results.
; Return the remaining results.
(define/contract (format-result-line results)
  ((listof string?) . -> . (listof string?))
  (display " ")
  (let ([results (format-result-line-core results 0)])
    (newline)
    results))

; Format result lines.
(define/contract (format-result-lines results)
  ((listof string?) . -> . void?)
  (unless (empty? results)
    (let ([remaining (format-result-line results)])
      (unless (empty? remaining)
        (format-result-lines remaining)))))

; Format results for comparison with known solution.
(define/contract (format-results digits results)
  (string? (listof string?) . -> . void?)
  (let ((count (length results)))
    (printf "~a~a result~a~%"
            (~a digits #:width 25)
            (~a count #:width 7 #:align 'right)
            (if (= count 1) "" "s"))
    (format-result-lines results)))

; Process a number using the specified function and format results.
(define/contract (process fn line)
  (procedure? string? . -> . void?)
  (let ([digits (string-trim line)])
    (unless (string=? digits "")
      (format-results digits (fn digits)))))

; Read lines from specified stream and process each line.
(define/contract (main fn [stream (current-input-port)])
  ((procedure?) (input-port?) . ->* . void?)
  (let ((fn-name (object-name fn)))
    (fprintf (current-error-port) "### Starting ~a~%" fn-name)
    (for ([line (in-lines stream)])
      (process fn line))
    (fprintf (current-error-port) "### Finished ~a~%" fn-name)))
