#lang racket/base

(require
  racket/contract
  racket/dict
  racket/function
  racket/list
  racket/string
  "infra.rkt")

(provide numeric)

(module+ test
  (require rackunit))

(define/contract (numeric-solution number columns [results '()])
  ((exact-nonnegative-integer? (listof (listof string?))) (string?) . ->* . (listof string?))
  (if (empty? columns)
      results
      (let* ([col (car columns)]
             [len (length col)]
             [mod (modulo number len)]
             [num (floor (/ number len))])
        (numeric-solution num (cdr columns) (cons (list-ref col mod) results)))))

(module+ test
  (check-exn exn:fail? (thunk (numeric-solution)))
  (check-exn exn:fail? (thunk (numeric-solution 17)))
  (check-exn exn:fail? (thunk (numeric-solution 17 23)))
  (check-exn exn:fail? (thunk (numeric-solution 17 '(("a")) 23)))
  (let ([columns '(("C" "D") ("A" "B"))])
    (check-equal? (numeric-solution 0 columns) '("A" "C"))
    (check-equal? (numeric-solution 1 columns) '("A" "D"))
    (check-equal? (numeric-solution 2 columns) '("B" "C"))
    (check-equal? (numeric-solution 3 columns) '("B" "D"))))

; Numeric solution to T9 problem.
(define/contract (numeric digits)
  (digits? . -> .  (listof string?))
  (let* ([columns (reverse (map (lambda (digit) (dict-ref t9 digit))
                                (string-split digits #rx"(?<=.)(?=.)")))]
         [maximum (apply * (map (lambda (column) (length column)) columns))])
    (for/list ([i (in-range maximum)])
      (string-join (numeric-solution i columns) ""))))

(module+ test
  (check-exn exn:fail? (thunk (numeric)))
  (check-exn exn:fail? (thunk (numeric 123))))
