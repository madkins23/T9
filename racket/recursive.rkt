#lang racket/base

(require
  racket/contract
  racket/dict
  racket/function
  "infra.rkt")

(provide recursive)

(module+ test
  (require rackunit))

; Recursive solution to T9 problem.
(define/contract (recursive-core starting remaining)
  (digits? digits? . -> . (listof string?))
  (let* ((focus (substring remaining 0 1))
         (tail (if (> (string-length remaining) 1) (substring remaining 1) ""))
         (tail-p (> (string-length tail) 0))
         (chars (dict-ref t9 focus)))
    (foldl (lambda (ch previous)
             (let ((stem (string-append starting ch)))
               (append previous
                       (if tail-p (recursive-core stem tail) (list stem)))))
           '()
           (if (> (length chars) 0) chars (list focus)))))

(module+ test
  (check-exn exn:fail? (thunk (recursive-core)))
  (check-exn exn:fail? (thunk (recursive-core 1 "two")))
  (check-exn exn:fail? (thunk (recursive-core 1 2 3))))

; Recursive solution to T9 problem.
; Outer call configures starting state for inner recursive call.
(define/contract (recursive digits)
  (digits? . -> .  (listof string?))
  (recursive-core "" digits))

(module+ test
  (check-exn exn:fail? (thunk (recursive)))
  (check-exn exn:fail? (thunk (recursive 123))))
