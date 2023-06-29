#lang racket/base

(require
  racket/class
  racket/contract
  racket/dict
  racket/function
  racket/list
  racket/string
  "infra.rkt")

(module+ test
  (require rackunit))

(provide odometer)

(define (single-char? str)
  (and (string? str) (= (string-length str) 1)))

(module+ test
  (check-exn exn:fail? (thunk (single-char?)))
  (check-false (single-char? 3))
  (check-true (single-char? "a"))
  (check-false (single-char? "ab")))

; Define class for a single 'wheel' in a mechanical odometer.
(define/contract wheel%
  (class/c (init [char-list (listof single-char?)])
           [click (->m boolean?)]
           [current (->m string?)])
  (class object%
    (init char-list)

    (super-new)

    (define chars char-list)
    (define index 0)

    ; Click a wheel to its next character.
    ; Returns #t if the wheel returns to its first character,
    ; indicating carry-over to the next wheel.
    (define/public (click)
      (if (< index (sub1 (length chars)))
          (begin (set! index (add1 index)) #f)
          (begin (set! index 0) #t)))

    ; Return the current character showing on the wheel.
    (define/public (current)
      (list-ref chars index))))

(define/contract (wheel? obj)
  (any/c . -> . boolean?)
  (is-a? obj wheel%))

(module+ test
  (check-false (wheel? 1))
  (check-exn exn:fail? (thunk (new wheel% [char-list '("ab")])))
  (let ([wheel (new wheel% [char-list '("a" "b" "c")])])
    (check-true (wheel? wheel))
    (check-exn exn:fail? (thunk (send wheel click 1)))
    (check-exn exn:fail? (thunk (send wheel current 1)))
    (check-eq? (send wheel current) "a")
    (check-false (send wheel click))
    (check-eq? "b" (send wheel current))
    (check-false (send wheel click))
    (check-eq? "c" (send wheel current))
    (check-true (send wheel click))
    (check-eq? "a" (send wheel current))))

; Define class for an 'odometer' made up of wheels.
(define/contract meter%
  (class/c (init [digits string?])
           [current (->m string?)]
           [click (->*m () ((listof wheel?)) boolean?)]
           [collect (->*m () ((listof string?)) (listof string?))])
  (class object%
    (init digits)

    (super-new)

    (define wheels
      (map (lambda (digit) (new wheel% [char-list (dict-ref t9 digit)]))
           (string-split digits #rx"(?<=.)(?=.)")))

    ; Return a string representing the current settings of odometer's wheels.
    (define/public (current)
      (string-join
       (map (lambda (wheel) (send wheel current)) wheels)
       ""))

    ; Click odometer to its next setting.
    ; Click wheels from end, continuing as long as
    ; individual wheels signal carry-over by returning #t.
    ; Returns true if the last (leftmost) wheel clicked returns #t,
    ; indicating all wheels have carried over.
    (define/public (click [wheel-list (reverse wheels)])
      (cond
        [(empty? wheel-list) #t]
        [(not (send (car wheel-list) click)) #f]
        [else (click (cdr wheel-list))]))

    ; Collect the results by clicking through the odometer settings.
    (define/public (collect [result-list '()])
      (let ([results (cons (current) result-list)])
        (if (click)
            (reverse results)
            (collect results))))
    ))

(module+ test
  (check-exn exn:fail? (thunk (new meter% [digits "ab"])))
  (check-exn exn:fail? (thunk (new meter% [digits 3])))
  (let ([meter (new meter% [digits "12"])])
    (check-equal? (send meter current) "1A")
    (check-false (send meter click))
    (check-equal? (send meter current) "1B")
    (check-false (send meter click))
    (check-equal? (send meter current) "1C")
    (check-true (send meter click))))

; Odometer solution to T9 problem.
(define/contract (odometer digits)
  (digits? . -> .  (listof string?))
  (send (new meter% [digits digits]) collect))


(module+ test
  (check-exn exn:fail? (thunk (odometer)))
  (check-exn exn:fail? (thunk (odometer 123))))
