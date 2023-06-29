#lang racket/base

; Use this to configure a namespace for interactive racket invocations
; without using -i flag which leaves racket in REPL afterwards.
;
; Examples:
;   racket -i -t infra.rkt -t recursive.rkt -e '(main recursive)'
;   racket -i -t infra.rkt -t odometer.rkt -e '(main odometer)'
;   racket -i -t infra.rkt -t numeric.rkt -e '(main numeric)'
(current-namespace (make-base-namespace))

