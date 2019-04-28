#lang racket/load
(load "preface.scm")

;(display (atom? 'atom))
;(display (atom? 'turkey))
;(display (atom? '1492))
;(display (list? '(atom)))
;(display (list? '(atom turkey or)))

(define s (list 'a 'b (list 'c)))
(define l (list 'd 'e 'f 'g))
(define a (cons s l))
(display a)