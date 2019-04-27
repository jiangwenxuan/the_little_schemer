#lang racket/load
(load "perface.rkt")

(display (atom? 'atom))
(display (atom? 'turkey))
(display (atom? '1492))
(display (list? '(atom)))
(display (list? '(atom turkey or)))