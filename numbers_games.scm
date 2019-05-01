#lang racket/load
(load "preface.scm")

(define add1
  (lambda (n)
    (+ n 1)))
(define sub1
  (lambda (n)
    (- n 1)))

(define my-add
  (lambda (m n)
    (cond
      ((zero? m) n)
      (else
       (my-add (sub1 m) (add n))))))
(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond
      ((zero? m 0) n)
      (else (sub1 (sub n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (add m (times m (sub1 n)))))))

















