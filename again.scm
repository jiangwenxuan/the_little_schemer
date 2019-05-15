#lang racket

;(define l1 '(6 2 4 caviar 5 7 3))
;(define l2 '(6 2 grits caviar 5 7 3))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define zero?
  (lambda (x)
    (equal? x 0)))

(define one?
  (lambda (x)
    (equal? x 1)))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define pick
  (lambda (num lat)
    (cond
      ((equal? num 1) (car lat))
      (else (pick (- num 1) (cdr lat))))))

(define keep-looking
  (lambda (a index lat)
    (cond
      ((number? index)
       (keep-looking a (pick index lat) lat))
      (else
       (eq? index a)))))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
          (build (second (first pair))
                (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else
       (build (first pora)
              (align (second pora)))))))

;(define l3 '((x y) (z w) (m n)))
;(display (align l3))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (length* (first pora))
          (length* (second pora)))))))

;(define l4 '((m n) (x y)))
;(display (length* l4))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (* 2 (weight* (first pora)))
          (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else
       (build (first pora)
              (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else
       (A (sub1 n)
          (A n (sub1 m)))))))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

(define will-stop?
  (lambda (x)
    '()))



















