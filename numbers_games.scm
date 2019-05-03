; #lang racket/load
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

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (add (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))
      
(define more-than
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (more-than (sub1 n) (sub1 m))))))

(define less-than
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (less-than (sub1 n) (sub1 m))))))

(define my-equal?
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else
       (my-equal? (sub1 n) (sub1 m))))))

(define power
  (lambda (a x)
    (cond
      ((zero? x) 1)
      (else
       (times a (power a (sub1 x)))))))

(define new-quotient
  (lambda (n m)
    (cond
      ((less-than n m) 0)
      (else (add1 (new-quotient (sub n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n) (cdr lat)))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-numbers
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat))
          (no-numbers (cdr lat)))
         (else
          (cons (car lat) (no-numbers (cdr lat)))))))))

(define all-numbers
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat))
          (cons (car lat) (all-numbers (cdr lat))))
         (else
          (all-numbers (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (my-equal? a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))
(define new-one?
  (lambda (n)
    (my-equal? n 1)))

(define new-rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons (car lat)
             (new-rempick (sub1 n) (cdr lat)))))))