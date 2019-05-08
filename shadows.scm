#lang racket

(define atom?
  (lambda (x)
    (or (null? x) (not (pair? x)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote *))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote expt))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(define simply-numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr (aexp))))))))))
(define first-sub-aexp
  (lambda (aexp)
    (car aexp)))

(define second-sub-aexp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define a-operator
  (lambda (aexp)
    (car (cdr aexp))))

(define new-number?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (new-number? (new-first-sub-aexp aexp))
            (new-number? (new-second-sub-aexp aexp)))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote *))
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote expt))
       (expt (value (car nexp))
             (value (car (cdr (cdr nexp))))))))))

(define new-first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define new-second-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define new-value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (new-value (new-first-sub-exp nexp))
          (new-value (new-second-sub-exp nexp))))
      ((eq? (operator nexp) (quote *))
       (* (new-value (new-first-sub-exp nexp))
          (new-value (new-second-sub-exp nexp))))
      (else
       (expt (new-value (new-first-sub-exp nexp))
             (new-value (new-second-sub-exp nexp)))))))

(define new-first-sub-aexp
  (lambda (aexp)
    (car aexp)))

(define new-second-sub-aexp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define new-a-operator
  (lambda (aexp)
    (car (cdr aexp))))

(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons '() n)))
(define zub1
  (lambda (n)
    (cdr n)))

(define plus
  (lambda (a b)
    (cond
      ((sero? b) a)
      (else
       (plus (edd1 a) (zub1 b))))))
(define new-plus
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else
       (edd1 (new-plus n (zub1 m)))))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))