#lang racket

(define atom?
  (lambda (x)
    (or (null? x) (not (pair? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (cdr lat))
      (else
       (cons (car lat)
             (rember a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat))
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define l1 '(apple 3 pear 4 9 apple 3 4))
(define l2 '(apple peach pear peach plum apple lemon peach))
;(display (set? l))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else
       (cons (car lat)
             (makeset (cdr lat)))))))
;(display (makeset l2))

(define new-makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat)
             (new-makeset (multirember (car lat) (cdr lat))))))))
;(display (new-makeset l1))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))
















