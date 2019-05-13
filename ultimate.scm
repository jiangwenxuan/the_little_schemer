#lang racket

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else
       (cons (car l)
             (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define new-rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else
         (cons (car l)
               ((new-rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l))
         (cons new
               (cons old (cdr l))))
        (else
         (cons (car l)
               ((insertL-f test?) new old l)))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? old (car l)) '())
        ((test? old (car l))
         (cons old
               (cons new (cdr l))))
        (else
         (cons (car l)
               ((insertR-f test?) new old (cdr l))))))))

(define g1
  (lambda (new old l)
    (cons new
          (cons old l))))
(define g2
  (lambda (new old l)
    (cons old
          (cons new l))))

(define insert-g
  (lambda (g test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l))
         (g new old (cdr l)))
        (else
         (cons (car l)
               ((insert-g g test?) new old (cdr l))))))))
;(define l '(apple banana orange pear))
;(display ((insert-g g1 eq?) 'strawberry 'orange l))
;(newline)
;(display ((insert-g g2 eq?) 'strawberry 'orange l))

(define insertL (insert-g g1 equal?))
(define insertR (insert-g g2 equal?))
(define new-insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))
   equal?))

(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old)
       (cons new (cdr l)))
      (else (cons (car l)
                  (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))
(define new-subst (insert-g seqS equal?))

(define yyy
  (lambda (a l)
    ((insert-g seqrem equal?) #f a l)))
(define seqrem
  (lambda (new old l)
    l))
;(define a 'sausage)
;(define l '(pizza with sausage and bacon))
;(display (yyy a l))




