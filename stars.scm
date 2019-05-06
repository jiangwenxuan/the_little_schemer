#lang racket/load
(load "preface.scm")
(load "numbers_games.scm")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l)) (rember* a (cdr l)))))))

;(define l '((coffee) cup ((tea) cup) (and (hick)) cup))
;(display (rember* 'and l))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
         (else
          (cons (car l) (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))
          
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? l)
       (cond
         ((eq? a l)
          (add1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))
      (else
       (add (occur* a (car l))
            (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old (insertL* new old (cdr l)))))
         (else
          (cons (car l) (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))

(define my-leftmost
  (lambda (l)
    (cond
      ((not (pair? l)) l)
      (else
       (my-leftmost (car l))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
       (leftmost (car l))))))
        
;(define l '(((() four)) 17 (seventeen)))
;(define l '((patato) (chips ((with) fish) (chips))))
;(display (my-leftmost l))

(define my-eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (atom? l1) (atom? l2))
       (eqan? l1 l2))
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (my-eqlist? (cdr l1) (cdr l2))))
      ((and (pair? (car l1)) (pair? (car l2)))
       (and (my-eqlist? (car l1) (car l2))
            (my-eqlist? (cdr l1) (cdr l2))))
      (else #f))))
      
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
;(define l1 '(strawberry ice cream))
;(define l2 '(strawberry ice cream))
;(define l3 '(strawberry cream ice))
;(define l4 '(banana ((split))))
;(define l5 '((banana) (split)))
;(define l6 '(beef ((sausage)) (and (soda))))
;(define l7 '(beef ((salami)) (and (soda))))
;(define l8 '(beef ((sausage)) (and (soda))))
;(display (my-eqlist? l1 l2))
;(display (my-eqlist? l1 l3))
;(display (my-eqlist? l4 l5))
;(display (my-eqlist? l6 l7))
;(display (my-eqlist? l6 l8))