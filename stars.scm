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
























