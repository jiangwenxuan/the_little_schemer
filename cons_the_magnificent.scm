#lang racket/load
(load "preface.scm")

(define new-rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (new-rember a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a (cdr lat)))))))))


;(define lat (list 'a 'b 'c 'd 'b 'd 'c 'b))
;(display (rember 'b lat))

(define new-firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (car l)) (new-firsts (cdr l)))))))

;(define l (list (list 'a 'b) (list 'd 'e) (list 'q 'b)))
;(display (new-firsts l))

(define new-insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old
                                 (cons new (cdr lat))))
      (else
       (cons (car lat) (new-insertR new old (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? old (car lat))
          (cons old (cons new (cdr lat))))
         (else (cons (car lat)
                     (insertR new old (cdr lat)))))))))

;(define lat1 (list 'a 'c 'd 'e 's 'h))
;(display (insertR 'z 'd lat1))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? old (car lat)) (cons new lat))
         (else
          (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst (new old (cdr lat))))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? o1 (car lat))
               (cons new (cdr lat)))
              ((eq? o2 (car lat))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

(define better-subst
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or (eq? o1 (car lat))
                   (eq? o2 (car lat)))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (better-subst new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? a (car lat)) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))))



















