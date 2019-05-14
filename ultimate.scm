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

; these two functions needs character 6's functions
;(define atom-to-function
;  (lambda (x)
;    (cond
;      ((eq? x '+) +)
;      ((eq? x '*) *)
;      (else power))))
;
;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      (else
;       ((atom-to-function (operator nexp))
;        (value (1st-sub-exp nexp))
;        (value (2st-sub-exp nexp)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))
(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
       (cons (car lat)
             (multiremberT test? (cdr lat)))))))
;(define l '(shrimp salad tuna salad and tuna))
;(display (multiremberT eq?-tuna l))

(define multirember-co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember-co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember-co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))
(define a-friend
  (lambda (x y)
    (null? y)))
;(multirember-co 'tuna '(tuna) a-friend)
;(eq? 'tuna (car '(tuna)))

(define lat1 '(strawberries tuna and swordfish))
;(display (multirember-co 'tuna lat1 a-friend))
(define lat2 '())
;(display (multirember-co 'tuna lat2 a-friend))
;(display (multirember-co 'tuna '(tuna) a-friend))

;(define new-friend
;  (lambda (newlat seen)
;    (col newlat (cons (car lat) seen))))
;(display (multirember-co 'tuna '(and tuna) a-friend))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertL new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR
                                  (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR
                                  (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR
                            (cdr lat)))))))

(define add1
  (lambda (x)
    (+ 1 x)))

(define multiremberLR-co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? oldL (car lat))
       (multiremberLR-co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL
                                            newlat))
                                (add1 L)
                                R))))
      ((eq? oldR (car lat))
       (multiremberLR-co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new
                                            newlat))
                                L
                                (add1 R)))))
      (else
       (multiremberLR-co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L
                                R)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else
          (evens-only* (cdr l)))))
      (else
       (cons (evens-only* (car l))
             (evens-only* (cdr l)))))))

(define evens-only*-co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*-co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l)
                                       newl)
                                 (* (car l) p)
                                 s))))
         (else
          (evens-only*-co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p
                                 (+ (car l) s)))))))
      (else
       (evens-only*-co (car l)
                       (lambda (al ap as)
                         (evens-only*-co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (* ap dp)
                                                (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

;(define l '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
;(display (evens-only*-co l the-last-friend))