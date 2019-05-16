#lang racket

(define add1
  (lambda (x)
    (+ x 1)))

(define eternity
  (lambda (x)
    (eternity x)))

;length0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

;(lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1 (length0 (cdr l))))))
;define doesn't work for length0, replace length0
;and we call it length1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 (eternity (cdr l))))))
                 (cdr l))))))
;length2
;(lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1 ((lambda (l)
;                   ((null? l) 0)
;                   (else
;                    (add1 ((lambda (l)
;                             (cond
;                               ((null? l) 0)
;                               (else
;                                (add1 (eternity (cdr l))))))
;                           (cdr l)))))
;                 (cdr l))))))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
        (add1 (length (cdr l)))))))
 eternity)

((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))

;length2
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  ((lambda (h)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (h (cdr l)))))))
   eternity)))

;name the function that takes length as an argument
;and returns a function that looks like length
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;length1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;length2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;still length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;still length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

(define l '(apples))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))
 l)

;length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length)
                    (cdr l))))))))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     (mk-length mk-length))))
 l)

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))