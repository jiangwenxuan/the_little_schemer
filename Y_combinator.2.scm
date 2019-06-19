#lang racket

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(define op-maker
  (lambda (op)
    (lambda (x y)
      (op x y))))

; the idea doesn't work
;(define fact-maker
;  (lambda (procedure)
;    (lambda (n)
;      (if (zero? n)
;          1
;          (* n (procedure (- n 1)))))))

(define fact-maker
  (lambda (procedure)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((procedure procedure) (- n 1)))))))

(define fact-step2
  ((lambda (procedure)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (- n 1))))))
   (lambda (procedure)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (- n 1))))))))

; ((procedure procedure) (- n 1))
; equals to
; ((lambda (arg) ((procedure procedure) arg)) (- n 1))

;(define F
;  (lambda (n)
;    (if (zero? n)
;        1
;        (* n ((lambda (arg) ((procedure procedure) arg)) (- n 1))))

(define F
  ((lambda (func-arg)
     (lambda (n)
       (if (zero? n)
           1
           (* n (func-arg (- n 1))))))
   (lambda (arg) ((procedure procedure) arg))))

(define fact-step4
  ((lambda (procedure)
     ((lambda (func-arg)
        (lambda (n)
          (if (zero? n)
              1
              (* n (func-arg (- n 1))))))
      (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     ((lambda (func-arg)
        (lambda (n)
          (if (zero? n)
              1
              (* n (func-arg (- n 1))))))
      (lambda (arg) ((procedure procedure) arg))))))

(define F*
  (lambda (func-arg)
    (lambda (n)
      (if (zero? n)
          1
          (* n (func-arg (- n 1)))))))

(define fact-step4-2
  ((lambda (procedure)
     (F* (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     (F* (lambda (arg) ((procedure procedure) arg))))))

(define Y
  (lambda (f)
    ((lambda (x)
       (f (lambda (arg) ((x x) arg))))
     (lambda (x)
       (f (lambda (arg) ((x x) arg)))))))

(define findmax
  (lambda (l)
    (if (null? l)
        'no-list
        (if (null? (cdr l))
            (car l)
            (max (car l) (findmax (cdr l)))))))

(define M
  (lambda (func-arg)
    (lambda (l)
      (if (null? l)
          'no-list
          (if (null? (cdr l))
              (car l)
              (max (car l) (func-arg (cdr l))))))))