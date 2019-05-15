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

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else
       (intersect? (cdr set1) set2)))))

(define easy-intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (or (member? (car set1) set2)
           (easy-intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))

(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (xxx (cdr set1) set2))
      (else
       (cons (car set1)
             (xxx (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
       (intersect (car l-set)
                  (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
                     
(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define firsts
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (first (car rel))
             (firsts (cdr rel)))))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (build (second (car rel))
                    (first (car rel)))
             (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define easy-revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel))
             (easy-revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (second (car rel))
             (seconds (cdr rel)))))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))