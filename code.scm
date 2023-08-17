;; Learning to code

#lang sicp

;; ex 1.3
(define (f a b c)
  (cond
    ((< a b)(f b a c))
    ((< b c)(f a c b))
    (else (+ (square a) (square b)))))

(define (square x) (* x x))

;; ex 1.7
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1))

(define (average-damp f)
  (lambda (a)
    (/ (+ a (f a)) 2)))

(define (fixed-point f guess)
  
  (define (iter old new)
    (if (good-enough? old new)
        new
        (iter new (f new))))
  
  (define (good-enough? x y)
    (< (/ (abs (- x y)) y) tolerance))
  
  (define tolerance 0.0001)
  
  (iter guess (f guess)))

;; ex1.8
(define (cbrt x)
  (fixed-point (lambda (y) (/ (+ (/ x (square y))
                                 (* 2 y))
                              3))
               1))

;; ex1.11
(define (f-rec n)
  (cond
    ((< n 3) n)
    (else (+ (f-rec (- n 1))
             (* 2 (f-rec (- n 2)))
             (* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (define (iter a b c count)
    (cond
      ((< n 3) n)
      (else (if (= count 2)
                c
                (iter b
                      c
                      (+ (* a 3)
                         (* b 2)
                         c)
                      (- count 1))))))
  (iter 0 1 2 n))


;; ex 1.12
(define (pascal row col)
  (cond
    ((or (= col row) (= col 0)) 1)
    (else (+ (pascal (- row 1) (- col 1))
             (pascal (- row 1) col)))))

;; ex 1.16
(define (expt-iter b n)
  (define (iter b n a)
    (cond
      ((= n 0) a)
      ((even? n) (iter (* b b) (/ n 2) a))
      (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

;; ex 1.18
;; works perfectly for positive numbers but figure out the sign yourself
;; if one or both of them are negative numbers!
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (mul-iter a b)
  (define (iter a b c)
    (cond
      ((= b 0) c)
      ((even? b) (iter (double a) (halve b) c))
      (else (iter a (- b 1) (+ c a)))))
  (iter a (abs b) 0))
    
 
      



                    

  