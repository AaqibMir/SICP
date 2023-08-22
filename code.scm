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
  
  (define (iter old new count)
    (display old)
    (display "---")
    (display new)
    (newline)
    (cond ((good-enough? old new)
           (display "No of guesses it took:")
           (display count)
           (newline)
           new)
        
        (else (iter new (f new) (+ count 1)))))
  
  (define (good-enough? x y)
    (< (/ (abs (- x y)) y) tolerance))
  
  (define tolerance 0.0001)
  
  (iter guess (f guess) 1))

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

;;ex 1.22
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (define (prime? n) (fast-prime? n 3))

  (if (prime? n)
      (report-prime (- (runtime) start-time)))
  )
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n)
  (define (iter n times)
    (cond
      ((= times 0 ) (newline))
      ((prime? n) (timed-prime-test n) (iter (+ n 1) (- times 1)))
      (else (iter (+ n 1) times))))
  (iter n 3))


;; ex 1.23
(define (next test-divisor)
  (if (= test-divisor 2)
      (+ test-divisor 1)
      (+ test-divisor 2)))

;; ex 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; ex 1.2
;; Numbers that fool the Fermat test are called Carmichael numbers. Fermat's test
;; implies they are prime, but they are not. e.g. 561, 1105, 6601
(define (congruent? a n)
  (= (expmod a n n) a))
(define (is-it-prime-fermat? n)
  (define (iter a n)
    (cond
      ((= a n) #t)
      ((congruent? a n) (iter (+ a 1) n))
      (else #f)))
        
  (iter 2 n))
      
;; ex 1.28

(define (robin-miller-test n)
  (define (try a)
    (= 0 (expmod-rob-mill a (- n 1) n)))   
        
  (try (+ 1 (random (- n 1)))))
(define (miller-robin-prime? n times)
  (cond
    ((= 0 times) true)
    ((robin-miller-test n) (miller-robin-prime? n (- times 1)))
    (else false)))
(define (expmod-rob-mill base exp mod)
  (define (square-and-test x)
    (cond
      ((= 1 (remainder (square x) mod))
       (if (or (not (= 1 (remainder x mod)))
               (not (= (- mod 1) (remainder x mod))))
           0
           (square x)))
      (else (square x))))
         
  
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder (square-and-test(expmod-rob-mill base
                                                 (/ exp 2)
                                                 mod)) mod))
    (else (remainder (* base (expmod-rob-mill base
                                              (- exp 1)
                                              mod))
                     mod))))


;; ex 1.29
;; Numerical Integration by Simpson's Rule.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (* (/ h 3)
     (+ (f a)
        (f b)
        (* 4 (sum-iter f (+ a h) (lambda (x) (+ x (* 2 h))) (- b h)))
        (* 2 (sum-iter f (+ a (* 2 h)) (lambda (x) (+ x (* 2 h))) (- b (* 2 h)))))))

(define (cube x) (* x x x))

;; ex 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; ex 1.31
;; recursive version
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) n))
;; iterative version
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
;; pi approximation
(define (pi-approx n)
  (* 4.0 (product (lambda (x) (/ (* x (+ x 2))
                          (square (+ x 1))))
           2
           (lambda (x) (+ x 2))
           n)))

                    
;; ex 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term
                            (next a) next b))))

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))

;; iterative version

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; ex 1.33
(define (filtered-accumulate combiner null-value term
                             a next b pred)
  
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (pred a)
                                            (term a)
                                            null-value)))))
  (iter a null-value))

;; a
;;(filtered-accumulate + 0 square 2 (lambda (x) (+ x 1))
 ;;                   5 prime?)

;; b
(define (relative-prime-product n)
  (define (pred? x)
    (= 1 (gcd x n)))
(filtered-accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ x 1))
                     (- n 1) pred?))
                                          

;; ex 1.35
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))


;; ex 1.36
(define (fp-fx)
  (fixed-point (lambda (x) (/ (log 1000)
                              (log x))) 2))

(define (fp-fx-ad)
  (fixed-point (average-damp (lambda (x) (/ (log 1000)
                                            (log x)))) 2))

;; ex 1.37
(define (cont-frac-rec n d k)
  (define (f n d m)
    
  (if (> m k)
      0
      (/ (n m)
         (+ (d m)
            (f n d (+ m 1))))))
  (f n d 1))

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (n k)
                         (+ (d k) result)))))
  (iter k 0))

;; ex 1.38
(define (euler-expansion k)
  (define (d k)
    (define (iter a b c k)
      (if (= k 1)
          c
          (iter (if (= c 1) 1 (+ 2 c)) a b (- k 1))))
    (iter 1 2 1 k))
  (+ 2 (cont-frac (lambda (i) 1.0)
                     d
                     k)))
      

;; ex 1.39
(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (-(square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))
    

;; ex 1.40
;; Below definitions copied from the book
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
;; Actual code for the given exercise
(define (cubic a b c)
  (lambda (x)
    (+ (* x (square x))
       (* a (square x))
       (* b x)
       c)))

;; ex 1.41
(define (double- f)
  (lambda (x) (f (f x))))

;; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

