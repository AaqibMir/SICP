;; Learning to code

;; #lang sicp
;; (#%require sicp-pict)
(define nil '())
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

;; ex 1.43
(define (repeated f n)
  (define (iter n x)
    (if (= n 0)
        x
        (iter (- n 1) (f x))))
  (lambda (x) (iter n x)))

;; ex 1.44
;; repeated procedure using compose function version

(define (repeated-rec f n)
  (if (= n 1)
      f
      (compose f (repeated-rec f (- n 1)))))
      
     

(define (smooth f)
  (define dx 0.000001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                3)))


(define (n-fold-smoothed f n)
  ((repeated smooth n) f))


;; ex 1.45
(define (nth-root root number)
  (define (times p k)
    
    (if (= k 0)
        1
        (* p (times p (- k 1)))))
  (fixed-point ((repeated average-damp (floor (log root 2)))
                (lambda (x) (/ number (times x (- root 1)))))
               1.0))

;; ex 1.46
;; iterative improvement strategy
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess new-guess)
      (display guess)
      (display "---")
      (display new-guess)
      (newline)
      (if (good-enough? guess new-guess)
          new-guess
          (iter new-guess (improve new-guess))))
    (iter guess (improve guess))))

(define (sqrt-new num)
  (define tolerance 0.0001)
  ((iterative-improve (lambda (x y)
                       (< (/ (abs (- x y)) y) tolerance))
                     (newton-transform (lambda (y) (- (square y) num)))) 1.0))
                                                   
    
(define (fixed-point-new f guess)
  (define tolerance 0.00001)
  ((iterative-improve (lambda (x y) (< (/ (abs (- x y)) y) tolerance))
                      (average-damp f)) guess))



;; CHAPTER 2
;; Building Abstractions With Data

;; ex 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (/ n d) 1)
        (cons (/ (- (abs n)) g)
              (/ (abs d) g))
        (cons (/ (abs n) g)
              (/ (abs d) g)))))
;; copied from the book
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))

(define (denom x) (cdr x))

;; ex 2.2
(define (make-segment- pt1 pt2)
  (cons pt1 pt2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (make-point x y)
  (cons x y))
(define (x-point pt)
  (car pt))
(define (y-point pt)
  (cdr pt))

;; copied from the book
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (mid-point seg)
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

(define (average x y)
  (/ (+ x y) 2))

#| test case

(define p1 (make-point 1 2))
(define p2 (make-point 4 5))
(define seg1 (make-segment- p1 p2))
(define midpt (mid-point seg1))
(print-point midpt)

|#


;; ex 2.3
(define (attach-type type content)
  (cons type content))
(define (type datum)
  (car datum))
(define (contents datum)
  (cdr datum))

;; type predicates
(define (A? datum)
  (eq? (type datum) 'A))
(define (B? datum)
  (eq? (type datum) 'B))

;; A's representation
(define (make-rect-A l b)
  (attach-type 'A (cons l b)))
(define (length-A rect)
  (car rect))
(define (breadth-A rect)
  (cdr rect))
(define (area-A rect)
  (* (length-A rect)
     (breadth-A rect)))
(define (perimeter-A rect)
  (* 2 (+ (length-A rect)
          (breadth-A rect))))

;; B's representation
(define (make-rect-B l b)
  (attach-type 'B (cons b l)))
(define (length-B rect)
  (cdr rect))
(define (breadth-B rect)
  (car rect))
(define (area-B rect)
  (* (length-B rect)
     (breadth-B rect)))
(define (perimeter-B rect)
  (* 2 (+ (length-B rect)
          (breadth-B rect))))

;; Manager looks at type of datum and sends it to appropriate representations
(define (area rect)
  (cond
    ((A? rect) (area-A (contents rect)))
    ((B? rect) (area-B (contents rect)))))

(define (perimeter rect)
  (cond
    ((A? rect) (perimeter-A (contents rect)))
    ((B? rect) (perimeter-B (contents rect)))))


;; ex 2.4
(define (cons- x y)
  (lambda (m) (m x y)))
(define (car- z)
  (z (lambda (p q) p)))
(define (cdr- z)
  (z (lambda (p q) q)))

;; ex 2.5
(define (cons-pair a b)
  (* (expt-iter 2 a)
     (expt-iter 3 b)))

;; helper procedure
(define (how-many-factors num factor)
  (define (iter num result)
    (if (= (remainder num factor) 0)
        (iter (/ num factor) (+ result 1))
        result))
  (iter num 0))

(define (car-pair num)
  (how-many-factors num 2))
(define (cdr-pair num)
  (how-many-factors num 3))

;; ex 2.6
(define (one f)
  (lambda (x) (f x)))

(define (two f)
  (lambda (x) (f (f x))))

(define (zero f)
  (lambda (x)
    x))

(define (add-cn a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (add1 x)
  (+ x 1))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (three f)
  (lambda (x) (f (f (f x)))))
  
;; ex 2.7
;; code copied from the book

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;; selectors
;; selectors will work regardless of positions of lower
;; and upper bounds in the pair
(define (upper-bound int)
  (max (car int) (cdr int)))

(define (lower-bound int)
  (min (car int) (cdr int)))


;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


;; ex 2.10
(define (div-interval-rectified x y)
  (if (or (= 0 (lower-bound y)) (= 0 (upper-bound y)))
      (error "Cannot divide by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; ex 2.12
(define (make-center-percent c p)
  (let ((w (/ (* p c) 100)))
    (make-interval (- c w) (+ c w))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent int)
  (* 100 (/ (abs (- (center int) (lower-bound int)))
            (center int))))

;; ex 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))

;; ex 2.18
(define (reverse lst)
  (define (helper lst rev)
    (if (null? lst)
        rev
        (helper (cdr lst) (cons (car lst) rev))))
  (helper lst nil))

;; ex 2.19
;; (define us-coins (reverse (list 50 25 10 5 1)))
;; (define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; ex 2.20

(define (same-parity x . list)
  (define (filter proc lst)
    (cond
      ((null? lst) nil)
      ((proc (car lst)) (cons (car lst)
                              (filter proc (cdr lst))))
      (else (filter proc (cdr lst)))))
  (let ((p (even? x)))
    (if p
        (cons x (filter even? list))
        (cons x (filter odd? list)))))

  
;; ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (square x)) items))


;; ex 2.23
(define (for-each- proc things)
  (cond
    ((null? things) (newline))
    (else (proc (car things))
          (for-each- proc (cdr things)))))
      
;; ex 2.27
(define (deep-reverse lst)
  (cond
    ((null? lst) '())
    ((pair? (car lst)) (append (deep-reverse (cdr lst))
                               (list (deep-reverse (car lst)))))
    (else (append (deep-reverse (cdr lst))
                  (list (car lst))))))


;; ex 2.28
(define (fringe x)
  (cond
    ((null? x) '())
    ((pair? (car x)) (append (fringe (car x))
                             (fringe (cdr x))))
    (else (cons (car x)
                (fringe (cdr x))))))

;; ex 2.29
;; binary mobile
;; 2.29a
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;; 2.29b
(define (total-weight mobile)
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define (weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (+ (weight (left-branch structure))
           (weight (right-branch structure)))
        structure)))

(define (balanced? mobile)
  (let ((torque-left (* (branch-length (left-branch mobile))
                        (weight (left-branch mobile))))
        (torque-right (* (branch-length (right-branch mobile))
                         (weight (right-branch mobile)))))
    (= torque-left torque-right)))
        

  
;; (balanced? (list (list 1 (list (list 3 5) (list 5 6)))(list 1 (list (list 3 5) (list 5 6))) ))

;; ex 2.30
(define (square-tree-map tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-map x)
             (square x)))
       tree))
;; recursive version
(define (square-tree-rec tree)
  (cond
    ((null? tree) '())
    ((pair? (car tree)) (cons (square-tree-rec (car tree))
                              (square-tree-rec (cdr tree))))
    (else (cons (square (car tree))
                (square-tree-rec (cdr tree))))))


;; ex 2.31
(define (tree-map-rec proc tree)
  (cond
    ((null? tree) '())
    ((pair? (car tree)) (cons (tree-map-rec proc (car tree))
                              (tree-map-rec proc (cdr tree))))
    (else (cons (proc (car tree))
                (tree-map-rec proc (cdr tree))))))

(define (tree-map proc tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map proc x)
             (proc x)))
       tree))
(define (square-tree tree)
  (tree-map square tree))

(define (square-tree-using-rec tree)
  (tree-map square tree))

;; when you want to write a recursive procedure, imagine your
;; procedure already works for some input n. Now find a way
;; to make it work for an extra input which itself may come in
;; different forms like type A, type B etc. and there you go
;; Start writing the procedure by thinking about how you would
;; handle each type and link it with the recursive call so it
;; starts to make sense. It helps if you can think of the form
;; your solution will have in the end.

;; ex 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))


;; ex 2.33
(define (accumulate- op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate- op initial (cdr sequence)))))
(define (map- p seq)
  (accumulate- (lambda (x y) (cons (p x) y)) nil seq))

(define (append- seq1 seq2)
  (accumulate- cons seq2 seq1))

(define (length seq)
  (accumulate- (lambda (x y) (+ 1 y)) 0 seq))

;; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate- (lambda (this-coeff higher-terms) (+ this-coeff
                                                   (* x
                                                      higher-terms)))
              0
              coefficient-sequence))


;; ex 2.35

(define (count-leaves t)
  (accumulate- +
              0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1))
                   t)))


;; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate- op init (map (lambda (x)
                                        (car x)) seqs))
            (accumulate-n op init (map (lambda (x)
                                         (cdr x)) seqs)))))


;; ex 2.37
(define (dot-product v w)
  (accumulate- + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (map (lambda (y)
                  (accumulate- + 0
                               (accumulate-n * 1 (list x y))))
                cols)) m)))

  
;;(define m (list (list 2 0 -1) (list 3 5 2) (list -4 1 4)))
;;(define n (list (list 5 1 -2) (list -1 0 4) (list 2 -3 3)))
;;(matrix-*-matrix m n)

;; ex 2.39
(define (fold-right op initial sequence)
  (accumulate- op initial sequence))

(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (fold-left op (op initial (car sequence)) (cdr sequence))))

(define (reverse- seq)
  (fold-right (lambda (x y) (append y (list x)))
              nil seq))

(define (reverse-- seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))


;; ex 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate- append nil (map proc seq)))
(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

;; prime-sum-pairs using unique-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


;; ex 2.41
(define (triplets n s)
  (filter (lambda (pair)
            (= (accumulate- + 0 pair) s))
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval (+ j 1) n)))
                      (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n))))

;; ex 2.42
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))
(define (safe? k positions)
  (define (position-sum position)
    (+ (car position) (cadr position)))
  (define (position-row position)
    (car position))
  (define (position-transform position)
    (list (+ 1 (- k (position-row position))) (cadr position)))
  
  (define (helper position seq)
     (cond
       ((null? seq) #t)
       ((= (position-row position)
           (position-row (car seq))) #f)
       ((= (position-sum position)
           (position-sum (car seq))) #f)
       ((= (position-sum (position-transform position))
           (position-sum (position-transform (car seq)))) #f)
       (else (helper position (cdr seq)))))
  (helper (car positions) (cdr positions)))

;; code from the book
;; temporary definitions of beside and below
;; (define (below x y) (lambda (frame) (x frame)(y frame)))	    
			    
;; (define (beside x y) (lambda (frame) (x frame)(y frame)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


;; ex 2.51a
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))



(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2







;; ex 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
        (below painter (beside up up)))))
  

;; ex 2.45
(define (split first second)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((img ((split first second) painter
                                         (- n 1))))
          (first painter (second img img))))))
              
(define right-split-new (split beside below))
(define up-split-new (split below beside))

;; ex 2.46
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

;; add vectors
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
           (xcor-vect v2))
        (+ (ycor-vect v1)
           (ycor-vect v2))))
;; subtract vectors
 (define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
           (xcor-vect v2))
        (- (ycor-vect v1)
           (ycor-vect v2))))

;; scale vectors
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; ex 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame- origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin- frame)
  (car frame))
(define (edge1- frame)
  (cadr frame))
(define (edge2- frame)
  (cddr frame))

;; ex 2.48
(define (make-segment v1 v2)
  (list v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cadr seg))


;; ex 2.49
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))



(define segment-list-outline
  (list (make-segment (make-vect 0 0)
                      (make-vect 0 1))
        (make-segment (make-vect 0 1)
                      (make-vect 1 1))
        (make-segment (make-vect 1 1)
                      (make-vect 1 0))
        (make-segment (make-vect 1 0)
                      (make-vect 0 0))))

;; ex 2.49a
;; outline-frame is a painter, given a frame it draws itself in it.
;;(define outline-frame (segments->painter segment-list-outline))


;; ex 2.49b
(define segment-list-x
  (list (make-segment (make-vect 0 0)
                      (make-vect 1 1))
        (make-segment (make-vect 0 1)
                      (make-vect 1 0))))
;; outline-x is a painter, given a frame it draws itself in it.
;;(define outline-x (segments->painter segment-list-x))

;; draw-x is a painter too but it uses ouline to outline the frame itself before
;; it draws x in it.
;;(define (draw-x frame)
;;  (outline-frame frame)
;;  (outline-x frame))



;; Switched to mit-scheme but the interpreter complained about a couple of
;; things so fixed a couple of them


;; execution of frame procedure will construct the window and put it up
;; on your screen. This is where the painter will draw its painting.
;; This window is named 'window' below and will be used by plot-line in
;; draw-line procedure.
(define window (frame -5 15 -5 15))


(define (draw-line a b)
  
  (plot-line window (xcor-vect a) (ycor-vect a) (xcor-vect b) (ycor-vect b))
  )


(define (segments->painter segment-list)
  
  (lambda (frame)
    
    (for-each
     (lambda (segment)
       ;;(display "in")
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
       ;;(display "out")
       ;;(newline))
       
     segment-list)))

(define test-frame (make-frame (make-vect 0 0)
			       (make-vect 10 0)
			       (make-vect 0 10)))


;; ex 2.49a
;; outline-frame is a painter, given a frame it draws itself in it.
(define outline-frame (segments->painter segment-list-outline))

;; ex 2.49b
;; outline-x is a painter, given a frame it draws itself in it.
(define outline-x (segments->painter segment-list-x))
;; draw-x is a painter too but it uses ouline to outline the frame itself
;; before it draws x in it.
(define (draw-x frame)
  (outline-frame frame)
  (outline-x frame))


;; ex 2.49c
(define segment-list-diamond
  (list (make-segment (make-vect 0.5 0)
		      (make-vect 0 0.5))
	(make-segment (make-vect 0 0.5)
		      (make-vect 0.5 1))
	(make-segment (make-vect 0.5 1)
		      (make-vect 1 0.5))
	(make-segment (make-vect 1 0.5)
		      (make-vect 0.5 0))))

(define outline-diamond (segments->painter segment-list-diamond))
(define (draw-diamond frame)
  (outline-frame frame)
  (outline-diamond frame))


;; took the segment list at http://community.schemewiki.org/?sicp-ex-2.49
(define segment-list-wave
  (list
   (make-segment (make-vect .25 0) (make-vect .35 .5))
   (make-segment (make-vect .35 .5) (make-vect .3 .6))
   (make-segment (make-vect .3 .6) (make-vect .15 .4))
   (make-segment (make-vect .15 .4) (make-vect 0 .65))
 ;;(make-segment (make-vect 0 .65) (make-vect 0 .85))
   (make-segment (make-vect 0 .85) (make-vect .15 .6))
   (make-segment (make-vect .15 .6) (make-vect .3 .65))
   (make-segment (make-vect .3 .65) (make-vect .4 .65))
   (make-segment (make-vect .4 .65) (make-vect .35 .85))
   (make-segment (make-vect .35 .85) (make-vect .4 1))
   ;;(make-segment (make-vect .4 1) (make-vect .6 1))
   (make-segment (make-vect .6 1) (make-vect .65 .85))
   (make-segment (make-vect .65 .85) (make-vect .6 .65))
   (make-segment (make-vect .6 .65) (make-vect .75 .65))
   (make-segment (make-vect .75 .65) (make-vect 1 .35))
   ;;(make-segment (make-vect 1 .35) (make-vect 1 .15))
   (make-segment (make-vect 1 .15) (make-vect .6 .45))
   (make-segment (make-vect .6 .45) (make-vect .75 0))
   ;;(make-segment (make-vect .75 0) (make-vect .6 0))
   (make-segment (make-vect .6 0) (make-vect .5 .3))
   (make-segment (make-vect .5 .3) (make-vect .4 0))
   ;;(make-segment (make-vect .4 0) (make-vect .25 0))
   ))
;; 2.49d
;; outline-wave is a painter which needs a frame befor it can draw itself
;; in it.
(define outline-wave (segments->painter segment-list-wave))

;; ex 2.50

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))


;; ex 2.51b
(define (beside-b painter1 painter2 rotation-1 rotation-2)
  (beside (rotation-1 painter1) (rotation-2 painter2)))


;; ex 2.52a
(define segment-list-wave-b
  (list
   (make-segment (make-vect 0 0) (make-vect 1 1))
   (make-segment (make-vect 1 0) (make-vect 0 1))
   (make-segment (make-vect .25 0) (make-vect .35 .5))
   (make-segment (make-vect .35 .5) (make-vect .3 .6))
   (make-segment (make-vect .3 .6) (make-vect .15 .4))
   (make-segment (make-vect .15 .4) (make-vect 0 .65))
 ;;(make-segment (make-vect 0 .65) (make-vect 0 .85))
   (make-segment (make-vect 0 .85) (make-vect .15 .6))
   (make-segment (make-vect .15 .6) (make-vect .3 .65))
   (make-segment (make-vect .3 .65) (make-vect .4 .65))
   (make-segment (make-vect .4 .65) (make-vect .35 .85))
   (make-segment (make-vect .35 .85) (make-vect .4 1))
   ;;(make-segment (make-vect .4 1) (make-vect .6 1))
   (make-segment (make-vect .6 1) (make-vect .65 .85))
   (make-segment (make-vect .65 .85) (make-vect .6 .65))
   (make-segment (make-vect .6 .65) (make-vect .75 .65))
   (make-segment (make-vect .75 .65) (make-vect 1 .35))
   ;;(make-segment (make-vect 1 .35) (make-vect 1 .15))
   (make-segment (make-vect 1 .15) (make-vect .6 .45))
   (make-segment (make-vect .6 .45) (make-vect .75 0))
   ;;(make-segment (make-vect .75 0) (make-vect .6 0))
   (make-segment (make-vect .6 0) (make-vect .5 .3))
   (make-segment (make-vect .5 .3) (make-vect .4 0))
   ;;(make-segment (make-vect .4 0) (make-vect .25 0))
   ))

(define outline-wave-b
  (segments->painter segment-list-wave-b))


;; ex 2.52b
(define (corner-split-b painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; ex 2.52c
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit-b painter n)
  (let ((combine4 (square-of-four flip-horiz rotate-270
                                  rotate-180 flip-vert)))
    (combine4 (corner-split painter n))))


