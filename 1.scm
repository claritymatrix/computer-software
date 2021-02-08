;;; excise 1.1 source code

10
(+ 5 3 4)
(/ 6 2)
(- 9 1)
(+ (* 2 4) (- 4 6))

(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))


(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))


(/ (+ (+ 5 4) (- 2 (- 3 (+ 6 (/ 4 5)))))(* 3 (* (- 6 2)
                                                (- 2 7))))


(define (sum a b) (+ a b))



(define (max a b) (if (> a b)
                      a
                      b))

(define (max-tr a b c)
  (max (max a b) c))


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))


;;; sqrt
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;;; good-enough? version 2


(define (good-enough? guess x)
  (< (abs (- (/ (improve guess x) guess) 1)) 0.00000001))


(define (cubic x) (* x x x))
(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (cubic guess) x)) 0.001))

(define (three x)
  (three-iter 1.0 x))


(define (improve-thr guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


(define (three-iter guess x)
  (if (good-enough? guess x)
      guess
      (three-iter (improve-thr guess x)
                  x)))



;;;; 过程抽象
;;;;f(x) = g(h(x))
;;;; 按照意愿设计程序
;;;; 上面那个程序我们可以看出，我们的good-enough?函数是可以替换的。
;;;; 我们只需要知道它能帮我们判断是否足够好。
;;;; 这个设计的关键在于保持每个函数的独立性。



;;;; version 2

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve geuss x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess) x)))
  (sqrt-iter 1.0 x))


;;;; version 3 提升数字的功用性


(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(define (factorial  n)
  (define (factorial-iter product
                          counter
                          max-count)
    (if (> counter max-count)
        product
        (factorial-iter (* counter product)
                        (+ counter 1)
                        max-count)))
  (factorial-iter 1 1 n))


(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b)
                  a
                  (- count 1))))
  (fib-iter 1 0 n))


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(define (f n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (f (- n 1))
                 (f (- n 2))
                 (f (- n 3))))))


;;;; iter version


(define (f n)
  (define (f-iter a b c d count max-count)
    (cond ((> count max-count) a)
          ((< max-count 3) max-count)
          (else (f-iter (+ a
                            (* 2 b)
                            (* 3 c))
                         a
                         b
                         c
                         (+ count 1)
                         max-count))))
  (if (< n 3)
      n
      (f-iter 4 2 1 0 3 (- n 1)))
  )


(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))


(define (sine angle)
  (if (not (> (abs angle) 0.01))
      angle
      (p (sine (/ angle 3.0)))))


(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))


;;;; 迭代过程

(define (exp b n)
  (define (exp-iter b counter result)
    (if (= counter 0)
        result
        (exp-iter b (- counter 1)
                  (* b result))))
  (exp-iter b n 1))



;;;; 提升时间效率


(define (fast-expr b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expr b (/ n 2))))
        (else (* b (fast-expr b (- n 1))))))

(define (even? x)
  (= (remainder x 2) 0))




(define (fast-expr b n)
  (define (fast-expr-iter b n a)
    (cond ((= 0 n) a)
          ((even? n) (fast-expr-iter (square b) (/ n 2) a))
          (else (fast-expr-iter b (- n 1 )(* b a)))))
  (fast-expr-iter b n 1))



(define (* a b)
  (if (= 0 b)
      0
      (+ a (* a (- b 1)))))


(define (double x) (+ x x))
(define (havle x) (/ x 2))

(define (* a b)
  (define (time-iter a b result)
    (cond ((= 0 b) result)
          ((even? b) (time-iter (double a) (havle b) result))
          (else (time-iter a (- b 1) (+ result a)))))
  (time-iter a b 0))



;;; gcd alog

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))


;;;;find prime

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next number)
  (if (= 2 number)
      3
      (+ number 2)))


(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;; fiema thought


(define (expmod base exp m)
  (cond ((= 0 exp) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
         (else (remainder (* base (expmod base (- exp 1) m))
                          m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))


(define (prime? n)
  (fast-prime? n 100))

(define (fast-prime? n times)
  (cond ((= 0 times) #t)
        ((miler-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))


(define (miler-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (miler-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))


(define (miler-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          square))
    (check-nontrivial-sqrt1 x (remainder (square x) m)))
  (cond ((= 0 exp) 1)
        ((even? exp) (squaremod-with-check
                      (miler-rabin-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (miler-rabin-expmod base (- exp 1) m))
                    m))))



(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a (lambda(x) (+ x 1)) b))

(define (sum-integers a b)
  (sum (lambda(x) x) a (lambda(x) (+ x 1)) b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))



;;;; y = f(g(x),x)
;;;; (term (lambda(k) (* (ce k) (f (y k)))))

(define (simpon-test f a b n)
  (let* ((h (/ (- b a) (* 1.0 n)))
         (y (lambda(k) (+ a (* k h))))
         (ce (lambda(k)
               (cond ((or (= 0 k) (= n k) ) 1)
                     ((even? k) 2)
                     (else 4))))
         (term (lambda(k)
                 (* (ce k) (f (y k)))))
         (next (lambda(k) (+ 1 k))))
    (* (/ h 3.0)
       (sum term 0 next n))
    ))



(define (productal f a next b)
  (if (> a b)
      1
      (* (f a)
         (productal f (next a) next b))))


(define (double x)
  (+ x x))
(define (square x)
  (* x x))

(define (factorial n)
  (/ (* (double n) (double (+ n 1)))
     (square (+ (* 2.0 n) 1))))

(productal factorial 1 (lambda(x) (+ x 1)) 100.0)
(* 4 (productal factorial 1 (lambda(x) (+ x 1)) 100.0))


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(accumulate + 0 (lambda(x) x) 1 (lambda(x) (+ x 1)) 10)

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter  a  result)
    (if (> a b)
        result
        (accumulate-iter  (next a)
                         (combiner (term a) result))))
  (accumulate-iter term a next b null-value))


(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) (combiner null-value result))))))



(define (average a b) (/ (+ a b) 2))
(define (postive? x) (> x 0))
(define (negtive? x) (< x 0))
(define (close-enough? a b) (< (abs (- a b)) 0.001))
(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((postive? test-value)
                 (search f neg-point mid-point))
                ((negtive? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negtive? a-value) (postive? b-value))
           (search f a b))
          ((and (negtive? b-value) (postive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b )))))
