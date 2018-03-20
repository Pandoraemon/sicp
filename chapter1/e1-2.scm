;fib
(define (fib n)
		(cond ((= n 0) 0)
			  ((= n 1) 1)
			  (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)

(define (fib n)
		(fib-iter 1 0 n))

(define (fib-iter a b count)
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))	

(fib 10)

;1.11
(define (f n)
		(if (< n 3)
			n
			(+ (f (- n 1)) 
			   (* 2 (f (- n 2))) 
			   (* 3 (f (- n 3))))))

(f 11)

(define (f n)
		(if (< n 3) n
		(f-iter 0 1 2 (- n 2))))

(define (f-iter a b c n)
		(if (= n 0)
			c
			(f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))

(f 11)

;exchange
(define (count-change amount)
		(cc amount 5))

(define (cc amount kinds-of-coins)
		(cond ((= amount 0) 1)
			  ((or (< amount 0) (= kinds-of-coins 0)) 0)
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
			  ((= kinds-of-coins 5)	50)))	

(count-change 100)


;1.12
(define (pascal row col)
		(if (or (= col 0) (= col row)) 
			1
			(+ (pascal (- row 1) (- col 1))
					(pascal (- row 1) col))))

(pascal 3 2)

(define (expt b n)
		(if (= n 0) 1
			(* b (expt b (- n 1)))))


(define (expt b n)
		(expt-iter b n 1))

(define (expt-iter b counter product)
		(if (= counter 0)
			product
			(expt-iter b 
					   (- counter 1)
					   (* b product))))

(define (fast-expt b n)
	(cond ((= n 0) 1)
		 ((even? n) (square (fast-expt b (/ n 2))))
		 (else (* b (fast-expt b (- n 1))))))

(define (even? n)
	(= (remainder n 2) 0))


;1.16
(define (fast-expt b n)
	(fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
	(cond ((= n 0) a)
		  ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
		  (else (fast-expt-iter b (- n 1) (* a b)))))

;不变量， 在任意状态下 a*b^n 不变


;1.17
(define (double x)
	(+ x x))
(define (halve x)
	(/ x 2))

(define (mul a b)
	(cond ((= b 0) 0)
		((even? b) (mul (double a) (halve b)))
		(else (+ a (mul a (- b 1))))))


;1.18
(define (fast-mul a b)
	(mul-iter a b 0))

(define (mul-iter a b x)
	(cond ((= b 0) x)
		((even? b) (mul-iter (double a) (halve b) x))
		(else (mul-iter a (- b 1) (+ x a)))))

(define (gcd a b)
	(if (= b 0) 
		a
		(gcd b (remainder a b))))

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

(define (next-odd n)
	(if (odd? n) 
		(+ n 2)
		(+ n 1)))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		 ((devides? test-divisor n) test-divisor)
		 (else (find-divisor n (next-odd test-divisor)))))

(define (devides? a b)
	(= (remainder b a) 0))
;1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;1.22
(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))


(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))


(define (prime? n)
	(= (smallest-divisor n) n))



(define (continue-primes n count)
	(cond ((= count 0)
		  (display "are primes"))
		  ((prime? n)
			(display n)
			(newline)
			(continue-primes (next-odd n) (- count 1)))
		  (else (continue-primes (next-odd n) count))))


(define (search-for-primes n)
	(let ((start-time (real-time-clock)))
        (continue-primes n 3)
        (- (real-time-clock) start-time)))
























