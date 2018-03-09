(load "example.scm")

;1.29
(define (simpson f a b n)
	(define h (/ (- b a) n))
	(define (y k)
		(f (+ a (* k h))))
	(define (factor k)
		(cond ((or (= k 0) (= k n)) 1)
			((odd? k) 4)
			(else 2)))
	(define (term k)
		(* (y k) (factor k)))
	(define (next k)
		(+ k 1))
	(if (odd? n)
		(error "n can't be odd")
		(* (/ h 3.0) (sum term (exact->inexact 0) next n))))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)


;1.30
(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ (term a) result))))
	(iter a 0))


(sum (lambda (x) x)
	1
	(lambda (x) (+ x 1))
	10)


;1.31
; (define (prod term a next b)
; 	(if (> a b) 
; 		1
; 		(* (term a) (prod term (next a) next b))))


; (prod 	(lambda (x) x)
; 		1
; 		(lambda (i) (+ i 1))
; 		10)


(define (prod term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* (term a) result))))
	(iter a 1))

(prod 	(lambda (x) x)
		1
		(lambda (i) (+ i 1))
		10)


(define (numer-term i)
	(cond ((= i 1) 2)
		((even? i) (+ i 2))
		(else (+ i 1))))

(define (denom-term i)
	(if (even? i)
		(+ i 1)
		(+ i 2)))


(define (pi n)
	(* 4 
		(exact->inexact
			(/ (prod numer-term 
					1 
					(lambda (x) (+ x 1)) n)
				(prod denom-term 
					1 
					(lambda (x) (+ x 1)) n)))))


(pi 10)
(pi 1000)
(pi 10000)

;1.32
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner 
			(term a)
			(accumulate 
				combiner 
				null-value 
				term
				(next a)
				next
				b ))))

(define (sum term a next b)
	(accumulate + 0 term a next b))

(define (prod term a next b)
	(accumulate * 1 term a next b))

(sum 
	(lambda (x) x)
	1
	(lambda (i) (+ i 1))
	10)

(prod 
	(lambda (x) x)
	1
	(lambda (i) (+ i 1))
	10)


(define (accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner (term a) result))))
	(iter a null-value))


;1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
	1.0)

;1.36
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess step)
		(display-info guess step)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next (+ step 1)))))
	(newline)
	(try first-guess 1))

(define (display-info guess step)
	(display "Step: ")
    (display step)
    (display " ")
    
    (display "Guess: ")
    (display guess)
    (newline))

(define (formula x) (/ (log 1000) (log x)))
(define (average-damp f)
    (lambda (x)
        (average x 
                 (f x))))

(fixed-point formula 2.0)
(fixed-point (average-damp formula) 2.0)

;1.37
(define (cont-frac n d k)
	(define (cf i)
		(if (= i k)
			(/ (n k) (d k))
			(/ (n k) (+ (d k) (cf (+ i 1))))))
	(cf 1))

(define (golden-ratio k)
	(+ 1
		(cont-frac (lambda (i) 1.0)
			   (lambda (i) 1.0)
			   k)))
(golden-ratio 11)

(define (cont-frac n d k)
	(define (iter i result)
		(if (= i 0)
			result
			(iter (- i 1)
				(/ (n i) (+ (d i) result)))))
	(iter (- k 1) (/ (n k) (d k))))

(define (golden-ratio k)
	(+ 1
		(cont-frac (lambda (i) 1.0)
			   (lambda (i) 1.0)
			   k)))
(golden-ratio 15)

;1.38
(define (e k)
	(define (n i)
		1)
	(define (d i)
		(if (= 0 (remainder (+ i 1) 3))
			(* 2 (/ (+ i 1) 3))
			1))
	(+ 2.0 (cont-frac n d k)))

(e 1000)

;1.39
(define (tan-cf x k)
	(define (n i)
		(if (= i 1)
			x
			(- (square x))))
	(define (d i)
		(- (* 2 i) 1))
	(exact->inexact (cont-frac n d k)))


(tan 10)
(tan-cf 10 100)
(tan 25)
(tan-cf 25 100)


;1.40
(define (cubic a b c)
	(lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
(newton-method (cubic 3 2 1) 1)
(newton-method (cubic 2 5 5) 1)


;1.41
(define (double f)
	(lambda (x) (f (f x))))

(define (inc n) (+ n 1))

(((double (double double)) inc) 5)

;1.42
(define (compose f g)
	(lambda (x) (f (g x))))


;1.43
(define (repeated f n)
	(if (= n 1)
		f
		(lambda (x) (f ((repeated f (- n 1)) x)))))

((repeated square 2) 5)

;1.44
(define dx 0.00001)
(define (smooth f)
	(lambda (x) 
		(/ (+ (f (- x dx))
			  (f x)
			  (f (+ x dx)))
			3)))

((smooth square) 5)

(define (smooth-n-times f n)
	(let ((n-times-smooth (repeated smooth n)))
    (n-times-smooth f)))

((smooth-n-times square 10) 5)
























