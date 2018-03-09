;1.2
(/ (+ 5 4 (- 2 (- 3 ( + 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))


;1.3
(define (smaller x y) 
	(if (< x y) x y))

(define (smallest x y z) 
        (smaller x (smaller y z)))

(define (sum-of-bigger x y z)
		(- (+ x y z) (smallest x y z)))

(sum-of-bigger 2 4 8)

;牛顿法求平方根
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
	  	guess
	  	(sqrt-iter (improve guess x) 
	  				x)))

(define (improve guess x)
		(average guess (/ x guess)))

(define (average x y)
		(/ (+ x y) 2))

(define (good-enough? guess x)
		(< (abs (- (square guess) x)) 0.0001))

(define (square x)
		(* x x))

(define (sqrt x)
		(sqrt-iter 1.0 x))

(sqrt 9)

(sqrt (* 0.0000001 0.0000001))

;1.6
(define (new-if predicate then-clause else-clause)
		(cond (predicate then-clause)
			  (else else-clause)))

(define (new-sqrt-iter guess x)
	(new-if (good-enough? guess x)
	  	guess
	  	(new-sqrt-iter (improve guess x) 
	  				x)))


(define (new-sqrt x)
		(new-sqrt-iter 1.0 x))

;(new-sqrt 11)






