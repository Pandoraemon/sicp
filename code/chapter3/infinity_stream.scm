; 3.5.2 无穷流
(load "stream.scm")
(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))
(define integers 
	(integers-starting-from 1))

(define (divisible? x y)
	(= (remainder x y) 0))
(define no-sevens
	(stream-filter 
		(lambda (x) (not (divisible? x 7)))
		integers))
(stream-ref no-sevens 100)

(define (fibgen a b)
	(cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(stream-ref fibs 5)