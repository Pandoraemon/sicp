; 3.5.2 无穷流
(load "stream.scm")
(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))
(define integers 
	(integers-starting-from 1))