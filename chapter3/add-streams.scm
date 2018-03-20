(load "3-50.scm")
(load "ones.scm")
(define (add-streams s1 s2)
	(stream-map + s1 s2))

(define integers (cons-stream
					1
					(add-streams + ones integers)))

(define fibs
	(cons-stream 0
		(cons-stream 1
			(add-streams (stream-cdr fibs)
						 (fibs)))))

