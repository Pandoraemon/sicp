(load "../chapter1/prime.scm")
(load "../chapter2/example.scm")
(define (sum-primes a b)
	(define (iter count accum)
		(cond ((> count b) accum)
			  ((prime? count) (iter (+ count 1) (+ count accum)))
			  (else (iter (+ count 1) accum))))
	(iter a 0))
(sum-primes 1 100)

(define (sum-primes a b)
	(accumulate +
			0
			(filter prime? (enumerate-interval a b))))
(sum-primes 1 100)