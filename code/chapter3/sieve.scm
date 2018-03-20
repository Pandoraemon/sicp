(load "stream.scm")
(load "infinity_stream")
(define (sieve stream)
	(cons-stream
		(stream-car stream)
		(sieve (stream-filter
				(lambda (x) (not (divisible? x (stream-car stream))))
				(stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)
(display-stream primes)