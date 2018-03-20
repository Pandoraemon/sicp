(load "euler-transform.scm")

(define (ln2-stream n)
		(cons-stream (/ 1.0 n)
				 (stream-map - (ln2-stream (+ n 1)))))

(define ln2
	(partial-sums (ln2-stream 1)))

(stream-head ln2 10)

(stream-head (euler-transform ln2) 10)

(stream-head (accelerated-sequence euler-transform ln2) 10)