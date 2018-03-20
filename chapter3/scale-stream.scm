(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

(define double
	(cons-stream 1 (scale-stream double 2)))