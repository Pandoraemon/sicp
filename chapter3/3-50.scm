(define (stream-map proc . argstreams)
	(if (null? (car argstreams))
		()
		(cons-stream
			(apply proc 
				(map (lambda (s)
						(stream-car s))
					argstreams))
			(apply stream-map
				(cons proc (map (lambda (s)
									(stream-cdr s))
							argstreamsz))))))