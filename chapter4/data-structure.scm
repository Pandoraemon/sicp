(load "expression.scm")
; 谓词检测
; 除了 flase 以外所有对象都是 true
(define (true? x)
	(not (eq? x false)))
(define (false? x)
	(eq? x false))

; 过程的表示
(define (make-procedure parameters body env)
	(list 'procedure parameters body env))

(define (compound-procedure? p)
	(tagged-list? p 'precedure))

(define (procedure-parameters p)
	(cadr p))

(define (procedure-body)
	(caddr p))

(define (procedure-env)
	(cadddr p))

; 对环境的操作

(define (enclosing-environment env)
	(cdr env))

(define (first-frame env)
	(car env))

(define the-empty-environment ())

(define (make-frame variables values)
	(cons variables values))

(define (frame-variables frame)
	(car frame))

(define (frame-values frame)
	(cdr frame))

(define (add-binding-to-frame! var val frame)
	(set-car! frame (cons var (car frame)))
	(set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars)
				   (env-loop (enclosing-environment env)))
				  ((eq? var (car vars))
				   (car vals))
				  (else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((fram (first-frame env)))
				(scan (frame-values frame)
					  (frame-variables frame)))))
	(env-loop))

(define (set-variable-value! var val env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars)
				   (env-loop (enclosing-environment env)))
				  ((eq? var (car vars))
				   (set-car! vals val))
				  (else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame)
					  (frame-values frame)))))
	(env-loop env))

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		 (define (scane vars vals)
		 	(cond ((null? vars)
		 		   (add-binding-to-frame! var frame))
		 		  ((eq? var (car vars))
		 		   (set-car! vals val))
		 		  (else (scan (cdr vars) (cdr vals)))))
		 (scan (frame-variables frame)
		 	   (frame-values frame))))















