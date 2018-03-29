(load "data-structure.scm")
(load "kernel.scm")


(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive))
(define (primitive-implementation proc)
	(cadr proc))

(define primitive-procedures
	(list (list 'car car)
		  (list 'cdr cdr)
		  (list 'cons cons)
		  (list 'null? null?)))

(define (primitive-procedure-name)
	(map car
		primitive-procedures))
(define (primitive-procedure-objects)
	(map (lambda (proc) (list 'primitive (cadr proc)))
		primitive-procedures))

(define (apply-primitive-procedure proc args)
	(applu-in-underlying-scheme
		(primitive-implementation proc) args))
(define (setup-environment)
	(let ((initial-env
		  (extend-environment (primitive-procedure-name)
		  				 	  (primitive-procedure-objects)
		  				 	  the-empty-environment)))
	(define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))

(define the-global-envrionment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		 (let ((output (eval input the-global-envrionment)))
		 	(announce-output output-prompt)
		 	(user-print output)))
	(driver-loop))

(define (prompt-for-input string)
	(newline)
	(newline)
	(display string)
	(newline))

(define (announce-output string)
	(newline)
	(display string)
	(newline))

(define (user-print object)
	(if (compound-procedure? object)
		(display (list 'compound-procedure
					   (producedure-parameters object)
					   (producedure-body object)
					   '<procedure-env>))
		(display object)))

(define the-global-envrionment (setup-environment))





