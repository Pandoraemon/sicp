; eval
(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((variable? exp) (lookup-variable-value exp env))
		  ((quoted? exp) (text-of-quotation exp))
		  ((assignment? exp) (eval-assignment exp env))
		  ((definition? exp) (eval-definition exp env))
		  ((if? exp) (eval-if exp env))
		  ((lambda? exp)
		  	(make-procedure (lambda-parameters exp)
		  					(lambda-body exp)
		  					env))
		  ((begin? exp)
		  	(eval-sequence (begin-actions exp) env))
		  ((cond? exp) (eval (cond->if exp) env))
		  ((application? exp)
		  	(apply (eval (operator exp) env)
		  			(list-of-values (operands exp) env)))
		  (else
		  	(error "Unkown expression type  -- EVAL" exp))))

; apply
(define (apply procedure arguments)
	(cond ((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments))
		  ((compound-procedure? procedure)
		  	(eval-sequence
		  		(procedure-body procedure)
		  		(extennd-environment
		  			(procedure-parameters procedure)
		  			arguments
		  			(procedure-environment procedure))))
		  (else
		  	(error "Unknown procedure type -- APPLY" procedure))))

; 过程参数
(define (list-of-values exps env)
	(if (no-operands? exp)
		()
		(cons (eval (first-operand exps) env)
			  (list-of-values (rest-operand exps) env))))

; 条件
(define (eval-if exp env)
	(if (true? (eval (if-predicate exp) env)
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env))))

; 序列
(define (eval-sequence exps env)
	(cond ((last-exp? exps)
			(eval (first-exp exps) env))
		  (else (eval (first-exp exps) env)
		  		(eval-sequence (rest-exps exps) env))))

; 赋值和定义
(define (eval-assignment exp env)
	(set-variable-value! (assignment-variable exp)
						 (eval (assignment-value exp) env)
						 env)
	'ok)

(define (eval-definition exp env)
	(define-variable! (definition-variable exp)
					  (eval (definition-value exp) env)
					  env)
	'ok)

; 自然求值表达式只有数字和字符串
(define (self-evaluating? exp)
	(cond ((number? exp) true)
		  ((string? exp) true)
		  (else false)))

; 变量
(define (variable? exp)
	(symbol? exp))

; 引号表达式
(define (quoted? exp)
	(tagged-list? exp 'quote))
;确定表的开始是否某个指定的符号
(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))
; 取引号表达式的文本内容
(define (text-of-quotation exp)
	(cadr exp))
;赋值
(define (assignment? exp)
	(tagged-list? exp 'set!))

(define (assignment-variable exp)
	(cadr exp))

(define (assignment-value exp)
	(caddr exp))
;定义
(define (definition? exp)
	(tagged-list? exp 'define))

(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		(caadr exp)))

(define (definition-value exp)
	(if (symbol? (cadr exp)
		(caddr exp)
		(make-lambda (cdadr exp)
					 (cddr exp)))))

; lambda
(define (lambda? exp)
	(tagged-list? exp 'lambda))

(define (lambda-parameters exp)
	(cadr exp))
(define (lambda-body exp)
	(cddr exp))
(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

; 条件表达式
(define (if? exp)
	(tagged-list? exp 'if))

(define (if-predicate exp)
	(cadr exp))

(define (if-consequent exp)
	(caddr exp))

(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(cdddr exp)
		'false))

(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

; begin 表达式
(define (begin? exp)
	(tagged-list? exp 'begin))

(define (begin-actions exp)
	(cdr exp))

(define (last-exp? seq)
	(null? (cdr seq)))

(define (first-exp seq)
	(car seq))

(define (rest-exps seq)
	(cdr seq))

(define (sequence->exp seq)
	(cond ((null? seq) seq)
		  ((last-exp? seq) (first-exp seq))
		  (else (make-begin seq))))

(define (make-begin seq)
	(cons 'begin seq))

; 过程应用
(define (application? exp)
	(pair? exp))

(define (operator exp)
	(car exp))

(define (operands exp)
	(cdr exp))

(define (no-operands? ops)
	(null? ops))

(define (first-operand ops)
	(car ops))

(define (rest-operand ops)
	(cdr ops))





; 

























