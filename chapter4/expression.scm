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
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda (cdadr exp)
					 (cddr exp))))

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

; 派生表达式, 语言中一些特殊形式可以基于其他表达式定义出来
(define (cond? exp)
	(tagged-list? exp 'cond))

(define (cond-clauses exp)
	(cdr exp)) 

(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
	(car clause))

(define (cond-actions clause)
	(cdr clause))

(define (cond->if exp)
	(expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
	(if (null? clauses)
		'false
		(let ((first (car clauses))
			  (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last -- COND->IF"))
				(make-if (cond-predicate first)
					(sequence->exp (cond-actions first))
					(expand-clauses rest))))))
