; 2.53
(load "example.scm")
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))



; 2.56
(define (exponentiation? e)
	(and (pair? e) (eq? (car e) '**)))
(define (base e)
	(cadr e))
(define (exponent e)
	(caddr e))
(define (make-exponentiation b e)
	(cond ((=number? e 0) 1)
		  ((=number? e 1) b)
		  (else (list '** e b))))
(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp)
		  	(if (same-variable? exp var) 1 0))
		  ((sum? exp)
		  	(make-sum (deriv (addend exp) var)
		  			  (deriv (augend exp) var)))
		  ((product? exp)
		  	(make-sum
		  		(make-product (multiplier exp)
		  					  (deriv (multiplicand exp) var))
		  		(make-product (deriv (multiplier exp) var)
		  					  (multiplicand exp))))
		  ((exponentiation? exp)
		  	(make-product (exponent exp)
		  				  (make-product
		  				  	(make-exponentiation (base exp)
		  				  					   (- (exponent exp) 1))
		  				  	(deriv (base exp) var))))
		  (else
		  	(error "unknown expression type" exp))))

(deriv '(** x 0) 'x)
(deriv '(** x 1) 'x)
(deriv '(** x 2) 'x)

; 2.57


;2.58


; 2.59
(define (element-of-set? x set)
	(cond ((null? set) false)
		  ((equal? x (car set)) true)
		  (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))
(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
		  ((element-of-set? (car set1) set2)
		  		(cons (car set1) 
		  			  (intersection-set (cdr set1) set2)))
		  (else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
	(cond ((null? set1) set2)
		  ((element-of-set? (car set1) set2)
		  		(union-set (cdr set1) set2))
		  (else (cons (car set1)
		  			  (union-set (cdr set1) set2)))))

(cons (car '(1)) (union-set (cdr '(1)) '(3 4 5 6)))
(union-set '() '(3 4 5 6))
(union-set '(1) '(3 4 5 6))
(union-set '(1 2 3) '(3 4 5 6))
; 2.60

; 2.61
(define (adjoin-set x set)
	(cond ((null? set) (list x))
		  ((= x (car set)) set)
		  ((< x (car set)) (cons x set))
		  (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 0 (list 1 2 3 7))

; 2.62
(define (element-of-set? x set)
	(cond ((null? set) false)
		  ((= x (car set)) true)
		  ((< x (car set)) false)
		  (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2)) 
		'()
		(let ((x1 (car set1)) (x2 (car set2)))
			 (cond ((= x1 x2)
			 		(cons x1 (intersection-set (cdr set1) (cdr set2))))
			 	   ((> x1 x2)
			 	   	(intersection-set set1 (cdr set2)))
			 	   ((< x1 x2)
			 	   	(intersection-set (cdr set1) set2))))))
(define (union-set set1 set2)
	(cond ((null? set1) set2)
		  ((null? set2) set1)
		  (else 
		  	(let ((x1 (car set1)) (x2 (car set2)))
		  		 (cond ((= x1 x2)
			 			(cons x1 (union-set (cdr set1) (cdr set2))))
			 	   	   ((> x1 x2)
			 	   		(cons x2 (union-set set1 (cdr set2))))
			 	       ((< x1 x2)
			 	   		(cons x1 (union-set (cdr set1) set2))))))))
(union-set () ())
(union-set '() (list 1 2 3))
(union-set (list 1 2 3) (list 1 3 5 7 9))



; 2.63
(define (tree2list-1 tree)
	(if (null? tree)
		()
		(append (tree2list-1 (left-branch tree))
				(cons (entry tree)
					  (tree2list-1 (right-branch tree))))))
; 树的先序遍历
(define (tree2list-2 tree)
	(define (copy2list tree result-list)
		(if (null? tree)
			result-list
			(copy2list (left-branch tree)
					   (cons (entry tree)
					   		 (copy2list (right-branch tree)
					   		 	result-list)))))
	(copy2list tree ()))

; 2.64
(define (entry tree) (car tree))
(define (left-branch tree)
	(cadr tree))
(define (right-branch tree)
	(caddr tree))
(define (make-tree entry left right)
	(list entry left right))
(define (element-of-set? x set)
	(cond ((null? set) false)
		  ((= x (entry set) true))
		  ((< x (entry set) (element-of-set? x (left-branch set))))
		  ((> x (entry set) (element-of-set? x (right-branch set))))))

(define (adjoin-set x set)
	(cond ((null? set) (make-tree x () ()))
		  ((= x (entry set)) set)
		  ((< x (entry set)) 
		  	(make-tree
		  		(entry set)
		  		(adjoin-set x (left-branch set))
		  		(right-branch set)))
		  ((> x (entry set))
		  	(make-tree
		  		(entry set)
		  		(right-branch set)
		  		(adjoin-set x (right-branch set))))))

(define (list2tree elements)
	(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
	(if (= n 0) 
		(cons () elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partial-tree elts left-size)))
				(let ((left-tree (car left-result))
					  (non-left-elts (cdr left-result))
					  (right-size (- n (+ left-size 1))))
					(let ((this-entry (car non-left-elts))
						  (right-result (partial-tree (cdr non-left-elts)
						  							  right-size)))
					(let ((right-tree (car right-result))
						  (remaining-elts (cdr right-result)))
						 (cons (make-tree this-entry left-tree right-tree)
						 	   remaining-elts))))))))

(list2tree '(1 3 5 7 9 11))

; 2.65

(define (intersection-tree tree1 tree2)
	(list2tree
		(intersection-set
			(tree2list-2 tree1)
			(tree2list-2 tree2))))

(define (union-tree tree1 tree2)
	(list2tree
		(union-set
			(tree2list-2 tree1)
			(tree2list-2 tree2))))


; 2.66
(define (lookup key set)
	(cond ((null? set) false)
		  ((= key (entry set)) true)
		  ((< key (entry set)) (lookup key (left-branch set)))
		  (else (lookup key (right-branch set)))))






























































