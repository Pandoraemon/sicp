(load "example.scm")

; 2.17
(define (last-pair lst)
	(if (null? (cdr lst))
		lst
		(last-pair (cdr lst))))

(last-pair (list 1 2 3))


; 2.18
(define (reserse lst)
	(define (reserse-iter remainder-items result)
		(if (null? remainder-items)
			result
			(reserse-iter (cdr remainder-items)
						  (cons (car remainder-items) result))))
	(reserse-iter lst ()))

(reserse (list 1 2 3 4))


; 2.20
; 函数传入不定长参数
(define (same-parity sample . others)
	(filter (if (odd? sample)
				odd?
				even?)
			(cons sample others)))
(same-parity 1 2 3 4 5 6 7)

; 2.21
(define (square-list lst)
	(map square lst))

(square-list (list 1 2 3))

(define (square-list items)
	(if (null? items)
		()
		(cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3))

; 2.22
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons (square (car things))
				  	    answer))))
	(iter items ()))

(square-list (list 1 2 3))

(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons answer
				  	(square (car things))))))
	(iter items ()))

(square-list (list 1 2 3))

(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons (square (car things))
				  	    answer))))
	(iter (reverse items) ()))

(square-list (list 1 2 3))

; 2.23
(define (for-each p items)
	(if (not (null? items))
		(begin
			(p (car items))
		 	(for-each p (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	(list 23 45 13))

; 2.24
(list 1 (list 2 (list 3 4)))

; 2.25
(car (cdaddr (list 1 3 (list 5 7) 9)))

(caar (list (list 7)))

(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadadr (cadadr (cadadr z)))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) 4 5 6))
(list x y)
; ((1 2 3) (4 5 6))


; 2.27
(define (deep-reverse tree)
	(cond ((null? tree) ())
	  ((not (pair? tree)) tree)
	  (else (reverse (list (deep-reverse (car tree)) 
	  		(deep-reverse (cadr tree)))))))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)

; 2.28
(define (fringe tree)
	(cond ((null? tree) ())
	  ((not (pair? tree)) (list tree))
	  (else (append (fringe (car tree))
	  			  (fringe (cadr tree))))))

(fringe x)


; 2.29
(define (make-mobile left right)
	(list left right))

(define (make-branch length stucture)
	(list length stucture))

(define (left-branch m)
	(car m))
(define (right-branch m)
	(cadr m))
(define (branch-length b)
	(car b))
(define (branch-structure b)
	(cadr b))

; test
(define mobile (make-mobile (make-branch 10 25)
                                  (make-branch 5 20)))
(left-branch mobile)
(branch-structure (right-branch mobile))

; total weight
(define (total-weight m)
	(+ (branch-weight (left-branch m))
	   (branch-weight (right-branch m))))
(define (has-another-mobile? b)
	(list? (branch-structure b)))

(define (branch-weight b)
	(if (not (has-another-mobile? b))
		(branch-structure b)
		(total-weight (branch-structure b))))

(total-weight mobile)
(define another-mobile (make-mobile (make-branch 10 mobile)   
                                          (make-branch 10 20)))

(total-weight another-mobile)

; balance
(define (branch-torque b)
	(* (branch-length b)
		(branch-weight b)))

(define (torque-equal? left right)
	(= (branch-torque left)
	   (branch-torque right)))

(define (mobile-balance? m)
	(let ((left (left-branch m))
		  (right (right-branch m)))
		 (and (torque-equal? left right)
		 	(branch-balance? left)
		 	(branch-balance? right))))

(define (branch-balance? b)
	(if (has-another-mobile? b)
		(mobile-balance? (branch-structure b))
		#t))
; test
(define balance-mobile (make-mobile (make-branch 10 10)
                                          (make-branch 10 10)))
(mobile-balance? balance-mobile)

;2.30
(define (square-tree tree)
	(cond ((null? tree) ())
		  ((not (pair? tree)) (square tree))
		  (else (cons (square-tree (car tree))
		  			  (square-tree (cdr tree))))))
(square-tree (list 1 (list 2 (list 3 4 ) 5) (list 6 7)))


(define (square-tree tree)
	(map (lambda (sub-tree)
		(if (not (pair? sub-tree))
			(square sub-tree)
			(square-tree sub-tree))) 
		tree))
(square-tree (list 1 (list 2 (list 3 4 ) 5) (list 6 7)))

; 2.31
(define (tree-map process tree)
	(map (lambda (sub-tree)
		(if (not (pair? sub-tree))
			(process sub-tree)
			(tree-map process sub-tree)))
		tree))

(define (square-tree tree)
	(tree-map square tree))
(square-tree (list 1 (list 2 (list 3 4 ) 5) (list 6 7)))


; 2.32
(define (subsets s)
	(if (null? s)
		(list ())
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (x) (cons (car s) x)) rest)))))
			;当前集合的所有自己等于除去第一个元素集合的子集加上第一个元素和rest 的各个子集的组合 
(subsets (list 1 2 3))


; 2.33
; (define (map p sequence)
; 	(accumulate (lambda (x y) (cons (p x) y)) () sequence))
(define (append seq1 seq2)
	(accumulate cons seq2 seq1))
(define (length sequence)
	(accumulate (lambda (x y) (+ y 1)) 0 sequence))
(define lst (list 1 2 3 4))
(map square lst)
(append lst lst)
(length lst)

; 2.34
(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms)
					(+ this-coeff (* higher-terms x)))
		0
		coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

; 2.35
(define (count-leaves t)
	(accumulate 
		+
		0
		(map (lambda (sub-tree)
				(if (pair? sub-tree)
					(count-leaves sub-tree)
					1))
			t)))
(count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7))))

; 2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		()
		(cons (accumulate op init 
				(map car seqs))
			  (accumulate-n op init
			  	(map cdr seqs)))))
(define x  (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 x)

;2.37
(define (dot-product v w)
	(accumulate + 0 (map * v w)))
(define v (list 1 2 3 4))
(define w (list 1 2 3 4))
(map + v w)
(dot-product v w)
(define (matrix-*-vector m v)
	(map (lambda (x) (dot-product x v)) m))

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(matrix-*-vector m v)

(define (transpose m)
	(accumulate-n cons () m))

(transpose m)

(define (matrix-*-matrix m n)
	(let ((trans-n (transpose n)))
		(map (lambda (col-of-m)
				(matrix-*-vector trans-n col-of-m))
			m)))

(define n (list (list 1 2 3 )
                (list 4 5 6 )
                (list 6 7 8 )
                (list 8 9 10)))

(matrix-*-matrix m n)

; 2.38
(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest)) (cdr rest))))
	(iter initial sequence))
(define accumulate fold-right)
(fold-left / 1 (list 1 2 3 4))
(fold-right / 1 (list 1 2 3 4))

; 2.39

(define (reverse sequence)
	(fold-right (lambda (x  y) (append y (list x))) () sequence))
(reverse (list 1 2 3 4 5 6 7))

(define (reserse sequence)
	(fold-left (lambda (x y) (append x (list y))) () sequence))
(reverse (list 1 2 3 4 5 6 7))














