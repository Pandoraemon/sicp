(load "../chapter1/e1-2.scm")
(define (fib n)
		(fib-iter 1 0 n))

(define (fib-iter a b count)
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))	

;2.1 数据抽象
;有理数的表示,使用序对(cons)组合
(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

;定义有理数计算
(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))

(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))

(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
			  (* (denom x) (denom y))))

(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
			  (* (denom x) (numer y))))

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
	   (* (denom x) (numer y))))

;test
(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat one-third)
(print-rat (add-rat one-third one-half))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;化简输出,只需要修改构造过程,不需要修改计算过程
(define (gcd a b)
	(if (= b 0) 
		a
		(gcd b (remainder a b))))

(define (make-rat n d)
	(let ((g (gcd n d)))
		(cons (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))


;2.2 层次性数据和闭包
(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))
(list-ref squares 3)
;递归求 list 长度
(define (length items)
	(if (null? items)
		0
		(+ (length (cdr items)) 1)))
(length squares)
;迭代版本
(define (length items)
	(define (length-iter a count)
		(if (null? a)
			count
			(length-iter (cdr a) (+ count 1))))
		(length-iter items 0))
(length squares)

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))

(append squares odds)

; map
(define (scale-list items factor)
	(if (null? items)
		()
		(cons (* (car items) factor)
			(scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

; (define (map process items)
; 	(if (null? items)
; 		()
; 		(cons (process (car items))
; 			(map process (cdr items)))))
(map abs (list -10 2 -4 18 -1.5))

(define (scale-list items factor)
	(map (lambda (x) (* x factor)) items))

(scale-list (list 1 2 3 4 5) 10)

; 层次性结构
(define (count-leaves tree)
	(cond ((null? tree) 0)
		  ((not (pair? tree)) 1)
		  (else (+ (count-leaves (car tree)) 
		  		(count-leaves (cdr tree))))))
(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x))

; 对树的映射
(define (scale-tree tree factor)
	(cond ((null? tree) ())
		  ((not (pair? tree)) (* tree factor))
		  (else (cons (scale-tree (car tree) factor)
		  			  (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4 ) 5) (list 6 7)) 10)


(define (scale-tree tree factor)
	(map (lambda (sub-tree)
			(if (pair? sub-tree)
				(scale-tree sub-tree factor)
				(* sub-tree factor)))
		tree))

(scale-tree (list 1 (list 2 (list 3 4 ) 5) (list 6 7)) 10)


; 信号流结构
(define (sum-odd-squares tree)
	(cond ((null? tree) 0)
		((not (pair? tree))
			(if (odd? tree)
				(squares tree)
				0))
		(else (+ (sum-odd-squares (car tree))
				 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
	(define (next k)
		(if (> k n)
			()
			(let ((f (fib k)))
				(if (even? f)
					(cons f (next (+ k 1)))
					(next (+ k 1))))))
	(next 0))
(even-fibs 10)
;enumerate->filter->map->accumulate

; 过滤
(define (filter predicate sequence)
	(cond ((null? sequence) ())
		  ((predicate (car sequence))
		  	(cons (car sequence) (filter predicate (cdr sequence))))
		  (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))

; 累计
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))

; 枚举
(define (enumerate-interval low high)
	(if (> low high)
		()
		(cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 1 10)

(define (enumerate-tree tree)
	(cond ((null? tree) ())
		  ((not (pair? tree))
		  	   (list tree))
		  (else (append (enumerate-tree (car tree))
		  				(enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; 重新用信号流结构组合
(define (sum-odd-squares tree)
	(accumulate + 0
		(map square
			(filter odd?
				(enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

(define (even-fibs n)
	(accumulate cons ()
		(filter even?
			(map fib
				(enumerate-interval 0 n)))))

(even-fibs 10)
; 重新安排序列操作
(define (list-fib-squares n)
	(accumulate cons ()
		(map square
			(map fib
				(enumerate-interval 0 n)))))
(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
	(accumulate * 1
		(map square
			(filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; 嵌套映射

(define (flatmap proc seq)
	(accumulate append () (map proc seq)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum?
			(flatmap
				(lambda (i)
					(map (lambda (j) (list i j))
						(enumerate-interval 1 (- i 1))))
					(enumerate-interval 1 n)))))

(prime-sum-pairs 6)



; 2.3 符号数据
(define (memq item x)
	(cond ((null? x) false)
		  ((eq? item (car x)) x)
		  (else (memq item (cdr x)))))


; 符号求导
(define (variable? e)
	(symbol? e))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? e)
	(and (pair? e) (eq? (car e) '+)))
(define (addend e)
	(cadr e))
(define (augend e)
	(caddr e))
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
		  ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2) (+ a1 a2)))
		  (else (list '+ a1 a2))))
(define (product? e)
	(and (pair? e) (eq? (car e) '*)))
(define (multiplier e)
	(cadr e))
(define (multiplicand e)
	(caddr e))
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
		  ((=number? m1 1) m2)
		  ((=number? m2 1) m1)
		  ((and (number? m1) (number? m2)) (* m1 m2))
		  (else (list '* m1 m2))))
	
(define (=number? exp num)
	(and (number? exp) (= exp num)))


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
		  (else
		  	(error "unknown expression type" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)




; 集合表示

; 集合作为未排序的表
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

; 集合作为排序的表

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


(intersection-set '(1 2 3 4) '(3 4 5 6))


; 集合作为二叉树
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


; Huffman tree
(define (make-leaf symbol weight)
	(list 'leaf symbol weight))

(define (leaf? object)
	(eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list left
		  right
		  (append (symbols left) (symbols right))
		  (+ (weight left) (weight right))))

(define (left-branch tree)
	(car tree))

(define (right-branch tree)
	(cadr tree))

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))

(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))

; decode huffman tree
(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch
				  (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch)
						  (decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
		  ((= bit 1) (right-branch branch))
		  (else (error "bad bit"))))

(define (adjoin-set x set)
	(cond ((null? set) (list x)
		  ((< (weight x) (weight (car set))) (cons x set))
		  (else (cons (car set) (adjoin-set x (cdr set)))))))


(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
			(adjoin-set (make-leaf (car pair)
								   (cadr pair))
			(make-leaf-set (cdr pairs))))))



; 2.4 抽象数据的多重表示

; 复数的定义-直角坐标表示
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
	(sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
	(atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y)
	(cons x y))
(define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))

;复数的定义-极坐标表示
(define (real-part z) 
	(* (magnitude z) (cos (angle z))))
(define (imag-part z) 
	(* (magnitude z) (sin (angle z))))
(define (magnitude z)
	(car z))
(define (angle z)
	(cdr z))
(define (make-from-real-imag x y)
	(cons (sqrt (+ (square x) (square y)
		  (atan y x)))))
(define (make-from-mag-ang r a)
	(cons r a))


; 复数的运算

(define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
					   (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					   (- (angle z1) (angle z2))))

; 带标志数据
(define (attach-tag type-tag contents)
	(cons type-tag contents))
(define (type-tag datum)
	(if (pairs? datum)
		(car datum)
		(error "bad tagged datum")))
(define (contents datum)
	(if (pairs? datum)
		(cdr datum)
		(error "bad tagged datum")))
(define (rectangular? z)
	(eq? (type-tag z) 'rectangular))
(define (polar? z)
	(eq? (type-tag z) 'polar))


; 复数的定义-直角坐标表示
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
	(sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
	(atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
	(attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
	(attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

;复数的定义-极坐标表示
(define (real-part-polar z) 
	(* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) 
	(* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z)
	(car z))
(define (angle-polar z)
	(cdr z))
(define (make-from-real-imag-polar x y)
	(attach-tag 'polar (cons (sqrt (+ (square x) (square y)
		  (atan y x))))))
(define (make-from-mag-ang-polar r a)
	(attach-tag 'polar (cons r a)))

; 通用型的选择函数
(define (real-part z)
	(cond ((rectangular? z)
			(real-part-rectangular '(contents z)))
		  ((polar? z)
		  	(real-part-polar '(contentsco z)))
		  (else (error "unknown type"))))

(define (imag-part z)
	(cond ((rectangular? z)
			(imag-part-rectangular '(contents z)))
		  ((polar? z)
		  	(imag-part-polar '(contentsco z)))
		  (else (error "unknown type"))))

(define (magnitude z)
	(cond ((rectangular? z)
			(magnitude-rectangular '(contents z)))
		  ((polar? z)
		  	(magnitude-polar '(contentsco z)))
		  (else (error "unknown type"))))

(define (angle z)
	(cond ((rectangular? z)
			(angle-rectangular '(contents z)))
		  ((polar? z)
		  	(angle-polar '(contentsco z)))
		  (else (error "unknown type"))))


(define (make-from-real-imag x y)
	(make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
	(make-from-mag-ang-polar r a))















































