(load "example.scm")

(define (average x y)
	(/ (+ x y) 2.0))

(define (distance p1 p2)
	(let ((x1 (x-point p1))
		(x2 (x-point p2))
		(y1 (y-point p1))
		(y2 (y-point p2)))
		(sqrt (+ (square (- x1 x2))
				 (square (- y1 y2))))))

;2.1
(define (make-rate n d)
		(if (< d 0)
			(cons (- n) (- d))
			(cons n d)))

;2.2
(define (make-segment x y)
	(cons x y))
(define (start-segment l)
	(car l))
(define (end-segment l)
	(cdr l))
(define (make-point x y)
	(cons x y))
(define (x-point p)
	(car p))
(define (y-point p)
	(cdr p))
(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))
(define (midpoint-segment l)
	(make-point (average (x-point (start-segment l)) (x-point (end-segment l)))
				(average (y-point (start-segment l)) (y-point (end-segment l)))))
(define (length-of-segment l)
	(let ((p1 (start-segment l)))
		((p2 (end-segment l)))
		(distance p1 p2)))

(define start (make-point 1 3))
(define end (make-point 4 3))
(define seg (make-segment start end))
(define mid (midpoint-segment seg))
(print-point mid)
(distance start end)

;2.3
(define (make-rectangle length width)
    (cons length width))
(define (length r)
	(car r))
(define (width r)
	(cdr r))
(define (length-of-rectangle r)
	(let ((start (start-segment (length r)))
		(end (end-segment (length r))))
		(- (x-point end)
			(x-point start))))

(define (width-of-rectangle r)
	(let ((start (start-segment (width r)))
		(end (end-segment (width r))))
		(- (y-point end)
			(y-point start))))



(define (perimeter-rectangle r)
	(let ((length (length-of-rectangle r))
		(width (width-of-rectangle r)))
		(* 2.0 (+ length width))))

(define (area-rectangle r)
	(let ((length (length-of-rectangle r))
		(width (width-of-rectangle r)))
		(* length width)))

(define l (make-segment (make-point 1 2)
           				(make-point 4 2)))
(define w (make-segment (make-point 1 2)
                    	(make-point 1 4)))
(define r (make-rectangle l w))
(length-of-rectangle r)
(width-of-rectangle r)
(perimeter-rectangle r)
(area-rectangle r)


;p1------------p2
;|              |
;|              |
;p3------------p4
(define (make-rectangle p1 p2 p3 p4)
    (cons (cons p1 p2)
    	(cons p3 p4)))
(define (length r)
	(car r))
(define (width r)
	(make-segment (car (car r))
				  (car (cdr r))))
(define (length-of-rectangle r)
	(let ((start (car (car r)))
		(end (cdr (car r))))
		(distance start end)))

(define (width-of-rectangle r)
	(let ((start (car (car r)))
		(end (car (cdr r))))
		(distance start end)))
(define r (make-rectangle (make-point 1 4)
						  (make-point 4 4)
						  (make-point 1 2)
						  (make-point 4 2)))
(length-of-rectangle r)
(width-of-rectangle r)
(perimeter-rectangle r)
(area-rectangle r)


;2.4
(define (cons x y)
	(lambda (m) (m x y)))
(define (car z)
	(z (lambda (p q) p)))
(define (cdr z)
	(z (lambda (p q) q)))
 (define x (cons 3 2))
 (car x)
 (cdr x)

;2.5
(define (cons x y)
	(* (expt 2 x)
		(expt 3 y)))
(define (car z)
	(if (= 0 (remainder z 2))
		(+ 1 (car (/ z 2)))
		0))
(define (cdr z)
	(if (= 0 (remainder z 3))
		(+ 1 (cdr (/ z 3)))
		0))
 (define x (cons 3 2))
 (car x)
 (cdr x)


;2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x) f) x))))
;((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
;(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
























