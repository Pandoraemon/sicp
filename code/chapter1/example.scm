;1.3 
;过程即是抽象
(define (cube n)
	(* n n n))
;为了提供抽象能力，参数不止可以是数值，也可以是过程，
;并且也可以以过程作为返回值，能操作过程的过程即是高阶过程

;1.3.1 过程作为参数

;求和函数
(define (sum-integers a b)
	(if (> a b)
		0
		(+ a (sum-integers (+ a 1) b))))

;求立方和

(define (sum-cubes a b)
	(if (> a b)
		0
		(+ (cube a) (sum-cubes (+ a 1) b))))
(sum-cubes 1 10)

;求π
(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;将过程抽象，作为参数传入

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))


;基于重新重新定义之前的求和过程
(define (sum-cubes a b)
	(define (inc n) (+ n 1))
	(sum cube a inc b))
(sum-cubes 1 10)


(define (sum-integers a b)
	(define (inc n) (+ n 1))
	(define (identity x) x)
	(sum identity a inc b))
(sum-integers 1 10)


(define (pi-sum a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term a pi-next b))

(* 8 (pi-sum 1 10000))

;求定积分的值
(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)


;1.3.3 过程作为一般性的方法

(define (average a b)
	(/ (+ a b) 2))

;区间折半法求根
(define (search f neg-point pos-point)
	(let ((midpoint (average neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
			midpoint ;如果区间够小，直接返回中间值
			(let ((test-value (f midpoint)))
				(cond ((positive? test-value)
						(search f neg-point midpoint))
					  ((negative? test-value)
					  	(search f midpoint pos-point))
					  (else midpoint))))))

;检查区间是否够小
(define (close-enough? x y)
	(< (abs (- x y)) 0.001))

;调用search前先检查2个点的取值是否一正一负
(define (half-interval-method f a b)
	(let   ((a-value (f a))
			(b-value (f b)))
		(cond ((and (negative? a-value) (positive? b-value))
				(search f a b))
			((and (negative? b-value) (positive? a-value))
				(search f b a))
			(else
				(error "Value are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
	1.0
	2.0)

;求不动点
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))


(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
	1.0)

(define (sqrt x)
	(fixed-point (lambda (y) (average y (/ x y)))
		1.0))
(sqrt 3)

;1.3.4 过程作为返回值
;平均阻尼
(define (average-damp f)
	(lambda (x) (average (f x) x)))

((average-damp square) 10)

(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y)))
		1.0))
(sqrt 10)

(define (cube-root x)
	(fixed-point (average-damp (lambda (y) (/ x (square y))))
		1.0))

(cube-root 8)

;导数
(define (deriv g)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x))
			dx)))

(define dx 0.00001)

;求导数
(define (cube x)
	(* x x x))

((deriv cube) 5)


;牛顿法
(define (newton-transform g)
	(lambda (x)
		(- x (/ (g x) ((deriv g) x)))))
;寻找牛顿表示的不动点
(define (newton-method g guess)
	(fixed-point (newton-transform g) guess))
(define (sqrt x)
	(newton-method (lambda (y) (- (square y) x))
		1.0))

(sqrt 9)

;抽象牛顿法和平均阻尼法
(define (fixed-point-of-transform g transform guess)
	(fixed-point (transform g) guess))

;利用这一抽象重新定义sqrt
(define (sqrt x)
	(fixed-point-of-transform (lambda (y) (/ x y))
		average-damp 1.0))
(sqrt 9.0)
(define (sqrt x)
	(fixed-point-of-transform (lambda (y) (- (square y) x))
		newton-transform
		1.0))
(sqrt 9.0)



















