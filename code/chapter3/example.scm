; 3.1.1 局部状态变量
(define balance 100)
(define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
				balance)
		(display "Insufficient funds")))

; 内部变量实现
(define (new-withdraw amount)
	(let ((balance 100))
		(lambda (amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
						balance)
				(display "Insufficient funds")))))


(define (make-withdraw balance)
	(lambda (amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				   balance)
			(display "Insufficient funds"))))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(w1 50)
(w2 70)
(w1 40)
(w2 40)

; 创建银行账户
(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance)
	(define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw)
			  ((eq? m 'deposit) deposit)
			  (else (error "Unkown request"
			  			m))))
	dispatch)


(define acc (make-account 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)


; 引进赋值带来的利益
(define rand
	(let ((x random-init))
		(lambda ()
			(set! x (rand-update x))
			x)))


; monte-carlo
(define (estimate-pi trials)
	(sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
	(= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond ((= trials-remaining 0)
				(/ trials-passed trials))
			  ((experiment)
			  	(iter (- trials-remaining 1) (+ trials-passed 1)))
			  (else (iter (- trials-remaining 1) trials-passed))))
	(iter trials 0))


; 队列的表示
(define (front-ptr queue)
	(car queue))

(define (rear-ptr queue)
	(cdr queue))

(define (set-front-ptr! queue item)
	(set-car! queue item))

(define (set-rear-ptr! queue item)
	(set-cdr! queue item))

(define (empty-queue? queue)
	(null? (front-ptr queue)))

(define (make-queue) (cons () ()))

(define (front-queue queue)
	(if (empty-queue queue)
		(error "FRONT called with empty queue" queue)
		(car (front-ptr queue))))

(define (insert-queue! queue item)
	(let ((new-pair (cons item ())))
		(cond ((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
				queue)
			  (else
			  	(set-cdr! (rear-ptr queue) new-pair)
			  	(set-rear-ptr! queue new-pair)
			  	queue))))

(define (delete-queue! queue)
	(cond ((empty-queue? queue)
			(error "Delete called with an empty queue" queue))
		  (else
		  	(set-rear-ptr! queue (cdr (front-ptr queue)))
		  	queue)))



; 一维表
(define (lookup key table)
	(let ((record (assoc key (cdr table))))
		(if record
			(cdr record)
			false)))

(define (assoc key records)
	(cond ((null? records)
			false)
		  ((= key (caar records))
		  	(car records))
		  (else
		  	(assoc key (cdr records)))))


(define (insert! key value table)
	(let ((record (assoc key (cdr table))))
			(if record
				(set-cdr! record value)
				(set-cdr! table
					(cons (cons key value) (cdr table)))))
	'ok)

(define (make-table)
	(list '*table*))


; 二维表
(define (lookup key1 key2 table)
	(let ((subtable (assoc key1 (cdr table))))
		(if subtable
			(let ((record (assoc key2 (cdr subtable))))
				(if record
					(cdr record)
					false))
			false)))


(define (insert! key1 key2 value table)
	(let ((subtable (assoc key1 (cdr table))))
		(if subtable
			(let ((record (assoc kye2 (cdr subtable))))
				(if record
					(set-cdr! record value)
					(set-cdr! subtable
								(cons (cons key2 value)
									(cdr subtable)))))
			(set-cdr! table
						(cons (list key1
									(cons key2 value))
								(cdr table)))))
	'ok)























