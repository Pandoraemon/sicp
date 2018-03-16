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

; 创建局部表格
(define (make-table)
	(let ((local-table (list '*table*)))
		(define (lookup key1 key2)
			(let ((subtable (assoc key1 (cdr local-table))))
				(if subtable
					(let ((record (assoc key2 (cdr subtable))))
						(if record
							(cdr record)
							false))
					false)))
	(define (insert! key1 key2 value)
		(let ((subtable (assoc key1 (cdr local-table))))
			(if subtable
				(let ((record (assoc kye2 (cdr subtable))))
					(if record
						(set-cdr! record value)
						(set-cdr! subtable
									(cons (cons key2 value)
										(cdr subtable)))))
				(set-cdr! local-table
							(cons (list key1
										(cons key2 value))
									(cdr local-table))))))
	(define (dispatch m)
		(cond ((eq? m 'lookup-proc) lookup)
			  ((eq? m 'insert-proc!) insert!)
			  (else (error "Unkown operation -- TABLE" m))))
	dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; 3.3.4 数字电路的模拟器
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
	(let ((d (make-wire)) (e (make-wire)))
		(or-gate a b d)
		(and-gate a b c)
		(inverter c e)
		(and-gate d e s)
		'ok))

(define (full-adder a b c-in sum c-out)
	(let (s (make-wire)
		 (c1 (make-wire))
		 (c2 (make-wire)))
		(half-adder b c-in s c1)
		(half-adder a s sum c2)
		(or-gate c1 c2 c-out)
		'ok))

; 基本功能块
(define (get-signal! wire)
	(wire 'get-signal))
(define (set-signal! wire new-value)
	((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
	((wire 'add-action!) action-procedure))
(define (inverter input output)
	(define (invert-input)
		(let ((new-value (logical-not (get-signal input))))
			(after-delay inverter-delay
				(lambda ()
					(set-signal! output new-value)))))
	(add-action! input inverter)
	'ok)
(define (logical-not s)
	(cond ((= s 0) 1)
		  ((= s 1) 0)
		  (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
	(define (and-action-procedure)
		(let ((new-value
				(logical-and (get-signal a1) (get-signal a2))))
			(after-delay and-gate-delay
				(lambda ()
					(set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

(define (logical-and a1 a2)
	(cond ((and (= a1 1) (= a2 1)) 1)
		  (else 0)))

; 3.28
(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
				(logical-or (get-signal a1) (get-signal a2))))
			(after-delay or-gate-delay
				(lambda ()
					(set-signal! output new-value)))))
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok)

(define (logical-or a1 a2)
	(if (and (= a1 0) (= a2 0)
		0
		1)))


; 3.29
;(or a b) = (not (and (not a) (not b)))
;a1---not---b1|
;	          |-----and-----c1----not----output
;a2---not---b2|
(define (or-gate a1 a2 output)
	(let ((b1 (make-wire))
		  (b2 (make-wire))
		  (c1 (make-wire)))
		(inverter a1 b1)
		(inverter a2 b2)
		(and-gate b1 b2 c1)
		(inverter c1 output))
	'ok)
; 延迟为3 * inverter-delay + and-gate-delay

; 线路的表示
(define (make-wire)
	(let ((signal-value 0) (action-procedure ()))
		(define (set-my-signal! new-value)
			(if (not (= singal-value new-value))
				(begin (set! signal-value new-value)
					   (call-each action-procedure))
				'done))
		(define (accept-action-procedure! proc)
			(set! action-procedure (cons proc action-procedure))
			(proc))
		(define (dispatch m)
			(cond ((eq? m 'get-signal!) signal-value)
				  ((eq? m 'set-signal!) set-my-signal!)
				  ((eq? m 'add-action!) accept-action-procedure!)
				  (else (error "Unkown operation -- WIRE" m))))
		dispatch))
(define (call-each procedures)
	(if (null? procedures)
		'done
		(begin
			((car procedures))
			(call-each (cdr procedures)))))

; 待处理表
(define (after-delay delay action)
	(add-to-agenda! (+ delay (current-time the-agenda))
					action
					the-agenda))

(define (propagate)
	(if (empty-agenda? the-agenda)
		'done
		(let ((first-item (first-agenda-item the-agenda)))
			(first-item)
			(remove-first-agenda-item! the-agenda)
			(propagate))))

; 实例模拟
(define (probe name wire)
	(add-action! wire
		(lambda ()
			(newline)
			(display name)
			(display " ")
			(display (current-time the-agenda))
			(display "  new-value = ")
			(display (get-signal wire))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)



















