; 2.73
(define (attach-tag type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for types -- apply-generic" 
		 (list op type-tags))))))

; set up table
(define *op-table* (make-equal-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) '()))
(hash-table/clear! *op-table*)
(hash-table->alist *op-table*)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) exp var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (deriv-sum exp var) 
  (make-sum (deriv (addend exp) var)
	    (deriv (augend exp) var)))
(put 'deriv '+ deriv-sum)

(define (make-product m1 m2) (list '* m1 m2))
(define (multiplicand x) (caddr x))
(define (multiplier x) (cadr x))
(define (deriv-product exp var)
  (make-sum
   (make-product (multiplier exp)
		 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
		 (multiplicand exp))))
(put 'deriv '* deriv-product)

(define (exponent x) (caddr x))
(define (base x) (cadr x))
(define (make-exponent base exp) (list '** base exp))
(define (deriv-exponent exp var)
  (let ((b (base exp))
	(e (exponent exp)))
    (make-product
     (make-product e (make-exponent b (make-sum e -1)))
     (deriv b var))))
(put 'deriv '** deriv-exponent)
(deriv '(* (+ 5 x) (** (+ x 3) 3)) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)


; 2.74
(define (get-record employee file) 
  (apply-generic 'get-record file employee))
; define get-record for each division's file
; each file needs to be tagged with its division
; (put 'get-record 'division 'employee get-record) for each division
; which tells how to get a record out of a file given an employee name
; employee is tagged with some generic employee tag, or division-specific tag

(define (get-salary record)
  (apply-generic 'get-salary record))
; each record needs to be tagged with a division
; (put 'get-salary 'division get-salary) for each division's record

(define (contains-record file employee)
  (apply-generic 'contains-record file employee))
(define (find-employee-record employee files)
  (let ((file (findf contains-record files)))
    (get-record file employee)))
; (put 'contains-record 'division 'employee contains-record) for each division's file


; 2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* (magnitude z) (cos (angle z))))
	  ((eq? op 'imag-part) (* (magnitude z) (sin (angle z))))
	  ((eq? op 'magnitude) (car z))
	  ((eq? op 'angle) (cdr z))
	  (else
	   (error "Unknown op -- make-from-real-imag" op))))
  dispatch)





