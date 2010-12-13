; 2.77
(define *op-table* (make-equal-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) '()))
(hash-table/clear! *op-table*)
(hash-table->alist *op-table*)

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for types -- apply-generic" 
		 (list op type-tags))))))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)
(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(real-part (make-from-real-imag 3 4))
(magnitude (make-from-real-imag 3 4))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex 
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)
(install-complex-package)

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) magnitude)
  
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(magnitude (make-complex-from-real-imag 3 4))