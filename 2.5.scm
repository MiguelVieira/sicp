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
(define (raise-multi val target-tag)
  (if (eq? (type-tag val) target-tag)
      val
      (raise-multi (raise val) target-tag)))

(define (enumerate-super-types types)
  (let ((super-type (get-super-type types)))
    (if (= (length types) 1)
	(if (eq? super-type '())
	    (list types)
	    (cons types (enumerate-super-types (list super-type))))
	(let ((remaining (enumerate-super-types (cdr types))))
	  (let ((first-part  
		 (map (lambda (x) (cons (car types) x)) remaining)))
	    (if (eq? super-type '())
		first-part
		(append
		 first-part
		 (enumerate-super-types (cons super-type (cdr types))))))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
	  (apply proc (map contents args))
	  (let ((super-types (enumerate-super-types type-tags)))
	    (define (apply-generic-raise types)
	      (let ((proc (get op (car types))))
		(if (not (null? proc))
		    (apply proc (map contents (map raise-multi args (car types))))
		    (if (not (null? (cdr types)))
			(apply-generic-raise (cdr types))
			(error "No method for types -- apply-generic")))))
	    (apply-generic-raise (cdr super-types)))))))

(define (drop x) 
  (let ((inner-project (get 'project (list (type-tag x)))))
    (if (not (null? inner-project))
	(apply inner-project (list (contents x)))
	x)))

(define (get-super-type x) (apply-generic 'get-super-type x))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
	(else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(error "Bad tagged datum -- type-tag" datum)))
(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(error "Bad tagged datum -- contents" datum)))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag-inner x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (add z1 z2) (make-from-real-imag-inner 
		       (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
  (define (tag x) (attach-tag 'rectangular x))
  (define (negate-inner x) 
    (make-from-real-imag 
     (negate (real-part x)) 
     (negate (imag-part x))))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'add '(rectangular rectangular) 
       (lambda (x y) (drop (tag (add x y)))))
  (put 'make-from-real-imag '(scheme-number scheme-number)
       (lambda (x y) (tag (make-from-real-imag-inner x y))))
  (put 'project '(rectangular)
       (lambda (x) 
	 (if (= (imag-part x) 0)
	     (real-part x)
	     x)))
  (put 'negate '(rectangular)
       (lambda (x)
	 (negate-inner x)))
  'done)
(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))

(define (make-from-real-imag x y) (apply-generic 'make-from-real-imag x y))
		
(define (install-complex-package)
  (define (add x y)
    ((get 'add '(rectangular rectangular)) (contents x) (contents y)))
  (define (tag z) (attach-tag 'complex z))
  (define (my-equ? z1 z2)
    (and (equ? (real-part z1) (real-part z2)) 
	 (equ? (imag-part z1) (imag-part z2))))
  (define (=zero?-inner z)
    (and (=zero? (real-part z))
	 (=zero? (imag-part z))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) 
	 (my-equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero?-inner z)))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add x y))))
  (put 'negate '(complex)
       (lambda (x) (tag (negate x))))
  (put 'make-complex-from-real-imag '(scheme-number scheme-number)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'get-super-type '(complex)
       (lambda (x) '()))
  (put 'project '(complex)
       (lambda (x) (project x)))
  'done)
(install-complex-package)

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) magnitude)

(define (make-complex-from-real-imag x y) 
  (apply-generic 'make-complex-from-real-imag x y))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'add x (negate y)))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (raise x) (apply-generic 'raise x))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'negate '(scheme-number)
       (lambda (x) (- x)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make '(scheme-number)
       (lambda (x) x))
  (put 'raise '(scheme-number)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'get-super-type '(scheme-number)
       (lambda (x) 'complex))
  (put 'project '(scheme-number)
       (lambda (x)
	 (if (= (round x) x)
	     (make-rational x 1)
	     x)))
  'done)
(install-scheme-number-package)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (equ? r1 r2)
    (and (= (numer r1) (numer r2))
	 (= (denom r1) (denom r2))))
  (define (=zero? r)
    (= 0 (numer r)))
  (define (raise r)
    (/ (numer r) (denom r)))
  (put 'raise '(rational)
       (lambda (r) (raise r)))
  (put 'negate '(rational)
       (lambda (r) 
	 (make-rat 
	  (negate (numer r)) 
	  (negate (denom r)))))
  (put 'equ? '(rational rational)
    (lambda (r1 r2)
      (equ? r1 r2)))
  (put '=zero? '(rational)
       (lambda (r) (=zero? r)))
  (define (tag x) (attach-tag 'rational x))
  (put 'make '(rational)
       (lambda (n d) (tag (make-rat n d))))
  (put 'get-super-type '(rational)
       (lambda (r) 'scheme-number))
  (put 'project '(rational)
       (lambda (r) (tag r)))
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make '(rational)) n d))

(define (project x) (apply-generic 'project x))

(define (div-terms l1 l2)
  (if (empty-termlist? l1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term l1))
	    (t2 (first-term l2)))
	(if (> (order t2) (order t1))
	    (list (the-empty-termlist) l1)
	    (let ((termlist
		   (adjoin-term 
		    (make-term (- (order t1) (order t2)) (div (coeff t1) (coeff t2)))
		    (the-empty-termlist))))	  
	      (let ((m (mul-terms termlist l2)))
		(let ((diff (sub-terms l1 m)))
		  (let ((result (div-terms diff l2)))
		    (list (adjoin-term (first-term termlist) (car result)) (cadr result))))))))))

(define (negate-termlist t)
  (apply-generic 'negate-termlist t))
	
(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
	((empty-termlist? l2) l1)
	(else
	 (let ((t1 (first-term l1)) (t2 (first-term l2)))
	   (cond ((> (order t1) (order t2))
		  (adjoin-term
		   t1 (add-terms (rest-terms l1) l2)))
		 ((< (order t1) (order t2))
		  (adjoin-term
		   t2 (add-terms l1 (rest-terms l2))))
		 (else 
		  (adjoin-term
		   (make-term (order t1)
			      (add (coeff t1) (coeff t2)))
		   (add-terms (rest-terms l1)
			      (rest-terms l2)))))))))

(define (mul-terms l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
		 (mul-terms (rest-terms l1) l2))))

(define (mul-term-by-all-terms t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
	(adjoin-term 
	 (make-term (+ (order t1) (order t2))
		    (mul (coeff t1) (coeff t2)))
	 (mul-term-by-all-terms t1 (rest-terms l))))))

(define (sub-terms l1 l2)
  (add-terms l1 (negate-termlist l2)))



(define (install-polynomial-package)
  (define (make-poly variable termlist)
    (cons variable termlist))
  (define (variable p) (car p))
  (define (termlist p) (cdr p))
  (define (same-variable? a b) (eq? a b))
  (define (negate-inner p) 
    (make-poly (variable p) (negate-termlist (termlist p))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (termlist p1)
			      (termlist p2)))
	(error "polys not in same var -- add-poly"
	       (list p1 p2))))
  (define (mul-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (termlist p1)
			      (termlist p2)))
	(error "polys not in same var -- mul-poly"
	       (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (div-terms (termlist p1)
			      (termlist p2)))
	(error "polys not in same var -- div-poly"
	       (list p1 p2))))
  (define (=zero?-inner p)
    (=zero?-termlist (termlist p)))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make '(polynomial) 
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-inner p)))
  'done)
(install-polynomial-package)

(define (make-poly var terms)
  ((get 'make '(polynomial)) var terms))


(define (dense-termlist-tag)
  '(termlist-dense))
(define (sparse-termlist-tag)
  '(termlist-sparse))

(define (the-empty-termlist) 
  (attach-tag (car (dense-termlist-tag)) '()))
(define (empty-termlist? termlist)
  (or 
   (equal? termlist (dense-termlist-tag))
   (equal? termlist (sparse-termlist-tag))))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (=zero?-termlist termlist) 
  (if (empty-termlist? termlist)
      #t
      (and (= 0 (coeff (first-term termlist))) 
	   (=zero?-termlist (rest-terms termlist)))))

        
(define (install-termlist-sparse)
  (define (first-term-sparse termlist) 
    (if (null? termlist)
	'()
	(car termlist)))
  (define (term-count-sparse termlist)
    (length termlist))
  (define (rest-terms-sparse termlist)
    (if (null? termlist)
	termlist
	(cdr termlist)))
  (define (adjoin-term-sparse term termlist)
    (cons term termlist))
  (define (negate-termlist-sparse t)
    (define (inner-negate src dst)
      (if (null? src)
	  dst
	  (let ((first (car src)))
	    (inner-negate 
	     (cdr src) 
	     (cons (make-term (order first) (- (coeff first))) dst)))))
    (if (null? t)
      '()
      (inner-negate (reverse t) '())))
  (put 'term-count (sparse-termlist-tag)
       (lambda (x) (term-count-sparse x)))
  (put 'first-term (sparse-termlist-tag)
       (lambda (x) (first-term-sparse x)))
  (put 'rest-terms (sparse-termlist-tag)
       (lambda (x) (attach-tag (car (sparse-termlist-tag)) (rest-terms-sparse x))))
  (put 'adjoin-term-generic (sparse-termlist-tag)
       (lambda (term termlist) (attach-tag (car (sparse-termlist-tag)) (adjoin-term-sparse term termlist))))
  (put 'negate-termlist (sparse-termlist-tag)
       (lambda (t) (attach-tag (car (sparse-termlist-tag)) (negate-termlist-sparse t))))
  'done)
(install-termlist-sparse)

(define (install-termlist-dense)
  (define (first-term-dense termlist)
    (if (null? termlist)
	'()
	(make-term (- (length termlist) 1) (car termlist))))
  (define (term-count-dense termlist)
    (define (term-count-inner l count)
      (if (null? l)
	  count
	  (if (= (car l) 0)
	      (term-count-inner (cdr l) count)
	      (term-count-inner (cdr l) (+ 1 count)))))
    (term-count-inner termlist 0))
  (define (rest-terms-dense termlist)
    (define (rest-terms-inner l)
      (if (null? l)
	  l
	  (if (= 0 (car l))
	      (rest-terms-inner (cdr l))
	      l)))
    (rest-terms-inner (cdr termlist)))
  (define (adjoin-term-dense term termlist)
    (define (adjoin-term-dense-inner t-inner tlist-inner)
      (if (> (order t-inner) (order (first-term-dense tlist-inner)))
	  (if (= (order t-inner) (+ (order (first-term-dense tlist-inner)) 1))
	      (cons (coeff t-inner) tlist-inner)
	      (adjoin-term-dense t-inner (cons 0 tlist-inner)))))
    (if (null? termlist)
	(if (=zero? (order term)) 
	    (list (coeff term))
	    (adjoin-term-dense-inner term '(0)))
	(adjoin-term-dense-inner term termlist)))
  (define (negate-termlist-dense t)
    (define (inner-negate src dst)
      (if (null? src)
	  dst
	  (inner-negate (cdr src) (cons (- (car src)) dst))))
    (if (null? t) 
	'()
	(inner-negate (reverse t) '())))
  (put 'first-term (dense-termlist-tag)
       (lambda (x) (first-term-dense x)))
  (put 'rest-terms (dense-termlist-tag)
       (lambda (x) (attach-tag (car (dense-termlist-tag)) (rest-terms-dense x))))
  (put 'adjoin-term-generic (dense-termlist-tag)
       (lambda (term termlist) (attach-tag (car (dense-termlist-tag)) (adjoin-term-dense term termlist))))
  (put 'term-count (dense-termlist-tag)
       (lambda (x) (term-count-dense x)))
  (put 'negate-termlist (dense-termlist-tag)
       (lambda (x) (attach-tag (car (dense-termlist-tag)) (negate-termlist-dense x))))
  'done)
(install-termlist-dense)



	  
; generic functions
(define (first-term termlist)
  (apply-generic 'first-term termlist))

(define (rest-terms termlist)
  (if (empty-termlist? termlist)
      termlist
      (if (= (term-count termlist) 1)
	  (list (type-tag termlist))
	  (apply-generic 'rest-terms termlist))))

(define (term-count termlist)
  (apply-generic 'term-count termlist))


(define (adjoin-term term termlist)
  (if (=zero? (coeff term))
      termlist
      (adjoin-term-generic term termlist)))

(define (adjoin-term-generic term termlist)
  ((get 'adjoin-term-generic (list (type-tag termlist))) term (contents termlist)))

(define (negate x) (apply-generic 'negate x))

; tests

(define (test val)
  (if val
      #t
      (error "test failed")))

(define (test-equal expected got)
  (if (equal? expected got)
      #t
      (begin
	(display "expected ")
	(display expected)
	(newline)
	(display " got ")
	(display got)
	(newline)
	(error "test failed"))))


(display '(first-term tests))
(first-term '(termlist-sparse (1 1) (2 2)))
(first-term '(termlist-sparse))
(first-term '(termlist-dense 1 0 0 0))
(first-term '(termlist-dense))


(display '(term-count tests))
(= (term-count (the-empty-termlist)) 0)
(= (term-count '(termlist-sparse (1 1))) 1)
(= (term-count '(termlist-sparse (2 2) (1 1))) 2)
(= (term-count '(termlist-dense 1 0 0)) 1)
(= (term-count '(termlist-dense 1 0 1)) 2)

(display '(rest-terms tests))
(equal? (rest-terms (the-empty-termlist)) (the-empty-termlist))
(equal? (rest-terms '(termlist-sparse)) '(termlist-sparse))
(equal? (rest-terms '(termlist-dense)) '(termlist-dense))
(equal? 
 (rest-terms '(termlist-sparse (1 1))) 
 '(termlist-sparse))
(equal? (rest-terms '(termlist-sparse (2 2) (1 1))) '(termlist-sparse (1 1)))
(equal? (rest-terms '(termlist-dense 1)) (the-empty-termlist))
(equal? (rest-terms '(termlist-dense 1 0 0 0)) (the-empty-termlist))
(equal? (rest-terms '(termlist-dense 1 0 1)) '(termlist-dense 1))

(display '(adjoin-term-generic tests))
(equal? (adjoin-term-generic '(2 2) (the-empty-termlist)) '(termlist-dense 2 0 0))
(equal? (adjoin-term-generic '(5 1) '(termlist-dense 1 0)) '(termlist-dense 1 0 0 0 1 0))
(equal? (adjoin-term-generic '(5 1) '(termlist-sparse (1 1))) '(termlist-sparse (5 1) (1 1)))
(equal? (adjoin-term-generic '(2 2) '(termlist-sparse)) '(termlist-sparse (2 2)))


(display '(adjoin-term tests))
(equal? 
 (adjoin-term (make-term 2 10) (the-empty-termlist))
 '(termlist-dense 10 0 0))
(equal? 
 (adjoin-term (make-term 4 10) '(termlist-dense 1 0 0)) 
 '(termlist-dense 10 0 1 0 0))
(equal?
 (adjoin-term '(2 1) '(termlist-sparse (1 1)))
 '(termlist-sparse (2 1) (1 1)))

(make-complex-from-real-imag 3 4)
(get-super-type (make-complex-from-real-imag 3 4))

(make-from-real-imag 1 1)
(add (make-from-real-imag 1 2) (make-from-real-imag 2 2))
(sub (make-from-real-imag 2 3) (make-from-real-imag 5 5))

(negate (make-from-real-imag 1 1))
(negate (make-complex-from-real-imag 1 1))
(sub (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))

(add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 2))
(sub (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 2))

(=zero? 0)
(add 3 4)
(sub 5 6)
(mul 2 3)
(div 5 3)
(raise 3)
(equ? 1 1)
(get-super-type (make-rational 1 1))
(get-super-type 1)


(make-rational 3 5)
(get-super-type (make-rational 1 2))

(equ? (make-rational 3 2) (make-rational 3 3))
(equ? 4 4)
(equ? (make-complex-from-real-imag 3 1) (make-complex-from-real-imag 3 1))

(=zero? (make-complex-from-real-imag 0 1))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-rational 1 1))
(=zero? (make-rational 0 1))
(=zero? 0)
	 
(enumerate-super-types '(rational))
(enumerate-super-types '(complex))
(enumerate-super-types '(rational rational))
(enumerate-super-types '(complex rational))
(enumerate-super-types '(rational complex))
(enumerate-super-types '(rational complex scheme-number))
(enumerate-super-types '(rational scheme-number complex))

(project (make-complex-from-real-imag 2 1))

((get 'project '(complex)) (make-complex-from-real-imag 2 1))

(raise (raise (make-rational 3 5)))
(raise-multi (make-rational 1 2) 'complex)
(raise-multi 1.2 'complex)
(raise-multi 1 'scheme-number)
(map raise-multi (list 1 (make-rational 1 2)) '(complex complex))

(add 1 (make-complex-from-real-imag 1.1 2))
(sub (make-complex-from-real-imag 2 2) 3)
(equ? (make-complex-from-real-imag 2 0) (make-rational 2 1))

(project (make-from-real-imag 2 0))
(project (make-from-real-imag 2 1))
(project (contents (make-complex-from-real-imag 2 1)))
(project (make-complex-from-real-imag 2 0))
(project (make-complex-from-real-imag 2 1))
(project 1)
(project 1.2)
(project (make-rational 1 1))

(sub (make-complex-from-real-imag 2 1) (make-complex-from-real-imag 1 1))
(add (make-complex-from-real-imag 2 (- 1)) (make-complex-from-real-imag 1 1))
(add 2.1 0.9)
(sub 2.1 0.1)

(make-rational 1 2)
(magnitude (make-complex-from-real-imag (make-rational 1 2) (make-rational 5 3)))

(make-poly 'x 
	   (adjoin-term (make-term 2 1)
			(adjoin-term (make-term 1 2) 
				     (adjoin-term (make-term 0 1) 
						  (the-empty-termlist)))))

(make-poly 'x '(termlist-sparse (0 1) (1 2) (2 1)))
(add (make-poly 'x '(termlist-sparse (2 2))) (make-poly 'x '(termlist-dense 2 0 0 0)))
(add (make-poly 'x '(termlist-sparse (3 2))) (make-poly 'x '(termlist-sparse (2 2))))
(add (make-poly 'x '(termlist-dense 2 0 0 0)) (make-poly 'x '(termlist-dense 2 0 0 0)))
(mul (make-poly 'x '(termlist-sparse (1 3))) (make-poly 'x '(termlist-sparse (2 3) (1 1))))
(mul (make-poly 'x '(termlist-dense 3 0)) (make-poly 'x '(termlist-dense 3 1 0)))

(test (=zero?-termlist '(termlist-sparse)))
(test (=zero?-termlist '(termlist-dense)))
(test (=zero?-termlist '(termlist-dense 0 0)))
(test (=zero?-termlist '(termlist-sparse (2 0) (1 0) (0 0))))

(test (=zero? (make-poly 'x '(termlist-sparse (1 0) (3 0)))))
(test (not (=zero? (make-poly 'x '(termlist-sparse (1 0) (3 1))))))


(make-poly 'x 
	   (adjoin-term (make-term 2 
				   (make-poly 'y '(termlist-sparse (1 1)))
				   )
			(the-empty-termlist))
)
(add 
 '(polynomial x termlist-sparse (2 (polynomial y termlist-sparse (1 1)))) 
 '(polynomial x termlist-sparse (2 (polynomial y termlist-sparse (1 1)))))

(negate (make-from-real-imag 1 2))
(negate (make-complex-from-real-imag 3 (- 1)))

(display "negate termlist tests")
(test (equal?
       (negate-termlist '(termlist-dense 1 0 -1 0 0 0))
       '(termlist-dense -1 0 1 0 0 0)))
(test (equal?
       (negate-termlist '(termlist-dense))
       '(termlist-dense)))
(test (equal?
       (negate-termlist '(termlist-sparse (3 3) (1 1) (0 -5)))
       '(termlist-sparse (3 -3) (1 -1) (0 5))))
(test (equal?
       (negate-termlist '(termlist-sparse))
       '(termlist-sparse)))

(display "div-terms tests")
(test-equal
 '((termlist-dense 1 0 1 0) (termlist-sparse (1 1) (0 -1)))
 (div-terms '(termlist-sparse (5 1) (0 -1)) '(termlist-sparse (2 1) (0 -1))))
(test-equal
 '((termlist-dense) (termlist-sparse (1 1)))
 (div-terms '(termlist-sparse (1 1)) '(termlist-sparse (2 1))))

(display "div-poly tests")
(test-equal
 '(polynomial x (termlist-dense 1 0 1 0) (termlist-sparse (1 1) (0 -1)))
 (div (make-poly 'x '(termlist-sparse (5 1) (0 -1))) (make-poly 'x '(termlist-sparse (2 1) (0 -1)))))
			
'done

