;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
  (define (quadratic-roots a b c)
  		(list (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
     		    (* 2 a))
  			    (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
        			 (* 2 a))))


;;Return the list resulting by multiplying each element of `list` by `x`.
  (define (mul-list list x)
                  (if (null? list)
                      '()
                      (cons (* x (car list))
                            (mul-list (cdr list) x))))


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
;(define (sum-lengths list)
  (define sum-lengths
  (lambda (list)
    (cond ((null? list) 0) 
               (else(+ (length (car list))
                       (sum-lengths (cdr list)))))))

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
  (define (poly-eval coeff x)
  (if (equal? (length coeff) 0)
      0
      (+ (* (car coeff) (expt x (- (length coeff) 1)))(poly-eval (cdr coeff) x))))

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
  (define (horner-aux list x acc)
    (if (null? list)
        acc
        (horner-aux (cdr list) x (+ (* acc x) (car list)))))

(define (poly-eval-horner list x)
  (horner-aux list x 0))


;;Return count of occurrences equal? to x in exp
(define (count-occurrences exp x)
	(cond 
          ((null? exp)0)
              ((list? (car exp))
                (if (equal? x(car exp))
		(+ 1 (count-occurrences (cdr exp) x))
		(+ (count-occurrences  (car exp) x) (count-occurrences (cdr exp) x))))
              (else 
                (if (equal? (car exp) x)
		(+ 1 (count-occurrences  (cdr exp) x))
		(+ 0 (count-occurrences  (cdr exp) x))))))   

;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
  
  (define (eval-arith exp)
  (if (number? exp)
      exp
  (let ((op (car exp))
        (arg1 (eval-arith (cadr exp)))
        (arg2 (eval-arith (caddr exp))))
     (cond ((eq? op 'add)
            (+ arg1 arg2))
           ((eq? op 'sub)
            (- arg1 arg2))
           ((eq? op 'mul)
            (* arg1 arg2))
           ((eq? op 'div)
            (/ arg1 arg2))))))


;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
  
(define (length-aux list count)
  (if (null? list)
      count
      (length-aux (cdr list) (+ (length (car list)) count))))

(define (sum-lengths-tr list)
  (length-aux list 0))


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.

  (define (poly-eval-tr-aux coeff x acc)
    (if (null? coeff)
        acc
        (poly-eval-tr-aux (cdr coeff) x  (+ (* (car coeff) (expt x (- (length coeff) 1))) acc))))


(define (poly-eval-tr  coeff x)
  (poly-eval-tr-aux coeff x 0))


;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.

  (define (mul-list-2 list x)
(map (lambda (z) (*  x z))  list))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
  (define (sum-lengths-2 list)
   (foldl + 0
          (map (lambda(x) (length x))    
           list)))
