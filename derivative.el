;; 2.73 - algebraic expression derivatives with dispatch table

;; construct expressions

(defun make-sum (operands)
  (cons '+ operands))
;;	(simplify-sum operands)))

(defun simplify-sum (operands)
  (cond ((null operands) nil)
	((eq (car operands) 0) (simplify-sum (cdr operands)))
	(t (cons (car operands) (simplify-sum (cdr operands))))))

(defun make-product (operands)
  (cons '* operands))

(defun make-exp (operands)
  (let ((base (car operands))
	(exponent (cadr operands)))
    (cond ((eq exponent 0) 1)
	  ((eq exponent 1) base)
	  (t (cons '** operands)))))

;; create dispatch table

(setq ops-table
      (list 
       (list 'deriv)))

;; table operations put / get

(defun put (entry operator procedure)
  (if (eq entry (caar ops-table))
      (setq ops-table
	    (list
	     (append (car ops-table) (list (cons operator procedure)))))
    (error "Adding new entry %s not implemented" entry)))

(defun get (entry operator)
  (get-from-table entry operator ops-table))

;; get helpers

(defun get-from-table (entry operator table)
  (cond ((null table) (error "No table entry for %s" entry))
	((eq (caar table) entry)
	 (get-operator operator (cdr (car table))))
	(t (get-from-table entry operator (cdr table)))))

(defun get-operator (operator ops)
  (cond ((null ops) (error "No such operator %s" operator))
	((eq (caar ops) operator) (cdr (car ops)))
	(t (get-operator operator (cdr ops)))))

;; deriv helpers

(defun variablep (x) (symbolp x))
(defun operator (x) (car x))
(defun operands (x) (cdr x))

;; We have to put var before (operands)
;; because the final operand to apply is interpreted as a list

(defun deriv (exp var)
  (cond ((numberp exp) 0)
	((variablep exp) (if (eq exp var) 1 0))
	(t (apply (get 'deriv (operator exp)) var (operands exp)))))

;; populate deriv operations

(put 'deriv '+ (lambda (var &rest operands)
		    (make-sum (mapcar (lambda (exp) (deriv exp var)) operands))))

(put 'deriv '* (lambda (var &rest operands)
		    (let ((x (car operands))
			  (y (cadr operands)))
		      (make-sum
		       (list
			(make-product (list x (deriv y var)))
			(make-product (list y (deriv x var))))))))

(put 'deriv '**
	(lambda (var &rest operands)
	  (let ((u (car operands))
		(n (cadr operands)))
					; (if (numberp n)
	    (make-product
			(list n
			      (make-exp (list u (- n 1)))
			      (deriv u var)))
					; (error "Non-numeric exponent %s" n))))
	    )))

;; test

(deriv '(* x 1) 'x)
(deriv '(+ 1 1) 'x)
(deriv '(+ x (+ 1 x)) 'x)
(deriv 1 'x)
(deriv '(+ 1 2) 'x)
(deriv '(** x 2) 'x)
