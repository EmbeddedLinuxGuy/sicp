;; leaf

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (branch)
    (eq (car branch) 'leaf))

(defun symbol-leaf (leaf)
  (cadr leaf))

(defun weight-leaf (leaf)
  (caddr leaf))

;; branch

(defun make-code-tree (left-branch right-branch)
  (list left-branch
	right-branch
	(append (symbols left-branch) (symbols right-branch))
	(+ (weight left-branch) (weight right-branch))))

(defun left-branch (tree)
  (car tree))

(defun right-branch (tree)
  (cadr tree))

(defun symbols (tree)
  (cond ((leaf? tree) (list (symbol-leaf tree)))
	(t (caddr tree))))

(defun weight (tree)
  (cond ((leaf? tree) (weight-leaf tree))
	(t (cadddr tree))))

(defun decode (bits tree)
  (defun decode-1 (bits current-branch)
    (if (null bits) '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
	(if (leaf? next-branch)
	    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
	  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(defun choose-branch (bit branch)
  (cond ((eq bit 0) (left-branch branch))
	((eq bit 1) (right-branch branch))
	(t (error "No such bit %d" bit))))

(defun encode-messge (message tree)
  (if (null message) '()
    (append
     (encode-symbol (car message) tree)
     (encode-message (cdr message) tree))))

(defun encode-symbol (symbol tree)
  (cond
   ((not (memq symbol (symbols tree))) (error "symbol not in tree!"))
   ((leaf? tree) nil)
   ((memq symbol (symbols (left-branch tree)))
    (cons 0 (encode-symbol symbol (left-branch tree))))
   ((memq symbol (symbols (right-branch tree)))
    (cons 1 (encode-symbol symbol (right-branch tree))))))

;; tests

(setq t1 (make-code-tree
	  (make-leaf 'b 2)
	  (make-leaf 'c 3)))
(setq t2 (make-leaf 'a 1))
(setq t3 (make-code-tree t1 t2))

(decode '(0 0 0 1 1 1 0 1 0 0) t3)
(not (memq 'a (symbols (left-tree t3))))

(encode-symbol 'a t3)
(encode-symbol 'b t3)
(encode-symbol 'c t3)
