
(defpackage :parser (:use :cl))

(in-package :parser)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defstruct parser-info
  terms
  start-symbol
  rules)

(defmacro define-exp ())

(defun make-rules (rules)
  (map 'list #'(lambda (rule) (make-grammer rule)) rules))

(defun parse-args (forms)
  (let ((options '()) (rules (make-hash-table)))
    (dolist (form forms)
      (cond
	((member (car form) '(:terms :start-symbol))
	 (if (null (cdr form))
	   (error ""))
	 (push (cdr form) options)
	 (push (car form) options))
	((symbolp (car form))
	 (if (null (cdr form))
	     (error ""))
	 (setf (gethash (car form) rules) (make-rules (cdr form))))))
    (values options rules)))

(defun inference (parser symbols)
  (with-slots (terms start-symbol rules) parser
    (let ((first-rules (gethash (car start-symbol) rules))
	  (lexer (make-lexers symbols)))
      (funcall (car first-rules) lexer)
      (funcall (car first-rules) lexer)
	
    )))

(defun make-symbols (lexer)
  (let ((l lexer))
    #'(lambda (&optional (all? nil)) (if all? l (let ((e (pop l))) (values (car e) (second e)))))))

(defun make-lexers (lex)
  (let ((f1 t)
	(f2 t))
    (make-symbols (loop while f1
			do (multiple-value-bind (x y) (funcall lex)
			     (setq f1 x) (setq f2 y))
			collect (list f1 f2)))))


(defun make-grammer (grammer)
  #'(lambda (lexer)
      "outputs -> (values pointer generated-tree)"
      (let ((failed (gensym))
	    (adeq   (gensym))
	    (l (make-symbols (funcall lexer t))))
	(labels ((next-symbol () (funcall l))
		 (forward (lexer grammer)
		   (multiple-value-bind (s1 s2) (next-symbol)
		     (append (if (eq s1 (car grammer))
				 (list (list s1 s2)) nil)
			     (cond
			       ((= 1 (length grammer))  (list adeq))
			       ((eq s1 (car grammer))   (forward lexer (cdr grammer)))
			       (T    (list failed)))))))
	  (let ((result (forward lexer grammer)))
	    (if (eq (car (last result)) adeq)
		(apply (eval (car (last grammer))) (remove adeq result)) ;lol
		nil))))))

(defmacro define-parser (name &body forms)
  (with-gensyms (opt rules)
    `(multiple-value-bind (,opt ,rules) (parse-args (quote ,forms))
       (defparameter ,name
	 (apply #'make-parser-info (concatenate 'list ,opt (list :rules ,rules)))))))


(define-parser *my-parser*
  (:terms :number
          :symbol)
  (:start-symbol :S)
  (:S (:+ :number :number #'(lambda (x y z) (list x y z)))
      (:- :number :number #'(lambda (x y z) (list x y z))))
  (:G (:+ S S #'(lambda (x y z) (list x y z)))))







