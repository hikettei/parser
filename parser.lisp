

(defpackage :parser (:use :cl))

(in-package :parser)

(defparameter *exp* nil)
(defparameter *rules* (make-hash-table))
(defparameter *chars* (make-hash-table))

(defmacro define-syntax (name &rest ant)
  ; Define Rules like ... A (exp: what A is?): B C
  `(setq *exp*
	 (concatenate 'list *exp* (list (list ',name (gethash ',name *chars*) ',ant)))))

(defmacro define-rule (con &rest ant)
  `(setf (gethash ',con *rules*) #'(lambda (exp)
				     (let ((obj nil))
				       (dolist (i (car (list ',ant)))
					 (let ((cd (gethash i *chars*)))
					   (if (funcall cd exp)
					       (setq obj exp))))
				       obj))))

(defmacro define-char (con cd)
  `(progn
     (define-rule ,con ,con)
     (setf (gethash ',con *chars*) ,cd)))

(defun failed (puterr?)
  #'(lambda () (if puterr?
		   (error "")
		   (progn (print "failed") '@))))

(defmacro with-following-rules (var rules query &body body)
  `(dotimes (i (length ,rules))
     (progn
       (let ((,var (inference ,query *exp* T))) ,@body))))
     ; issue (+ 1 1) 1 ga num to site ninsiki sarenai
(defun suit? (rule token query)
  (if (funcall (second rule) token) 
      ; Then following (third rule), tokenizering (cdr query)
      (let ((nexts (list token))
	    (failed? nil))
	(if (third rule)
	    (progn
	      (with-following-rules x (third rule) (cdr query)
		(if x (setq nexts (concatenate 'list nexts x)))
		(unless x (setq failed? t)))
	      (if (eq nexts (list token))
		  nil
		  (if failed?
		      '@
		      nexts)))
	    (let ((cn (car rule)))
	      (if (funcall (gethash cn *chars*) token)
		  (list (list token)) nil))))
      nil))

(defmacro tokenizer (query a)
  `(inference ,query *exp* ,a))

(defun inference (query exp next? &optional (puterr? T))
  (let ((paths nil))
    (labels ((next () (let ((r (funcall (if paths (pop paths) (failed puterr?)))))
			(if r (if (eq r '@) (if puterr?
						(next)
						nil) r) (next))))
	     (forward () (let ((tree (next)))
			   (unless (or (eq tree '@) (eq tree nil))
	                     (progn
			       (setq query (subseq query (length tree)
						   (length query)))     
			       tree))))
	     (setpaths ()
	       (setq paths nil)
	       (mapcar #'(lambda (e) (push #'(lambda ()
					       (suit? e (car query) query))
					   paths)) exp))
	     (nexttree () (setpaths) (forward))
	     (generate () (if query (concatenate 'list
						 (list (nexttree))
						 (generate) nil))))
      (if query
	  (if next?
	    (nexttree) (generate)) nil))))

 ; (tokenizer `(+ a b c)) = (+ A B)
