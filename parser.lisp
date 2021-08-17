


(defpackage :parser (:use :cl))

(in-package :parser)

(defparameter *exp* nil)
(defparameter *exp-name-table* (make-hash-table))
(defparameter *rules* (make-hash-table))
(defparameter *chars* (make-hash-table))

(defparameter *pointer* 0)

(defmacro define-syntax (var syntax-name name &rest ant)
  `(let ((syntax (list (list ',name ',ant))))
     (setq ,var (concatenate 'list ,var syntax))
     (let ((exp-v (gethash ',syntax-name *exp-name-table*)))
       (setf (gethash ',syntax-name *exp-name-table*)
	     (if exp-v
		 (concatenate 'list exp-v syntax)
		 (list syntax))))))

(defmacro define-rule (con &rest ant)
  `(setf (gethash ',con *rules*) #'(lambda (exp)
				     (let ((obj nil))
				       (dolist (i (car (list ',ant)))
					 (let ((cd (gethash i *chars*)))
					   (if (funcall cd exp)
					       (setq obj exp))))
				       obj))))

(defmacro define-char (var con cd)
  `(progn
     (define-rule ,con ,con)
     (setf (gethash ',con *chars*) ,cd)
     (define-syntax ,var ,con ,con)))

(defun failed (puterr?)
  #'(lambda () (if puterr?
		   (error "")
		   (progn '@))))

(defmacro with-following-rules (var rules query &body body)
  `(let ((found-size 0))
     (dotimes (i (length ,rules))
       (let* ((oexp (gethash (nth i ,rules) *exp-name-table*))
	      ;*exp-name-table*内のルールを二重リストにすれば
	      ;*exp*実装できそ, ruleも参照したい
	      (,var (inference (subseq ,query (+ i found-size)) oexp T NIL NIL)))
	 (setq found-size (+ found-size (length ,var) -1)) ; mark
	 ,@body))))

(defmacro match-exp? (name token)
  `(let ((when-exp (gethash ,name *exp-name-table*))
	 (when-char (gethash ,name *chars*))
	 (when-rules (gethash ,name *rules*)))
     (declare (ignore when-exp))
     (cond
       ;(when-exp (inference (list token))) 一文字目にSyntax使えない!!
       (when-char (funcall when-char ,token))
       (when-rules (funcall when-rules ,token)))))

(defun suit? (rule token query)
  (if (match-exp? (car rule) token)
      ; Then following (third rule), tokenizering (cdr query)
      (let ((nexts (list token))
	    (failed? nil))
	(if (second rule)
	    (progn
	      (with-following-rules x (second rule) (cdr query)
		(if x (setq nexts (concatenate 'list nexts (list x)))
		    (setq failed? t)))
	      (if (eq nexts (list token))
		  nil
		  (if failed?
		      '@
		       nexts)))
	    (let ((cn (car rule)))
	      (if (funcall (gethash cn *chars*) token)
		  (progn
		    (list token)) nil))))
      nil))

(defmacro parse (query a)
  `(progn
     (setq *pointer* 0)
     (inference ,query *exp* ,a)))

(defun inference (query exp next? &optional (puterr? T) (cq T))
  (let ((paths nil))
    (labels ((next () (let ((r (funcall (if paths (pop paths) (failed puterr?)))))
			(if r (if (eq r '@) (if puterr?
						(next)
						nil) (values r iter)) (next))))
	     (forward () (let ((tree (next)))
			   (unless (or (eq tree '@) (eq tree nil))
	                     (progn
			       (setq *pointer* (+ *pointer* (length tree)))
			       (if cq
				   (dotimes (_ *pointer*)
				     (pop query)))
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
