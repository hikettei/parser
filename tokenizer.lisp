
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

(define-rule args vars nums)

(define-char vars #'(lambda (x) (symbolp x)))
(define-char nums #'(lambda (n) (typep n 'number)))

(define-syntax vars nums nums)

(defun failed () #'(lambda () (print "Detected Undefined Syntax.")))


(defmacro with-following-rules (var rules query &body body)
  `(dotimes (i (length ,rules))
     (let ((r (nth i ,rules)))
     (let ((s (funcall (gethash r *rules*) (car ,query))))
       (pop ,query) ; 多分まずい
       (let ((,var s)) ,@body)))))
     
(defun suit? (rule token query)
  (if (funcall (second rule) token)
  ; Then following (third rule), tokenizering (cdr query)
      (let ((nexts (list token))
	    (failed? nil))
	(with-following-rules x (third rule) (cdr query)
	  (setq nexts (concatenate 'list nexts (list x)))
	  (unless x (setq failed? t)))
	(if (eq nexts (list token))
	    nil
	    (if failed? nil nexts)))
      nil))

(defmacro tokenizer (query)
  `(inference ,query *exp*))

(defun inference (query exp)
  (let ((tkn (car query))
	(paths nil))
    (labels ((next () (let ((r (funcall (if paths (pop paths) (failed)))))
			(if r r (next)))))
      (mapcar #'(lambda (e) (push #'(lambda () (suit? e tkn query)) paths)) exp)
      (next))))

 ; (tokenizer `(+ a b c)) = (+ A B)
