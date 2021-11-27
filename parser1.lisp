
(defpackage :parser (:use :cl))

(in-package :parser)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defstruct parser-info
  terms
  start-symbol
  grammers)

(defmacro define-exp ())

(defun parse-args (forms)
  (let ((options '()) (grammers '()))
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
	 (push (cdr form) grammers)
	 (push (car form) grammers))))
    (values options grammers)))

(defun inference ()
  ; start-symbolからTreeを辿っていく
  )

(defmacro define-parser (name &body forms)
  (with-gensyms (opt grammers)
    `(multiple-value-bind (,opt ,grammers) (parse-args (quote ,forms))
       (defparameter ,name
	 (apply #'make-parser-info (concatenate 'list ,opt (list :grammers ,grammers)))))))

(define-parser *my-parser*
  (:terms :number
          :symbol)
  (:start-symbol program)
  (function
   (:symbol :number :number #'(lambda (x y z)
				(list y x z)))))

