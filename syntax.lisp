
(in-package :parser)

(define-rule args vars nums)

(define-char *exp* vars #'(lambda (x) (typep x 'symbol)))
(define-char *exp* nums #'(lambda (n) (typep n 'number)))
(define-char *exp* setq #'(lambda (n) (eq n 'setq)))

(define-syntax *exp* funcall  vars nums nums)
(define-syntax *exp* funcall  vars funcall funcall)
(define-syntax *exp* set-query vars vars setq @)
(define-syntax *exp* set-query vars vars setq funcall)

