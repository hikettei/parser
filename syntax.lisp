
(in-package :parser)

(define-rule args vars nums)

(define-char *exp* vars #'(lambda (x) (typep x 'symbol)))
(define-char *exp* nums #'(lambda (n) (typep n 'number)))
(define-char *exp* setq #'(lambda (n) (eq n 'setq)))

(define-syntax *exp* nest     vars funcall funcall)
(define-syntax *exp* addnum   nums vars nums)
(define-syntax *exp* nest1 vars nest nest)
(define-syntax *exp* set-query vars vars setq nums)
;(define-syntax *exp* SyntaxName CHAR SYNTAX SYNTAX...) <- (仮)
;(define-syntax *exp* callfunc1 vars nest)
; (tokenizer '(+ 1 + 1 1)) = (+ 1 (+ 1 1))
