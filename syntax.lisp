
(in-package :parser)

(define-rule args vars nums)

(define-char vars #'(lambda (x) (typep x 'symbol)))
(define-char nums #'(lambda (n) (typep n 'number)))

(define-syntax *exp* var-name vars) ; 
(define-syntax *exp* numbers   nums) ; 

(define-syntax *exp* nest     vars funcall funcall)

(define-syntax *exp* funcall  vars numbers numbers)
(define-syntax *exp* addnum   nums var-name numbers)
; (tokenizer '(+ 1 + 1 1)) = (+ 1 (+ 1 1))
