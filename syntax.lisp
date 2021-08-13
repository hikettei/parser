
(in-package :parser)

(define-rule args vars nums)

(define-char vars #'(lambda (x) (symbolp x)))
(define-char nums #'(lambda (n) (typep n 'number)))

(define-syntax vars) ; 
(define-syntax nums) ; 

(define-syntax vars nums nums)

; (tokenizer '(+ 1 + 1 1)) = (+ 1 (+ 1 1))
