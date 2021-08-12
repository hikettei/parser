
(in-package :parser)

(define-rule args vars nums)

(define-char vars #'(lambda (x) (symbolp x)))
(define-char nums #'(lambda (n) (typep n 'number)))

(define-syntax vars vars nums)
(define-syntax vars nums nums)
(define-syntax vars vars vars)
