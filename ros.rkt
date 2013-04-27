#lang racket
;;; Representation of a generic function
(struct generic-function (parameters methods)
  #:mutable
  #:property prop:procedure (lambda vars
                              (let* ((instance (car vars))
                                     (args (cdr vars))
                                     (temp-method (find-method (generic-function-methods instance) args)))
                                (if (equal? (length (generic-function-parameters instance)) (length args))
                                    (eval `(,(method-body temp-method) ,@args))
                                    (error "Invalid number of args")))))

;;; Representation of a method
(struct method (parameters body))

;;;; Main Syntax Rules
;;; Defines a new generic function
(define-syntax-rule 
  (defgeneric name parameters)
  (define name (generic-function 'parameters '() )))

;;; Defines a new generic method
(define-syntax-rule
  (defmethod method-name ( (arg . predicate) ... ) body)
  (make-gen-method method-name '( (arg . predicate) ... ) 'body))

(define (make-gen-method method-name parameteres body)
  (set-generic-function-methods! method-name (update-gen-method method-name parameteres body)))

(define (update-gen-method name parameters body)
  (append (generic-function-methods name) (list (make-method parameters body))))

(define (make-method parameters body)
  (let ((args (make-args-list parameters)))
    (method parameters `(lambda ,args ,body))))

(define (make-args-list parameters)
  (if (null? parameters)
      '()
      (cons (caar parameters) (make-args-list (cdr parameters)))))

(define (can-apply? parameters  args)
  (define (can-apply-aux parameters args)
    (cond ((or (null? parameters) (null? args)) #t)
          ((not (apply (eval (cadar parameters)) (list (car args)))) #f) ;look?!?
          (else (can-apply-aux (cdr parameters) (cdr args)))))
  (if (not(equal? (length parameters) (length args)))
      #f
      (can-apply-aux parameters args)))

(define (find-method methods args)
  (cond ((null? methods) (error "Method missing for arguments" args))
        ((can-apply? (method-parameters (car methods)) args) (car methods))
        (else (find-method (cdr methods) args))))

;;;; Test Examples
;;; Factorial example
(defgeneric fact (n))

(defmethod fact ((n zero?))
  1)

(defmethod fact ((n integer?))
  (* n (fact (- n 1))))

;;; Add example
(defgeneric add (x y))

(defmethod add ((x number?) (y number?))
  (+ x y))

(defmethod add ((x string?) (y string?))
  (string-append x y))

;;; Test
(define (test-can-apply) (can-apply? '((x number?) (y number?)) '("12" 2)))
(define (test-find-method) (find-method (generic-function-methods add) '(1 1)))