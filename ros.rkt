#lang racket
;;; Representation of a generic function
(struct generic-function (parameters methods)
  #:mutable
  #:property prop:procedure   (lambda vars
                                (let* ((instance (car vars))
                                       (args (cdr vars))
                                       (temp-method (car (generic-function-methods instance))))
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
  (defmethod method-name ( (name . predicate) ... ) body)
  (make-gen-method method-name '( (name . predicate) ... ) 'body))

(define (make-gen-method name parameteres body)
  (set-generic-function-methods! name (update-gen-method name parameteres body)))

(define (update-gen-method name parameters body)
  (append (generic-function-methods name) (list (make-method parameters body))))

(define (make-method parameters body)
  (let ((args (make-args-list parameters)))
    (method parameters `(lambda ,args ,body))))

(define (make-args-list parameters)
  (if (null? parameters)
      '()
      (cons (caar parameters) (make-args-list (cdr parameters)))))

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