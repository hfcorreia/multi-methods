#lang racket

;;; Representation of a generic function
(struct generic-function (parameters methods)
  #:mutable
  #:property prop:procedure (lambda (instance parameters) (print "Not done")))


;;;; Main Syntax Rules

;;; Defines a new generic function
(define-syntax-rule 
  (defgeneric name parameters)
  (define name (generic-function 'parameters '() )))

;;; Defines a new generic method
(define-syntax-rule
  (defmethod method-name ( (name predicate) ... ) body)
  (make-gen-method method-name '( (name predicate) ... ) 'body))

(define (make-gen-method name parameteres body)
  (set-generic-function-methods! name (update-gen-method name parameteres body)))

(define (update-gen-method name parameters body)
  (append (generic-function-methods name) (list (lambda parameters body))))
;;;; Test Examples

;;; Factorial example
(defgeneric fact (n))

(defmethod fact ((n zero?))
  0)

(defmethod fact ((n integer?))
  (* n (fact (- n 1))))