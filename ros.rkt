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

;;; Representation of a type
(struct type (predicate [subtypes #:mutable]))
(define type-tree (type 'T? '()))
;;;; Main Syntax Rules
;;; Defines a new generic function
(define-syntax-rule 
  (defgeneric name parameters)
  (define name (generic-function 'parameters '() )))

;;; Defines a new generic method
(define-syntax-rule
  (defmethod method-name ( (arg . predicate) ... ) body)
  (make-gen-method method-name '( (arg . predicate) ... ) 'body))

;;; Defines subtype relations
(define-syntax-rule
  (defsubtype predicate1 predicate2)
  (add-subtype predicate1 predicate2))

;;;; Aux Functions
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

(define (add-subtype predicate1 predicate2)
  (let ((found-type  (find-type predicate2)))
    (if (false? found-type)
        (set-type-subtypes! type-tree (append (type-subtypes type-tree) (list (type predicate2 (list (type predicate1 '()))))))
        (set-type-subtypes! found-type (append (type-subtypes found-type) (list (type predicate1 '())))))))

(define (find-type predicate)
  (define (find-type-aux predicate types)
    (map (lambda (x) (displayln (type-predicate x))) types)
    (let ((found-predicate #f))
      (cond ((null? types) #f)
            ((equal? predicate (type-predicate (car types))) (car types))
            (else
             (begin 
               (set! found-predicate (find-type-aux predicate (type-subtypes (car types))))
               (if (not (false? found-predicate))
                   found-predicate
                   (find-type-aux predicate (type-subtypes (cdr types)))))))))
  (find-type-aux predicate (type-subtypes type-tree)))

(define (print-tree)
  (define (print-tree-aux types lvl)
    (if (not (null? types))
        (begin 
          (print-tree-aux (type-subtypes (car types)) (+ lvl 1))
          (displayln (cons (type-predicate (car types)) lvl))
          (print-tree-aux (cdr types)  (+ lvl 1)))
        #f))
  (print-tree-aux (type-subtypes type-tree) 0))

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

;;; Test subtypes
(defsubtype complex? number?)
(defsubtype real? complex?)
(defsubtype rational? real?)
(defsubtype integer? rational?)
(defsubtype zero? integer?)