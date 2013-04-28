#lang racket
(require racket/trace) ; Enables trace!

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
  (define (update-gen-method-aux methods parameters body result)
    (cond ((null? methods) (append result (list (make-method parameters body))))
          ((equal? parameters (method-parameters (car methods))) (update-gen-method-aux (cdr methods) parameters body result))
          (else (update-gen-method-aux (cdr methods) parameters body (append result (list (car methods)))))))
  (update-gen-method-aux (generic-function-methods name) parameters body '()))

(define (make-method parameters body)
  (let ((args (make-args-list parameters)))
    (method parameters `(lambda ,args ,body))))

(define (make-args-list parameters)
  (if (null? parameters)
      '()
      (cons (caar parameters) (make-args-list (cdr parameters)))))

(define (method-types method)
  (define (method-types-aux parameters)
    (if (null? parameters)
        '()
        (cons (eval (cadar parameters)) (method-types-aux (cdr parameters)))))
  (method-types-aux (method-parameters method)))


(define (can-apply? parameters  args)
  (define (can-apply-aux parameters args)
    (cond ((or (null? parameters) (null? args)) #t)
          ((not (apply (eval (cadar parameters)) (list (car args)))) #f) ;look?!?
          (else (can-apply-aux (cdr parameters) (cdr args)))))
  (if (not(equal? (length parameters) (length args)))
      #f
      (can-apply-aux parameters args)))

(define (find-method methods args)
  (define (find-method-aux methods args)
    (cond ((null? methods) (error "Method missing for arguments" args))
          ((can-apply? (method-parameters (car methods)) args) (car methods))
          (else (find-method (cdr methods) args))))
  (find-method-aux (sort methods more-specific-method?) args)) 

(define (add-subtype predicate1 predicate2)
  (let ((found-type  (find-type predicate2)))
    (if (false? found-type)
        (set-type-subtypes! type-tree (append (type-subtypes type-tree) (list (type predicate2 (list (type predicate1 '()))))))
        (set-type-subtypes! found-type (append (type-subtypes found-type) (list (type predicate1 '())))))))

(define (find-type predicate)
  (define (find-type-aux predicate types)
    (cond ((null? types) #f)
          ((equal? predicate (type-predicate (car types))) (car types))
          (else (let ((found-predicate (find-type-aux predicate (type-subtypes (car types)))))
                  (if (not (false? found-predicate))
                      found-predicate
                      (find-type-aux predicate (type-subtypes (cdr types))))))))
  (find-type-aux predicate (type-subtypes type-tree)))


(define (find-type-level predicate)
  (define (find-type-level-aux types lvl)
    (cond ((null? types) #f)
          ((equal? predicate (type-predicate (car types))) lvl)
          (else (let ((result (find-type-level-aux (type-subtypes (car types)) (add1 lvl))))
                  (if (not (false? result))
                      result
                      (find-type-level-aux (cdr types) lvl))))))
  (find-type-level-aux (type-subtypes type-tree) 1))

(define (more-specific-method? method1 method2)
  (more-specific-predicates? (method-parameters method1) (method-parameters method2)))

(define (more-specific-predicate? predicate1 predicate2)
  (> (find-type-level predicate1) (find-type-level predicate2)))

(define (more-specific-predicates? method-predicates1 method-predicates2)
  (if (or (null? method-predicates1) (null? method-predicates2))
      #f
      (or (more-specific-predicate? (eval (cadar method-predicates1)) (eval (cadar method-predicates2))) 
          (more-specific-predicates? (cdr method-predicates1) (cdr method-predicates2)))))

;;;; Test Examples
;;; Aux test functions
(define (print-tree)
  (define (print-tree-aux types lvl)
    (if (not (null? types))
        (begin 
          (print-tree-aux (type-subtypes (car types)) (+ lvl 1))
          (displayln (cons (type-predicate (car types)) lvl))
          (print-tree-aux (cdr types)  (+ lvl 1)))
        #f))
  (print-tree-aux (type-subtypes type-tree) 1))

;;; Factorial example
(defgeneric fact (n))

(defmethod fact ((n zero?))
  0)

(defmethod fact ((n integer?))
  (* n (fact (- n 1))))

(defmethod fact ((n zero?))
  1)

;;; Add example
(defgeneric add (x y))

(defmethod add ((x number?) (y number?))
  (+ x y))

(defmethod add ((x integer?) (y integer?))
  (+ x y 10000000))

(defmethod add ((x string?) (y string?))
  (string-append x y))

;;; what are you test
(defgeneric what-are-you? (x))

(defmethod what-are-you? ((x integer?))
  "an integer")

(defmethod what-are-you? ((x positive?))
  "an integer")

;;; Test
(define (test-can-apply) (can-apply? '((x number?) (y number?)) '("12" 2)))
(define (test-find-method) (find-method (generic-function-methods add) '(1 1)))

;;; Test subtypes
(defsubtype complex? number?)
(defsubtype real? complex?)
(defsubtype rational? real?)
(defsubtype integer? rational?)
(defsubtype positive? rational?)
(defsubtype zero? integer?)
(defsubtype string? zero?)