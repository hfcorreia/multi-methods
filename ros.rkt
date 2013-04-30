#lang racket
(require racket/trace) ; Enables trace!

;;; Representation of a generic function
(struct generic-function (parameters methods)
  #:mutable
  #:property prop:procedure (lambda vars
                              (let* ((instance (car vars))
                                     (args (cdr vars))
                                     (temp-method (car (find-methods (generic-function-methods instance) args))))
                                (if (equal? (length (generic-function-parameters instance)) (length args))
                                    (eval `(,(method-body temp-method) ,@args))
                                    (error "Invalid number of args")))))
;;; Representation of a method
(struct method (parameters body))

;;; Representation of a type
(struct type (predicate [supertype #:mutable]))
;;; Global predicate-hash
(define king-type (type 'T? '()))
(define predicates-hash (make-hash))
(hash-set! predicates-hash 'T?  king-type)

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
          ((equal? parameters (method-parameters (car methods))) (append result (list (make-method parameters body)) (cdr methods)))
          (else (update-gen-method-aux (cdr methods) parameters body (append result (list (car methods)))))))
  (update-gen-method-aux (generic-function-methods name) parameters body '()))

(define (make-method parameters body)
  (let ((args (make-args-list parameters)))
    (method parameters `(lambda ,args ,body))))

(define (make-args-list parameters)
  (if (null? parameters)
      '()
      (cons (caar parameters) (make-args-list (cdr parameters)))))
;; can-apply is true if the number of arguments is equal to the number of parameters
;; and every parameter predicate is true when applied to the given argument
(define (can-apply? parameters  args)
  (define (can-apply-aux parameters args)
    (cond ((or (null? parameters) (null? args)) #t)
          ((not ((apply (eval (cadar parameters)) (list (car args)))) #f)
          (else (can-apply-aux (cdr parameters) (cdr args)))))
  (if (not(equal? (length parameters) (length args)))
      #f
      (can-apply-aux parameters args)))

(define (find-methods methods args)
  (displayln "BEFORE SORT")
  (map (lambda (x) (displayln (method-body x))) (applicable-methods methods args))
  (displayln "AFTER SORT")
  (map (lambda (x) (displayln (method-body x))) (sort (applicable-methods methods args) more-specific-method?))
  (sort (applicable-methods methods args) more-specific-method?))

(define (applicable-methods methods args)
  (define (applicable-methods-aux methods args result)
    (cond ((null? methods) result)
          ((can-apply? (method-parameters (car methods)) args) (applicable-methods-aux (cdr methods) args (append result (list (car methods)))))
          (else (applicable-methods-aux (cdr methods) args result))))
  (applicable-methods-aux methods args '()))

(define (add-subtype subtype supertype)
  (let ((found-supertype (find-predicate supertype))
        (found-subtype (find-predicate subtype)))
    (cond ((and (not (false? found-subtype)) (not (false? found-supertype))); subtype and supertype allready exists
           (set-type-supertype! found-subtype found-supertype))
          ((and (not (false? found-subtype)) (false? found-supertype)) ; subtype exists but super doesn'tI
           (set-type-supertype! found-subtype king-type)
           (hash-set! predicates-hash supertype king-type))
          ((and (false? found-subtype) (not (false? found-supertype))) ; subtype doesn't exist but super does
           (hash-set! predicates-hash subtype (type subtype found-supertype)))
          (else (let* ((super (type supertype king-type))
                       (sub (type subtype super)))
                  (hash-set! predicates-hash subtype sub) ;both types don't exist
                  (hash-set! predicates-hash supertype super))))))

(define (find-predicate predicate)
  (if (hash-has-key? predicates-hash predicate)
      (hash-ref predicates-hash predicate)
      #f))

(define (more-specific-method? method1 method2)
  (more-specific-predicates? (method-parameters method1) (method-parameters method2)))


(define (more-specific-predicates? method-predicates1 method-predicates2)
  (display "more-specific-predicates? - ")
  (displayln (list method-predicates1 method-predicates2))
  (if (or (null? method-predicates1) (null? method-predicates2))
      #f
      (let ((p1 (hash-ref predicates-hash (eval (cadar method-predicates1)))) (p2 (hash-ref predicates-hash (eval (cadar method-predicates2)))))
      (or (if (equal? p1 p2)
              (more-specific-predicates? (cdr method-predicates1) (cdr method-predicates2))
              (more-specific-predicate? (hash-ref predicates-hash (eval (cadar method-predicates1)))
                                    (hash-ref predicates-hash (eval (cadar method-predicates2)))))
          (begin (displayln (more-specific-predicates? (cdr method-predicates1) (cdr method-predicates2)))
                 (more-specific-predicates? (cdr method-predicates1) (cdr method-predicates2)))))))

(define (more-specific-predicate? predicate1 predicate2)
  (display "more-specific-predicate? - ")
  (displayln (list (type-predicate predicate1) (type-predicate predicate2)))
  (cond ((equal? predicate1 king-type) (begin (displayln "FALSEEEE") #f))
        ((equal? predicate2 king-type) #t)
        ((equal? predicate1 predicate2) #t)
        (else (more-specific-predicate? (type-supertype predicate1) predicate2))))

;;;; Test Examples
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

(defmethod add ((x zero?) (y integer?))
  (+ (* 10 y) 1))

(defmethod add ((x integer?) (y zero?))
  (+ (* 20 x) 2))

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
(define (test-find-methods) (car (find-methods (generic-function-methods add) '(1 1))))

(define m1 (car (generic-function-methods add))) ; number
(define m2 (cadr (generic-function-methods add))) ; integer
(define m3 (caddr (generic-function-methods add))) ; zero

(define (dump-hierarquie father result)
  (if (null? father)
      result
      (dump-hierarquie (type-supertype father) (append result (list (type-predicate father))))))



(define (test-more-specific)
  (displayln (method-parameters m1))
  (displayln (method-parameters m2))
  (displayln (method-parameters m3))
  (displayln (more-specific-method? m1 m2))
  (displayln (more-specific-method? m2 m3))
  (displayln (more-specific-method? m1 m3)))

;;; Test subtypes
(defsubtype complex? number?)
(defsubtype real? complex?)
(defsubtype rational? real?)
(defsubtype integer? rational?)
(defsubtype positive? rational?)
(defsubtype zero? integer?)
(defsubtype string? zero?)

(dump-hierarquie (hash-ref predicates-hash zero?) '())

(define (p-m) 
  (map (lambda (x) (displayln (method-body x))) (generic-function-methods fact)))