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
(struct type (predicate [supertypes #:mutable]))
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
  (defmethod method-name ( (arg . predicate) ... ) body ...)
  (make-gen-method method-name '( (arg . predicate) ... ) '(begin body ...)))

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
  (let ((args (map (lambda (x) (car x)) parameters)))
    (method parameters `(lambda ,args ,body))))

;; can-apply is true if the number of arguments is equal to the number of parameters
;; and every parameter predicate is true when applied to the given argument
(define (can-apply? parameters  args)
  (cond ((not (equal? (length parameters) (length args))) #f)
        ((or (null? parameters) (null? args)) #t)
        ((not (apply (eval (cadar parameters)) (list (car args)))) #f)
        (else (can-apply? (cdr parameters) (cdr args)))))

(define (find-method methods args)
  (car (sort (applicable-methods methods args) more-specific-method?)))

(define (applicable-methods methods args)
  (filter (lambda (method) (can-apply? (method-parameters method) args)) methods))

(define (add-subtype subtype supertype)
  (let ((found-supertype (find-predicate supertype))
        (found-subtype (find-predicate subtype)))
    (cond 
      ;; subtype and supertype allready exists
      ((and (not (false? found-subtype)) (not (false? found-supertype)))
       (add-supertype found-subtype found-supertype))
      ;; subtype exists but super does not
      ((and (not (false? found-subtype)) (false? found-supertype))
       (let ((super (type supertype (list king-type))))
         ((add-supertype found-subtype super)
          (hash-set! predicates-hash supertype super))))
      ;; subtype doesn't exist but super does
      ((and (false? found-subtype) (not (false? found-supertype))) 
       (hash-set! predicates-hash subtype (type subtype (list found-supertype))))
      ;; both super and sub types don't exist
      (else (let* ((super (type supertype (list king-type)))             
                   (sub (type subtype (list super))))
              (hash-set! predicates-hash subtype sub) 
              (hash-set! predicates-hash supertype super))))))

(define (add-supertype type supertype)
  (set-type-supertypes! type (append (type-supertypes type) (list supertype))))

(define (find-predicate predicate)
  (if (hash-has-key? predicates-hash predicate)
      (hash-ref predicates-hash predicate)
      #f))

(define (more-specific-method? method1 method2)
  (more-specific-predicates? (method-parameters method1) (method-parameters method2)))

(define (more-specific-predicates? method-predicates1 method-predicates2)
  (let ((p1 (hash-ref predicates-hash (eval (cadar method-predicates1)))) 
        (p2 (hash-ref predicates-hash (eval (cadar method-predicates2)))))
    (cond ((or (null? method-predicates1) (null? method-predicates2)) #f)
          ((not (equal? p1 p2)) (more-specific-predicate? (type-supertypes p1) p2))
          (else (more-specific-predicates? (cdr method-predicates1) (cdr method-predicates2))))))

(define (more-specific-predicate? supertypes1 type2)
  (or (not (false? (member type2 supertypes1)))
      (ormap (lambda (super) (more-specific-predicate? (type-supertypes super) type2)) supertypes1)))


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
  (displayln "number? number?")
  (+ x y))

(defmethod add ((x zero?) (y integer?))
  (displayln "zero? integer?")
  (+ x y))

(defmethod add ((x integer?) (y zero?))
  (displayln "integer? zero?")
  (+ x y))

(defmethod add ((x integer?) (y integer?))
  (displayln "integer? integer?")
  (+ x y))

(defmethod add ((x string?) (y string?))
  (displayln "string? string?")
  (string-append x y))

(defmethod add ((x even?) (y even?))
  (displayln "even? even?")
  (+ x y))

;;; what are you test
(defgeneric what-are-you? (x))

(defmethod what-are-you? ((x integer?))
  "an integer")

(defmethod what-are-you? ((x positive?))
  "an integer")

;;; Test
(define (test-can-apply) (can-apply? '((x number?) (y number?)) '("12" 2)))
(define (test-find-methods) (method-body (find-method (generic-function-methods add) '(1 1))))

(define m1 (car (generic-function-methods add))) ; number
(define m2 (cadr (generic-function-methods add))) ; integer
(define m3 (caddr (generic-function-methods add))) ; zero

(define (dump-hierarquie father result)
  (if (null? father)
      result
      (dump-hierarquie (type-supertypes father) (append result (list (type-predicate father))))))

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
(defsubtype string? 'T?)

(defsubtype odd? integer?)
(defsubtype even? integer?)
(defsubtype zero? even?)

;(dump-hierarquie (hash-ref predicates-hash zero?) '())

(define (p-m) 
  (map (lambda (x) (displayln (method-body x))) (generic-function-methods fact)))