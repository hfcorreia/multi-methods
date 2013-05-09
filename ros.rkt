#lang racket
(require racket/provide)

;;; Representation of a generic function
(struct generic-function (parameters methods before after around)
  #:mutable
  #:property prop:procedure 
  (lambda vars
    (let* ((instance (car vars))
           (args (cdr vars))
           (around-methods (find-methods (generic-function-around instance) args))
           (before-methods  (find-methods (generic-function-before instance) args))
           (primary-methods (find-methods (generic-function-methods instance) args))
           (after-methods (reverse (find-methods (generic-function-after instance) args))))                 
      (if (null? primary-methods) 
          (error "Method missing for arguments" args)
          (eval-generic-function around-methods before-methods primary-methods after-methods args)))))

;;; Main eval of a generic method
;;; Evaluates if exits around, before after and primary methods
;;; This is done in a CLOS simular way
(define (eval-generic-function around before primaries after args)
  (let ((result '()))
    (apply-methods around args)
    (map (lambda (method) (apply (method-body method) args)) before)
    (set! result (apply-methods primaries args))
    (map (lambda (method) (apply (method-body method) args)) after)
    (set! next-methods '())
    result))

;;; Evaluates methods that can call more applicable methods
(define (apply-methods methods args)
  (if (not (null? methods))
      (begin (set! next-methods (cdr methods))
             (apply (method-body (car methods)) args))
      #f))

;;; Tests if there exists a new applicable method
(define (next-method?) (not (null? next-methods)))

;;; Calls the next applicable method available
(define (call-next-method ...)
  (if (null? next-methods)
      (begin
        (set! next-methods '())
        (error "No-next-method with" (list ...)))
      (let ((method (car next-methods)))
        (if (can-apply? (method-parameters method) (list ...))
            (begin 
              (set! next-methods (cdr next-methods))
              (apply (method-body method) (list ...)))
            (begin
              (set! next-methods '())
              (error "Call-next-method: cannot apply " (list ...)))))))

;;; Stores the next-method to call for call-next-method
(define next-methods '())

;;; Representation of a method
;;; Has the list of parameters of the method and a lambda of the body.
(struct method (parameters body) #:mutable)

;;; Representation of a type
;;; Has the corresponding predicate and a list of more specific predicates.
(struct type (predicate [supertypes #:mutable]))

;;; Defines the global type table
(define predicate-hash (make-hash)) ;
(define T-type (type 'T? '())) ; Super Type of all types
(hash-set! predicate-hash 'T?  T-type)

;;; Defines a new generic function
(define-syntax-rule
  (defgeneric name parameters)
  (define name (generic-function 'parameters '() '() '() '())))

;;; Defines a new generic method
(define-syntax-rule
  (defmethod method-name ( (args predicates) ... ) instr ...)
  (make-gen-method method-name (list predicates ...) (lambda (args ...) (begin instr ...)) 
                   (generic-function-methods method-name) set-generic-function-methods!))

;;; Defines a new before generic method
(define-syntax-rule
  (defbefore method-name ( (args predicates) ...) instr ...)
  (make-gen-method method-name (list predicates ...) (lambda (args ...) (begin instr ...))
                   (generic-function-before method-name) set-generic-function-before!))

;;; Defines a new before generic method
(define-syntax-rule
  (defafter method-name ( (args predicates) ...) instr ...)
  (make-gen-method method-name (list predicates ...) (lambda (args ...) (begin instr ...)) 
                   (generic-function-after method-name) set-generic-function-after!))

;;; Defines a new around generic method
(define-syntax-rule
  (defaround method-name ( (args predicates) ...) instr ...)
  (make-gen-method method-name (list predicates ...) (lambda (args ...) (begin instr ...))
                   (generic-function-around method-name) set-generic-function-around!))

;;; Defines subtype relations
(define-syntax-rule
  (defsubtype predicate1 predicate2)
  (add-subtype predicate1 predicate2))

;;; Returns all predicates args for a given method
(define (method-types method)
  (map (lambda (x)  x) (method-parameters method)))

;;; Add a new method to the generic-funtion methods list
;;; In case of redefinition it overrides the existing method's lambda.
(define (make-gen-method method-name parameters body methods setter)
  (if (equal? (length (generic-function-parameters method-name)) (length parameters))
      (let ((methods-parameters (map (lambda (x) (method-parameters x)) methods)))
        (if (not (false? (member parameters methods-parameters)))
            (let ((method (car (filter (lambda(x) (equal? parameters (method-parameters x))) methods))))
              (set-method-body! method body))
            (setter method-name (append methods (list (method parameters body))))))
      (error "Method missing for arguments" parameters)))

;;; Returns all orignal args for a given parameters list
(define (method-args parameters)
  (map (lambda (x) (car x)) parameters))

;;: Tests if every parameter predicate is applicable to the given arguments
(define (can-apply? parameters  args)
  (cond ((not (equal? (length parameters) (length args))) #f)
        ((or (null? parameters) (null? args)) #t)
        ((not (try-apply (car parameters) (list (car args)))) #f)
        (else (can-apply? (cdr parameters) (cdr args)))))

;;; Encapsulates ocuring exceptions when check for applicable methods
(define (try-apply predicate arg)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (apply predicate arg)))

;;; Returns all aplicable methods orderd by its specificity.
;;; From the most specific to the least one.
(define (find-methods methods args)
  (sort (applicable-methods methods args) more-specific-method?))

;;; Returns all applicable methods
(define (applicable-methods methods args)
  (filter (lambda (method) (can-apply? (method-parameters method) args)) methods))

;;; Adds a new subtype to the hash and updates their supertypes.
(define (add-subtype subtype supertype)
  (let ((found-supertype? (find-type supertype))
        (found-subtype? (find-type subtype)))
    (cond 
      ;; subtype and supertype allready exists
      ((and (not (false? found-subtype?)) (not (false? found-supertype?)))
       (add-supertype found-subtype? found-supertype?))
      ;; subtype exists but super does not
      ((and (not (false? found-subtype?)) (false? found-supertype?))
       (let ((super (type supertype (list T-type))))
         ((add-supertype found-subtype? super)
          (hash-set! predicate-hash supertype super))))
      ;; subtype doesn't exist but super does
      ((and (false? found-subtype?) (not (false? found-supertype?))) 
       (hash-set! predicate-hash subtype (type subtype (list found-supertype?))))
      ;; both super and sub types don't exist
      (else (let* ((super (type supertype (list T-type)))             
                   (sub (type subtype (list super))))
              (hash-set! predicate-hash subtype sub) 
              (hash-set! predicate-hash supertype super))))))

;;; Addes a new super type to a given type.
(define (add-supertype type supertype)
  (set-type-supertypes! type (append (type-supertypes type) (list supertype))))

;;; Returns a type or #f otherwise
(define (find-type predicate)
  (if (hash-has-key? predicate-hash predicate) (hash-ref predicate-hash predicate) #f))

;;; Checks if method1 is more specific than method2
(define (more-specific-method? method1 method2)
  (define (more-specific-predicates? method-predicates1 method-predicates2)
    (let ((p1 (hash-ref predicate-hash (car method-predicates1))) 
          (p2 (hash-ref predicate-hash (car method-predicates2))))
      (cond ((or (null? method-predicates1) (null? method-predicates2)) #f)
            ((not (equal? p1 p2)) (more-specific-predicate? (type-supertypes p1) p2))
            (else (more-specific-predicates? (cdr method-predicates1) (cdr method-predicates2))))))
  (more-specific-predicates? (method-parameters method1) (method-parameters method2)))

;;; Checks if their type2 belongs in any of the supertypes of type1
(define (more-specific-predicate? supertypes1 type2)
  (or (not (false? (member type2 supertypes1)))
      (ormap (lambda (super) (more-specific-predicate? (type-supertypes super) type2)) supertypes1)))

;;; Exported functions
(provide defgeneric (struct-out generic-function) defmethod 
         defbefore defafter defaround
         defsubtype method-types 
         call-next-method next-method?)