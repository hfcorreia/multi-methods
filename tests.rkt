#lang racket
(require "ros.rkt")

;;;; Factorial
(defgeneric fact (n))

(defmethod fact ((n zero?))  0)

(defmethod fact ((n integer?))
  (* n (fact (- n 1))))

;;; Sould substitute the previously defined method
(defmethod fact ((n zero?))  1)

(define (run-tests-fact)
 (displayln (if (equal? (fact 1) 1) "passed" "failed")))

;;;; Add
(defgeneric add (x y))

(defmethod add ((x number?) (y number?))
  (+ x y)
  (call-next-method x y))

(defmethod add ((x integer?) (y number?))
  (+ x y 10)) ; this is more specific

(defaround add ((x number?) (y number?))
  (displayln "Going around and doing nothing")
  (if (next-method?)
      (call-next-method x y)
      (displayln "There is no next method")))

(defaround add ((x integer?) (y number?))
  (displayln "Just going to call the next method")
  (call-next-method x y))

(define (run-tests-add)
 (displayln (if (equal? (add 1 1) 12) "passed" "failed"))
 (displayln (if (equal? (add 1.2 1) 2.2) "passed" "failed")))