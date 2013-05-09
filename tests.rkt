#lang racket
(require "ros.rkt")
;;;; Definition of subtypes
(defsubtype integer? number?)
(defsubtype zero? integer?)

;;;; Tests

(defgeneric fact (x))

(defmethod fact ((x integer?))
  (call-next-method x)
  (* x (fact (- x 1))))

(defmethod fact ((x number?))
  (displayln x))

(defmethod fact ((x zero?))
  1)
      