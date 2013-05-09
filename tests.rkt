#lang racket
(require "ros.rkt")
;;;; Definition of subtypes
(defsubtype integer? number?)
(defsubtype zero? integer?)

;;;; Tests
                