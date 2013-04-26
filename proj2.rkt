#lang racket
(struct cena (field1 field2))

(struct estrutura ([f1 #:mutable] [f2 #:mutable])) 
;ou
(struct estrutura2 (f1 f2) #:mutable)
 
;(estrutura-f1 z)
;(set-estrutura-f1! z 10)
  
(struct fish (weight color) #:mutable
          #:property prop:procedure (lambda (f n) (let ([w (fish-weight f)])
                                                    (set-fish-weight! f (+ n w)))))
  
(define-syntax-rule
  (swap x y)
  (let ([temp x])
    (set! x y)
    (set! y tmp)))


;deixar ordenacao de escolha do metodo para depois

(define-syntax-rule
  (defgeneric function (args))
  (let ()
    ))