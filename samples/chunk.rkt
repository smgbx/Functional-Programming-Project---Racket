#lang racket

(define (chunk-into n xs)
  (define N (length xs))
  (cond [(= 1 n) (list xs)]
        [(> n N) 
         (cons empty 
               (chunk-into (sub1 n) xs))]
        [else
         (define m (ceiling (/ N n)))
         (cons (take xs m) 
               (chunk-into (sub1 n) (drop xs m)))]))


(module+ test
  (require rackunit)
  (check-equal? (length (chunk-into 4 (range 5))) 4)
  (check-equal? (length (chunk-into 2 (range 5))) 2))
