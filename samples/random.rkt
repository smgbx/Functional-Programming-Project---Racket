#!/usr/bin/env racket
#lang racket

(define (worker pch) 
  (define my-id (place-channel-get pch)) ; get worker id
  (define wch-w (place-channel-get pch)) ; get work channel (shared between controller and all workers) - worker side
  (define f     (place-channel-get pch)) ; get function
  (define ns    (make-base-namespace))   ; for eval
  (let loop ()
    (define n (place-channel-get wch-w)) ; get work order
    (let ((res (eval `(,f ,n) ns)))      ; need to use eval here !!
      (eprintf "~a says ~a\n" my-id res)
      (place-channel-put wch-w  res)     ; put response
      (loop))))                          ; loop forever

(define (parallel-map f xs)  
  (define l (length xs))
  (define-values (wch-c wch-w) (place-channel))    ; create channel (2 endpoints) for work dispatch (a.k.a. shared queue)
  (for ((i (in-range (processor-count))))
    (define p (place pch (worker pch)))            ; create place
    (place-channel-put p (format "worker_~a" i))   ; give worker id
    (place-channel-put p wch-w)                    ; give response channel
    (place-channel-put p f))                       ; give function
  (for ((n xs))
    (place-channel-put wch-c n))                   ; create work orders
  (let loop ((i 0) (res '()))                      ; response loop
    (if (= i l)
        (reverse res)
        (let ((response (sync/timeout 10 wch-c)))  ; get answer with timeout (place-channel-get blocks!)
          (loop 
           (+ i 1) 
           (if response (cons response res) res))))))

(module+ main 
  (displayln (parallel-map 'add1 (range 10))))