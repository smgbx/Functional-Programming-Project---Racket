#lang racket
(require 2htdp/batch-io)
(require racket/place)
(provide main)

(define (merge A B)
  (cond [(empty? A) B]
        [(empty? B) A]
        [(>= (first A) (first B))
         (cons (first A) (merge (rest A) B))]
        [else
         (cons (first B) (merge A (rest B)))]))

(define (firstHalf L)
  (firstHalfH L (quotient (length L) 2))
  )

(define (firstHalfH L num)
  (if (= 0 num)
      '()
      (cons (first L) (firstHalfH (rest L) (- num 1)))))

(define (secondHalf L)
  (secondHalfH L (quotient (length L) 2))
  )

(define (secondHalfH L num)
  (if (= num 0)
      L
      (secondHalfH (rest L) (- num 1))))
      
(define (mergeSort L)
  (cond [(empty? L) L]
        [(empty? (rest L)) L]
        [else (merge
               (mergeSort (firstHalf L))
               (mergeSort (secondHalf L)))]))

; File of ints -> list of unordered ints
(define (get-numbers filename)
  (map string->number (file->lines filename))
  )

; File of ints -> list of sorted ints (desc)
(define (sort-file-numbers filename)
  (mergeSort (get-numbers filename))
  )

; List of sorted ints -> file
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))




(define (main)
  (define p
    (place ch
           (define l (place-channel-get ch))
           (define mrgsort (mergeSort l))
           (place-channel-put ch mrgsort)))
  (define p2
    (place ch
           (define l (place-channel-get ch))
           (define mrgsort (mergeSort l))
           (place-channel-put ch mrgsort)))
  
  (place-channel-put p (list 1 2 8 10 11))
  (place-channel-put p2 (list 1 2 8 10 11))

  (list (place-channel-get p) (place-channel-get p2)))

(time
 (list->file (main) "mainTest.txt")
(void))