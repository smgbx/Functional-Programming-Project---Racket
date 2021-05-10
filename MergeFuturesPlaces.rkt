#lang racket
(require 2htdp/batch-io)
(require racket/place)

(provide main)

; mergeSort helper function
; merges two list together in order
(define (merge A B)
  (cond [(empty? A) B]
        [(empty? B) A]
        [(<= (first A) (first B))
         (cons (first A) (merge (rest A) B))]
        [else
         (cons (first B) (merge A (rest B)))]))

; gets the first half of the input list
(define (firstHalf L)
  (firstHalfH L (quotient (length L) 2))
  )

; firstHalf helper function
; recursively gets the first half of the input list
(define (firstHalfH L num)
  (if (= 0 num)
      '()
      (cons (first L) (firstHalfH (rest L) (- num 1)))))

; gets the second half of the input list
(define (secondHalf L)
  (secondHalfH L (quotient (length L) 2))
  )

; secondHalf helper function
; recursively gets the second half of the input list
(define (secondHalfH L num)
  (if (= num 0)
      L
      (secondHalfH (rest L) (- num 1))))

(define (mergeSortFutures L)
  (cond [(empty? L) L]
        [(empty? (rest L)) L]
        [else (let ([f (future (lambda () (mergeSort (firstHalf L))))])
                  (or (mergeSort (secondHalf L))
                    (touch f)))
         (merge
               (mergeSort (firstHalf L))
               (mergeSort (secondHalf L)))]))
  
; Getting list of numbers from file
(define (get-numbers filename)
  (map string->number (file->lines filename))
  )

; Mergesort numbers in a file
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
  (define L (list 10 1 6 2 3 9 8 4 7 5))

  (define firstH
    (place ch
           (define l (place-channel-get ch))
           (define mrg (firstHalf l))
           (place-channel-put ch mrg)))
  (define secondH
    (place ch
           (define l (place-channel-get ch))
           (define mrg (secondHalf l))
           (place-channel-put ch mrg)))

  (place-channel-put firstH (firstHalf L))
  (place-channel-put secondH (secondHalf L))
  
  (time
   (merge (mergeSortFutures (place-channel-get firstH)) (mergeSortFutures (place-channel-get secondH)))
   (void)))
 

(main)
