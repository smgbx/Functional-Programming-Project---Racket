#lang racket
(require 2htdp/batch-io)

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

(define (mergeSort L)
  (cond [(empty? L) L]
        [(empty? (rest L)) L]
        [else (let ([f (future (lambda () (mergeSort (firstHalf L))))])
                  (or (mergeSort (secondHalf L))
                    (touch f)))
         (merge
               (mergeSort (firstHalf L))
               (mergeSort (secondHalf L)))]))

; get a list of numbers from a file
(define (get-numbers filename)
  (map string->number (file->lines filename))
  )

; gets a list of numbers from a file
; and sorts them using mergeSort
(define (sort-file-numbers filename)
  (mergeSort (get-numbers filename))
  )

(module+ test
  (require rackunit)
  (check-equal? (mergeSort '()) '())
  (check-equal? (mergeSort '(3)) '(3))
  (check-equal? (mergeSort '(9 2)) '(2 9))
  (check-equal? (mergeSort '(10 1 6 2 3 9 8 4 7 5)) '(1 2 3 4 5 6 7 8 9 10))
  (check-equal? (sort-file-numbers "test.txt") '(10 20 30 40 50 60 70 80 90 100))
  )

; writes list to file
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))

(time
 (list->file (sort-file-numbers "numbersTest.txt") "output_futures.txt")
 (void))