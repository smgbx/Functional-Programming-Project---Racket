#lang racket
(define (get-numbers)
  (map string->number (file->lines "test.txt"))
  )

(time
(for-each display (get-numbers))
(void))

(length (get-numbers))

(range 10)