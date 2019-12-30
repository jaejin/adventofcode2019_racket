#lang racket

(module+ test
  (require rackunit))

(define (check-double num)
  (if (or (empty? num) (equal? (length num) 1))
      #f
      (let ([firstValue (first num)]
            [secondValue (second num)])
        (if (equal? firstValue secondValue) #t
            (check-double (rest num))))))

(define (check-increase num)
  (let ([numlist (string->list num)])
    (equal? (list->string (sort numlist char<=?)) num)))

(define (is-password num)
  (and (check-increase num) (check-double (string->list num))))


(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-true (check-double (string->list "111111")))
  (check-true (check-double (string->list "122345")))
  (check-true (not (check-double (string->list "123789"))))
  (check-true (check-increase "123789"))
  (check-true (check-increase "111111"))
  (check-true (check-increase "122345"))
  (check-true (not (check-increase "122340")))
  (check-true (not (check-increase "120345")))
  (check-true (not (check-increase "114345")))
  (check-true (is-password "111111"))
  (check-true (is-password "122345"))
  (check-true (not (is-password "123789")))
  
  )
