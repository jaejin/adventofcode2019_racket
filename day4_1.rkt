#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (check-double num)
  (if (or (empty? num) (equal? (length num) 1))
      #f
      (let ([firstValue (first num)]
            [secondValue (second num)])
        (if (equal? firstValue secondValue) #t
            (check-double (rest num))))))


(module+ test
  (check-true (check-double (string->list "111111")))
  (check-true (check-double (string->list "122345")))
  (check-true (not (check-double (string->list "123789"))))
  )

(define (check-increase num)
  (let ([numlist (string->list num)])
    (equal? (list->string (sort numlist char<=?)) num)))

(module+ test
  (check-true (check-increase "123789"))
  (check-true (check-increase "111111"))
  (check-true (check-increase "122345"))
  (check-true (not (check-increase "122340")))
  (check-true (not (check-increase "120345")))
  (check-true (not (check-increase "114345")))
)

(define (check-double-thread num)
  (~> num string->list check-double))


(define (check-increase-thread num)
  (~> num check-increase))


(define (is-password num)
  (and (check-increase-thread num) (check-double-thread num)))

(for/sum ([n (in-range 402328 (add1 412328))])
   (if (is-password (~> n make-string)) 1 0))

(module+ test
   (check-true (is-password "111111"))
 ;; (check-true (is-password "122345"))
  ;; (check-true (not (is-password "123789")))
  
  )
