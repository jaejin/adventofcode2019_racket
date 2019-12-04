#lang racket/base
(require racket/file)



(define (massFuel num)
  (let [[value (- (floor (/ num 3.0)) 2)]]
    (if (<= value 0)
        0 value)))


(define (caculateFuel num sum)
  (if (<= num 0)
      sum
      (let [[divValue (massFuel num)]]
        (caculateFuel divValue (+ divValue sum)))))


(foldr (lambda (value sum)
         (let* [[num (string->number value)]
                [divValue (caculateFuel num 0)]]
           (+ sum divValue))) 0 (file->lines "day1_2.txt") )



(println (caculateFuel 14 0))
(println (caculateFuel 1969 0))
(println (caculateFuel 100756 0))
