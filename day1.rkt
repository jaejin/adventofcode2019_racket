#lang racket/base
(require racket/file)

(foldr (lambda (value sum)
         (let* [[num (string->number value)]
                [divValue (- (floor (/ num 3.0)) 2)]]
           (+ sum divValue))) 0 (file->lines "day1_1.txt") )
