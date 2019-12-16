#lang racket

(define ZERO 48)
(define (char->number char)
  (- (char->integer) ZERO))


(define table (make-hash))

(define (last-point data)
  (map (lambda (x) (string->number x)) (string-split (last data) "_")))

(define (get-xpoint point)
  (string->number (first point)))

(define (get-ypoint point)
  (string->number (second point)))

(define (get-point point)
  (first point))

(define (get-direction point)
  (last point))


(define (make-key x y)
  (string-append (number->string x) "_" (number->string y)))


(define (right startpoint num ypoint)
  (foldl (lambda (x result) (append result (list (make-key x ypoint) "R"))) '() (range startpoint num)))

(define (up data startpoint num xpoint)
  (for-each (lambda (y) (hash-set! data (make-key xpoint y) "U")) (range startpoint num))
  (make-key xpoint num))

(define (down data startpoint num xpoint)
  (for-each (lambda (y) (hash-set! data (make-key xpoint (- startpoint y)) "D")) (range startpoint num))
  (make-key xpoint (- startpoint num)))

(define (left data startpoint num ypoint)
  (for-each (lambda (x) (hash-set! data (make-key (- startpoint x) ypoint) "L")) (range startpoint num))
  (make-key (- startpoint num) ypoint))



(define data (map (lambda (str) (string->number str)) (string-split (first (file->lines "day2_1.txt")) ",")))

(define sample1 '("R8" "U5" "L5" "D3"))

(define (dispatcher data xpoint ypoint)
  (let ([direction (string-ref data 0)]
        [number (string->number (substring data 1))])
    (cond
      [(equal? #\R direction) (right table xpoint number ypoint)]
      [(equal? #\L direction) (left table xpoint number ypoint)]
      [(equal? #\U direction) (left table ypoint number xpoint)]
      [(equal? #\D direction) (left table ypoint number xpoint)])))


(foldl (lambda (x result) (let ([point (string-split result "_")])
                            (printf "~s~n" (string-ref x 0))
                            (dispatcher x (get-xpoint point) (get-ypoint point)))) "0_0" sample1)

(printf "~s" table)
