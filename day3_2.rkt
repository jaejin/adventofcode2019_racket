#lang racket

(module+ test
  (require rackunit))

(require plot)

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

(define (make-step direction num)
  (string-append direction (number->string num)))

(define (right startpoint num ypoint step)
  (foldl (lambda (x result) (cons (list (make-key (+ startpoint x) ypoint) "R" (cons x step)) result)) '() (range 0 (+ num 1))))

(define (up startpoint num xpoint step)
  (foldl (lambda (y result) (cons (list (make-key xpoint (+ startpoint y)) "U" (cons y step)) result)) '() (range 0 (+ num 1))))

(define (down startpoint num xpoint step)
  (foldl (lambda (y result) (cons (list (make-key xpoint (- startpoint y)) "D" (cons y step)) result)) '() (range 0 (+ num 1))))
  
(define (left startpoint num ypoint step)
  (foldl (lambda (x result) (cons (list (make-key (- startpoint x) ypoint) "L" (cons x step)) result)) '() (range 0 (+ num 1))))



(define sample1 '("R8" "U5" "L5" "D3"))
(define sample2 '("U7" "R6" "D4" "L4"))

(define (dispatcher data xpoint ypoint step)
  (let ([direction (string-ref data 0)]
        [number (string->number (substring data 1))])
    (cond
      [(equal? #\R direction) (right xpoint number ypoint step)]
      [(equal? #\L direction) (left xpoint number ypoint step)]
      [(equal? #\U direction) (up ypoint number xpoint step)]
      [(equal? #\D direction) (down ypoint number xpoint step)])))


(define (make-line data)
  (cdr (reverse (foldl (lambda (x result)
                       (let ([point (string-split  (caar result) "_")]
                             [step (caddr (first result))])
                         (append (dispatcher x (get-xpoint point) (get-ypoint point) step) result)))
                     (list (list "0_0" "" '())) data))))

(define (transform-hash data)
  (foldl (lambda (point-data result)
         (if (non-empty-string? (second point-data))
             (hash-set result (first point-data) (cdr point-data))
             result))
       (make-immutable-hash) (make-line data)))

(define (get-distance first-line second-line)
  (let ([hashdata (transform-hash first-line)])
    (apply min (map (lambda (data)
                      (let* ([point (first data)]
                             [points (string-split point "_")]
                             [value (last data)])
                        (apply + (append value (second (hash-ref hashdata point))))))
                    (filter (lambda (point)
                              (let* ([ref-data (first (hash-ref hashdata (first point) (list "")))]
                                     [value (second point)]
                                     [key (first point)])
                                (and (non-empty-string? ref-data)
                                     (not (equal? value ref-data))
                                     (not (equal? key "0_0")))))
                            (make-line second-line))))))


(define (tranform-points line)
   (map (lambda (data)
         (let ([point (string-split (first data) "_")])
           (vector (get-xpoint point) (get-ypoint point)))) line))

;; (first (hash-ref (transform-hash sample1) "3_4" (list "" '())))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (get-distance sample1 sample2) 30)
  (check-equal? (get-distance (string-split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ",") (string-split "U62,R66,U55,R34,D71,R55,D58,R83" ",")) 610)
  (check-equal? (get-distance (string-split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" ",") (string-split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ",")) 410)
  
  )

;; 



;; (get-distance (string-split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" ",") (string-split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ","))

(define input-data (file->lines "day3_1.txt") )

(get-distance (string-split (first input-data) ",") (string-split (second input-data) ","))



