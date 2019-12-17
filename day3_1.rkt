#lang racket
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


(define (right startpoint num ypoint)
  (foldl (lambda (x result) (cons (list (make-key (+ startpoint x) ypoint) "R") result)) '() (range 0 (+ num 1))))

(define (up startpoint num xpoint)
  (foldl (lambda (y result) (cons (list (make-key xpoint (+ startpoint y)) "U") result)) '() (range 0 (+ num 1))))

(define (down startpoint num xpoint)
  (foldl (lambda (y result) (cons (list (make-key xpoint (- startpoint y)) "D") result)) '() (range 0 (+ num 1))))
  
(define (left startpoint num ypoint)
  (foldl (lambda (x result) (cons (list (make-key (- startpoint x) ypoint) "L") result)) '() (range 0 (+ num 1))))


(define data (map (lambda (str) (string->number str)) (string-split (first (file->lines "day2_1.txt")) ",")))


(define sample1 '("R8" "U5" "L5" "D3"))
(define sample2 '("U7" "R6" "D4" "L4"))

(define (dispatcher data xpoint ypoint)
  (let ([direction (string-ref data 0)]
        [number (string->number (substring data 1))])
    (cond
      [(equal? #\R direction) (right xpoint number ypoint)]
      [(equal? #\L direction) (left xpoint number ypoint)]
      [(equal? #\U direction) (up ypoint number xpoint)]
      [(equal? #\D direction) (down ypoint number xpoint)])))


(define (make-line data)
  (cdr (reverse (foldl (lambda (x result)
                       (let ([point (string-split  (caar result) "_")])
                         (append (dispatcher x (get-xpoint point) (get-ypoint point)) result)))
                     (list (list "0_0" "")) data))))

(define (transform-hash data)
  (foldl (lambda (point-data result)
         (if (non-empty-string? (second point-data))
             (hash-set result (first point-data) (second point-data))
             result))
       (make-immutable-hash) (make-line data)))

(define (get-distance first-line second-line)
  (apply min (map (lambda (data)
         (let* ([point (string-split (first data) "_")]
                [x (get-xpoint point)]
                [y (get-ypoint point)])
           (+ (abs x) (abs y))))
 (let ([hashdata (transform-hash first-line)])
         (filter (lambda (point)
              (let* ([ref-data (hash-ref hashdata (first point) "")]
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


(get-distance sample1 sample2)

(get-distance (string-split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ",") (string-split "U62,R66,U55,R34,D71,R55,D58,R83" ","))

(get-distance (string-split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" ",") (string-split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ","))

(define input-data (file->lines "day3_1.txt") )

(get-distance (string-split (first input-data) ",") (string-split (second input-data) ","))



;;(parameterize ([plot-width    550]
;;                 [plot-height   550])
;;  (plot (points (append (tranform-points (make-line sample1)) (tranform-points (make-line sample2))) 
;;                #:alpha 0.3
;;                #:sym 'fullcircle1
;;                #:color "blue") 
 ;;      #:x-min -1 #:x-max 10 #:y-min -1 #:y-max 10)
 ;; )

;;	(append (tranform-points (make-line (string-split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ","))) (tranform-points (make-line (string-split "U62,R66,U55,R34,D71,R55,D58,R83" ","))))

;;(parameterize ([plot-width    550]
;;                 [plot-height   550])
;;  (plot (points (tranform-points (make-line (string-split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ",")))
;;                #:alpha 0.3
;;                #:sym 'fullcircle1
;;                #:color "blue") 
;;       #:x-min -10 #:x-max 100 #:y-min -50 #:y-max 100)
;;  )

;;(make-line (string-split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ","))

;; (printf "~s" table)
