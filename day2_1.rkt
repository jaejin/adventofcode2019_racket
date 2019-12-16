#lang racket/base
(require racket/file)
(require racket/list)
(require racket/string)

(module+ test
  (require rackunit))


(define (multiply first second position list)
  (let ([result (+ (* first second))])
    (list-set list position result)))

(define (adds first second position list)
  (let ([result (+ first second)])
    (list-set list position result)))


(define (get-value list position)
  (list-ref list position))

(define (get-second position)
  (+ position 2))

(define (get-first position)
  (+ position 1))

(define (get-third position)
  (+ position 3))


(define (opcode+ position list)
  (let [[op-code (list-ref list position)]]
    (define (_add position list)
      (let ([first  (get-value list (list-ref list (get-first position)))]
            [second (get-value list (list-ref list (get-second position)))]
            [value-position (list-ref list (get-third position))])
        (let ([new-list (adds first second value-position list)])
          (opcode+ (+ position 4) new-list))))
    (define  (_multiply position list)
      (let ([first  (get-value list (list-ref list (get-first position)))]
            [second (get-value list (list-ref list (get-second position)))]
            [value-position (list-ref list (get-third position))])
        (let ([new-list (multiply first second value-position list)])
          (opcode+ (+ position 4) new-list))))
    (cond
      [(equal? op-code 1) (_add position list)]
      [(equal? op-code 2) (_multiply position list)]
      [(equal? op-code 99) list])))

(define (opcode list)
  (opcode+ 0 list))


(define data (map (lambda (str) (string->number str)) (string-split (first (file->lines "day2_1.txt")) ",")))

(define (update-data data position value)
  (list-set data position value))

(define (update-i-j data i j)
  (update-data (update-data data 1 i) 2 j))

 (for* ([i (in-range 100)]
       [j (in-range 100)]
       #:when (= (first (opcode (update-i-j data i j) )) 19690720))
    (printf "~s ~s ~n" i j))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (opcode '(2 3 0 3 99)) '(2 3 0 6 99))
  (check-equal? (opcode '(2 4 4 5 99 0)) '(2 4 4 5 99 9801))
  (check-equal? (opcode '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99))
  
  )


