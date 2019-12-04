#lang racket/base
(require racket/file)
(require racket/list)

(module+ test
  (require rackunit))


(define (multiply first second position list)
  (let ([result (* first second)])
    (printf "multiply ~s *  ~s ~n" first second)
    (values (list-set list position result) result)))

(define (adds first second position list)
  (let ([result (+ first second)])
    (values (list-set list position (+ first second)) result)))


(define (get-value list position)
  (list-ref list position))

(define (get-second position)
  (+ position 2))

(define (get-first position)
  (+ position 1))

(define (get-third position)
  (+ position 3))


(define (opcode+ position list result)
  (printf "start : position ~s list ~s result ~s  ~n" position list result)
  (let [[op-code (list-ref list position)]]
    (define (_add position list)
      (let ([first  (get-value list (list-ref list (get-first position)))]
            [second (get-value list (list-ref list (get-second position)))]
            [value-position (get-value list (list-ref list (get-third position)))])
        (let-values ([(new-list new-result)
                        (adds first second value-position list)])
         (opcode+ (+ position 4) new-list new-result))))
    (define  (_multiply position list)
      (let ([first  (get-value list (list-ref list (get-first position)))]
            [second (get-value list (list-ref list (get-second position)))]
            [value-position (get-value list (list-ref list (get-third position)))])
        (printf "first ~s second ~s value-position ~s ~n" first second value-position)
        (let-values ([(new-list new-result)
                        (multiply first second value-position list)])
         (printf "postion ~s new-result ~s ~n" position new-result)
                                (opcode+ (+ position 4) new-list new-result))))
      (cond
        [(equal? op-code 1) (_add position list)]
        [(equal? op-code 2) (_multiply position list)]
        [(equal? op-code 99) result])))


        

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (opcode+ 0 '(2 3 0 3 99) 0)
  ;; (check-equal? (opcode '(2 4 4 5 99 0)) 9801)
    (letrec-values ([(list result) (adds 1 1 0 '(1 0 0 0))])
    (check-equal? list '(2 0 0 0))
    (check-equal? result 2))
  (letrec-values ([(list result) (multiply 1 2 0 '(1 0 0 0))])
    (check-equal? list '(2 0 0 0))
    (check-equal? result 2))
  )


