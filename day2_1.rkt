#lang racket/base
(require racket/file)
(require racket/list)

(module+ test
  (require rackunit))


(define (multiply first second position list)
  (let ([result (* first second)])
    (values (list-set list position result) result)))

(define (adds first second position list)
  (let ([result (+ first second)])
    (values (list-set list position (+ first second)) result)))


(define (get-value list position)
  (list-ref list position))

(define (get-second position list)
  (let ([length (length list)])
    (if (> length posiition)
        0
        (list-ref list (+ position 2)))))

(define (get-first position)
    (let ([length (length list)])
    (if (> length posiition)
        0
        (list-ref list (+ position 1)))))

(define (get-third position)
  (+ position 3))


(define (opcode+ position list result)
  (printf "start : position ~s list ~s result ~s  ~n" position list result)
  (let [[op-code (list-ref list position)]
        [first (list-ref list (get-first position))]
        [second (list-ref list (get-second position))]
        [value-position (list-ref list (get-third position))]]
    (printf "let : opcode ~s first ~s second ~s value-position ~s ~n" op-code first second value-position)
     (define (_add first second value-position list)
       (letrec-values ([(new-list new-result)
                        (adds first second value-position list)])
         (opcode+ (+ position 4) new-list new-result)))
     (define  (_multiply first second value-position list)
       (letrec-values ([(new-list new-result)
                        (multiply first second value-position list)])
         (printf "postion ~s~n" position)
                                (opcode+ (+ position 4) new-list new-result)))
      (cond
        [(equal? op-code 1) (_add first second value-position list)]
        [(equal? op-code 2) (_multiply first second value-position list)]
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


