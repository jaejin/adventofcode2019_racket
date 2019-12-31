#lang racket/base
(require racket/file)
(require racket/list)
(require racket/string)

(module+ test
  (require rackunit))

(define POSITION-MODE 0)
(define IMMEDIATE-MODE 1)

(struct instructor-struct (third-mode second-mode first-mode opcode) #:inspector #f #:extra-name INSTRUCTOR)

(struct store-data-struct (list input output) #:inspector #f)

(define (input inputValue position list)
  (let ([result (+ (* first second))])
    (list-set list position result)))

(define (output position list)
  (list-ref list position))

(define (multiply first second position list)
  (let ([result (+ (* first second))])
    (list-set list position result)))

(define (adds first second position list)
  (let ([result (+ first second)])
    (list-set list position result)))

(define (store value position list)
  (list-set list position value))

(define (get-value list position mode)
  (cond
    [(= POSITION-MODE mode) (list-ref list (list-ref list position))]
    [(= IMMEDIATE-MODE mode) (list-ref list position)] 
    ))

(define (get-result-position list position mode)
  (cond
    [(= POSITION-MODE mode) (list-ref list position)]
    [(= IMMEDIATE-MODE mode) position] 
    ))


(module+ test
  (check-true (= POSITION-MODE 0))
  (check-equal? (get-value '(2 4 4 5 99 0) 1 0) 99)
  (check-equal? (get-value '(2 4 4 5 99 0) 1 1) 4)
  (check-equal? (get-result-position '(2 4 4 5 99 0) 3 0) 5)
  (check-equal? (get-result-position '(2 4 4 5 99 0) 3 1) 3)
  )


(define (get-second position)
  (+ position 2))

(define (get-first position)
  (+ position 1))

(define (get-third position)
  (+ position 3))

(define (get-instructor instructor)
  (if (> instructor 100)
      (manuplate-instructor instructor)
      (INSTRUCTOR 0 0 0 instructor)))

(define (get-position-value position-value)
  (let* ([length (string-length (number->string position-value))]
         [append-value (list->string (make-list (- 3 length) #\0))])
    (string->list (string-append append-value (number->string position-value) ))))

(define (char->number char)
    (- (char->integer char) 48))


(module+ test
  (check-equal? (get-position-value 10) '(#\0 #\1 #\0))
  (check-equal? (get-position-value 110) '(#\1 #\1 #\0))
  )

(define (manuplate-instructor instructor)
  (let ([opcode (remainder instructor 100)]
        [position-value (get-position-value (quotient instructor 100))])
    (INSTRUCTOR (char->number (first position-value)) (char->number (second position-value)) (char->number (third position-value)) opcode)))

(module+ test
  (check-equal? (get-instructor 99) (INSTRUCTOR 0 0 0 99))
  (check-equal? (get-instructor 2) (INSTRUCTOR 0 0 0 2))
  (check-equal? (get-instructor 1) (INSTRUCTOR 0 0 0 1))
  (check-equal? (get-instructor 1) (INSTRUCTOR 0 0 0 1))
  (check-equal? (get-instructor 101) (INSTRUCTOR 0 0 1 1))
  (check-equal? (get-instructor 1001) (INSTRUCTOR 0 1 0 1))
  (check-equal? (get-instructor 1101) (INSTRUCTOR 0 1 1 1))
  (check-equal? (get-instructor 11101) (INSTRUCTOR 1 1 1 1))
  (define x (INSTRUCTOR 1 1 1 1))
  (check-equal? (instructor-struct-opcode x) 1)
  )



(define (opcode+ position list store-data)
  (let* ([mode (get-instructor (list-ref list position))]
        [op-code (instructor-struct-opcode mode)])
    (define (_add position list mode store-data)
      (let ([first  (get-value list (get-first position) (instructor-struct-first-mode mode))]
            [second (get-value list (get-second position) (instructor-struct-second-mode mode))]
            [value-position (get-result-position list (get-third position) (instructor-struct-third-mode mode))])
        (let ([new-list (adds first second value-position list)])
          (opcode+ (+ position 4) new-list (store-data-struct new-list (store-data-struct-input store-data) (store-data-struct-output store-data) )))))
    (define  (_multiply position list mode store-data)
      (let ([first  (get-value list (get-first position) (instructor-struct-first-mode mode))]
            [second (get-value list (get-second position) (instructor-struct-second-mode mode))]
            [value-position (get-result-position list (get-third position) (instructor-struct-third-mode mode))])
        (let ([new-list (multiply first second value-position list)])
          (opcode+ (+ position 4) new-list (store-data-struct new-list (store-data-struct-input store-data) (store-data-struct-output store-data) )))))
    (define (_output position list mode store-data)
      (let ([outputValue (get-value list (get-first position) (instructor-struct-first-mode mode))])
        (opcode+ (+ position 2) list (store-data-struct list (store-data-struct-input store-data) outputValue))))
    (define (_input position list mode store-data)
      (let* ([value-position (get-result-position list (get-first position) (instructor-struct-first-mode mode))]
             [new-list (store (store-data-struct-input store-data) value-position list)])
        (opcode+ (+ position 2) new-list (store-data-struct new-list (store-data-struct-input store-data) (store-data-struct-output store-data)))))
    (cond
      [(equal? op-code 1) (_add position list mode store-data)]
      [(equal? op-code 2) (_multiply position list mode store-data)]
      [(equal? op-code 3) (_input position list mode store-data)]
      [(equal? op-code 4) (_output position list mode store-data)]
      [(equal? op-code 99) (store-data-struct list (store-data-struct-input store-data) (store-data-struct-output store-data))])))

(define (opcode list inputdata)
  (opcode+ 0 list (store-data-struct '() inputdata 0)))

(define data (map (lambda (str) (string->number str)) (string-split (first (file->lines "day5_1.txt")) ",")))

(printf "~s~n" data)
(get-instructor (list-ref data 38))
(opcode data 1)

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (opcode '(2 3 0 3 99) 1)  (store-data-struct '(2 3 0 6 99) 1 0))
  (check-equal? (opcode '(3 1 2 3 0 3 4 0 99) 1)  (store-data-struct '(3 1 2 9 0 3 4 0 99) 1 3))
  (check-equal? (opcode '(2 4 4 5 99 0) 1)  (store-data-struct '(2 4 4 5 99 9801) 1 0))
  (check-equal? (opcode '(1 1 1 4 99 5 6 0 99) 1)  (store-data-struct '(30 1 1 4 2 5 6 0 99) 1 0))
  (check-equal? (opcode '(1 1 1 4 99 5 6 0 4 0 99) 1)  (store-data-struct '(30 1 1 4 2 5 6 0 4 0 99) 1 30))
  (check-equal? (opcode '(1 1 1 4 99 5 6 0 4 0 99) 1)  (store-data-struct '(30 1 1 4 2 5 6 0 4 0 99) 1 30))
  (check-equal? (opcode '(1 1 1 4 99 5 6 0 4 0 99) 1)  (store-data-struct '(30 1 1 4 2 5 6 0 4 0 99) 1 30))
  
  )


