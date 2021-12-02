#lang typed/racket

(define-type Direction (U 'up 'down 'forward 'nil))
(struct Instruction ([dir : Direction] [dist : Number])
  #:transparent)
(struct Posn ([x : Number] [y : Number])
  #:transparent)

(: test (Listof Instruction))
(define test (list (Instruction 'forward 5)
                   (Instruction 'down 5)
                   (Instruction 'forward 8)
                   (Instruction 'up 3)
                   (Instruction 'down 8)
                   (Instruction 'forward 2)))

; --------------------------- PART 1 ---------------------------
(: dive (-> (Listof Instruction) Number))
(define (dive instr*)
  (dive/a instr* 0 0))

(: dive/a (-> (Listof Instruction) Number Number Number))
(define (dive/a instr* horz depth)
  (cond
    [(empty? instr*) (* horz depth)]
    [else
     (: curr Instruction)
     (define curr (car instr*))
     (cond
       [(eq? (Instruction-dir curr) 'forward)
        (dive/a (cdr instr*) (+ horz (Instruction-dist curr)) depth)]
       [(eq? (Instruction-dir curr) 'down)
        (dive/a (cdr instr*) horz (+ depth (Instruction-dist curr)))]
       [(eq? (Instruction-dir curr) 'up)
        (dive/a (cdr instr*) horz (- depth (Instruction-dist curr)))]
       [else (error "unhandled dir")])
     ]))

  
; --------------------------- PART 2 ---------------------------
(: dive-aim (-> (Listof Instruction) Number))
(define (dive-aim instr*)
  (dive-aim/a instr* 0 0 0))

(: dive-aim/a (-> (Listof Instruction) Number Number Number Number))
(define (dive-aim/a instr* horz depth aim)
  (cond
    [(empty? instr*) (* horz depth)]
    [else
     (: curr Instruction)
     (define curr (car instr*))
     (cond
       [(eq? (Instruction-dir curr) 'forward)
        (dive-aim/a (cdr instr*)
                    (+ horz (Instruction-dist curr))
                    (+ depth (* aim (Instruction-dist curr)))
                    aim)]
       [(eq? (Instruction-dir curr) 'down)
        (dive-aim/a (cdr instr*) horz depth (+ aim (Instruction-dist curr)))]
       [(eq? (Instruction-dir curr) 'up)
        (dive-aim/a (cdr instr*) horz depth (- aim (Instruction-dist curr)))]
       [else (error "unhandled dir")])
     ]))


; --------------------------- PUZZLE ---------------------------
(: format-puzzle (-> String (Listof Instruction)))
(define (format-puzzle s)
  (reverse (format-puzzle/helper (string-split s) empty)))


(: format-puzzle/helper (-> (Listof String) (Listof Instruction) (Listof Instruction)))
(define (format-puzzle/helper ss res)
  (cond
    [(empty? ss) res]
    [else
     (define dir (cast (string->symbol (car ss)) Direction))
     (define dist (cast (string->number (cadr ss)) Number))
     (format-puzzle/helper (cddr ss) (cons (Instruction dir dist) res))]))

(: run-puzzle (-> String Number))
(define (run-puzzle input)
  (dive (format-puzzle input)))

(: run-puzzle-aim (-> String Number))
(define (run-puzzle-aim input)
  (dive-aim (format-puzzle input)))