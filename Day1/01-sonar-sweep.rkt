#lang typed/racket

; --------------------------- PART 1 ---------------------------
;; Takes a list of real numbers and counts the number of
;; times a depth measurement inreases from the previous measurement
(: sonar-sweep (-> (Listof Real) Real))
(define (sonar-sweep depths)
  (if (empty? depths)
      0
      (sonar-sweep/a (cdr depths) (car depths) 0)))

(: sonar-sweep/a (-> (Listof Real) Real Real Real))
(define (sonar-sweep/a depths prev count)
  (cond
    [(empty? depths) count]
    [else
     (sonar-sweep/a (cdr depths)
              (car depths)
              (if  (> (car depths) prev)
                   (add1 count)
                   count))]))

; --------------------------- PART 2 ---------------------------

(: t (Listof Real))
(define t (list 199 200 208 210 200 207 240 269 260 263))

(: sonar-sweep-slide (-> (Listof Real) Real))
(define (sonar-sweep-slide depths)
  (: depths^ (Listof Real))
  (define depths^ (reverse (window-depths depths empty)))
  (if (empty? depths)
      0
      (sonar-sweep/a (cdr depths^) (car depths^) 0)))

(: window-depths (-> (Listof Real) (Listof Real) (Listof Real)))
(define (window-depths depths result)
  (cond
    [(< (length depths) 3) result]
    [else
     (define cut (take depths 3))
     (define sum (foldr + 0 cut))
     (window-depths (cdr depths) (cons sum result))]))


 








