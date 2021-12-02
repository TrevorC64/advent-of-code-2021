#lang typed/racket

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