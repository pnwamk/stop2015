#lang typed/racket

(define-type Int Integer)

(: inc (case-> [-> Int Int]
               [-> Flonum Flonum]
               [-> (U Int Flonum)  
                   (U Int Flonum)]))
(define (inc x)
  (cond
    [(exact-integer? x) (add1 x)]
    [else (+ .1 x)]))

(: next-num (-> (U Int Flonum) Int))
(define (next-num y)
  (let ([z (inc y)])
    (if (flonum? z)
        (exact-ceiling z)
        y)))

#;(^-> [-> Int Int]
       [-> Flonum Flonum])

#;(-> [d : (U Int Flonum)]
      [r : (U Int Flonum)
         (or (and (d : Int) 
                  (r : int))
             (and (d : Flonum) 
                  (r : flonum)))])

#;(: safe-vec-ref
   (All (α) (->i [v : (Vectorof α)]
                 [i : Integer (≤ i (vec-len v))]
                 α)))
#;(define (safe-vec-ref v i)
  (unsafe-vector-ref v i))

#;(define (dot-product v1 v2)
  (for/sum ([i (in-range 0 (vec-len v1))])
    (* (safe-vec-ref v1 i)
       (safe-vec-ref v2 i))))