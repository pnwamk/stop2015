#lang typed/racket

(define (string-reverse [x : String]) : String x)
(define (symbol-reverse [x : Symbol]) : Symbol x)

(: reversal 
   (case->
    [-> String String]
    [-> Symbol Symbol]
    [-> (U String Symbol)  
        (U String Symbol)]))
(define (reversal x)
  (cond
    [(string? x) (string-reverse x)]
    [else (symbol-reverse x)]))

(: silly-string
   (-> (U String Symbol)
       String))
(define (silly-string y)
  (define y* (reversal y))
  (cond
    [(string? y*) (string-append y y*)]
    [else (string-append
           (symbol->string y)
           (symbol->string y*))]))

(: safe-vec-ref
   (All (α) (->i [v : (Vectorof α)]
                 [i : Integer (≤ i (vec-len v))]
                 α)))
(define (safe-vec-ref v i)
  (unsafe-vector-ref v i))

(define (dot-product v1 v2)
  (for/sum ([i (in-range 0 (vec-len v1))])
    (* (safe-vec-ref v1 i)
       (safe-vec-ref v2 i))))