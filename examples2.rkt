#lang typed/racket

(define (foo [x : (U Number String)] 
             [p : (Pairof Any Any)])
  (cond
    [(and (number? x) (number? (car p)))
     (+ x (car p))]
    [(number? (car p))
     (+ (string-length x) (car p))]
    [else 0]))

(-> [x : (U (Listof Any) String)]
    [y : (U (Listof Any) String)
       (or (and [x : (Listof Any)]
                [y : (Listof Any)])
           (and [x : String]
                [y : String]))]
    [result : (U (Listof Any) String)
            (or (and [x : (Listof Any)]
                     [result : (Listof Any)])
                (and [x : String]
                     [result : String]))])

(define (flexible-append x y)
  (cond
    [(string? x) (string-append x y)]
    [else (append x y)]))