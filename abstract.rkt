#lang scribble/sigplan @nocopyright @notimes 

@(require scribble/manual
          andmkent-bib
          scriblib/autobib)
          
@(define-cite ~cite citet generate-bibliography)

@(title "Enriching Typed Racket with Dependent Types")

@(authorinfo "Andrew M. Kent" "Indiana University" "andmkent@indiana.edu")
@(authorinfo "Sam Tobin-Hochstadt" "Indiana University" "samth@indiana.edu")

Typed Racket is a statically-typed dialect of Racket that 
allows idiomatic Racket programs to be enriched with types. 
It can reason about many dynamically typed programming 
patterns--- e.g. type-based control flow @~cite[thf-icfp-2010], 
a rich numereic tower @~cite[sthff-padl-2012], first-class 
classes @~cite[tsdthf-oopsla-2012]---while 
providing sound interoperability with
untyped Racket programs. We have designed and modeled an extension 
to Typed Racket's core calculus which adds support for logical 
refinement types and linear integer constraints. This abstract 
discusses our findings and current progress in implementing this
extension.

@section{Logical Refinement Types}

In order to typecheck a language such as Racket, Typed Racket
employs features not found in traditional type systems. Consider
a simple increment function @racket[inc]:

@racketblock[(define (inc x)
               (cond
                 [(exact-integer? x) (add1 x)]
                 [else (+ .1 x)]))]

Typed Racket, with its usage of untagged unions and 
ordered function intersections, allows 
@racket[inc]'s type to contain detailed 
information about its specification:

@racketblock[(: inc (case-> [-> Int Int]
                            [-> Flonum Flonum]
                            [-> (U Int Flonum)  
                                (U Int Flonum)]))]
             
When typechecking function application, ordered intersections are 
fixed to the first suitable function type based on context.
Because of this, calling @racket[inc] with a value of 
type @racket[(U Int Flonum)] causes the relation between 
the input and output types to be forgotten. This loss of information 
means some sound programs, such as our @racket[next-num] below, 
will not typecheck:

@racketblock[(: next-num (-> (U Int Flonum) Int))
             (define (next-num y)
               (let ([z (inc y)])
                 (if (flonum? z)
                     (exact-ceiling z)
                     y)))]
@(image "tcerror.png"
        #:scale .33)

Our work extending Typed Racket offers @bold{logical refinement types} 
as a solution to this class of problems. Logical refinements allow us to
specify a subset of type τ where a logical proposition ψ holds, written
as {x : τ | ψ}. For @racket[inc], we can use refinements
to express this relation between input and output types:

@racketblock[(-> [d : (U Int Flonum)]
                 {r : (U Int Flonum)
                    (or (and [d : Int] 
                             [r : int])
                        (and [d : Flonum] 
                             [r : flonum]))})]

Because this is a common pattern (and the propositions can quickly
become quite verbose) we abbreviate these sorts of dependent function
types as follows:


@racketblock[(^-> [-> Int Int]
                  [-> Flonum Flonum])]

With this simple dependently typed specification for @racket[inc],
Typed Racket is now able to correctly associate the types of @racket[y] 
and @racket[z] in @racket[next-num]---we are not forced to alter our program
to successfully typecheck. This program is one simple example of a class of
programs which rely dependent type-based reasoning that may now be gradually 
typed in Racket with little or no modification.


@section{Linear Integer Constraints}

Discuss @bold{linear integer constraints}, similar approach to that
of Dependent ML @~cite[x-jfp-2007], show quickly the below example, etc...

@racketblock[(: safe-vec-ref
                (∀ (α) (->i [v : (Vectorof α)]
                            [i : Natural (< i (len v))]
                            α)))
             (define (safe-vec-ref v i)
               (unsafe-vector-ref v i))
                                       
             (define (dot-product v1 v2)
               (for/sum ([i (in-range 0 (vec-len v1))])
                 (* (safe-vec-ref v1 i)
                    (safe-vec-ref v2 i))))]

@section{A Natural Extension to λTR}
 
To effectively typecheck these traditionally dynamically typed 
programs, Typed Racket uses logical formulas to represent known 
type information and utilizes @~cite[thf-icfp-2010]...

Our extension is a conservative extension to what TR already offers
blah blah blah.

@section{Contractual Obligations}

Our additions have sister-features in the dependent contracts world,
enabling us to ensure sound interop of these features in both directions 
(typed to untyped and vice versa) blah blah blah.

@section{Related Work}
These other approaches are similar but we're the first @emph{gradually typed} 
approach in that our system will guarantee run-time sound interop with
completely untyped programs blah blah blah.

@~cite[chj-oopsla-2012]
@~cite[ktgff-tech-2007]

@generate-bibliography[]