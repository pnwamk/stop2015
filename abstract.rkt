#lang at-exp scribble/sigplan @nocopyright @notimes 

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
this somewhat generic string/symbol reverse function @racket[reversal]:

@racketblock[(define (reversal x)
               (cond
                 [(string? x) (string-reverse x)]
                 [else (symbol-reverse x)]))]

Typed Racket, with it's usage of untagged unions and 
linear intersections of function types, allows 
@racket[reversal]'s type to contain detailed 
information about its specification:

@racketblock[(: reversal 
                (case->
                 [-> String String]
                 [-> Symbol Symbol]
                 [-> (U String Symbol)  
                     (U String Symbol)]))]
             
Unfortunately, as the type illustrates, the specificity of
@racket[reversal]'s range type depends on a priori
knowledge about the input type. Calling @racket[reversal]
with a value of type @racket[(U String Symbol)] causes
the relation between the input and output types to be forgotten. This
loss of information means some sound programs, such as our 
@racket[silly-string] below, will not typecheck:

@racketblock[(: silly-string
                (-> (U String Symbol) String))

             (define (silly-string y)
               (define y* (reversal y))
               (cond
                 [(string? y*) (string-append y y*)]
                 [else (string-append
                        (symbol->string y)
                        (symbol->string y*))]))]

Our work extending Typed Racket offers @bold{logical refinement types} 
as a solution to this problem. Instead of using a simple 
@racket[case->] for the type of @racket[reversal], we can use a single function 
type which @emph{refines} the range based on its dependent relation with the 
input type (@emph{we syntactically borrow } @racket[->i] 
@emph{from Racket's dependent contracts}):

@racketblock[(: reversal
                (->i [x : (U String Symbol)]
                     [res (x) : (U String Symbol)
                          (or (and [x res : String])
                              (and [x res : Symbol]))]))]

With this dependently typed specification for @racket[reversal]
Typed Racket is now able to correctly associate the types of @racket[y] 
and @racket[y*] in @racket[silly-string], allowing it to successfully typecheck.
This program is one simple example of a class of programs which rely 
on this sort of dependent type-based reasoning that may now be gradually 
typed in Racket while requiring little or no modification.


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