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

@section{Refining already logical types}

In order to effectively typecheck idiomatic code in a dynamically typed 
language like Racket, Typed Racket (TR) employs features not found in 
traditional type systems. One such key feature is @emph{occurrence typing}.
Simply put, occurrence typing allows different occurrences of the same
term in a program to have different types based on the logical results of 
conditional control flow. This is accomplished by having the type system 
track the type-based logical propositions implied by the results of
conditional tests and allows logical combinations of type-related testing,
such as those seen in the function @racket[foo], to aid typechecking:

@#reader scribble/comment-reader

@(racketblock
  (define (foo [x : (U Number String)] 
               [p : (Pairof Any Any)])
    (cond
      [(and (number? x) (number? (car p)))
       ;; [x : Number] ∧ [(car p) : Number]
       (+ x (car p))]
      ;; ¬[x : Number] ∨ ¬[(car p) : Number]
      [(number? (car p))
       ;; [(car p) : Number]
       ;; thus [x : String]
       (+ (string-length x) (car p))]
      ;; ¬[(car p) : Number]
      [else 0])))

The importance of considering these typed-based 
propositions is clear when observing the form
typing judgment take on in TR @~cite[thf-icfp-2010]:

@centered{Γ ⊢ e : τ ; @bold{ψ}@subscript{+} | @bold{ψ}@subscript{-} ; o} 

The two ψ terms in the judgment are the logical propositions 
that hold when the term evaluates to a non-@racket[#f] value
(@bold{ψ}@subscript{+}) or @racket[#f] (@bold{ψ}@subscript{-}) 
and are key to successfully reasoning about interesting Racket 
programs.

By utilizing these propositions already present in the type system 
we can naturally extend TR's ability to describe how different values 
relate to one another by adding @bold{logical refinement types}
of the form {x : τ | ψ}. These types define a subset of τ where ψ 
holds, allowing us to describe a new class of types for functions
such as the following @racket[flexible-append]:

@racketblock[(define (flexible-append x y)
              (cond
                [(string? x) (string-append x y)]
                [else (append x y)]))]

This function is meant to take either two strings or lists
and will produce a result with the same type as the two
arguments. With our addition of refinements, we can now
precisely describe this function's behavior:

@racketblock[([x : (U (Listof Any) String)]
              [y : (U (Listof Any) String)
                 (or (and [x : (Listof Any)]
                          [y : (Listof Any)])
                     (and [x : String]
                          [y : String]))]
              -> 
              [result : (U (Listof Any) String)
                      (or (and [x : (Listof Any)]
                               [result : (Listof Any)])
                          (and [x : String]
                               [result : String]))])]

Since this is a potentially comment pattern (and the above
type is painfully verbose) we can introduce a simple 
combinator notation that describes a function whose 
argument(s) and return type(s) depend on one another:

@racketblock[(^-> 
              [String String -> String]
              [(Listof Any) (Listof Any) -> (Listof Any)])]

As an interesting aside, the lack of this feature has actually 
been a subtle point of confusion for many users learning TR.
While adding types to Racket code, many have assumed, for 
functions overloaded with several types, that TR would be able 
to fully reason about the dependent relations they 
intuitively saw while writing their code. Hopefully this 
feature not only adds expresiveness, but also helps bring
TR one step closer towards a point where programs may be 
gradually typed in the most natural way possible.


@section{With a side of integer inequalities, please!}

@centered{@emph{``God made the integers, all else is the work of man.'' 
                -- Leopold Kronecker}}

Discuss @bold{linear integer constraints}, similar approach to that
of Dependent ML @~cite[x-jfp-2007],  it's a lightweight, practical extension,
ties in perfectly with the logical flow of facts we already have,
doesn't add any huge dependencies but adds a lot of expressiveness. 
show quickly the below example, etc...

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