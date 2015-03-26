#lang scribble/sigplan @nocopyright @notimes 

@(require scribble/manual
          andmkent-bib
          scriblib/autobib
          scriblib/footnote)
          
@(define-cite ~cite citet generate-bibliography)

@(title "Enriching Typed Racket with Practical Dependent Types")

@(authorinfo "Andrew M. Kent" "Indiana University" "andmkent@indiana.edu")
@(authorinfo "Sam Tobin-Hochstadt" "Indiana University" "samth@indiana.edu")

Typed Racket is a statically-typed dialect of Racket that 
allows idiomatic Racket programs to be enriched with types. 
It can reason about many dynamically typed programming 
patterns--- e.g. type-based control flow @~cite[thf-icfp-2010], 
a rich numereic tower @~cite[sthff-padl-2012], first-class 
classes @~cite[tsdthf-oopsla-2012]---while 
providing sound interoperability with untyped Racket programs.
We have designed and modeled an extension 
to Typed Racket's core calculus which adds support for logical 
refinement types and linear integer constraints. This abstract 
discusses our findings and current progress in implementing this
extension.

@section{Refining already logical types}

In order to typecheck dynamically typed idioms,
Typed Racket (TR) employs features not found in traditional languages.
One such feature is @emph{occurrence typing},
which allows different occurrences of the same
term in a program to be typechecked as different types. This is accomplished
by tracking the type-based information (@emph{i.e. logical propositions}) implied
by the results of conditional tests:

@(racketblock
  (define (foo [x : (U Number String)])
    (if (number? x)
        (add1 x)
        (string-length x))))

In @racket[foo], @racket[x] is initially known to be of type @racket[Number]
@emph{or} @racket[String]. The logical propositions implied by 
@racket[(number? x)]'s result---@racket[(x : Number)] and @racket[¬(x : Number)]---are
then appropriately added to the type environment while checking each branch.

By utilizing these logical typed-based propositions @emph{within types} we can naturally extend
TR to relate the types of different values. To this end, we have
added @bold{logical refinement types} of the form {x : τ | ψ}.
These types define a subset of type τ where the logical proposition ψ holds, 
allowing us to more precisely type many programs. For example, consider what a simplified
version of @racket[add1]'s type looks like without these refinements:

@racketblock[(case-> [Integer -> Integer]
                     [Float -> Float]
                     [(U Integer Float)
                      -> (U Integer Float)])]

Using simple function overloading we can express how prior knowledge
about the argument type gives us a more specific return type (@emph{e.g. add1 acts as a binary
operator for the type @racket[Integer]}). However, this relation between input and output is easily lost:
calling @racket[add1] with something of type @racket[(U Integer Float)] means the fact that the
return value is of type @racket[Integer] iff the argument was of type @racket[Integer] is
lost. The typechecker chooses the first valid function type and checks the rest of the program.

Using our logical refinements, however, can can exactly describe this function's behavior:


@racketblock[([in : (U Integer Float)]
              -> 
              [out : (U Integer Float)
                   (or (and [in  : Integer]
                            [out : Integer])
                       (and [in  : Float]
                            [out : Float]))])]

Since this is a common pattern, we can use a spoonful of
syntactic-sugar to help users describe these sorts of dependent
functions:

@racketblock[(dependent->
              [Integer -> Integer]
              [Float -> Float])]

This allows for a convenient balance between precise specification and
readable, intuitive type definitions.

@section{Verifying numeric constraints}

In addition to supporting refinements that relate run-time values' types,
we will support a decidable subset of integer constraints similar to those presented
by Xi and Pfenning in Dependent ML @~cite[xp-pldi-1998]. With this addition, TR
will be able to automatically verify and eliminate many runtime checks for
numeric constraints. For example, in the program @racket[L2-norm] below,
TR will be able to replace @racket[vector-ref] with its runtime-check-free
counterpart @racket[unsafe-vector-ref] since the bounds-safety requirement
can be statically guaranteed:

@racketblock[(define (L2-norm [v : (Vectorof Real)])
               (sqrt (for/sum ([i (range (vec-len v))])
                       (expt (vector-ref v i) 2))))]



Furthermore, our extension will allow developers to require the static
enforcement of integer constraints by explicitly including them in types. This
allows them to benefit from otherwise risky optimizations---such as explicit uses
@racket[unsafe-vector-ref]---in a safe, statically verified fashion:

@racketblock[(define (safe-vec-ref [v : (Vectorof Real)]
                                   [i : Integer
                                      (≤ 0 (- (len v) 1))])
               (unsafe-vector-ref v i))]


It also provides a gradual path by which more of a program's specification may be
statically checked. Here we can see how the typechecker can enforce the precondition
for vector @racket[dot-product] requiring the passed vectors be of equal length:

@racketblock[(define (dot-product [v1 : (Vectorof Real)]
                                  [v2 : (Vectorof Real)
                                      (= (vec-len v1)
                                         (vec-len v2))])
               (for/sum ([i (range (vec-len v1))])
                 (* (safe-vec-ref v1 i)
                    (safe-vec-ref v2 i))))]

Similary, by applying these refinements to the return types of functions,
such as @racket[add1], we can describe the function's behavior so
other programs may successfully verify contexts in which it is used:

@racketblock[(dependent->
              [[n : Integer] -> [n* : Integer
                                    (= n* (+ 1 n))]]
              [Float -> Float])]

@section{Dependently typing the numeric tower?}

Racket, like many dynamically typed languages in the Lisp tradition,
features a rich numeric tower. The developers of TR have taken great
care to ensure the types assigned to operations involving this tower
are appropriately expressive@~cite[sthff-padl-2012]. This means that
seemingly simple operations, such as @racket[+], may actually have a
quite specific (@emph{i.e. verbose}) type to allow more programs to
typecheck:

@racketblock[(case->
              (-> Pos-Byte Pos-Byte Pos-Index)
              (-> Byte Byte Index)
              (-> Index Pos-Index Pos-Fixnum)
              (-> Pos-Index Index Index Pos-Fixnum)
              ...
              (-> Number * Number))]

Because of the overlap between certain types in the numeric tower
and integers with linear refinements, we plan to explore the benefits
and costs of using integer refinements in place of some of these simpler
nominal integer subtypes (e.g. @racket[Byte]). It seems certain that
more programs will typecheck when utilizing integer refinements, but
there may be an interesting balance between the expresiveness of
integer refinements and the simplicity of nominal subtypes that is
only uncoverable through experimentation.


@section{Playing nice with the dynamically typed world}

Because of the relatively simple nature of our dependent types,
there should be a direct mapping between our type extensions 
and the well studied dependent contracts already present in the Racket
standard libraries @~cite[dthf-esop-2012]. This should allow us to provide
sound, performant interoperability between traditional Racket
modules and TR modules utilizing dependent types.


@section{Related Work}

With Dependently Typed JavaScript Chugh et al provide a method
for statically typechecking a dynamically typed language with a
clever usage of refinement types, ?, and an SMT solver
to typecheck the array of idioms found in their target language, JavaScript.

These other approaches are similar but we're the first @emph{gradually typed} 
approach in that our system will guarantee run-time sound interop with
completely untyped programs blah blah blah.

@~cite[chj-oopsla-2012]
@~cite[ktgff-tech-2007]

@generate-bibliography[]