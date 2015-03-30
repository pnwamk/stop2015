#lang scribble/sigplan @nocopyright @notimes 

@(require scribble/manual
          andmkent-bib
          scriblib/autobib
          scriblib/footnote)
          
@(define-cite ~cite citet generate-bibliography)

@(title "Enriching Typed Racket with Practical Dependent Types")

@(authorinfo "Andrew M. Kent" "Indiana University" "andmkent@indiana.edu")
@(authorinfo "Sam Tobin-Hochstadt" "Indiana University" "samth@indiana.edu")

Typed Racket is a statically-typed dialect of Racket that allows
idiomatic Racket programs to be enriched with types.  It can reason
about many dynamically typed programming patterns @emph{while}
providing sound interoperability and optimizations.  We have designed
and modeled an extension to Typed Racket's core calculus which will
add support for logical refinement types and linear integer
constraints. This abstract discusses our work implementing this novel
combination of precise specifications and optimizations which maintain
sound interoperability with dynamically typed code.

@section{Refining already logical types}

In order to typecheck dynamically typed idioms and provide sound
interoperability, Typed Racket utilizes a unique set of features. One
such feature is @emph{occurrence typing}, which allows different
occurrences of the same term in a program to be typechecked as
different types. This is accomplished by tracking the type-based
information (@emph{i.e. logical propositions}) implied by the results
of conditional tests:

@(racketblock
  (define (plus1 [x : (U Fixnum Float)])
    (if (fixnum? x)
        (fx+ x 1)
        (fl+ x 1.0))))

In @racket[plus1], @racket[x] is initially known to be of type
@racket[Fixnum] @emph{or} @racket[Float]. The logical propositions
implied by @racket[(fixnum? x)]'s result---@racket[(x : Fixnum)] and
@racket[¬(x : Fixnum)]---are then appropriately added to the type
environment while checking each branch, thus allowing the
type-specific versions of @racket[+] to typecheck.

By utilizing these logical typed-based propositions @emph{within
types} we can naturally extend Typed Racket to relate the types of
different values. To this end, we have added @bold{logical refinement
types} of the form {x : τ | ψ}.  This type defines a subset of type τ
where the logical proposition ψ holds, allowing us to more precisely
type many programs. For example, consider what a precise type for
@racket[plus1]'s might look like without these refinements:

@racketblock[(case->
              [Fixnum -> Fixnum]
              [Float -> Float]
              [(U Fixnum Float)
               -> (U Fixnum Float)])]

Using simple function overloading we can express how prior knowledge
about the argument type gives us a more specific return
type (@emph{e.g. @racket[plus1] is a unary operator over the
@racket[Fixnum] type}). However, this relation between input and
output is easily lost: calling @racket[plus1] with something of type
@racket[(U Fixnum Float)] means the fact that the return value is of
type @racket[Fixnum] iff the argument was of type @racket[Fixnum] is
forgotten. The typechecker chooses the first valid function type and
continues checking the rest of the program.

Using our logical refinements, however, we can exactly describe this
function's behavior:


@racketblock[([in : (U Fixnum Float)]
              -> 
              [out : (U Fixnum Float)
                   (or (and [in  : Fixnum]
                            [out : Fixnum])
                       (and [in  : Float]
                            [out : Float]))])]

Since this is a common pattern, we can use a spoonful of
syntactic-sugar to help users describe these sorts of dependent
functions:

@racketblock[(dependent-case->
              [Fixnum -> Fixnum]
              [Float -> Float])]

This allows for a convenient balance between precise specification and
readable, intuitive type definitions.

@section{Verifying numeric constraints}

In addition to supporting refinements that relate run-time values'
types, we will support a decidable subset of integer constraints
similar to those presented by Xi and Pfenning in Dependent ML
@~cite[xp-popl-1999]. With this addition, Typed Racket will be able to
automatically verify and eliminate many runtime checks for numeric
constraints. For example, in the program @racket[norm] @note{Racket's
@racket[for/sum] iterates like @racket[for], but each result of the
body expression is accumulated into a result with @racket[+]} below,
Typed Racket will be able to replace @racket[vector-ref] with its
runtime-check-free counterpart @racket[unsafe-vector-ref] since the
bounds-safety requirement can be statically guaranteed:

@racketblock[(define (norm [v : (Vectorof Real)])
               (sqrt (for/sum ([i (vec-len v)])
                       (square (vector-ref v i)))))]

Furthermore, our extension will allow developers to require the static
enforcement of integer constraints by explicitly including them in
types. This allows them to benefit from otherwise risky
optimizations---such as explicit uses @racket[unsafe-vector-ref]---in
a safe, statically verified fashion:

@racketblock[(define (safe-vec-ref
                         [v : (Vectorof Real)]
                         [i : Fixnum
                            (≤ 0 (- (vec-len v) 1))])
               (unsafe-vector-ref v i))]


It also provides a gradual process by which more of a program's
specification may be statically checked. Here we can see how the
typechecker can enforce the precondition for vector
@racket[dot-product] requiring the passed vectors be of equal length:

@racketblock[(define (dot-product [v1 : (Vectorof Real)]
                                  [v2 : (Vectorof Real)
                                      (= (vec-len v1)
                                         (vec-len v2))])
               (for/sum ([i (vec-len v1)])
                 (* (safe-vec-ref v1 i)
                    (safe-vec-ref v2 i))))]

Similary, by applying these refinements to the return types of
functions, such as @racket[plus1], we can more accurately describe
their behavior so other programs may successfully verify contexts in
which it is used:

@racketblock[(dependent-case->
              [[n : Fixnum] -> [m : Fixnum
                                  (= m (+ 1 n))]]
              [Float -> Float])]


@section{Playing nice with the dynamically typed world}

Because of the relatively simple nature of our dependent types, it is
easy to see the mapping between our type extensions and the well
studied dependent contracts already present in the Racket
@~cite[dthf-esop-2012]. For example, the dependent specification we
gave for @racket[plus1] is easy to express as a run-time contract:

@racketblock[(->i ([n (or/c fixnum? flonum?)])
                  [m (n) (or/c (and/c fixnum?
                                      (=/c (+ 1 n)))
                               flonum?)])]

Because of this, we anticipate being able to provide sound, performant
interoperability between traditional Racket modules and Typed Racket
modules utilizing dependent types with no additional overhead for the
user.

@section{Dependently typing the numeric tower}

Racket, like many dynamically typed languages in the Lisp tradition,
features a rich numeric tower. The developers of Typed Racket have
taken great care to ensure the types assigned to operations involving
this tower are appropriately expressive@~cite[sthff-padl-2012]. This
means that seemingly simple operations, such as @racket[+], may
actually have a quite specific (@emph{i.e. verbose}) type to allow
more programs to typecheck:

@racketblock[(case->
              (-> Pos-Byte Pos-Byte Pos-Index)
              (-> Byte Byte Index)
              (-> Index Pos-Index Pos-Fixnum)
              (-> Pos-Index Index Index Pos-Fixnum)
              ...
              (-> Number * Number))]

Because of the overlap between types in the numeric tower and refined
integers, we plan to explore the benefits and costs of using integer
refinements in place of some of these simpler nominal integer
subtypes (e.g. @racket[Byte]). It seems certain that more programs
will typecheck when utilizing the more detailed types integer
refinements can describe, but it is unclear how this will impact the
performance of typechecking large complex numeric libraries. Upon
completing our implementation of this system we plan to explore and
report on these costs.


@section{Related Works}

There is a history of using refinements and dependent types to enrich
already existing type systems. Our method borrows inspiration from Xi
and Pfenning, who added a practical set of dependent types to ML to
allow for richer specifications and compiler optimizations
@~cite[xp-popl-1999]. We similarly strive to provide an expressive yet
practical extension which preserves the decidability of typechecking
and requires no external support (@emph{e.g. an SMT solver}). Our
approach, however, is designed explicitly to reason about a
dynamically typed programming language and provide sound
interoperability with code that provides no static type-guarantees.

Chugh et al. have shown how extensive use of dependent refinement
types and an SMT solver can enable typechecking for complex,
real-world JavaScript programs @~cite[chj-oopsla-2012]. Our system's
usage of refinements to express dynamically typed idioms is similar,
but we use a different approach for typechecking programs: our system
is designed to allow for the direct expression of the mutually
dependent subtyping and proves relations instead of translating
problems into SMT-formatted queries. This allows us to express and
reason about subtyping in the more traditional way and prevents
potentially expensive external logical queries when relatively simple
subtyping checks will suffice. Additionally, since our system is more
tightly integrated with the compilation process, we can provide
optimizations and guarantees of sound interoperability that Dependent
JavaScript cannot.

Sage's use of dynamic runtime checks and static types is similar to
our approach for providing sound interoperability between dynamically
and statically typed values, however their usage of first-class
dependent types and arbitrary refinements means typechecking is
undecidable @~cite[ktgff-tech-2007]. Our system instead utlizes a less
expressive but decidable approach and forgoes the use of types as
first-class objects to maintain consistency between typed and untyped
Racket programs.


@generate-bibliography[]
