# Lambad
_A Lambda Calculus-like Programming Language, but Worse_

## Concept (and a brief introduction to lambda calculus)

**_Lambad_** is meant as an [esoteric](https://en.wikipedia.org/wiki/Esoteric_programming_language) (AKA _cursed_) programming language based on lambda calculus.

[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a mathematical formalism developed by Alonzo Church (widely considered to be one of the 'founding fathers of computer science') in the 1930's as a means to rigorously define the concept of 'computation'. This 'calculus' consists of a family of expressions (or terms) constructed using a certain syntax (more on that later) and a couple operations that allow us to transform a given expression into another (hopefully simpler) expression; a process known as reduction. Church intended to represent any computation (for instance, an algorithm for adding two numbers) as a lambda calculus expression which might then be reduced until we arrive to a term that can no longer be reduced, which is interpreted as the resulting value.

Although lambda calculus has a remarkably simple structure (terms are constructed using only 3 rules and solved using just 2 reduction operations) the kind of computations it is able to model is surprisingly large. In fact, the computing power of lambda calculus was proven to be equivalent to that of a rather famous model designed by one of Church's students, one Alan Turing. Turing machines, the basis for modern computers (and basically all programming languages except for a handful renegades) and lambda calculus were proven to be equivalent in the [Church-Turing thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis), which we might as well call the very foundation of computer science.

Lambda calculus has sits at the core of many useful applications such as [functional programming](https://en.wikipedia.org/wiki/Functional_programming), as well as number of niche or less useful application, such as functional programming [_using Haskell_](https://xkcd.com/1312/). Many derivative formalisms build upon lambda calculus by adding features (as in _typed_ lambda calculus) or which manage to achieve Turing-completeness with an even simpler structure (as in the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) or [Iota and Jot systems](https://en.wikipedia.org/wiki/Iota_and_Jot)).

To the best of my knowledge, _Lambad_ does not contribute any helpful features to standard lambda calculus and, if forever reason it does, it's not by design. That said, a Lambad term will often be shorter than the corresponding lambda expression (for example, the identity function `λx.x` might be written as just `:` in Lambad) and the way expressions are constructed by keeping a list of sub-expressions could possibly be make some derivations easier.

It must also be said that while lambda calculus is connected to Church and churches are places of worship, Lambad [consistently strays away from any good-intending god](## "a feature it shares with other lambda-calculus derivatives, very much including Haskell").

## Even more lambda calculus concepts

In order to [approach Lambad](## "not necessarily a good idea, be warned"), it will be useful to at least have a basic understanding of how lambda calculus expressions works.

Fortunately, as mentioned above, lambda calculus is remarkably simple in its structure. A lambda expression (or [term](## "the two words are used equivalently, or at least _I_ do so")) can only take one of the following three forms:
- A variable (usually represented by a letter such as `x` or `y`, it represents a certain value which could possibly be a function)
- An _application_ of one lambda expression to another. This is represented by writing one expression after the other, seperated by a space (you might also want to add brackets as well). For instance, `f x`  represents an application of a function `f` (represented as a variable) to a value `x` (another variable); what we'd write as _f(x)_ in maths. Keep in mind that applications can have more complex lambda terms on either side.
- An _abstraction_ which indicates an expression that can be computed for a certain variable. This can be understood as a definition for a function with a single variable. In maths, we might define a function by indicating what variable we're using and what we're doing with; for instance, in the identity function  _f(x) = x_ the left hand side (the _f(x)_) tells us we're using a variable _x_ while the right hand side (_= x_) tells us how we compute the result. Lambda abstractions do the same, except that they use the funky notation `λ VARIABLE . EXPRESSION` (in the case of the identity, _f(x) = x_ we get `λx.x`) and we require the 'body' of the function to also be a lambda expression. Keep in mind that the results might also be functions themselves; in general just be aware that [everything can and _will_ work as a function](https://en.wikipedia.org/wiki/First-class_function).
