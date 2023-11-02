# Lambad
_A Lambda Calculus-like Programming Language, but Worse_

## Concept (and a brief introduction to lambda calculus)

**_Lambad_** is meant as an [esoteric](https://en.wikipedia.org/wiki/Esoteric_programming_language) (AKA _cursed_) programming language based on lambda calculus.

[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a mathematical formalism developed by Alonzo Church (widely considered to be one of the 'founding fathers of computer science') in the 1930's as a means to rigorously define the concept of 'computation'. This 'calculus' consists of a family of expressions (or terms) constructed using a certain syntax (more on that later) and a couple operations that allow us to transform a given expression into another (hopefully simpler) expression; a process known as reduction. Church intended to represent any computation (for instance, an algorithm for adding two numbers) as a lambda calculus expression which might then be reduced until we arrive to a term that can no longer be reduced, which is interpreted as the resulting value.

Although lambda calculus has a remarkably simple structure (terms are constructed using only 3 rules and solved using just 2 reduction operations) the kind of computations it is able to model is surprisingly large. In fact, the computing power of lambda calculus was proven to be equivalent to that of a rather famous model designed by one of Church's students, one Alan Turing. Turing machines, the basis for modern computers (and basically all programming languages except for a handful renegades) and lambda calculus were proven to be equivalent in the [Church-Turing thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis), which we might as well call the very foundation of computer science.

Lambda calculus has sits at the core of many useful applications such as [functional programming](https://en.wikipedia.org/wiki/Functional_programming), as well as a number of niche or less useful application, such as functional programming [_using Haskell_](https://xkcd.com/1312/). Many derivative formalisms build upon lambda calculus by adding features (as in _typed_ lambda calculus) or which manage to achieve Turing-completeness with an even simpler structure (as in the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) or [Iota and Jot systems](https://en.wikipedia.org/wiki/Iota_and_Jot)).

To the best of my knowledge, _Lambad_ does not contribute any helpful features to standard lambda calculus and, if forever reason it does, it's not by design. That said, a Lambad term will often be shorter than the corresponding lambda expression (for example, the identity function `λx.x` might be written as just `:` in Lambad) and the way expressions are constructed by keeping a list of sub-expressions could possibly be make some derivations easier.

It must also be said that while lambda calculus is connected to Church and churches are places of worship, Lambad [consistently strays away from any good-intending god](## "a feature it shares with other lambda-calculus derivatives, very much including Haskell").

## Even more lambda calculus concepts

In order to [approach Lambad](## "not necessarily a good idea, be warned"), it will be useful to at least have a basic understanding of how lambda calculus expressions works. If you're already used to the topic, feel [free](## "hopefully no one is forcing you to read the rest of this article either") to skip this section over.

Fortunately, as mentioned above, lambda calculus is remarkably simple in its structure. A lambda expression (or [term](## "the two words are used equivalently, or at least _I_ do so")) can only take one of the following three forms:
- A _variable_ (usually represented by a letter such as `x` or `y`, it represents a certain value which could possibly be a function)
- An _application_ of one lambda expression to another. This is represented by writing one expression after the other, seperated by a space (you might also want to add brackets as well). For instance, `f x`  represents an application of a function `f` (represented as a variable) to a value `x` (another variable); what we'd write as _f(x)_ in maths. Keep in mind that applications can have more complex lambda terms on either side.
- An _abstraction_ which indicates an expression that can be computed for a certain variable. This can be understood as a definition for a function with a single variable. In maths, we might define a function by indicating what variable we're using and what we're doing with; for instance, in the identity function  _f(x) = x_ the left hand side (the _f(x)_) tells us we're using a variable _x_ while the right hand side (_= x_) tells us how we compute the result. Lambda abstractions do the same, except that they use the funky notation `λ VARIABLE . EXPRESSION` (in the case of the identity, _f(x) = x_ we get `λx.x`) and we require the 'body' of the function to also be a lambda expression. The expression within an abstraction is sometimes referred to as its 'body'. Keep in mind that the results might also be functions themselves; in general just be aware that [everything can and _will_ work as a function](https://en.wikipedia.org/wiki/First-class_function).

These constructor operations can be used repeatedly to create increasingly complex lambda terms. For instance, `λf.λx.(f (f x))` is a lambda expression which can be interpreted as an _apply twice_ opperator: it expects a function parameter `f`, a value `x` and 'returns' a `(f (f x))`, which in standard mathematical notation corresponds to `f(f(x))` (`f` applied twice to `x`). Observe that this longer expression can be built using the constructors as follows:
- `x` is a valid expression because it is a _variable_.
- `f` is a valid expression because it is a _variable_.
- `f x` is a valid expression because it is an _application_ of the expression `f` to the expression `x`.
- `f (f x)` is a valid expression because it is an _application_ of the expression `f` to the expression `f x`.
- `λx.(f (f x))` is a valid expression because it is an _abstraction_ with the variable `x` and the expression `f (f x)`.
- `λf.λx.(f (f x))` is a valid expression because it is an _abstraction_ with the variable `f` and the expression `λx.(f (f x))`.

This idea of constructing expressions sequentially will be important for _Lambad_.

The name (or symbol) used for a variable doesn't really matter, it's only the relationship between variables (their relative order, if you will) that matters: `λx.x` is the same as `λy.y`, `x (λy.λz.y)` is the same as `q (λw.λe.w)`, `λx.(x y)` is the same as `λy.(y x)`, but you couldn't freely replace `λx.(x y)` with `λx.(y x)` as that alters the relationship between the variables. Properly-made variable re-labelling is one of the operations defined by Church, known as α-conversion (that's _alpha_ conversion).

There are a number of subtleties in the way lambda expressions are used in computations (I'd very much encourage you to look [elsewhere](https://www.google.com/search?q=lambda+calculus) for in depth descriptions of how β-redexes and α-conversions work; they're considerably less scary than the Greek letters in their names would suggest), but the gist of it is that if can resolve the _application_ of an _abstraction_ to a certain argument by taking the body in the abstraction and replacing any occurrence of the variable with the argument. That is exactly the same as computing the value of a function in maths: if you have _f(x) = x² + x_, and you want to compute the value _f(3)_, you just have to replace the variable _x_ with _3_ in the body _x² + x_, which gives you _f(3) = 3² + 3 = 9 + 3 = 12_. In the same vein, `(λx.f x) (g y)` will _reduce_ to `f (g y)` because that's what we get by replacing the variable `x` with the argument `g y` in the body expression `f x`.

Programmers who haven't had to deal with functional programming (_yet_) might have noticed something odd: lambda calculus has _variables_ that might stand for a value (including functions!) but it has no way of _setting_ the value of a variable. This is by design; in lambda calculus we aren't concerned with with values, it's only functions and the way they might be composed that really matters. Back to the _apply twice_ example, we could have had just `f (f x)` to represent _f(f(x))_, but we aren't able to do anything with that expression as there's nothing to tell us how `f` and `x` might behave. That is why we're interested in including abstractions as in `λf.λx.(f (f x))`: this allows us to receive `f` and `x` as values which might have some internal structure (such as being abstractions or applications themselves) which might allow us to operate further.

The occurrences of an abstraction's variable within its body (such as `x` and `f` in `λf.λx.(f (f x))`) are said to be _bound_. By contrast, variables which are not bound within an abstraction (such as `f` in the intermediate step `λx.(f (f x))`) are said to be _free variables_. When handling computations, we're only interested in lambda expressions without any free variables ('fully qualified', which is why _Lambad_ is only able to build terms without free variables.

Given an arbitrary lambda expression which _might_ contain free variables, it's pretty simple to transform it into a Lambad-compatible fully qualified expression: just add abstractions for any free variable in the same way we transformed `f (f x)` into `λf.λx.(f (f x))`. This can be fairly useful: if know we're only using a certain set of variables and all of our expressions are meant to be fully-qualified, we could write the unqualified form with free variables _as a shortcut_: then _`f (f x)`_ wouldn't stand for the actual expression `f (f x)` but just by an abbreviation for `λf.λx.(f (f x))`. This sort of abbreviations are justifiedly not allowed in standard lambda calculus (where non-fully-qualified expressions such as `f (f x)` are valid) but they will be a major component of _Lambad_, where the requirement that all expressions be fully qualified prevents any ambiguity.

## Lambad flavors

There are two variants of Lambad, both are bad. Since they could be said to be two _flavors_ of the language, I considered naming them 'Bitter Lambad' and 'Salty Lambad', but I thought that was distasteful so I'll go with the more straightforward names 'Verbose Lambad' and 'Shortened Lambad', even if those names are rather _bland_.

Verbose Lambad fully reveals the underlying structure of the language and it might make the translation between Lambad and lambda expressions a bit less painful. On the other hand, as the name implies, this will usually result in a longer, more cumbersome code being required to construct a given expression.

By contrast, Shortened Lambad introduces some syntactic [sugar](## "bad syntactic sugar is sometimes called 'syntactic salt'; maybe I should have gone with the 'Bitter' and 'Salty' names after all") which allows for much shorter expressions although at the cost of a more opaque representations. This could make Shortened Lambad better to work with than Verbose Lambad, something that goes clearly against the objective of making an esoteric language. I'd conclude that Verbose Lambad is better because it is worse but this sentence has already confused me too much to make any conclussions.

As we're already nearly 20 paragraphs in without seeing any actual Lambad code, it shouldn't come as a surprise that I'll keep things verbose and go on to explaining Verbose Lambad. There is a good reason for this decision (something I couldn't say about other decisions such as creating this language): Verbose Lambad makes it clearer to see what is going on at each step. Once the verbose variant is introduced, learning the shortcuts provided in the Shortened Lambad will be easy.

## Program structure and syntax

A Lambad program is composed of a series of statements which build on a structure we'll call a _context_ and a return statement that _usually_ will select an output value from the _context_.

### BNF

If you know how to read a [Backus-Naur form (BNF)](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form), this specification should give you a good idea of how the Verbose variant of the language works. If you don't, don't worry, the following description should be enough.

```bnf
<program>      ::= <vars> <ops> ":" <return_value>
<vars>         ::= "" | "+" <vars>
<ops>          ::= "" | <apply> <ops> | <compose> <ops>
<apply>        ::= <id> "." <id> ";"
<id>           ::= <natural> | "-" <natural>
<natural>      ::= <digit> | <natural> <digit>
<digit>        ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
<compose>      ::= "[" <program> "×" <program> "]"
<return_value> ::= <id> | <compose>
```

Shortened Lambda allows for some variations not covered in this BNF.

### Comments and whitespace

Whitespace, line breaks, alphabetic letters and all non-numeric characters not included in the BNF are **ignored**, so a program corresponding to the Lambda expression `λx.λy.λz.λs.((x ((y z) s)) s)` could be given as `+++1.2;4.3;0.5;6.2;:7` in its simplest form, be padded with spaces for better legibility as `+ + + 1.2; 4.3; 0.5; 6.2; : 7` or be painstakingly annotated with commentaries as in the following block:

```
+ Introduce variable y in position one
+ Introduce variable z in position two
+ Introduce variable s in position three
1.2; Application y z in position four
4.3; Application (y z) s in position five
0.5; Application x ((y z) s) in position six
6.2; Application (x ((y z) s)) s in position seven
: 7 Return postion seven ~ λx.λy.λz.λs.(x ((y z) s))
```

That last example could be noted as being worryingly clear; fortunately Lambad's lack of practicality and cursedness is derived from the inherent cursedness of more complicated lambda calculus expressions and thus improved legibility is not too much of a concern for its status as an esolang.

### Program context and expression identifiers

The **context** of a Lambad conlang consists of two elements: an ordered, indexed list of variables which will be bound in any returned expressions and a list of expressions. Expressions listed in the content will appear to have free variables but will be qualified by the variables in the context.

Elements are never removed from the context, once added they'll remain available in that position; which makes it possible to reutilize one expression multiple times when building further expressions. At any one point, all 'intermediate steps' will be available.

All programs begin with a one variable and one expression defined in their context. _Lambad_ doesn't use names for variables, instead they are uniquely described by their positions within the list; for _convenience_ we'll say that this variable introduced by default is `x`. An expression using this variable (`x`, `λx.x` when qualified) is automatically added to the expression list in the 0-th position. This allows the programmer use the expression `x` when building new expressions by referencing its position: `0`. In a similar way, as new expressions are added to the context, each expression will be referenced by a positive integer indicating its position in the list.

Negative integers might also be used to reference expressions _outside_ the context. This will lead to an error when used within the 'main body' of a program, but will be useful for subprograms, as will be discussed later.

### Variable introduction

The **`+`** operator is used to introduce another variable to the context. Each time this operator is used, a new variable is added to the context's variable list and an expression consisting on that variable is appended to the expression list. For instance, after using a single `+` the context will keep track of two variables (which we may deem `x` and `y`) and its expression list will contain two expressions, namely `x` in position `0` and `y` in position `1`.

Expressions are qualified using all the variables tracked by the context. As a result, introducing a new variable affects all expressions within the context, including terms that do not use the new variable. For instance, while the expression in position 0 `x` was originally qualified as `λx.x`, after the `y` variable is introduced with `+` it goes on to be qualified as `λx.λy.x`. This is significant as `λx.x` and `λx.λy.x` represent different functions: the former (`λx.x`) is the identity function, returning the same argument it receives, the latter is a function that successively takes two arguments (`x` and `y`) and returns the first one it received (`x`) regardless of whatever value `y` might have.

Variable introduction is only allowed at the start of a program.


