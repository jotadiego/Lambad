# Lambad
_A Lambda Calculus-like Programming Language, but Worse_

![It might seem complicated, but it actually is](https://github.com/jotadiego/Lambad/blob/main/img/spook.png)

## Contents
- [Concept (and a brief introduction to lambda calculus)](#concept-and-a-brief-introduction-to-lambda-calculus)
- [Even more lambda calculus concepts](#even-more-lambda-calculus-concepts)
- [Lambad flavors](#lambad-flavors)
- [Program structure and syntax](#program-structure-and-syntax)
    - [BNF](#bnf)
    - [Comments and whitespace](#comments-and-whitespace)
    - [Program context and expression identifiers](#program-context-and-expression-identifiers)
    - [Variable introduction](#variable-introduction)
    - [Functional application](#functional-application)
    - [Simple return values](#simple-return-values)
    - [Subprogram composition](#subprogram-composition)
        - [Compositions within a program](#compositions-within-a-program)
        - [Compositions as a return value](#compositions-as-a-return-value)
- [Shortened Lambad](#shortened-lambad)
    - [BNF for Shortened Lambad](#bnf-for-shortened-lambad)
- [LambadA - a visual representation for Lambad programs](#lambada---a-visual-representation-for-lambad-programs)
    - [Context representation](#context-representation)
    - [Applications](#applications)
    - [Compositions](#compositions)
- [Examples](#examples)
    - [Recursive functions](#recursive-functions)
    - [Turing-complete Combinators](#turing-complete-combinators)
    - [Church Encodings](#church-encodings)
        - [Booleans](#booleans)
        - [Natural numbers](#natural-numbers)
    - [Text and bytestreams](#text-and-bytestreams)
        - [Morse encoding](#morse-encoding)
        - [Numerical encoding](#numerical-encoding)
        - [Bit by bit](#bit-by-bit)
        - [Byte by byte](#byte-by-byte)

## Concept (and a brief introduction to lambda calculus)

**_Lambad_** is meant as an [esoteric](https://en.wikipedia.org/wiki/Esoteric_programming_language) (AKA _cursed_) programming language based on lambda calculus.

[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a mathematical formalism developed by Alonzo Church (widely considered to be one of the 'founding fathers of computer science') in the 1930's as a means to rigorously define the concept of 'computation'. This 'calculus' consists of a family of expressions (or terms) constructed using a certain syntax (more on that later) and a couple operations that allow us to transform a given expression into another (hopefully simpler) expression; a process known as reduction. Church intended to represent any computation (for instance, an algorithm for adding two numbers) as a lambda calculus expression which might then be reduced until we arrive to a term that can no longer be reduced, which is interpreted as the resulting value.

Although lambda calculus has a remarkably simple structure (terms are constructed using only 3 rules and solved using just 2 reduction operations) the kind of computations it is able to model is surprisingly large. In fact, the computing power of lambda calculus was proven to be equivalent to that of a rather famous model designed by one of Church's students, one Alan Turing. Turing machines, the basis for modern computers (and basically all programming languages except for a handful renegades) and lambda calculus were proven to be equivalent in the [Church-Turing thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis), which we might as well call the very foundation of computer science.

Lambda calculus sits at the core of many useful applications such as [functional programming](https://en.wikipedia.org/wiki/Functional_programming), as well as a number of niche or less useful applications, such as functional programming [_using Haskell_](https://xkcd.com/1312/). Many derivative formalisms build upon lambda calculus by adding features (as in _typed_ lambda calculus) or manage to achieve Turing-completeness with an even simpler structure (as in the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) or [Iota and Jot systems](https://en.wikipedia.org/wiki/Iota_and_Jot)).

To the best of my knowledge, _Lambad_ does not contribute any helpful features to standard lambda calculus and, if for whatever reason it does, it's not by design. That said, a Lambad term will often be shorter than the corresponding lambda expression (for example, the identity function `λx.x` might be written as just `:` in Lambad) and the way expressions are constructed by keeping a list of sub-expressions could possibly be make some derivations easier.

_Lambad_ expressions can also be represented visually using a scheme called **_LambadA_** (short for Lambad Art). In principle, a _LambadA_ graph could be an useful way of conceptualizing how an operation works, but in practice it's just an abstract squiggle hardly any more legible than the Lambad program it represents. Automatic conversion between _Lambad_ programs and _LambadA_ graphs _could_ be handled programatically, although there is no such implementation so far.

It must also be said that while lambda calculus is connected to Church and churches are places of worship, Lambad [consistently strays away from any good-intentioned god](## "a feature it shares with other lambda-calculus derivatives, very much including Haskell").

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

Given an arbitrary lambda expression which _might_ contain free variables, it's pretty simple to transform it into a Lambad-compatible fully qualified expression: just add abstractions for any free variable in the same way we transformed `f (f x)` into `λf.λx.(f (f x))`. This can be fairly useful: if know we're only using a certain set of variables and all of our expressions are meant to be fully-qualified, we could write the unqualified form with free variables _as a shorthand_: then _`f (f x)`_ wouldn't stand for the actual expression `f (f x)` but just by an abbreviation for `λf.λx.(f (f x))`. This sort of abbreviations are justifiedly not allowed in standard lambda calculus (where non-fully-qualified expressions such as `f (f x)` are valid) but they will be a major component of _Lambad_, where the requirement that all expressions be fully qualified prevents any ambiguity.

## Lambad flavors

There are two variants of Lambad, both are bad. Since they could be said to be two _flavors_ of the language, I considered naming them 'Bitter Lambad' and 'Salty Lambad', but I thought that was distasteful so I'll go with the more straightforward names 'Verbose Lambad' and 'Shortened Lambad', even if those names are rather _bland_.

Verbose Lambad fully reveals the underlying structure of the language and it might make the translation between Lambad and lambda expressions a bit less painful. On the other hand, as the name implies, this will usually result in a longer, more cumbersome code being required to construct a given expression.

By contrast, Shortened Lambad introduces some syntactic [sugar](## "bad syntactic sugar is sometimes called 'syntactic salt'; maybe I should have gone with the 'Bitter' and 'Salty' names after all") which allows for much shorter expressions although at the cost of a more opaque representations. This could make Shortened Lambad better to work with than Verbose Lambad, something that goes clearly against the objective of making an esoteric language. I'd conclude that Verbose Lambad is better because it is worse but this sentence has already confused me too much to make any conclussions.

As we're already nearly 20 paragraphs in without seeing any actual Lambad code, it shouldn't come as a surprise that I'll keep things verbose and go on to explaining Verbose Lambad. There is a good reason for this decision (something I couldn't say about other decisions such as creating this language): Verbose Lambad makes it clearer to see what is going on at each step. Once the verbose variant is introduced, learning the shorthands provided in the Shortened Lambad will be easy.

## Program structure and syntax

A Lambad program is composed of a series of statements which build on a structure we'll call a _context_ and a return statement that _usually_ will select an output value from the _context_.

The result of a Lambad program is a lambda calculus expression, which may then be reduced to perform the equivalent computation. Thus, it could be said that Lambad is first 'compiled' to a lambda expression before being run by operating on the latter.

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

### Functional application

Further expressions may be added to the context by applying an expression to another (or by applying an expression to itself, as in `λx.(x x)`).

This is done by referencing the first expression of the application (the function to apply), writing a dot **`.`**, referencing the second expression (the argument) and then writing a semicolong **`;`**. The resulting expression is subsequently appended to the context expression list.

References to the expression listed in the _n_ position within the context expression list are made by writing the corresponding number; for instance `0.0;` corresponds to applying the default variable to itself, `x x`. Using an _n_ larger than the last defined position in the list will result in an error. For subprograms negative indexes may be used to reference variables in an outer context, more on that later.

### Simple return values

An expression within the context might be returned as the result for the program by using a return statement. This statement marks the end of the program (or subprogram).

The expression to be returned is selected by indicating its position within the context expression list as discussed in the previous section. The return statement is comprised of a colon character **`:`** followed by the number representing the intended position within the list.

As discussed before, expressions are wrapped by abstractions for each variable tracked by the context.

The simplest Lambad program corresponds to the expression `λx.x` (the identity function) which can be written simply as `:0`. This returns the expression in the 0th position, which is automatically assigned to `x` and is subsequently bound by the abstraction `λx.x`.

Programs will typically return the last expression added to the expression list (as building additional terms ordinarily wouldn't be have any benefits). One notable exception to this can be found in programs which return a variable other than the last one being introduced. For instance, in order to build a _Lambad_ term for `λx.λy.x` it will be necessary to use the sequence `+:0`, selecting position `0` as the return value instead of the last defined position (`1`, corresponding to `λx.λy.y`).

### Subprogram composition

Variable introduction and functional application suffice to represent any fully-qualified lambda calculus expression where all abstractions (or, in easier terms, all the `λ` thingies) appear in the outermost positions. This is to say, terms expressable using those operations alone are equivalent to lambda terms entirely comprised of free variables which are later qualified by adding the required abstractions.

This leaves out terms where an abstraction appears within the scope of an application such as `λx.(x (λy.y))`. It should be noted that such term differs from `λx.λy.(x y)` (which we might construct with the operators defined above) as `λx.(x (λy.y))` has the effect of applying `x` (seemingly an operator between functions) to the identity function (`λy.y`) whereas `λx.λy.(x y)` just applies a function `x` to an argument `y`. Expressions with a top-level application such as `(λx.x) (λy.y)` are also impossible to construct with the above methods.

In order to incorporate these expressions, _Lambad_ introduces the concept of subprogram composition where two lambda expressions are defined in _Lambad_ and their application is introduced as a new _Lambad_ term.

A composition is denoted by opening a square bracket **`[`**, writing a _Lambad_ program for the first expression, adding the operator **`×`** (that's a [multiplication sign](https://graphemica.com/%C3%97), not an 'x'), writing a _Lambad_ program for the second expression and then closing the square brackets with **`]`**. Programs within compositions are known as 'subprograms'. Each subprogram works with its own context and returns a result qualified by its own variables. Subprograms might have subprograms of their own.

For instance, `[ :0 × :0 ]` corresponds to the composition of two identity functions: `(λx.x) (λy.y)` (or, equivalently, `(λy.y) (λz.z)`, the names used for each variable might be freely changed through α-conversion as long as structural relationships are preserved).

Compositions might be used within a program or as return value.

#### Compositions within a program

Including a composition statement within a program adds the resulting expression (the application of the two expression defined by the two subprograms) as a new element in the 'parent' context's expression list. This new element might be returned (qualified by the variables in the parent context) or used to build further expressions through application.

For example, in the program `[ :0 × :0 ] : 1` the composition of identity functions `(λy.y) (λz.z)` is added to the expression list in position 1 and then returned as a result. As in all other cases, the result is wrapped with the abstractions for the variables in the parent context, yielding `λx.((λy.y) (λz.z))`.

This allows us to construct an expression directly mapping to `λx.(x ((λy.y) (λz.z)))` which we might write as `[ :0 × :0 ] 0.1; : 2` (the composition followed by an application of the 0th term, `x`, to the term in position 1) but it still doesn't provide any direct way of constructing an expression such as `λx.(x (λy.y))`, with an application between a variable and a single abstraction. _Lambad_ does **not** provide a way to construct such an expression but it should be noted that the two expressions are, in fact, equivalent. The composition has the identity function applied to itself, this _reduces_ to an identity function such as `λy.y`. In general, a computational equivalent to any `λx.E` (where `E` is an stand-in for an arbitrary expression) can be introduced to the expression list as a composition of the identity and `λx.E`.

There is yet another sort of expression that isn't covered by the structures described up to this point: consider the lambda term `λx.((λy.y x) (λz.x z))`. In this expression, the variable `x`, defined in the outermost context, occurs within the sub-expressions appearing in the composition `(λy.y x) (λz.x z)`. It can be further noted that no _Lambad_ program as reviewed so far is able to return the terms `λy.y x` or `λz.x z`, as they contain `x` as a free variable outside the scope of ttheir own contexts. In order to allow for such expressions, subprograms are permitted to reference variables with the context of their parent program. This is done by using negative indices. If the parent context tracks _n_ variables (internally accessible as positions _0_ to _n-1_ in the expression list), a negative index _-m_ from _-1_ to _-n_ will be used to reference the parent context variable internally available as _m-1_. For subprograms nested within other subprograms, variables in outer contexts are accessible by referencing values below _-n_.

Alternatively, it could be considered that subprograms are initialized with a context that includes all variables of the parent context within its own expression list, albeit with negative indexing.

Thus `λx.((λy.y x) (λz.x z))` may be represented as `[ 0.-1; :1 × -1.0; :1 ] : 1`, whose internals may be described as follows:

```
[               A composition is opened
    0.-1; :1    The default variable in the first subprogram is applied to the default variable from the parent context
  ×             Application of the first subprogram to the the second one
    -1.0; :1    The default variable from the parent context is applied to the default variable in the second subprogram
]               The composition is closed, its result is appended to the parent context's expression list in position one
: 1             The result of the composition is returned, qualified by the variables in the outer context
```

#### Compositions as a return value

In order to represent lambda expressions with an outermost application, _Lambad_ allows for compositions to be used as return values, simply by preceding the composition with a colon **`:`**. Thus, `: [ 0.-1; :1 × -1.0; :1 ]` is a valid Lambad program corrsponding to the lambda term `(λy.y) (λz.z)`, as opposed to the expression `λx.((λy.y) (λz.z))` we got in the previous section.

These are only kind of terms in _Lambad_ for which the corresponding lambda calculus term doesn't begin with an abstraction. It should be noted, however, that as long as each expression within the application is fully-qualified, so will be the resulting term.

As these terms do not make use of the context, there is no reason to add any statement to the 'main' program, as any expressions or variable introductions will be ignored.

## Shortened Lambad

The shortened variant of the language introduces syntactic sugar that alleviates some of the most repetitive parts of writing a Verbose Lambad code, although possibly at the cost of clarity.

Some of the most cumbersome aspects of Verbose Lambad include:
- Being based on lambda calculus, a formalism that was devised to explore theoretical considerations, not for practical algorithm-writing.
- Having to add multiple `+` characters at the beginning of a program in order to introduce variables.
- Expressions are built sequentially, with each step typically building off the previous one, yet we need to keep track of the index of each element.
- Similarly, programs will typically return the last element in the expression list, but we need to know its index in order to select it.
- Compositions will often use the identity function as its first expression so we will be writing those `[:1 × ___ ]` quite often.
- Too many semicolons; we could do without some in certain positions; certain combinations such as `;:` look ugly.

Shorteneed Lambad does _not_ address the first problem but provides useful shorthands for the rest:
- Variable introduction is made optional: variables are added to ensure that the highest index in the first statement (as long as it's an application or a return statement) references a variable. Expressions corresponding to each variable are still appended to the expression list, so a 'first statement' with an application referencing position 5 will result in 5 new variables being introduced in positions 1 to 5 and the application itself being appended to the list in position 6.
    - Examples:
        - `:2` references position 2 in its first statement, which requires 2 variables to be introduced. Equivalent to Verbose `++:2`, lambda term `λx.λy.λz.z`.
        - `0.1; : 2` references position 1 in its first statements, requires 1 variable to be introduced. Equivalent to Verbose `+ 0.1; : 2`, lambda term `λx.λy.(x y)`.
        - The lambda term `λx.λy.x`, `+:0` in Verbose Lambad, still requires variable introduction as the `:0` return statement doesn't indicate that a second variable is needed.
- Alternatively, the number of variables to be introduced might be indicated before a single `+`.
    - Examples:
        - `5+:0` is equivalent to Verbose `+ + + + + : 0`, lambda term `λx.λy.λz.λw.λv.λu.x`; optional variable introduction is not available in this case as the first statement only references position 0.
- A semicolon **`;`** can be omitted if directly preceding the characters **`:`** or **`[`**.
    - Examples:
        - `0.1:2` instead of `0.1;:2`. Equivalent to Verbose `+ 0.1; : 2`, lambda term `λx.λy.(x y)`.
- References to the last expression currently on the context can be omitted.
    - Examples:
        - The identity, `λx.x`, can be written as simply **`:`** instead of `:0` as position 0 is the last (and only) expression in the context.
        - `.:` corresponds to `0.0 : 1` (lambda `λx.x x`); the first term `0.0` can omit both references to `0` as position 0 is the last defined positions up to that point. This adds the expression `x x` to the context in position 1, which is referenced by the omitted return value.
        - Similarly, `.;.;.:` corresponds to `0.0;1.1;2.2;:3`, lambda `λx.(x (x (x x)))`.
        - `2.:` and `.2:` both correspond to `+ + 2.2; : 3`, lambda `λx.λy.λz.(z z)`. The explicit mention of position 2 allows for 2 variables to be introduced automatically, just as in `2.2:`.
- Subprogram composition with the identity as the first expression can be written as `[ second_subprogram ]`:
    - Examples:
        - `[:]0.:` is equivalent to `[: × :] 0.; :`, Verbose `[ : 0 × : 0 ] 0.1; : 2`, lambda `λx.(x ((λy.y) (λz.z)))` (computationally equivalent to `λx.(x (λy.y))`).
        - `[.:]0.:` is equivalent to `[: × .:] 0.; :`, Verbose `[ : 0 × 0.0; : 1 ] 0.1; : 2`, lambda `λx.(x ((λy.y) (λz.z z)))` (computationally equivalent to `λx.(x (λy.y y))`).

### BNF for Shortened Lambad

```bnf
<program>      ::= <vars> <ops> ":" <return_value>
<vars>         ::= "" | "+" <vars> | <natural> "+"
<ops>          ::= "" | <apply> <ops> | <compose> <ops> | <apply2> | <apply2> <compose> <ops>
<apply>        ::= <id> "." <id> ";"
<apply2>       ::= <id> "." <id>
<id>           ::= <natural> | "-" <natural> | ""
<natural>      ::= <digit> | <natural> <digit>
<digit>        ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
<compose>      ::= "[" <program> "×" <program> "]" | "[" <program> "]"
<return_value> ::= <id> | "[" <program> "×" <program> "]"
```

## LambadA - a visual representation for Lambad programs

As hinted by the section title, _**LambadA**_ (short for Lambad Art) is a visual representation of a Lambad program. The name is also fairly appropriate as [it will only make you cry](## "I'm honestly quite curious about the interesection between the set of people who read articles about esoteric functional programming languages and the set of people who can get a reference to a Brazilian song from the 1980's").

In theory, a Lambad program could be converted automatically into a _LambadA_ graph and _vice versa_ (which would be far more impressive), although implementing such a program falls well outside the scope of 'what I feel like doing anytime soon'.

The _LambadA_ representation of a _Lambad_ program closely follows the structure of the latter. Each expression within the context is represented by a vertical line with connections to related expressions (such as applications).

_LambadA_ representations are always based on the underlying Verbose Lambad program; any shorthands introduced by Shortened Lambad are not considered.

### Context representation

The initialization of the context of a program is indicated with a `T`-like mark. The vertical stroke of this mark will then branch into **as many vertical lines** as there are **variables** in the context (including both the default variable and any variables introduced with the `+` operator).

For instance, the contexts corresponding to an empty program (with one variable), `+` (two variables) and `++`  (three variables) will be represented as follows:

![Context headers](https://github.com/jotadiego/Lambad/blob/main/img/context_headers.png)

The _n_-th line from left to right corresonds to the expression in position _n-1_ (the leftmost line corresponds to the variable `x`, defined to be in position 0).

The **return value** is specified by adding a small square (a 'return box') at the end (bottom) of the line representing the returned expression.

For example, the following graphs correspond to the expressions `: 0` (`λx.x`, only one variable, which is returned), `+ : 1` (`λx.λy.y`, two variables, the variable in position 1 is returned) and `+ + : 1` (three variables, the variable in position 1 is returned).

![Simple examples](https://github.com/jotadiego/Lambad/blob/main/img/simple_examples.png)

### Applications

The application of an expression to another is marked by connecting the two lines for each involved expressions with a horizontal line (a 'joiner') and adding a new vertical line on the right (corresponding to the application expression which enters the context list in a new position). The new vertical line will be connected to either the top or the bottom of the joiner depending on the 'direction' of the application: if the expression on the left side of the application has a lower position (if the application goes 'from left to right') then the new line will branch from the bottom of the joiner. On the other hand, if the expression on the right side of the application has a higher position (if the application goes 'from right to left'), the line will branch from the bottom of the joiner instead. Applications of one expression to itself branch directly from the 'parent' expression, which is marked with a small loop.

Hopefully, a sample image will make explain this better than words. The graph on the left corresponds to the expression `+ 0.1; : 2` or `λx.λy.(x y)`: the left-side expression in the diagram (corresponding to the  `x` in position 0) is applied to the right-side expression (the `y` in position 1), a new line corresponding to the composition `x y` is added a position 2, branching from the bottom side of the joiner to indicate the left-to-right direction. Conversely, the graph in the middle corresponds to the expression `+ 1.0; :2` or `λx.λy.(y x)` where the expression in the highest position (`y`, position 1, on the right) is applied to an expression in a lower position (`x`, position 0, on the left); the line corresponding to the application `y x` branches from the top side of the joiner to indicate this right-to-left direction. The third graph, on the right, represents `0.0; : 1` or `λx.(x x)`, a term where an expression (`x`) is applied to itself; with the line for `x x` stemming off a loop on the line for `x`.

![Applications](https://github.com/jotadiego/Lambad/blob/main/img/applications.png)

### Compositions

A composition is graphed by representing the graph of each subprogram and then joining their _return_ boxes to a new vertical line representing the resulting expression.

The following examples represent the composition `:[:0 × :0]` or `(λx.(x x)) (λy.(y y))` (composition of the identity function to itself, returned as the result of the program), `[:0 × :0] : 1` or `λx.((λy.(y y)) (λz.(z z)))` (the same composition but qualified by a variable in its parent context) and `λx.(x ((λy.(y y)) (λz.(z z))))` (the outer variable `x` applied to the result of the composition).

![Compositions](https://github.com/jotadiego/Lambad/blob/main/img/Compositions.png)

## Examples

A few simple functions:

| Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|-------------------|----------------|------------------|
| Identity | `λx.x` | `: 0` | **`:`** |
| Choose first of two arguments | `λx.λy.x` | `+ : 0` | **`+:0`** |
| Choose second of two arguments | `λx.λy.y` | `+ : 1` | **`:1`** |
| Apply argument to itself | `λx.x x` | `0.0; : 1` | **`.:`** |
| Apply f to x | `λf.λx.f x` | `+ 0.1; : 2` | **`.1:`** |

![Examples](https://github.com/jotadiego/Lambad/blob/main/img/examples1.png)

### Recursive functions

Two recursive functions (the non-halting expression reduces to itself, it is the equivalent to an infinite loop in a Turing Machine; the Y combinator may be used to implement halting recursion with some cleverness).

| Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|-------------------|----------------|------------------|
| A non-halting expression | `(λx.x x) (λx.x x)` | `: [0.0; : 1 × 0.0; :1 ]` | **`:[.: × .:]`** |
| Y combinator | `λx.((λy. (x (y y))) (λz.x ((z z))))` | `[0.0; -1.1; :2 × 0.0; -1.1; :2] :1` | **`[.;-1.: × .;-1.:]:`** |

![Examples](https://github.com/jotadiego/Lambad/blob/main/img/recursive.png)

### Turing-complete Combinators

Some combinators which are known to form Turing-complete systems ([SKI](https://en.wikipedia.org/wiki/SKI_combinator_calculus), [Iota](https://en.wikipedia.org/wiki/Iota_and_Jot)).

| Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|-------------------|----------------|------------------|
| S combinator | `λx.λy.λz.((x z) (y z))` | `+ + 0.2; 1.2; 3.4; :5` | **`0.2;1.2;3.:`** |
| K combinator | `λx.λy.x` | `+ : 0` | **`+:0`** |
| I combinator | `λx.x` | `: 0` | **`:`** |
| Iota combinator | `λf.((f (λa.λb.λc.((ac)(bc)))) (λd.λe.d))` | `[ : 0 × + + 0.2; 1.2; 3.4; : 5] 0.1; [ :0 × + : 0] 2.3; : 4` | **`[0.2;1.2;3.:]0.[+:0]2.:`** |

![Examples](https://github.com/jotadiego/Lambad/blob/main/img/combinators.png)

The fact that these combinators can be implemented in Lambad is indicative that Lambad is Turing complete (something which was not a given considering that some lambda expressions lack a direct counterpart in Lambad).

### Church Encodings

The sequence of bits `11100010 10011011 10101010` correspond to the UTF-8 encoding of the Unicode character `U+26EA`, which is an emoji of a church ⛪.

Unrelated to that fact, [Church](https://en.wikipedia.org/wiki/Church_encoding) encodings are clever ways to represent values such as numbers and booleans in lambda calculus. As anyone should expect by this point, values are encoded as functions. Then functions that are normally defined for numbers or booleans can be defined for those functions, so you apply functions to functions to compute functions.

Unfortunately, as of November 2023, the _"Yo Dawg, I herd you like (noun X), so I put an (noun X) in your (noun Y) so you can (verb Z) while you (verb Z)."_ meme from 2007 has faded away from pop culture (that's for the better, no doubt), so what was an obvious joke when I was half my age would now be out of place and outdated. That's for the better, too.

#### Booleans

Back to Church encodings; it can be noted that the defining characteristic of **booleans** (values that can be either 'true' or 'false') is that only two values are possible. This is why the obvious Church encoding booleans is to use the functions `λx.λy.x` and `λx.λy.y` (which take two parameters and choose either the first or the second).

We've seen those functions in the examples before (in fact `λx.λy.x` also showed up as the K combinator). It doesn't really matter which function we choose for either value as long as we're consistent when defining other functions. Typically `λx.λy.x` is used for 'true' and ``λx.λy.y`` is used for 'false' but I feel like it's more natural for a programmer to do the opposite in _Lambad_, not only out of contrarianism, but also because that gives us expressions for false that have a 0 (universally understood as a representation of false in any universe without Ruby programmers) and expressions for 'true' that have a 1.

| Boolean  | Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|----------|-------------------|----------------|------------------|
| **False** | Choose first of two arguments | `λf.λt.f` | `+ : 0` | **`+:0`** |
| **True**  | Choose second of two arguments | `λf.λt.t` | `+ : 1` | **`:1`** |

Operations on Booleans can be defined taking advantage of this 'choose one or the other' behavior.

| Operation | Description | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|----------|-------------------|----------------|------------------|
| P **and** Q | If P is _true_, take Q, else take P | `λp.λq.((p p) q)` | `+ 0.0; 2.1; : 3` | **`+0.0;.1:`** |
| P **or** Q | If P is _true_, take P, else take Q | `λp.λq.((p q) p)` | `+ 0.1; 2.0; : 3` | **`.1;.0:`** |
| **not** P | If P is _true_, build a _false_ value, else build a _true_ | `λp.λf.λt.((p t) f)` | `+ + 0.2; 3.1; : 4` | **`0.2;.1:`** |

![Examples](https://github.com/jotadiego/Lambad/blob/main/img/Booleans.png)

In order to use these operators, we need to apply the expression for the operator to the expression for their arguments (in case of operators with multiple arguments, multiple successive applications will be needed). For instance, we can compute `true AND false` as `:[[+0.0;.1:×:1]×+:0]`:

```
:
[
    [
        +0.0;.1:    And
      ×
        :1          True
    ]
  ×
    +:0             False
]
```

This code corresponds to the lambda expression `((λp.λq.((p p) q)) (λf.λt.t)) (λf.λt.f)` which reduces to `λf.λt.f`, our representation for the _false_ value. Thus, we can assert that `true AND false` is equivalent to `false`, as we would expect.

More complex propositions such as `(P or Q) and not (P and Q)` can be made by combining the aforementioned operators:

```
+ We'll make two variables available for P and Q
[ (P or Q) and not (P and Q)
    [ (P or Q) and ____
        +0.0;.1:    And
      ×
        [  P or Q
            [ P or ____
                .1;.0:   Or
              ×
                :-1      P
            ]
          ×
            :-2     Q
        ]
    ]
    ×
    [ Not (P and Q)
        0.2;.1:     Not
        ×
        [ P and Q
            [ P and ___
                +0.0;.1:    And
              ×
                :-1      P
            ]
          ×
            :-2     Q
        ]
    ]
]
:    Return λP λQ ((P or Q) and not (P and Q))
```

Or, for short: `+[[+0.0;.1:×[[.1;.0:×:-1]×:-2]]×[0.2;.1:×[[+0.0;.1:×:-1]×:-2]]]:`.

#### Natural numbers

While the idea of an artificial language meant for international communication (what is known in the business as an 'auxilliary language' or _auxlang_ for short) is most commonly associated to Esperanto, a constructed language as practical for its intended purpose as _Lambad_ itself, the fact is that Esperanto is only one of several such proposals. The fact that you're reading this article in English and not Ido, Volapük or Communicationssprache can give you an adequate notion of how succesful such proposals were. Still, one curious proposal was that of _Latino sine flexione_, a much simplified form of Latin without noun cases and with relatively minimal verb conjugation. While a cool idea in theory, the language didn't gather much support and its creator, Italian mathematician Giusseppe Peano, is mostly known instead for developing the modern mathematical understanding of natural numbers among other notorious contribuitions to the field.

Peano's axiomatization gave us an inductive definition for natural numbers where any value can be constructed out of two elements: a value corresponding to the first natural _zero_ and a _successor_ function which we can apply repeatedly on the _zero_ value to construct any positive integer.

A Church encoding for naturals will use two variables corresponding to those elements, the _zero_ value and the _successor_ function. As it was the case when encoding booleans, it is necessary to pick a convention on what element will come first; both options will allows us to build equally powerful representations of natural numbers but they will impact on the way individual values are built as well as in the way any operations on natural numbers are defined. Being a little naïve, we'll easily believe that both options are just as good.

Picking the successor function as the first variable seems to be the most used convention, although that gives `λs.λz.z` as a representation for zero, which translates to Lambad as `:1`. The opposite order (using the variable we'll associate to the zero value first) gives us `λz.s.z` as a representation for zero, which becomes `+:0`, the same representation we used to encode the 'false'. The latter convention feels much more convenient when using this notation, even if it goes at odds with general usage in lambda calculus. Turns out that _Lambad_ goes does the opposite of good lambda, who could tell.

| Number  | Representation | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|----------|-------------------|----------------|------------------|
| **0** | 0 | `λz.λs.z` | `+ : 0` | **`+:0`** |
| **1**  | S(0) | `λz.λs.s z` | `+ 1.0; : 2` | **`1.0:`** |
| **2**  | S(S(0)) | `λz.λs.s (s z)` | `+ 1.0; 1.2; : 3` | **`1.0;1.:`** |
| **3**  | S(S(S(0))) | `λz.λs.s (s (s z))` | `+ 1.0; 1.2; 1.3; : 4` | **`1.0;1.;1.:`** |
| **4**  | S(S(S(S(0)))) | `λz.λs.s (s (s (s z)))` | `+ 1.0; 1.2; 1.3; 1.4; : 5` | **`1.0;1.;1.;1.:`** |
| **5**  | S(S(S(S(S(0))))) | `λz.λs.s (s (s (s (s z)))` | `+ 1.0; 1.2; 1.3; 1.4; 1.5; : 6` | **`1.0;1.;1.;1.;1.:`** |

As with booleans, we can define functions that operate on these representations.

| Operation | Lambda expression | Verbose Lambad | Shortened Lambad |
|-----------|-------------------|----------------|------------------|
| _**n + 1**_ | `λn.λz.λs.(s ((n z) s))` | `+ + 0.1; 3.2; 2.4; :5` | **`2+0.1;.2;2.:`** |
| _**m + n**_ | `λm.λn.λz.λs.((m ((n z) s)) s)` | `+ + + 1.2; 4.3; 0.5; 6.3; :7` | **`3+1.2;.3;0.;.3:`** |

Once we get to multiplication, we're forced to pay the price of our naïvity. It turns out there _was_ a good reason why 'successor first, then zero' is the most used convention: that allows us to work with partial results where only the successor is defined. Using the 'successor first, then zero' schema, multiplication can be written as `λm.λn.λs.(m (n s))` (`1.2;0.:` in Lambad). If there is a similarly convenient form of writing multiplication for the 'zero first, then successor' schema, I wasn't able to find it. Fortunately, there is a way to switch the order of the arguments for a function, we just have to use `λf.λa.λb.((f b) a)` (an operator that takes a function _f_, two arguments _a_ and _b_ and computes _f(b,a)_). The simplest solution I got for multiplication while keeping the 'zero first, then successor' order that yields nicer representations in Lambad was to use this trick in an expression equivalent to `λm.λn.λs.((λms.λmz.((m mz) ms)) ((λns.λnz.((n nz) ns)) s))`, which can be represented in Lambad as `2+[-1.1;.0:][-2.1;.0:].2;3.:`, constructed as follows:

```
2+  Three variables for m, n and s
[ An expression equivalent to (λms λmz ((m mz) ms)) is appended to the expression list in position three
    -1.1; m mz
    .0:   (m mz) ms
]
[ An expression equivalent to (λns λnz ((n nz) ns)) is appended to the expression list in position four
    -2.1; n nz
    .0:   (n nz) ns
]
.2; Equivalent to (λns λnz ((n nz) ns)) s
3.; Equivalent to (λms.λmz.((m mz) ms)) ((λns.λnz.((n nz) ns)) s)
:
```

When building a programming language to be used for practical purposes, having a much simpler multiplication (`1.2;0.:`) is well worth the cost of having a less elegant representation for numbers, so it would be logical to stick to the well-justified convention of using the 'successor first' order, even if it means that we'd have counterintuitive expressions such as `:1` for 'zero'. However, by this point there should be no doubts that logic and practicality are _not_ priorities in the design of Lambad. We'll stick to the 'zero first' order, with its cumbersome multiplication and slightly nicer numbers because I _like_ it that way.

### Text and bytestreams

Up to this point we've seen that Lambad is Turing-complete and able to manipulate values equivalent to booleans and integers. There is still one major problem, though: I've been calling Lambad a _programming language_ yet I haven't formally proved that it satisfies all the requirements needed for a programming language:

1. Turing-completeness.
2. Painful bugfixing.
3. You can write a 'Hello World' program in it.

The fact that we could implement the SKI combinators in Lambad is already a proof that the first requirement is fulfilled. You will have to take my word about the second one, but I'm quite confident that you'll have no problem believing it. This leaves the 'Hello World' issue.

Alonzo Church did not define a lambda calculus encoding for text, so there is no widely used encoding we could use to shoot ourselves in the foot by using a variant with the opposite conventions as everyone else.

There are various ways we could represent text, most of which involve writing a sequence of characters. Let's examine some alternatives which range from bad to very bad.

#### Morse encoding

Yep, we're starting with the 'very bad'.

Telegraph communication was the first form of text transmission by electronic means. Building a communication system using some lengthy wires and 19th century technology had its limitations, it's not immediately clear whether those were more severe than our self-imposed restriction of writing stuff in Lambad.

Morse code was by far the main method to encode text in telegraphs. There is a common misconception that Morse code uses only two symbols: the dot (or 'dit') and the dash (or 'dah'). While it is true that any letter or numeral can be represented as a sequence of _dits_ and _dahs_, this fails to take into account the fact that _pauses_ ('spaces') are needed to separate characters; otherwise we would be unable to distinguish between the letter 'W' `.--` and the sequence 'AT' `.- -`.

It could be argued that we should actually need a fourth symbol for the longer pause used to separate _words_, so as to distinguish between 'AT' `.- -` and 'A T' `.-  -` though we could approximate that by using two _space_ symbols. It could also be argued that two symbols are actually enough, we'd just need to consider the 'pixels' (or 'units') that compose each _dit_, _dah_ or _space_. Working with _dits_, _dahs_ and _spaces_, however, has the undeniable advantage that saying 'dit' and 'dah' is way more fun than using a more efficient representation.

In addition to our three symbols, we'll need a way to represent an empty string. Our encoding will use four variables: one representing the empty string and one representing a function that adds each symbol to the string (not unlike the way our _successor_ function added an unit to the zero value in our encoding for natural numbers). With this, we'll have:
- `λe.λdit.λdah.λs.e` or `3+:0` for the empty string.
- `λe.λdit.λdah.λs.dit e` or `3+1.0:` for the string `.` (the empty string followed by a dot, equivalent to the letter 'e').
- `λe.λdit.λdah.λs.dah e` or `3+2.0:` for the string `.` (the empty string followed by a dash, equivalent to the letter 't').
- `λe.λdit.λdah.λs.s e` or `3.0:` for the string ` ` (the empty string followed by a space).
- `λe.λdit.λdah.λs.dit (dit e)` or `3+1.0;1.:` for the string `..` (two dots, equivalent to the letter 'i').
- `λe.λdit.λdah.λs.dah (dit e)` or `3+1.0;2.:` for the string `.-` (two dots, equivalent to the letter 'a'). The lambda expression seems to show the 'dit' and the 'dah' in the wrong order as the function to append a new character to the end of the string must be placed on the left. By contrast, the `1` and `2` representing 'dits' and 'dahs' in Lambad _do_ appear in the right order, making Lambad notation considerably more convenient this time around.
- `λe.λdit.λdah.λs.(dit (dit (dah (s (dit (dit (dah (dit (s (dit (dah( (dit (s (dah (dah (dah (s (dah (dah (dit (s (s (dah (dah (dah (s (dit (dit (dah (dit (s (dit (s (dit (dit (dit (dit e)))))))))))))))))))))))))))))))))))))` or `3+1.0;1.;1.;1.;3.0;1.;3.;1.;2.;1.;1.;3.;2.;2.;2.;3.;3.;1.;2.;2.;3.;2.;2.;2.;3.;1.;2.;1.;3.;1.;2.;1.;1.;3.;2.;1.;1.:` for `.... . .-.. ---  .-- --- .-. .-.. -..`, equivalent to 'hello world'.
- `λe.λdit.λdah.λs.(dit (dit (dit (dah (dah (dah (dit (dit (dit e)))))))))` or `3+1.0;1.;1.;2.;2.;2.;1.;1.;1.:` for `...---...`, a code that will come in handy if you ever need to represent text in morse code using lambda calculus.

There are various downsides to using this encoding, the most obvious being the fact that the resulting expressions end up being excessively long. Other issue include the lack of distinction between lowercase and uppercase characters as well as the wider lack of Unicode support which, in our present day and age, is hard to justify (on good Samuel Morse's behalf, his code predates Unicode by over a century, so he can get away with it).

#### Numerical encoding

If we want full Unicode support, looking at the way Unicode text is encoded in actual computers is probably a good start. Unicode-compatible text is usually encoded using the UTF-8 standard, where each character is represented by a sequence of up to 4 bytes. While the algorithm for encoding a given character is well worth [taking a look](https://en.wikipedia.org/wiki/UTF-8#Encoding_process), just being able to encode an arbitrary sequence of bytes will be enough for our purposes and, in fact, will also provide a useful way to handle other kinds of binary-encoded data.

Of course, a byte isn't but a sequence 8 of binary digits (bits) which could be interpreted as a 8-digit binary number (a value ranging from 0 to 2⁸-1 = 255). Analogously, a sequence of _n_ bytes can be interpreted as a binary number with _8n_ digits, equivalent to a value ranging from 0 to 2⁸ⁿ-1.

Fortunately, we've already got a way to encode natural numbers! Unfortunately, Church encoding represents numbers in _unary_, requiring as many applications of the successor function as the value we want to represent, which grows by a factor of 256 for each new byte.

Latin alphabet letters and spaces use only 1 byte each in UTF-8, so "Hello world!" will be take 12 bytes. We could estimate the numerical interpretation of that string to be around 2⁹⁵, a number in the _octillions_ (that's a billion billion billions, well above the number of atoms in a human body). The amount of memory needed to represent either the lambda expression or the Lambad term for such a number far exceeds the total amount of data storage ever created.

Let's not do that.

#### Bit by bit

What if we did something similar to how we handled Morse code, except that we encoded each individual bit as a string of ones and zeroes instead of dits and dahs?

This time around it will be more convenient to encode the zero string using the _last_ variable in our expression, so that the Lambad variables in positions 0 and 1 can be used to represent zeroes and ones respectively.

Thus we'd have:
- `λzero.λone.λe.e` or `:2` for the empty string.
- `λzero.λone.λe.(zero e)` or `0.3:` for the bit sequence `0`.
- `λzero.λone.λe.(one e)` or `1.3:` for the bit sequence `1`.
- `λzero.λone.λe.(one (zero e)` or `0.3;1.:` for the bit sequence `01`.
- `λzero.λone.λe.(zero (zero (zero (one (zero (zero (one (zero e))))))))` or `0.3;1.;0.;0.;1.;0.;0.;0:` for the bit sequence `01001000`, which is the UTF-8 (and ASCII) representation of the capital letter 'H'.

It's clear that this method still takes to long, although it's able to represent any byte using only 8 applications, as opposed to the up to 255 required in pure numerical encoding. Furthermore, the number of applications will always be just 8 times the number of bytes, which means "Hello world!" will only take 96 applications as opposed to literal _octillions_.

A 'bit by bit' Lambad representation of the string 'Hello World!' may be written as follows:

```
0.2;1.;0.;0.;1.;0.;0.;0.; Bits for the character "H"
0.;1.;1.;0.;0.;1.;0.;1.;  Bits for the character "e"
0.;1.;1.;0.;1.;1.;0.;0.;  Bits for the character "l"
0.;1.;1.;0.;1.;1.;0.;0.;  Bits for the character "l"
0.;1.;1.;0.;1.;1.;1.;1.;  Bits for the character "o"
0.;0.;1.;0.;0.;0.;0.;0.;  Bits for the character " "
0.;1.;1.;1.;0.;1.;1.;1.;  Bits for the character "w"
0.;1.;1.;0.;1.;1.;1.;1.;  Bits for the character "o"
0.;1.;1.;1.;0.;0.;1.;0.;  Bits for the character "r"
0.;1.;1.;0.;1.;1.;0.;0.;  Bits for the character "l"
0.;1.;1.;0.;0.;1.;0.;0.;  Bits for the character "d"
0.;0.;1.;0.;0.;0.;0.;1:   Bits for the character "!"
```

Still, there is one much more convenient way to work with bytes in Lambad:

#### Byte by byte

So, what if instead of encoding bits one by one we encoded one byte at a time? That will require distinguishing between 256 values, so the representation of a single Latin script letter such as 'H' will take 256 variable introductions (which Shortened Lambad allows us to write as just `256+` or even omit complete) followed by one application. This seems considerably more complex than the 2 variable introductions and 8 applications needed on our 'bit by bit' schema but there's one crucial difference between the two methods: variable introductions are only needed once for the entire text, whereas the amount of applications depends on the number of characters.

For a text encoded as a sequence of _n_ bytes, the 'byte by byte' scheme will take _256 + n_ Lambad operations (256 variable introductions, _n_ applications), whereas the 'bit by bit' scheme will take _2 + 8n_ operations (2 variable introductions, _8n_ applications). Any text whose UTF-8 representation is at least 37 bytes long will take less Lambad operations when working by bytes. Shortened Lambad will allow us to hide away those 256 variable introductions, so even texts comprised of a single letter will _appear_ to have a simpler structure in the byte by byte scheme.

For instance, the uppercase letter 'H', corresponding to the byte 0x48 (`01001000` in binary, **72** in decimal) will be represented in Lambad as `72.256:`. This expression seems to be comprised of a single application between the expressions in positions 72 (corresponding to the desired byte) and position 256 (the empty string variable); the fact that position 256 is referenced is enough to hint that 256 new variables need to be introduced. The corresponding lambda expression, where all variables must be noted explicitly, results in the following abomination:

```
λx0.λx1.λx2.λx3.λx4.λx5.λx6.λx7.λx8.λx9.λx10.λx11.λx12.λx13.λx14.λx15.λx16.λx17.λx18.λx19.λx20.λx21.λx22.λx23.λx24.λx25.λx26.λx27.λx28.λx29.λx30.λx31.λx32.λx33.λx34.λx35.λx36.λx37.λx38.λx39.λx40.λx41.λx42.λx43.λx44.λx45.λx46.λx47.λx48.λx49.λx50.λx51.λx52.λx53.λx54.λx55.λx56.λx57.λx58.λx59.λx60.λx61.λx62.λx63.λx64.λx65.λx66.λx67.λx68.λx69.λx70.λx71.λx72.λx73.λx74.λx75.λx76.λx77.λx78.λx79.λx80.λx81.λx82.λx83.λx84.λx85.λx86.λx87.λx88.λx89.λx90.λx91.λx92.λx93.λx94.λx95.λx96.λx97.λx98.λx99.λx100.λx101.λx102.λx103.λx104.λx105.λx106.λx107.λx108.λx109.λx110.λx111.λx112.λx113.λx114.λx115.λx116.λx117.λx118.λx119.λx120.λx121.λx122.λx123.λx124.λx125.λx126.λx127.λx128.λx129.λx130.λx131.λx132.λx133.λx134.λx135.λx136.λx137.λx138.λx139.λx140.λx141.λx142.λx143.λx144.λx145.λx146.λx147.λx148.λx149.λx150.λx151.λx152.λx153.λx154.λx155.λx156.λx157.λx158.λx159.λx160.λx161.λx162.λx163.λx164.λx165.λx166.λx167.λx168.λx169.λx170.λx171.λx172.λx173.λx174.λx175.λx176.λx177.λx178.λx179.λx180.λx181.λx182.λx183.λx184.λx185.λx186.λx187.λx188.λx189.λx190.λx191.λx192.λx193.λx194.λx195.λx196.λx197.λx198.λx199.λx200.λx201.λx202.λx203.λx204.λx205.λx206.λx207.λx208.λx209.λx210.λx211.λx212.λx213.λx214.λx215.λx216.λx217.λx218.λx219.λx220.λx221.λx222.λx223.λx224.λx225.λx226.λx227.λx228.λx229.λx230.λx231.λx232.λx233.λx234.λx235.λx236.λx237.λx238.λx239.λx240.λx241.λx242.λx243.λx244.λx245.λx246.λx247.λx248.λx249.λx250.λx251.λx252.λx253.λx254.λx255.λxe.(x72 e)
```

The representation of 'Hello world!' can be constructed by indicating the decimal values for each of the 12 bytes in a fairly concise manner: `72.256;101.;108.;108.;111.;32.;119.;111.;114.;108.;100.;33.:`

```
72.256; Append byte  72 ("H") to the empty string
101.;   Append byte 101 ("e") to the last expression
108.;   Append byte 108 ("l") to the last expression
108.;   Append byte 108 ("l") to the last expression
111.;   Append byte 111 ("o") to the last expression
32.;    Append byte  32 (" ") to the last expression
119.;   Append byte 119 ("w") to the last expression
111.;   Append byte 111 ("o") to the last expression
114.;   Append byte 114 ("r") to the last expression
108.;   Append byte 108 ("l") to the last expression
100.;   Append byte 100 ("d") to the last expression
33.;    Append byte  33 ("!") to the last expression
:       Return last expression
```

This is equivalent to the following lambda monstrosity:

```
λx0.λx1.λx2.λx3.λx4.λx5.λx6.λx7.λx8.λx9.λx10.λx11.λx12.λx13.λx14.λx15.λx16.
λx17.λx18.λx19.λx20.λx21.λx22.λx23.λx24.λx25.λx26.λx27.λx28.λx29.λx30.λx31.λx32.
λx33.λx34.λx35.λx36.λx37.λx38.λx39.λx40.λx41.λx42.λx43.λx44.λx45.λx46.λx47.λx48.
λx49.λx50.λx51.λx52.λx53.λx54.λx55.λx56.λx57.λx58.λx59.λx60.λx61.λx62.λx63.λx64.
λx65.λx66.λx67.λx68.λx69.λx70.λx71.λx72.λx73.λx74.λx75.λx76.λx77.λx78.λx79.λx80.
λx81.λx82.λx83.λx84.λx85.λx86.λx87.λx88.λx89.λx90.λx91.λx92.λx93.λx94.λx95.λx96.
λx97.λx98.λx99.λx100.λx101.λx102.λx103.λx104.λx105.λx106.λx107.λx108.λx109.λx110.λx111.λx112.
λx113.λx114.λx115.λx116.λx117.λx118.λx119.λx120.λx121.λx122.λx123.λx124.λx125.λx126.λx127.λx128.
λx129.λx130.λx131.λx132.λx133.λx134.λx135.λx136.λx137.λx138.λx139.λx140.λx141.λx142.λx143.λx144.
λx145.λx146.λx147.λx148.λx149.λx150.λx151.λx152.λx153.λx154.λx155.λx156.λx157.λx158.λx159.λx160.
λx161.λx162.λx163.λx164.λx165.λx166.λx167.λx168.λx169.λx170.λx171.λx172.λx173.λx174.λx175.λx176.
λx177.λx178.λx179.λx180.λx181.λx182.λx183.λx184.λx185.λx186.λx187.λx188.λx189.λx190.λx191.λx192.
λx193.λx194.λx195.λx196.λx197.λx198.λx199.λx200.λx201.λx202.λx203.λx204.λx205.λx206.λx207.λx208.
λx209.λx210.λx211.λx212.λx213.λx214.λx215.λx216.λx217.λx218.λx219.λx220.λx221.λx222.λx223.λx224.
λx225.λx226.λx227.λx228.λx229.λx230.λx231.λx232.λx233.λx234.λx235.λx236.λx237.λx238.λx239.λx240.
λx241.λx242.λx243.λx244.λx245.λx246.λx247.λx248.λx249.λx250.λx251.λx252.λx253.λx254.λx255.
λxe.(x33 (x100 (x108 (x114 (x111 (x119 (x32 (x111 (x108 (x108 (x101 (x72 e)))))))))))))
```

And the equivalent Verbose Lambad:
```
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
72.256; 101.257; 108.258; 108.259; 111.260; 32.261; 119.262;
111.263; 114.264; 108.265; 100.266; 33.267; : 268
```

Unsurprisingly, the corresponding _LambadA_ graph isn't much nicer:

![Examples](https://github.com/jotadiego/Lambad/blob/main/img/lambad_helloworld.png)

Back to Church encodings, the church emoji ⛪ is represented in Unicode as a sequence of three bytes `0xE2 0x9B 0xAA` or, in decimal, `226, 155, 170`. Accordingly, the Lambad church encoding can be given as `226.256;155.;170:`.
