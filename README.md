# Lambad
_A Lambda Calculus-like Programming Language, but Worse_

## Concept (and a brief introduction to lambda calculus)

**_Lambad_** is meant as an [esoteric](https://en.wikipedia.org/wiki/Esoteric_programming_language) (AKA _cursed_) programming language based on lambda calculus.

[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a mathematical formalism developed by Alonzo Church (widely considered to be one of the 'founding fathers of computer science') in the 1930's as a means to rigorously define the concept of 'computation'. This 'calculus' consists of a family of expressions (or terms) constructed using a certain syntax (more on that later) and a couple operations that allow us to transform a given expression into another (hopefully simpler) expression; a process known as reduction. Church intended to represent any computation (for instance, an algorithm for adding two numbers) as a lambda calculus expression which might then be reduced until we arrive to a term that can no longer be reduced, which is interpreted as the resulting value.

Although lambda calculus has a remarkably simple structure (terms are constructed using only 3 rules and solved using just 2 reduction operations) the kind of computations it is able to model is surprisingly large. In fact, the computing power of lambda calculus was proven to be equivalent to that of a rather famous model designed by one of Church's students, one Alan Turing. Turing machines, the basis for modern computers (and basically all programming languages except for a handful renegades) and lambda calculus were proven to be equivalent in the [Church-Turing thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis), which we might as well call the very foundation of computer science.

Lambda calculus sits at the core of many useful applications such as [functional programming](https://en.wikipedia.org/wiki/Functional_programming), as well as a number of niche or less useful applications, such as functional programming [_using Haskell_](https://xkcd.com/1312/). Many derivative formalisms build upon lambda calculus by adding features (as in _typed_ lambda calculus) or manage to achieve Turing-completeness with an even simpler structure (as in the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) or [Iota and Jot systems](https://en.wikipedia.org/wiki/Iota_and_Jot)).

To the best of my knowledge, _Lambad_ does not contribute any helpful features to standard lambda calculus and, if for whatever reason it does, it's not by design. That said, a Lambad term will often be shorter than the corresponding lambda expression (for example, the identity function `λx.x` might be written as just `:` in Lambad) and the way expressions are constructed by keeping a list of sub-expressions could possibly be make some derivations easier.

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

### Composition as a return value

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
<vars>         ::= "" | "+" <vars> | <natural> "+" <vars>
<ops>          ::= "" | <apply> <ops> | <compose> <ops> | <apply2> | <apply2> <compose> <ops>
<apply>        ::= <id> "." <id> ";"
<apply2>       ::= <id> "." <id>
<id>           ::= <natural> | "-" <natural> | ""
<natural>      ::= <digit> | <natural> <digit>
<digit>        ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
<compose>      ::= "[" <program> "×" <program> "]"
<return_value> ::= <id> | <compose>
```

## Examples

A few simple functions:

| Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|-------------------|----------------|------------------|
| Identity | `λx.x` | `: 0` | **`:`** |
| Choose first of two arguments | `λx.λy.x` | `+ : 0` | **`+:0`** |
| Choose second of two arguments | `λx.λy.y` | `+ : 1` | **`:1`** |
| Apply argument to itself | `λx.x x` | `0.0; : 1` | **`.:`** |
| Apply f to x | `λf.λx.f x` | `+ 0.1; : 2` | **`.1:`** |

### Recursive functions

Two recursive functions (the non-halting expression reduces to itself, it is the equivalent to an infinite loop in a Turing Machine; the Y combinator may be used to implement halting recursion with some cleverness).

| Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|-------------------|----------------|------------------|
| A non-halting expression | `(λx.x x) (λx.x x)` | `: [0.0; : 1 × 0.0; :1 ]` | **`:[.: × .:]`** |
| Y combinator | `λx.((λy. (x (y y))) (λz.x ((z z))))` | `[0.0; -1.1; :2 × 0.0; -1.1; :2] :1` | **`[.;-1.: × .;-1.:]:`** |

### Turing-complete Combinators

Some combinators which are known to form Turing-complete systems ([SKI](https://en.wikipedia.org/wiki/SKI_combinator_calculus), [Iota](https://en.wikipedia.org/wiki/Iota_and_Jot)).

| Function | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|-------------------|----------------|------------------|
| S combinator | `λx.λy.λz.((x z) (y z))` | `+ + 0.2; 1.2; 3.4; :5` | **`0.2;1.2;3.:`** |
| K combinator | `λx.λy.x` | `+ : 0` | **`+:0`** |
| I combinator | `λx.x` | `: 0` | **`:`** |
| Iota combinator | `λf.((f (λa.λb.λc.((ac)(bc)))) (λd.λe.d))` | `[ : 0 × + + 0.2; 1.2; 3.4; : 5] 0.1; [ :0 × + : 0] 2.3; : 4` | **`[0.2;1.2;3.:]0.;[+:0]2.:`** |

The fact that these combinators can be implemented in Lambad is indicative that Lambad is Turing complete (something which was not a given considering that some lambda expressions lack a direct counterpart in Lambad).

### Church Encodings

The sequence of bits `11100010 10011011 10101010` correspond to the UTF-8 encoding of the Unicode character `U+26EA`, which is an emoji of a church ⛪.

Unrelated to that fact, [Church](https://en.wikipedia.org/wiki/Church_encoding) encodings are clever ways to represent values such as numbers and booleans in lambda calculus. As anyone should expect by this point, values are encoded as functions. Then functions that are normally defined for numbers or booleans can be defined for those functions, so you apply functions to functions to compute functions.

Unfortunately, as of November 2023, the _"Yo Dawg, I herd you like (noun X), so I put an (noun X) in your (noun Y) so you can (verb Z) while you (verb Z)."_ meme from 2007 has faded away from pop culture (that's for the better, no doubt), so what was an obvious joke when I was half my age would now be out of place and outdated. That's for the better, too.

#### Booleans

Back to Church encodings; it can be noted that the defining characteristic of **booleans** (values that can be either 'true' or 'false') is that only two values are possible. This is why the obvious Church encoding booleans is to use the functions `λx.λy.x` and `λx.λy.y` (which take two parameters and choose either the first or the second).

We've seen those functions in the examples before (in fact `λx.λy.x` also showed up as teh K combinator). It doesn't really matter which function we choose for either value as long as we're consistent when defining other functions. Typically `λx.λy.x` is used for 'true' and ``λx.λy.y`` is used for 'false' but I feel like it's more natural for a programmer to do the opposite in _Lambad_, not only out of contrarianism, but also because that gives us expressions for false that have a 0 (universally understood as a representation of false in any universe without Ruby programmers) and expressions for 'true' that have a 1.

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

#### Natural numbers

While the idea of an artificial language meant for international communication (what is known in the business as an 'auxilliary language' or _auxlang_ for short) is most commonly associated to Esperanto, a constructed language as practical for its intended purpose as _Lambad_ itself, the fact is that Esperanto is only one of several such proposals. The fact that you're reading this article in English and not Ido, Volapük or Communicationssprache can give you an adequate notion of how succesful such proposals were. Still, one curious proposal was that of _Latino sine flexione_, a much simplified form of Latin without noun cases and with relatively minimal verb conjugation. While a cool idea in theory, the language didn't gather much support and its creator, Italian mathematician Giusseppe Peano, is mostly known instead for developing the modern mathematical understanding of natural numbers among other notorious contribuitions to the field.

Peano's axiomatization gave us an inductive definition for natural numbers where any value can be constructed out of two elements: a value corresponding to the first natural _zero_ and a _successor_ function which we can apply repeatedly on the _zero_ value to construct any positive integer.

A Church encoding for naturals will use two variables corresponding to those elements, the _zero_ value and the _successor_ function. As it was the case when encoding booleans, it is necessary to pick a convention on what element will come first; both options will allows us to build equally powerful representations of natural numbers but they will impact on the way individual values are built as well as in the way any operations on natural numbers are defined.

Picking the successor function as the first variable seems to be the most used convention, although that gives `λs.λz.z` as a representation for zero, which translates to Lambad as `:1`. The opposite order (using the variable we'll associate to the zero value first) gives us `λz.s.z` as a representation for zero, which becomes `+:0`, the same representation we used to encode the 'false'. The latter convention feels much more convenient when using this notation, even if it goes at odds with general usage in lambda calculus. Turns out that _Lambad_ goes does the opposite of good lambda, who could tell.

| Number  | Representation | Lambda expression | Verbose Lambad | Shortened Lambad |
|----------|----------|-------------------|----------------|------------------|
| **0** | 0 | `λz.λs.z` | `+ : 0` | **`+:0`** |
| **1**  | S(0) | `λz.λs.s z` | `+ 1.0; : 2` | **`1.0:`** |
| **2**  | S(S(0)) | `λz.λs.s (s z)` | `+ 1.0; 1.2; : 3` | **`1.0;1.:`** |
| **3**  | S(S(S(0))) | `λz.λs.s (s (s z))` | `+ 1.0; 1.2; 1.3; : 4` | **`1.0;1.;1.:`** |
| **4**  | S(S(S(S(0)))) | `λz.λs.s (s (s (s z)))` | `+ 1.0; 1.2; 1.3; 1.4; : 5` | **`1.0;1.;1.;1.:`** |
| **5**  | S(S(S(S(S(0))))) | `λz.λs.s (s (s (s (s z)))` | `+ 1.0; 1.2; 1.3; 1.4; 1.5; : 6` | **`1.0;1.;1.;1.;1.:`** |

