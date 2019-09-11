# kd-Tree MonCat

## usage

	npm run dev

## about


using bidirectional type checking, see this [tutorial](http://davidchristiansen.dk/tutorials/bidirectional.pdf)

monoidal category generated from a binary tree of
- compose :: (b->c, a->b) => a->c
- tensor :: (a->b, c->d) => (a+c -> b+d)
- identity :: 1 -> 1

we use kd-tree-ish algo to divide the diagram of boxes into a suitable tree representing the diagram

![](screenshot.png)

## About Bidirectional Typechecking

bidirectional type checking, instead of one typing judgement
`Gamma |- t : Ty`, we have two

- `checkTy` check that a term inhabits some type
- `inferTy` a means of synthesizing a type for some term

our language for smc

- box `TB :: pos * pos`
- identity: `TI :: (1;1)`
- compose `TC :: (b;c) -> (a;b) -> (a;c)`
- tensor `TT :: (a;b) -> (c;d) -> (a+c;b+d)`

*syntax directed* A syntax-directed system is one in which each *typing judgment*
has a unique derivation, determined entirely by the syntactic structure of
the term in question. Syntax-directed type systems allow the typing rules to
be considered as pattern-match cases in a recursive function over the term
being checked. [1](http://davidchristiansen.dk/tutorials/bidirectional.pdf)

a typing rule
```
Premise
----------
Conclusion
```
is *not syntax directed* when either

1. it's ambiguous in the conclusion
2. contains a premise that is not determined by the conclusion

---

### Example

Terms

```
 t ::= x,y,z..              variables
    | t t                   application
    | \x. t                 abstraction
    | true | false          boolean constants
    | if t then t else t    conditional expressions
```
Types
```
 ty ::= Bool               boolean type
    | ty -> ty             function type
```

typing judgement:

    \Gamma |- t : ty

"in context \Gamma, we judge term t to be of type ty$

bidirectional checking turns this judgment into two judgements

*inference judgement*

    \Gamma |- t ==> ty`

"t's type can be inferred to be ty in context Gamma"

`inferTy :: (ctx, t) => Ty`

*checking judgement*

    \Gamma |- t <== tau

"t can be checked to have the given type as an additional argument"

`checkTy :: (ctx, t, ty) => Bool`

- checking function calls inference function when it reaches a term whose type can be checked
- inference function calls checking function when it encounters a type annotation

exending the example to add type annotations:

```
    t ::=
      | ...
      | (t:ty)
```

variables can be inferred
```
inferType :: Ctx -> Term -> Maybe Type
inferType ctx (Var x) = lookup x ctx
```

etc.

---

