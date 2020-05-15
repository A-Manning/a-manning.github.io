---
title: The Identity Monad
---

# The Identity Monad

There is an abundance of tutorials and blog posts discussing common monadic structures - lists, options, readers, writers, et cetera.
The identity monad, however, is often overlooked.
This is a shame, since it's not only an incredibly simple monad, but also one of the most frequently used!

The definition of the identity functor is a good starting point; note that we can type `id`{.haskell} as `(a -> b) -> id a -> id b`{.haskell}, which means that `fmap`{.haskell} is just `id`{.haskell} in the identity functor.

The identity applicative is similarly effortless - we can type `id` as `a -> id a` for `pure`, and then type `id` as `id (a -> b) -> id a -> id b` to use as `ap`.

The identity monad is only slightly more work - we'll have to use `flip id` for `bind`, giving it the type `id a -> (a -> id b) -> id b`. Of course, if we were to define the identity monad in terms of flipped `bind`, we could just use `id` again.

It's trivial to verify that the monad laws are satisfied here. So why doesn't Haskell have an instance for the identity monad?
The issue is that Haskell's type class system can't accept instances that aren't fully applied type constructors.
This restriction can't be avoided, even if `id` is defined at the type level with ` -XTypeSynonymInstances` and ` -XFlexibleInstances`.

This is unfortunate, as it leads to some notable duplication.
The ubiquitous function application operator `$` is identical to `<$>` in the identity monad - as well as `<*>` and `=<<`.
The Kleisli composition operator `<=<` from `Control.Monad`, better known as 'backwards fish', is typed as `(b -> id c) -> (a -> id b) -> a -> id c` in the identity monad - this is equivalent to the function composition operator `.` and its alias `compose`.
Considering how pervasive `$` and `.` are in Haskell, it's safe to say that the identity monad is one of the most frequently used, despite being incompatible with Haskell's type classes.

What a shame then, that an instance cannot be defined in Haskell - if it were possible, then Haskell's syntax could be cleaned up significantly, unifying `$` and `<$>`, as well as `.` and `<=<`.
Finally, `do`-notation would remove the need for `let..in` notation entirely!

Nevertheless, it is possible to define an instance for the identity monad in languages with more powerful type class/implicit resolution systems. For reference, here is an instance in Coq. I'm using the Std++ "standard library", because Coq's default standard library does not define classes for monads.

```coq
From stdpp Require Import prelude.

Instance id_fmap: FMap id := fun A B => id.

Instance id_mret: MRet id := fun A => id.

Instance id_mbind: MBind id := fun A B => id.
```
