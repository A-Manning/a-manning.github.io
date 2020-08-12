---
title: Generalizing Biapplicatives
---

## Preliminaries

Applicative functors are pervasive in functional programming.
Lists, options, effects, and a myriad others.

The definition of an applicative functor (in Haskell) is worth reviewing.

```haskell
class Functor f => Applicative f where

  pure :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b
```

Why are `pure` and `(<*>)` bundled together like this?
Surely we could define a hierarchy, as with `Semigroup` and `Monoid`?

The difference here is that we need `pure`,
in order to define meaningful laws for `(<*>)`.

The applicative laws are as follows:

```haskell
-- Identity law
pure id <*> v = v

-- Homomorphism law
pure f <*> pure x = pure (f x)

-- Interchange law
u <*> pure y = pure ($ y) <*> u

-- Composition law
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

## Biapplicative functors

With that in mind, let's take a look at "biapplicative" functors.
Not so different!
```haskell
class Bifunctor p => Biapplicative p where

  bipure :: a -> b -> p a b

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
```

Biapplicative functors are a generalization of applicative functors that support two type parameters, instead of one.
Tuples are a familiar example of a type supporting a definition of `Biapplicative`.

```haskell
instance Biapplicative (,) where

  bipure = (,)

  (f, g) <<*>> (a, b) = (f a, g b)
```

What might be surprising is that it is *not* possible to define an instance of `Biapplicative` for `Either`.

```haskell
instance Biapplicative Either where
  -- We could try this with `Right` instead;
  -- Since `Either` is symmetric, there is no difference.
  bipure = Left . const

  (<<*>>) (Left f) (Left x) = Left (f x)
  (<<*>>) (Right g) (Right y) = Right (g y)
  -- There is no way to define these cases
  (<<*>>) (Left f) (Right y) =
  (<<*>>) (Right g) (Left x) =
```

We can, however, define `(<<*>>)` on `Maybe (Either a b)`:

```haskell
(<<*>>) (Just (Left f)) (Just (Left x)) = Just (Left (f x))
(<<*>>) (Just (Left _)) _ = Nothing
(<<*>>) (Just (Right g)) (Just (Right y)) = Just (Right (g y))
(<<*>>) (Just (Right _)) _ = Nothing
(<<*>>) Nothing _ = Nothing
```

What should we choose for `bipure`? All of the following seem reasonable:
```haskell
bipure = Left . const
bipure = Right . const
bipure = const (const Nothing)
```

At this point, it's worth looking into the "Biapplicative laws",
which will tell us if we can define an instance `Biapplicative Maybe (Either a b)`,
and perhaps resolve the choice of `bipure`.
The biapplicative laws are fairly intuitive -
they are almost the same as the applicative laws.

```haskell
-- Identity law
bipure id id <<*>> v = v

-- Homomorphism law
bipure f g <<*>> bipure x y = bipure (f x) (g y)

-- Interchange law
u <<*>> bipure x y = bipure ($ x) ($ y) <<*>> u

-- Composition law
bipure (.) (.) <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
```

At this point we can clearly see that no choice of `bipure` will satisfy the identity law or composition laws for `Maybe (Either a b)`.

## Generalizing Biapplicative Functors

The definition of `Applicative` includes `pure` so that we can state the applicative laws.
For the same reason, the definition of `Biapplicative` includes `bipure`.
But does it have to? What if we were to rethink `Biapplicative`, with an alternative to `bipure`?

```haskell
class Bifunctor p => Biapplicative p where

  pureL :: a -> p a b
  pureR :: b -> p a b

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
```

This definition is much nicer to work with;
The instance for `Maybe (Either a b)` is much as it was before:

```haskell
instance Biapplicative Maybe (Either a b) where

  pureL = Just . Left
  pureR = Just . Right

  (<<*>>) (Just (Left f)) (Just (Left x))   = Just (Left (f x))
  (<<*>>) (Just (Left _)) _                 = Nothing
  (<<*>>) (Just (Right g)) (Just (Right y)) = Just (Right (g y))
  (<<*>>) (Just (Right _)) _                = Nothing
  (<<*>>) Nothing _                         = Nothing
```

What about the laws for this definition?
Since we have a left and right version of `pure` now,
there are a few more laws:

```haskell
-- Identity laws
pureL id <<*>> v = v
pureR id <<*>> v = v

-- Homomorphism laws
pureL f <<*>> pureL x = pureL (f x)
pureR g <<*>> pureR y = pureR (g y)

-- Interchange laws
u <<*>> pureL x = pureL ($ x) <<*>> u
u <<*>> pureR y = pureR ($ y) <<*>> u

-- Composition laws
pureL (.) <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
pureR (.) <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
```

Unfortunately our instance still won't satisfy the identity laws.
What we really need is two versions of `(<<*>>)`, in the same vein as how `Bifunctor` has two versions of `Functor`'s `<$>`.

```haskell
class Bifunctor p => Biapplicative p where

  pureL :: a -> p a b
  pureR :: b -> p a b

  applyL :: p (a -> b) (c -> c) -> p a c -> p b c
  applyL = (<<*>>)

  applyR :: p (a -> a) (b -> c) -> p a b -> p a c
  applyR = (<<*>>)

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
  (<<*>>) p = (first (const id) p `applyR`) . (second (const id) p `applyL`)
```

Now we can write some more laws:

```haskell
-- Identity laws
pureL id `applyL` v = v
pureR id `applyR` v = v

-- Homomorphism laws
pureL f `applyL` pureL x = pureL (f x)
pureR g `applyL` pureR y = pureR (g y)
pureL f `applyR` pureL x = pureL (f x)
pureR g `applyR` pureR y = pureR (g y)

-- Interchange laws
u <<*>> pureL x = pureL ($ x) <<*>> u
u <<*>> pureR y = pureR ($ y) <<*>> u

-- Composition laws
pureL (.) <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
pureR (.) <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
```

These laws are effectively the applicative functor laws but mirrored for each "side" of our biapplicative.

Finally, we can write a *lawful* instance!
```haskell
instance Biapplicative Maybe (Either a b) where
  pureL = Just . Left
  pureR = Just . Right

  applyL (Just (Left f)) (Just (Left x))   = Just (Left (f x))
  applyL (Just (Left _)) (Just (Right y))  = pureR y
  applyL (Just (Left _)) Nothing           = Nothing
  applyL (Just (Right g)) (Just (Right y)) = Just (Right (g y))
  applyL (Just (Right _)) _                = Nothing
  applyL Nothing _                         = Nothing

  applyR (Just (Right g)) (Just (Right y)) = Just (Right (g y))
  applyR (Just (Right _)) (Just (Left x))  = pureL x
  applyR (Just (Right _)) Nothing          = Nothing
  applyR (Just (Left f)) (Just (Left x))   = Just (Left (f x))
  applyR (Just (Left _)) _                 = Nothing
  applyR Nothing _                         = Nothing
```

There's still a problem. Because we've replaced `bipure` with `pureL` and `pureR`, we can no longer define an instance of `Biapplicative (,)`.

I think that this is sufficiently motivating to split our `Biapplicative` definition into three parts.

```haskell
class Bifunctor p => Biapplicative p where

  applyL :: p (a -> b) (c -> c) -> p a c -> p b c
  applyL = (<<*>>)

  applyR :: p (a -> a) (b -> c) -> p a b -> p a c
  applyR = (<<*>>)

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
  (<<*>>) p = (first (const id) p `applyR`) . (second (const id) p `applyL`)

class Biapplicative p => Biapplicative1 p where
  pureL :: a -> p a b
  pureR :: b -> p a b

class Biapplicative p => Biapplicative2 p where
  bipure :: a -> b -> p a b
```

The laws for `Biapplicative1` and `Biapplicative2` are as we saw above,
using `pureL` and `pureR` to describe the `Biapplicative1` laws,
and `bipure` to describe the `Biapplicative2` laws.

Indeed, it is possible for a biapplicative functor to have instances of both `Biapplicative1` and `Biapplicative2`.

One such example is given below:
```haskell
data EitherBothNothing a b = B a b | L a | R b | N

instance Bifunctor EitherBothNothing where

  bimap f g = \case
    B x y -> B (f x) (g y)
    L x -> L (f x)
    R y -> R (g y)
    N -> N

instance Biapplicative EitherBothNothing where

  applyL (B f g) (B x y) = B (f x) (g y)
  applyL (B f _) (L x) = L (f x)
  applyL (B _ g) (R y) = R (g y)
  applyL (L f) (B x y) = B (f x) y
  applyL (L f) (L x) = L (f x)
  applyL (L _) (R y) = R y
  applyL (R g) (B _ y) = R (g y)
  applyL (R _) (L x) = N
  applyL (R g) (R y) = R (g y)
  applyL N _ = N
  applyL _ N = N

  applyR (B f g) (B x y) = B (f x) (g y)
  applyR (B f _) (L x) = L (f x)
  applyR (B _ g) (R y) = R (g y)
  applyR (L f) (B x _) = L (f x)
  applyR (L f) (L x) = L (f x)
  applyR (L _) (R _) = N
  applyR (R g) (B x y) = B x (g y)
  applyR (R _) (L x) = L x
  applyR (R g) (R y) = R (g y)
  applyR N _ = N
  applyR _ N = N

  (<<*>>) (B f g) (B x y) = B (f x) (g y)
  (<<*>>) (B f _) (L x) = L (f x)
  (<<*>>) (B _ g) (R y) = R (g y)
  (<<*>>) (L f) (B x _) = L (f x)
  (<<*>>) (L f) (L x) = L (f x)
  (<<*>>) (L _) (R _) = N
  (<<*>>) (R g) (B x y) = R (g y)
  (<<*>>) (R _) (L _) = N
  (<<*>>) (R g) (R y) = R (g y)
  (<<*>>) N _ = N
  (<<*>>) _ N = N

instance Biapplicative1 EitherBothNothing where

  pureL = L

  pureR = R

instance Biapplicative2 EitherBothNothing where

  bipure = B

```
