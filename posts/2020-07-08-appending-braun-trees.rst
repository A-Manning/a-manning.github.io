---
title: Appending Braun Trees
---


=====================
Appending Braun Trees
=====================

`Braun trees<https://users.cs.northwestern.edu/~robby/courses/395-495-2013-fall/three-algorithms-on-braun-trees.pdf>`__ are excellent for simulating flexible arrays.
Braun trees are a purely functional data structure, supporting
:math:`\Theta(n)` conversions to/from lists,
:math:`\Theta(\log n)` random read/update, and
calculating the number of elements in :math:`\Theta(\log^2 n)`.
If the size of a tree is known, then insertion is :math:`\Theta(\log n)`.

What about appending Braun trees?
For most tree structures, appending trees of sizes :math:`m` and :math:`n` will be
:math:`\Theta(n\log(m))`.
For arrays, the performance is
:math:`\Theta(m+n)`, and for lists,
:math:`\Theta(m)`.
Since Braun trees can be converted to/from lists/arrays in
:math:`\Theta(n)`, there is a trivial algorithm to append Braun trees in
:math:`\Theta(m+n)` by converting the two trees to lists or arrays, appending them, and converting back to a Braun tree.

It is possible to do better! In this blog post, I will present a new algorithm for appending Braun trees, and derive the asymptotic complexity.



I'll start by defining a type to represent a Braun Tree, and proceed to define a few operations that will be useful later.

.. code-block:: haskell

  data BraunTree a = Leaf | Node a (BraunTree a) (BraunTree a)


------------------------
Cons-ing to a Braun Tree
------------------------

We can insert an element at the head of a Braun tree (AKA. 'cons') with a simple algorithm.
Note that when we Cons to a Braun tree, each of the elements in the tree with position
:math:`i` will need to end up in position
:math:`i+1`.
In a Braun tree, all of the elements with an odd position relative to the root will be in the left branch.

The left branch will become the new right branch;
the right branch will move to the new left branch, as will the old root. We cons the root to the old right branch to obtain the new left branch.

.. code-block:: haskell

  cons :: a -> BraunTree a -> BraunTree a
  cons x Leaf = Node x Leaf Leaf
  cons x (Node v l r) = Node x (cons v r) l

This algorithm is quite clearly
:math:`\Theta(\log n)`.

----------------------------------------------------------
Getting the Odd- and Even-Indexed Elements of a Braun Tree
----------------------------------------------------------

These algorithms are nothing new; they are both components of the algorithm used for `cons`.

.. code-block:: haskell

  odds :: BraunTree a -> BraunTree a
  odds Leaf = Leaf
  odds (Node _ l _) = l

  evens :: BraunTree a -> BraunTree a
  evens Leaf = Leaf
  evens (Node v _ r) = cons v r

It's very clear that `odds` is an
:math:`\Theta(1)` algorithm, whereas `evens` is
:math:`\Theta(\log n)`.

---------
Appending
---------

Consider the Braun tree that results from appending the Braun trees :math:`x` and :math:`y`, of sizes :math:`m` and :math:`n` respectively.

If :math:`m` is odd, then the even elements of :math:`y` will be found in the left of the result, and the odd elements of :math:`y` will be found in the right.
If :math:`m` is even, then the odd elements of :math:`y` will be found in the left of the result, and the even elements of :math:`y` will be found in the right.

In my implementation, I will assume the function

.. code-block:: haskell

  size :: BraunTree a -> Int

as described in Okasaki's `Three Algorithms on Braun Trees<https://users.cs.northwestern.edu/~robby/courses/395-495-2013-fall/three-algorithms-on-braun-trees.pdf>`__.

This leads us to the following algorithm:

.. code-block:: haskell

  -- `append' m x y` appends `y` to the tree `x` that is known to be of size `m`.
  append' :: Int -> BraunTree a -> BraunTree a -> BraunTree a
  append' 0 Leaf y = y
  appemd' 0 _ _ = undefined
  append' _ Leaf _ = undefined
  append' _ x Leaf = x
  append' m (Node v l r) y =
    if m `mod` 2 == 0 then
      Node v $ append' (m `div` 2) l (odds y) $ append' ((m-1) `div` 2) r (evens y)
    else
      Node v $ append' (m `div` 2) l (evens y) $ append' (m `div` 2) r (odds y)

  append :: BraunTree a -> BraunTree a -> BraunTree a
  append x = append' (size x) x

----------------------------------
Algorithmic Complexity of `append`
----------------------------------

To make this a bit easier to reason about, we will assume that :math:`m` and :math:`n` are of the forms :math:`2^a` and :math:`2^b` respectively.

We can represent the recurrence equation for the complexity of `append'`, :math:`T'(a, b)` as

.. math::

  \begin{align*}
    &T(0,\ \_) =& &1 \\
    &T(a,\ 0) =& &1 \\
    &T(a,\ 1) =& &a \\
    &T(a,\ b) =& &2T(a-1,\ b-1) + a
  \end{align*}

Let :math:`c := min(a,\ b)`, then we have that
:math:`T(a,\ b) = 2^c (b+2-c) - (b+2)`.

We can expand :math:`c` to write this as

.. math::
  T(a,\ b) =
    \begin{cases}
      2^{b+1} - 2(b + 1) + a &\ a \ge b\\
      (b+2)(2^a-1) - a2^a &\ b \ge a
    \end{cases}

We can thereby conclude that the complexity of `append'` is as follows:

.. math::
  \begin{cases}
      \Theta(\log m + n) &\ m \ge n\\
      \Theta(m \log(\frac{n}{m}) + m) &\ n > m
  \end{cases}

Finally, we can consider that the complexity of `append` is as for `append'`, but with an additional :math:`\log^2 m` to compute :math:`m`.

Therefore, the complexity of my `append` algorithm is

.. math::
  \begin{cases}
      \Theta(\log^2 m + n) &\ m \ge n\\
      \Theta(m \log(\frac{n}{m}) + m) &\ n > m
  \end{cases}

which is superior to the trivial :math:`\Theta(m + n)` algorithm based on conversion to/from lists/arrays.
