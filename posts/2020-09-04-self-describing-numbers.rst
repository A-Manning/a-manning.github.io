---
title: Self-describing Numbers
---


=======================
Self-describing Numbers
=======================

------------
Introduction
------------
The number :math:`23172132` has an interesting property;
when represented in base :math:`10`, it 'describes' it's own digits.
Reading left-to-right, there are

- two threes
- one seven
- two ones
- three twos

within the digits of :math:`23172132` (in base :math:`10`).

Other such examples in base :math:`10` include :math:`22` (containing two twos), :math:`4444` (containing four fours), and :math:`21212442` (containing two ones, two fours, and four twos).

Informally, :math:`x` is a self-describing number (in base :math:`b`) iff

- There is some :math:`k \in \mathbb{N}`,
  such that :math:`x` has :math:`2 k` digits
- :math:`\forall\ i < k \in \mathbb{N}`,
  the :math:`2i`-th digit is the multiplicity of the :math:`2i+1`-th digit
- If the digit :math:`d` appears in :math:`x`,
  then :math:`\exists\ i < k \in \mathbb{N}` such that the :math:`2i+1`-th digit is :math:`d`

where digits are indexed left-to-right, counting from :math:`0`.

In a series of blog posts,
I will state and possibly prove various properties about the self-describing numbers,
and discuss how we can find self-describing numbers.

-----------------
Formal Definition
-----------------

With a little number theory,
we can give a more rigorous definition of the self-describing numbers.
The following observations will be helpful:

- A number :math:`x` in base :math:`b` has :math:`\lfloor \log_b x \rfloor + 1` digits. Here we extend :math:`\log_b` such that :math:`\lfloor \log_b 0 \rfloor = 0`.
- The :math:`i`-th digit of :math:`x`, **counting from the right**, expressed as :math:`d_i(x)`, can be calculated using the formula

  .. math::
    d_i(x) = \frac{x \mod b^{i+1} - x \mod b^i}{b^i}

We can now define the multiplicity of the digit :math:`\delta` in :math:`x`,
which we can denote as :math:`m(\delta,\ x)`, such that

.. math::
  m(\delta,\ x) =
  \sum^{\lfloor \log_b x \rfloor}_{i=0} [d_i(x) = \delta]

where the square brackets are `Iverson brackets<https://en.wikipedia.org/wiki/Iverson_bracket>`__.


Finally, we can precisely define the self-describing numbers.

:math:`x \in \mathbb{N}` is said to be *self-describing* in base :math:`b` iff

- :math:`\lfloor \log_b x \rfloor \mod 2 = 1`
- :math:`\forall\ i,\ m(d_{2i}(x),\ x) = d_{2i+1}(x)`
- :math:`\forall\ j,\ \exists\ i.\ d_j(x) = d_{2i}(x)`

The reader may wish to verify these conditions hold for the aforementioned exemplary self-describing numbers.

I'll also define a subset,
which I call the *non-repeating* self-describing numbers.
If :math:`x` is self-describing in base :math:`b`,
then it is non-repeating iff

- :math:`\forall\ i\ j.\ d_{2i}(x) = d_{2j}(x) \Longleftrightarrow i = j`.

The numbers :math:`23172132` and :math:`22` are both non-repeating self-describing numbers in base :math:`10`, but :math:`4444` and :math:`21212442` are only self-describing.

---------------------------
Alternative Representations
---------------------------

When working with the self-describing numbers, it is often advantageous to make use of an alternative representation.

Because the self-describing numbers always have an even number of digits,
we can express them as n-tuples of pairs, where

.. math::
  \begin{align*}
  &encode : \mathbb{N} \longrightarrow (\mathbb{N} \times \mathbb{N})^* \\
  &encode(x)_i = (d_{2i+1},\ d_{2i}) \\
  \\
  &decode : (\mathbb{N} \times \mathbb{N})^* \longrightarrow \mathbb{N} \\
  &decode(x) = \sum^{(length\ x) - 1}_{i=0} b^{2i}\ (b\ (x_i)_0 + (x_i)_1)
  \end{align*}

In this form,
we would represent the base :math:`10` number :math:`23172132` as
:math:`[(3,\ 2),\ (2,\ 1),\ (1,\ 7),\ (2,\ 3)]`.

This representation is useful because it allows us to conveniently express certain theorems regarding the self-describing numbers.

--------------
Basic theorems
--------------

An important theorem is that if :math:`x` encodes a self-describing number,
then any permutation of the pairs in :math:`x` also encode a self-describing number.

For example,
since we know that :math:`23172132` is self-describing in base :math:`10`,
all of the permutations of it's encoding also encode self-describing numbers:

- :math:`[(2,\ 1),\ (3,\ 2),\ (2,\ 3),\ (1,\ 7)]`
- :math:`[(2,\ 1),\ (3,\ 2),\ (1,\ 7),\ (2,\ 3)]`
- :math:`[(2,\ 1),\ (2,\ 3),\ (3,\ 2),\ (1,\ 7)]`
- :math:`[(2,\ 1),\ (2,\ 3),\ (1,\ 7),\ (3,\ 2)]`
- :math:`[(2,\ 1),\ (1,\ 7),\ (3,\ 2),\ (2,\ 3)]`
- :math:`[(2,\ 1),\ (1,\ 7),\ (2,\ 3),\ (3,\ 2)]`
- :math:`[(3,\ 2),\ (2,\ 1),\ (2,\ 3),\ (1,\ 7)]`
- :math:`[(3,\ 2),\ (2,\ 3),\ (2,\ 1),\ (1,\ 7)]`
- :math:`[(3,\ 2),\ (2,\ 3),\ (1,\ 7),\ (2,\ 1)]`
- :math:`[(3,\ 2),\ (1,\ 7),\ (2,\ 1),\ (2,\ 3)]`
- :math:`[(3,\ 2),\ (1,\ 7),\ (2,\ 3),\ (2,\ 1)]`
- :math:`[(2,\ 3),\ (2,\ 1),\ (3,\ 2),\ (1,\ 7)]`
- :math:`[(2,\ 3),\ (2,\ 1),\ (1,\ 7),\ (3,\ 2)]`
- :math:`[(2,\ 3),\ (3,\ 2),\ (2,\ 1),\ (1,\ 7)]`
- :math:`[(2,\ 3),\ (3,\ 2),\ (1,\ 7),\ (2,\ 1)]`
- :math:`[(2,\ 3),\ (1,\ 7),\ (2,\ 1),\ (3,\ 2)]`
- :math:`[(2,\ 3),\ (1,\ 7),\ (3,\ 2),\ (2,\ 1)]`
- :math:`[(1,\ 7),\ (2,\ 1),\ (3,\ 2),\ (2,\ 3)]`
- :math:`[(1,\ 7),\ (2,\ 1),\ (2,\ 3),\ (3,\ 2)]`
- :math:`[(1,\ 7),\ (3,\ 2),\ (2,\ 1),\ (2,\ 3)]`
- :math:`[(1,\ 7),\ (3,\ 2),\ (2,\ 3),\ (2,\ 1)]`
- :math:`[(1,\ 7),\ (2,\ 3),\ (2,\ 1),\ (3,\ 2)]`
- :math:`[(1,\ 7),\ (2,\ 3),\ (3,\ 2),\ (2,\ 1)]`

A second useful theorem is stated below:

.. math::
  \sum^{(length\ x) - 1}_{i=0} (x_i)_0\
  [\forall\ j < i.\ (x_i)_1 \neq (x_j)_1 ]
  = 2\ (length\ x)

In other words,
the sum of left elements of pairs is equal to twice the number of pairs,
where repeated pairs are only counted once.

With these two theorems in hand, one might attempt to prove that

- :math:`22` is the only number that is non-repeating self-describing in base :math:`10` when read left-to-right and right-to-left.
- If a number is self-describing in base :math:`10` when read left-to-right and right-to-left, then it contains only a single digit.
  Examples are :math:`22`, :math:`4444`, and :math:`88888888`.

The reader is welcome to contact me via email for help with these proofs.

In the next blog post in this series, I'll discuss an algorithm to find all non-repeating self-describing numbers.
