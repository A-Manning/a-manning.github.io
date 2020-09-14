---
title: Self-describing Numbers Part 2
---


===============================
Self-describing Numbers: Part 2
===============================

In my previous blog post, I introduced the self-describing numbers,
as well as some associated theorems and definitions.

In this blog post, I'll show how the non-repeating self-describing numbers can be efficiently found. To the best of my knowledge, I am the first to find such an algorithm.

The first observation is that when a self-describing number is written as an n-tuple of pairs, any permutation of these pairs is also a self-describing number.

This implies that we need only generate the sorted n-tuples of pairs that form self-describing numbers, because we can then compute their permutations to get the self-describing numbers.

The second observation is that when a non-repeating self-describing number is written as an :math:`n`-tuple of pairs,
the sum of the left elements of these pairs must be :math:`2n`.

In combinatrics, a *partition of an integer* :math:`m` is a sorted n-tuple of  positive integers such that the sum of the n-tuple is :math:`m`.

As an example, the partitions of :math:`4` are as follows:

* :math:`4`
* :math:`1,\ 3`
* :math:`2,\ 2`
* :math:`1,\ 1,\ 2`
* :math:`1,\ 1,\ 1,\ 1`

A partition of an integer :math:`m` *with* :math:`k` *parts* is a partition of :math:`m` of length :math:`k`.
For example, the :math:`3`-tuple :math:`(1,\ 1,\ 2)` is a partition of :math:`4` with :math:`3` parts.

When a non-repeating self-describing number in base :math:`b` is written as an :math:`n`-tuple of pairs, the left elements of the pairs must be a permutation of a partition of :math:`2n` with :math:`n` parts, and each part at most :math:`b-1`.

We can restrict our focus to computing only the possible partitions of :math:`2n` with :math:`n` parts, and each part at most :math:`b-1`, since we can then compute their permutations in order to find all possible :math:`n`-tuples that can occur as the left elements of pairs.

Consider the non-repeating self-describing numbers in base :math:`10`.
Any such number with :math:`6` digits in base :math:`10` will consist of :math:`3` pairs, where the left elements of these pairs is a permutation of a partition of :math:`6` with each part at most :math:`9`.

The partitions of :math:`6` with :math:`3` parts are:

* :math:`1,\ 1,\ 4`
* :math:`1,\ 2,\ 3`
* :math:`2,\ 2,\ 2`

When a non-repeating self-describing number in base :math:`b` is written as an :math:`n`-tuple of pairs, if a digit :math:`d` exists in the :math:`n`-tuple of left-elements of pairs with multiplicity :math:`m`, then the left element of the pair with right element :math:`d` must be :math:`m + 1`.

This means that we can exclude certain partitions.
Returning to our example of the non-repeating self-describing numbers in base :math:`10` with :math:`6` digits, consider those numbers where the left elements of pairs are :math:`1,\ 1,\ 4`.
Such a number must representable as a permutation of an n-tuple of the form :math:`[(1,\ x),\ (1,\ y),\ (4,\ z)]`.
Since the digit :math:`1` occurs in the :math:`3`-tuple of left elements with multiplicity :math:`2`, we know that the left element of the pair with right element :math:`1` must be :math:`2`. Since :math:`2` does not occur in the :math:`3`-tuple of left elements, we can conclude that there is no non-repeating self-describing number in base :math:`10` with :math:`6` digits such that the left elements are a permutation of the :math:`3`-tuple :math:`1,\ 1,\ 4`.
Similarly, we can exclude the :math:`3`-tuple :math:`2,\ 2,\ 2`, since the digit :math:`2` occurs with multiplicity :math:`3`, but the digit :math:`4` does not occur.

How about the :math:`3`-tuple :math:`1,\ 2,\ 3`? Consider the numbers represented as :math:`3`-tuples of the form :math:`[(1,\ x),\ (2,\ y),\ (3,\ z)]`. Since :math:`1`, :math:`2`, and :math:`3` all have multiplicity :math:`1` in the left, we would require that the digit :math:`2` occurs with multiplicity :math:`3` in the left. Therefore, we can exclude the :math:`3`-tuple :math:`1,\ 2,\ 3`, and conclude that there are no non-repeating self-describing numbers in base :math:`10` with :math:`6` digits.

We can express the above observation more generally;
When a non-repeating self-describing number in base :math:`b` is written as an :math:`n`-tuple of pairs, if there are :math:`d` digits in the :math:`n`-tuple of left-elements of pairs with multiplicity :math:`m`, then the digit :math:`m + 1` must occur with multiplicity :math:`d`.

Consider now the non-repeating self-describing numbers in base :math:`10` with :math:`8` digits.
The partitions of :math:`8` with :math:`4` parts are:

1. :math:`1,\ 1,\ 1, 5`
2. :math:`1,\ 1,\ 2, 4`
3. :math:`1,\ 1,\ 3, 3`
4. :math:`1,\ 2,\ 2, 3`
5. :math:`2,\ 2,\ 2, 2`

We can exclude partition 1 since the digit :math:`1` occurs with multiplicity :math:`3`, but :math:`4` does not occur.
We can exclude partition 2 since the digit :math:`1` occurs with multiplicity :math:`2`, but :math:`3` does not occur.
We can exclude partition 5 since the digit :math:`2` occurs with multiplicity :math:`4`, but :math:`5` does not occur.

Considering partition `3`, note that this partition corresponds to numbers that are permutations of the form :math:`[(3,\ 1),\ (3,\ 3),\ (1,\ x),\ (1, y)]`, where :math:`x` and :math:`y` are neither :math:`1` or :math:`3`.
Indeed any permutation of this form is non-repeating self-describing in base :math:`10`; Examples are :math:`31331219` and :math:`17153133`.

Considering partition `4`, note that this partition corresponds to numbers that are permutations of the form :math:`[(2,\ 1),\ (3,\ 2),\ (2,\ 3),\ (1, x)]`, where :math:`x` is neither :math:`1`, :math:`2`, or :math:`3`.
Any permutation of this form is non-repeating self-describing in base :math:`10`; Examples are :math:`23211632` and :math:`32142321`.

This strategy of computing non-repeating self-describing numbers in base :math:`b` with :math:`2n` digits by considering the partitions of :math:`2n` with :math:`n` parts and each part less than :math:`b` and then eliminating impossible partitions is remarkable effective. It is possible to express all of the non-repeating self-describing numbers in base :math:`10` as permutations of particular forms, given only a pen and paper, and a few minutes.

In my next blog post, I'll write a program that computes all non-repeating self-describing numbers in base :math:`b`, based on the algorithm expressed above.
