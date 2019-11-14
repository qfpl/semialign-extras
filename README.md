# semialign-extras

![Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

[![Build Status](https://travis-ci.org/qfpl/semialign-extras.svg?branch=master)](https://travis-ci.org/qfpl/semialign-extras)

The `Semialign` typeclass (from
[`semialign`](https://hackage.haskell.org/package/semialign)) lets us
line up two structures of the same type. By combining this with the
`Filterable` and `Witherable` typeclasses from the
[`witherable`](https://hackage.haskell.org/package/witherable)
package, we can derive a number of useful diff/patch/merge-style
operations.

## Scope of the Library

`semialign-extras` aims to be a collection of interesting
abstractions/operations that:

1. Build on top of (at least) the `Semialign` typeclass, or related
   classes from the `semialign` universe; and

2. Do not belong inside other packages in the `semialign` universe.

### Rationale

`semialign` has a very lean dependency footprint, and its authors
intend to keep it that way. If your PR can work within the small
dependency footprint of `semialign`, it probably should be offered up
there first.

`semialign-extras` already depends on `lens`, so the marginal cost of
additional dependencies is likely to be small.
