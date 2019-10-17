# semialign-diff

[![Build Status](https://travis-ci.org/qfpl/semialign-diff.svg?branch=master)](https://travis-ci.org/qfpl/semialign-diff)

The `Semialign` typeclass (from
[`semialign`](https://hackage.haskell.org/package/semialign)) lets us
line up two structures of the same type. It's then possible to take a
simple diff by comparing the points of overlap. This is most useful
with map-like types (`Map`, `HashMap`, `IntMap`).
