# Union-find data structure in Prolog
## A union-find algorithm implementation for SWI-Prolog

A union-find data structure is a data structure that tracks a set of elements partitioned into a number of disjoint (non-overlapping) subsets. It provides near-constant-time operations to add new sets, to merge existing sets, and to determine whether elements are in the same set.

## Installation (SWI-Prolog)

```prolog
?- pack_install(union_find).
```

## Usage

```
:- use_module(library(union_find)).
```

* **union_find/2** - initializes a new union-find structure with `n` elements.
* **make_set/2** - adds a new element to a union-find structure.
* **union/3** - merges two sets of a union-find structure.
* **union_all/2** - merges a list of sets of a union-find structure.
* **find/3** - finds the root of an element in a union-find structure.
* **find/4** - finds the root and the rank of an element in a union-find structure.
* **disjoint_sets/2** - gets a list of disjoint sets of a union-find structure.