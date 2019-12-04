# Union-find data structure in Prolog
## A union-find algorithm implementation for SWI-Prolog

A union-find data structure is a data structure that tracks a set of elements partitioned into a number of disjoint (non-overlapping) subsets. It provides near-constant-time operations to add new sets, to merge existing sets, and to determine whether elements are in the same set.

## Installation (SWI-Prolog)

```prolog
?- pack_install(union_find).
```

## Usage

### Union-find with indices

```
:- use_module(library(union_find)).
```

This module uses `union_find/n` terms for the representation of union-find structures, where the `i`-th argument stores the root and the rank of the element `i` as a pair `Root-Rank`. For instance, the term `union_find(1-1, 1-0, 3-0)` represents a union-find structure with disjoint sets `[1, 2]` and `[3]`. The `union/3`, `union_all/2`, `find/[3-4]` and `disjoint_sets/2` predicates perform destructive assignments (see [setarg/3](https://www.swi-prolog.org/pldoc/doc_for?object=setarg/3)) that are undone if backtracking brings the state back into a position before the call.

Note that `find/[3-4]` predicates perform destructive assignments for **path compression**, that flattens the structure of the tree by making every node point to the root whenever `find/[3-4]` are used on it.

* **union_find/2** - initializes a new union-find structure as a term with `n` elements.
* **make_set/2** - adds a new element to a union-find structure.
* **union/3** - merges two sets of a union-find structure.
* **union_all/2** - merges a list of sets of a union-find structure.
* **find/3** - finds the root of an element in a union-find structure.
* **find/4** - finds the root and the rank of an element in a union-find structure.
* **disjoint_sets/2** - gets a list of disjoint sets of a union-find structure.

### Union-find with association lists

```
:- use_module(library(union_find_assoc)).
```

This module uses association lists for the representation of union-find structures, where each value is a pair `Root-Rank`. For instance, the term `t(b, a-0, -, t(a, a-1, -, t, t), t(c, c-0, -, t, t))` represents a union-find structure with disjoint sets `[a, b]` and `[c]`. The [association lists library](https://www.swi-prolog.org/pldoc/man?section=assoc) provides methods for creating, queriying and modifiying association lists in `O(log(n))` worst-case time.

Note that `find_assoc/[4-5]` predicates also produce new union-find structures for **path compression**, that flattens the structure of the tree by making every node point to the root whenever `find_assoc/[4-5]` are used on it.

* **union_find_assoc/2** - initializes a new union-find structure as an association list from a list of terms.
* **make_set_assoc/3** - adds a new element to a union-find structure.
* **union_assoc/4** - merges two sets of a union-find structure.
* **union_all_assoc/3** - merges a list of sets of a union-find structure.
* **find_assoc/4** - finds the root of an element in a union-find structure.
* **find_assoc/5** - finds the root and the rank of an element in a union-find structure.
* **disjoint_sets_assoc/2** - gets a list of disjoint sets of a union-find structure.