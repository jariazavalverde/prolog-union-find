# Union-find data structure in Prolog
## A union-find algorithm implementation for SWI-Prolog

A union-find data structure is a data structure that tracks a set of elements partitioned into a number of disjoint (non-overlapping) subsets. It provides near-constant-time operations to add new sets, to merge existing sets, and to determine whether elements are in the same set.

This package provides an implementation of the union-find algorithm with the following features:

* **Path compression**: Path compression flattens the structure of the tree by making every node point to the root whenever a find predicate is used on it.
* **Union by rank**: Union predicates always attach the shorter tree to the root of the taller tree. Thus, the resulting tree is no taller than the originals unless they were of equal height, in which case the resulting tree is taller by one node.

## Installation (SWI-Prolog)

```prolog
?- pack_install(union_find).
```

## Usage

### Union-find with indices

```prolog
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

#### Creating union-find structures

```prolog
union_find(?UnionFind, +Size)
```
This predicate initializes a new `?UnionFind` structure of size `+Size`. A union-find structure consists of a term `union_find/(+Size)` with a number of elements each of which stores a parent pointer and a rank value. `union_find/2` takes `O(n)` time.

```prolog
make_set(+UnionFindIn, ?UnionFindOut)
```
This predicate makes a new set in `+UnionFindIn` by creating a new element with a unique id, a rank of `0`, and a parent pointer to itself. The parent pointer to itself indicates that the element is the representative member of its own set. `make_set/2` takes `O(n)` time.

#### Modifying union-find structures

```prolog
union(+UnionFind, +Element1, +Element2)
```
This predicate uses `find/4` to determine the roots of the trees `+Element1` and `+Element2` belong to. If the roots are distinct, the trees are combined by attaching the root of one to the root of the other. This predicate succeeds attaching the shorter tree (by rank) to the root of the taller tree in `+UnionFind`. **This predicate performs destructive assignments into `+UnionFind`.**

```prolog
union_all(+UnionFind, +Elements)
```
This predicate succeeds joining all the elements `+Elements` in the union-find structure `+UnionFind`. **This predicate performs destructive assignments into `+UnionFind`.**

#### Querying union-find structures

```prolog
find(+UnionFind, ?Element, ?Root)
```
This predicate follows the chain of parent pointers from `?Element` up the tree until it reaches a `?Root` element, whose parent is itself. `?Root` is the representative member of the set to which `?Element` belongs, and may be `?Element` itself. Path compression flattens the structure of the tree by making every node point to the root whenever `find/3` is used on it. **This predicate performs destructive assignments into `+UnionFind`.**

```prolog
find(+UnionFind, ?Element, ?Root, ?Rank)
```
Same as `find/3`, but returning also the `?Rank` of the `?Root`. **This predicate performs destructive assignments into `+UnionFind`.**

```prolog
disjoint_sets(+UnionFind, ?Sets).
```
This predicate succeeds when `?Sets` is the list of disjoint sets on the `+UnionFind` structure. **This predicate performs destructive assignments into `+UnionFind`.**


### Union-find with association lists

```prolog
:- use_module(library(union_find_assoc)).
```

This module uses association lists for the representation of union-find structures, where each value is a pair `Root-Rank`. For instance, the term `t(b, a-0, -, t(a, a-1, -, t, t), t(c, c-0, -, t, t))` represents a union-find structure with disjoint sets `[a, b]` and `[c]`. The [association lists library](https://www.swi-prolog.org/pldoc/man?section=assoc) provides methods for creating, querying and modifying association lists in `O(log(n))` worst-case time.

Note that `find_assoc/[4-5]` predicates also produce new union-find structures for **path compression**, that flattens the structure of the tree by making every node point to the root whenever `find_assoc/[4-5]` are used on it.

* **union_find_assoc/2** - initializes a new union-find structure as an association list from a list of terms.
* **make_set_assoc/3** - adds a new element to a union-find structure.
* **union_assoc/4** - merges two sets of a union-find structure.
* **union_all_assoc/3** - merges a list of sets of a union-find structure.
* **find_assoc/4** - finds the root of an element in a union-find structure.
* **find_assoc/5** - finds the root and the rank of an element in a union-find structure.
* **disjoint_sets_assoc/2** - gets a list of disjoint sets of a union-find structure.

#### Creating union-find structures (assoc)

```prolog
union_find_assoc(?UnionFind, +Elements)
```
This predicate initializes a new `?UnionFind` structure with a list of elements `+Elements` as keys.

```prolog
make_set_assoc(+UnionFindIn, +Element, ?UnionFindOut)
```
This predicate makes a new set by creating a new element with a unique id `+Element`, a rank of `0`, and a parent pointer to itself. The parent pointer to itself indicates that the element is the representative member of its own set.


#### Modifying union-find structures (assoc)

```prolog
union_assoc(+UnionFindIn, +Element1, +Element2, ?UnionFindOut)
```
This predicate uses `find_assoc/5` to determine the roots of the trees `+Element1` and `+Element2` belong to. If the roots are distinct, the trees are combined by attaching the root of one to the root of the other. This predicate succeeds attaching the shorter tree (by rank) to the root of the taller tree in `+UnionFindIn`.

```prolog
union_all_assoc(+UnionFindIn, +Elements, ?UnionFindOut)
```
This predicate succeeds joining all the elements of the list `+Elements` in the union-find structure `+UnionFindIn`, producing the union-find structure `?UnionFindOut`.

#### Querying union-find structures (assoc)


## License

Source code is released under the terms of the [BSD 3-Clause License](https://github.com/jariazavalverde/prolog-smtlib/blob/master/LICENSE).