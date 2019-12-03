/**
  * 
  * FILENAME: union_find.pl
  * DESCRIPTION: This module contains predicates for manipulating union-find structures.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-union-find
  * UPDATED: 03.12.2019
  * 
  **/



:- module(union_find, [
	union_find/2,
	make_set/3,
	union/3,
	find/3,
	find/4
]).

% union_find/2
% union(?UnionFind, +Size)
%
% This predicate initializes a new ?UnionFind structure of size +Size. A union-find structure consists of a
% term union_find/(+Size) with a number of elements each of which stores a parent pointer and a rank value.
% union_find/2 takes O(n) time.
union_find(UF, N) :-
	union_find(1, N, Args),
	UF =.. [union_find|Args].

% union_find/3
% union(?UnionFind, +LastID, +Size)
%
% NOT EXPORTED
union_find(_, 0, []).
union_find(I, N, [I-0|UF]) :-
	succ(M, N),
	succ(I, J),
	union_find(J, M, UF).

% make_set/2
% make_set(+UnionFindIn, ?UnionFindOut)
%
% This predicate makes a new set by creating a new element with a unique id, a rank of 0, and a parent pointer
% to itself. The parent pointer to itself indicates that the element is the representative member of its own set.
% make_set/2 takes O(n) time. It is 
make_set(UF0, UF1) :-
	UF0 =.. [union_find|Args0],
	length(Args0, N),
	succ(N, M),
	append(Args0, [M-0], Args1),
	UF1 =.. [union_find|Args1].

% union/3
% union(+UnionFind, +Element1, +Element2)
%
% This predicate uses find_with_rank/3 to determine the roots of the trees +Element1 and +Element2 belong to.
% If the roots are distinct, the trees are combined by attaching the root of one to the root of the other.
% This predicate succeeds attaching the shorter tree (by rank) to the root of the taller tree in +UnionFind.
union(UF, I, J) :-
	find_with_rank(UF, I, X, RankI),
	find_with_rank(UF, J, Y, RankJ),
	(X \== Y ->
		(RankI < RankJ -> setarg(X, UF, Y-RankI) ; 
			(RankI > RankJ -> setarg(Y, UF, X-RankJ) ; 
				setarg(Y, UF, X-RankJ),
				succ(RankI, SrankI),
				setarg(X, UF, X-SrankI))) ; true).

% find/3
% find(+UnionFind, ?Element, ?Root)
%
% This predicate follows the chain of parent pointers from ?Element up the tree until it reaches a ?Root element,
% whose parent is itself. ?Root is the representative member of the set to which ?Element belongs, and may be
% ?Element itself. Path compression flattens the structure of the tree by making every node point to the root
% whenever find/3 is used on it.
find(UF, I, X) :-
	arg(I, UF, J-R),
	(I == J -> X = J ; find(UF, J, X), nb_setarg(I, UF, X-R)).

% find/4
% find(+UnionFind, ?Element, ?Root, ?Rank)
%
% Same as find/3, but returning also the ?Rank of the ?Root.
find(UF, I, X, S) :-
	arg(I, UF, J-R),
	(I == J -> X = J, S = R ; find(UF, J, X, S), nb_setarg(I, UF, X-R)).