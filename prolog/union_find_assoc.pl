/**
  * 
  * FILENAME: union_find_assoc.pl
  * DESCRIPTION: This module contains predicates for manipulating union-find structures with association lists.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-union-find
  * UPDATED: 04.12.2019
  * 
  **/



:- module(union_find_assoc, [
	union_find_assoc/2,
	make_set_assoc/3,
	union_assoc/4,
	union_all_assoc/3,
	find_assoc/4,
	find_assoc/5,
	disjoint_sets_assoc/2
]).



% union_find_assoc/2
% union_find_assoc(?UnionFind, +Elements)
%
% This predicate initializes a new ?UnionFind structure with a list of elements +Elements as keys.
union_find_assoc(UF, List) :-
	list_to_set(List, Set),
	empty_assoc(Assoc),
	union_find_assoc(1, Set, Assoc, UF).

% union_find_assoc/3
% union_find_assoc(+LastID, +Set, +InitialAssoc, ?UnionFind)
%
% NOT EXPORTED
union_find_assoc(_, [], UF, UF).
union_find_assoc(I, [X|Xs], UF0, UF2) :-
	put_assoc(X, UF0, (X-0), UF1),
	succ(I, J),
	union_find_assoc(J, Xs, UF1, UF2).

% make_set_assoc/3
% make_set_assoc(+UnionFindIn, +Element, ?UnionFindOut)
%
% This predicate makes a new set by creating a new element with a unique id +Element, a rank of 0, and a parent pointer
% to itself. The parent pointer to itself indicates that the element is the representative member of its own set.
make_set_assoc(UF0, X, UF1) :-
	\+get_assoc(X, UF0, _),
	put_assoc(X, UF0, (X-0), UF1).

% union_assoc/4
% union_assoc(+UnionFindIn, +Element1, +Element2, ?UnionFindOut)
%
% This predicate uses find_assoc/5 to determine the roots of the trees +Element1 and +Element2 belong to.
% If the roots are distinct, the trees are combined by attaching the root of one to the root of the other.
% This predicate succeeds attaching the shorter tree (by rank) to the root of the taller tree in +UnionFindIn.
union_assoc(UF0, I, J, UF1) :-
	find_assoc(UF0, I, X, RankI, UF2),
	find_assoc(UF2, J, Y, RankJ, UF3),
	(X \== Y ->
		(RankI < RankJ -> put_assoc(X, UF3, Y-RankI, UF1) ; 
			(RankI > RankJ -> put_assoc(Y, UF3, X-RankJ, UF1) ; 
				put_assoc(Y, UF3, X-RankJ, UF4),
				succ(RankI, SrankI),
				put_assoc(X, UF4, X-SrankI, UF1))) ; UF1 = UF3).

% union_all_assoc/3
% union_all_assoc(+UnionFindIn, +Elements, ?UnionFindOut)
%
% This predicate succeeds joining all the elements of the list +Elements in the union-find structure
% +UnionFindIn, producing the union-find structure ?UnionFindOut.
union_all_assoc(UF, [], UF).
union_all_assoc(UF, [_], UF).
union_all_assoc(UF0, [X,Y|Xs], UF2) :-
	union_assoc(UF0, X, Y, UF1),
	union_all_assoc(UF1, [Y|Xs], UF2).

% find_assoc/4
% find_assoc(+UnionFindIn, +Element, ?Root, ?UnionFindOut)
%
% This predicate follows the chain of parent pointers from +Element up the tree until it reaches a ?Root element,
% whose parent is itself. ?Root is the representative member of the set to which ?Element belongs, and may be
% +Element itself. Path compression flattens the structure of the tree by making every node point to the root
% whenever find_assoc/4 is used on it.
find_assoc(UF0, I, X, UF1) :-
	get_assoc(I, UF0, J-R),
	(I == J -> X = J, UF1 = UF0 ; find_assoc(UF0, J, X, UF2), put_assoc(I, UF2, X-R, UF1)).

% find_assoc/5
% find_assoc(+UnionFindIn, +Element, ?Root, ?Rank, ?UnionFindOut)
%
% Same as find_assoc/4, but returning also the ?Rank of the ?Root.
find_assoc(UF0, I, X, S, UF1) :-
	get_assoc(I, UF0, J-R),
	(I == J -> X = J, S = R, UF1 = UF0 ; find_assoc(UF0, J, X, S, UF2), put_assoc(I, UF2, X-R, UF1)).

% disjoint_sets_assoc/2
% disjoint_sets_assoc(+UnionFind, ?Sets).
%
% This predicate succeeds when ?Sets is the list of disjoint sets on the +UnionFind structure.
disjoint_sets_assoc(UF, Sets) :-
	findall(Set, bagof(I, (Value,UF1)^(gen_assoc(I, UF, Value), find_assoc(UF, I, _, UF1)), Set), Sets).