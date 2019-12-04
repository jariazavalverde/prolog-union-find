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
    union_find_assoc/2
]).



% union_find_assoc/2
% union_find_assoc(?UnionFind, +Elements)
%
% This predicate initializes a new ?UnionFind structure with a list of elements ?Elements as keys.
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