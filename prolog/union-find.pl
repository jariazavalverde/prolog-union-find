/**
  * 
  * FILENAME: union-find.pl
  * DESCRIPTION: This module contains predicates for manipulating union-find structures.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-union-find
  * UPDATED: 03.12.2019
  * 
  **/



:- module(union_find, [
    union_find/2,
    union/3,
    find/3
]).

union_find(UF, N) :-
    union_find(1, N, Args),
    UF =.. [union_find|Args].

union_find(_, 0, []).
union_find(I, N, [I-0|UF]) :-
    succ(M, N),
    succ(I, J),
    union_find(J, M, UF).

union(UF, I, J) :-
    find_with_rank(UF, I, X-RankI),
    find_with_rank(UF, J, Y-RankJ),
    (X \== Y ->
        (RankI < RankJ -> setarg(X, UF, Y-RankI) ; 
            (RankI > RankJ -> setarg(Y, UF, X-RankJ) ; 
                setarg(Y, UF, X-RankJ),
                succ(RankI, SrankI),
                setarg(X, UF, X-SrankI))) ; true).

find(UF, I, X) :-
    arg(I, UF, J-R),
    (I == J -> X = J ; find(UF, J, X), setarg(I, UF, X-R)).

find_with_rank(UF, I, X-S) :-
    arg(I, UF, J-R),
    (I == J -> X = J, S = R ; find_with_rank(UF, J, X-S), setarg(I, UF, X-R)).