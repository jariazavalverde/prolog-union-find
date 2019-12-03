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
    make_set/2,
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

make_set(UF0, UF1) :-
    UF0 =.. [union_find|Args0],
    length(Args0, N),
    succ(N, M),
    append(Args0, [M-0], Args1),
    UF1 =.. [union_find|Args1].

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
    (I == J -> X = J ; find(UF, J, X), nb_setarg(I, UF, X-R)).

find_with_rank(UF, I, X-S) :-
    arg(I, UF, J-R),
    (I == J -> X = J, S = R ; find_with_rank(UF, J, X-S), nb_setarg(I, UF, X-R)).