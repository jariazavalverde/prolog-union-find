/**
  * 
  * FILENAME: kruskal.pl
  * DESCRIPTION: This module contains the implementation of Kruskal's algorithm.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-union-find
  * UPDATED: 04.12.2019
  * 
  **/



:- module(kruskal, [kruskal/2]).
:- use_module(library(union_find_assoc)).



% kruskal/2
% kruskal(+Graph, -MinimumSpanningTree)
kruskal(g(Vertices-Edges), g(Vertices-Tree)) :-
	union_find_assoc(UF, Vertices),
	keysort(Edges, Sorted),
	kruskal(UF, Sorted, Tree).

% kruskal/3
% kruskal(+UnionFind, +SortedEdges, -MinimumSpanningEdges)
%
% NOT EXPORTED
kruskal(_, [], []).
kruskal(UF0, [Edge|Edges], [Edge|Tree]) :-
	Edge = _-(V1, V2),
	find_assoc(UF0, V1, R1, UF1),
	find_assoc(UF1, V2, R2, UF2),
	R1 \== R2, !,
	union_assoc(UF2, V1, V2, UF3),
	kruskal(UF3, Edges, Tree).
kruskal(UF, [_|Edges], Tree) :-
	kruskal(UF, Edges, Tree).