/* Copyright(C) 1992, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : UGRAPHS.PL							      %
%   Maintainer : Mats Carlsson						      %
%            New versions of transpose/2, reduce/2, top_sort/2 by Dan Sahlin  %
%   Updated: 3 September 1999						      %
%   Purpose: Unweighted graph-processing utilities			      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  Adapted from shared code written by Richard A O'Keefe */

/*  An unweighted directed graph (ugraph) is represented as a list of
    (vertex-neighbors) pairs, where the pairs are in standard order
    (as produced by keysort with unique keys) and the neighbors of
    each vertex are also in standard order (as produced by sort), and
    every neighbor appears as a vertex even if it has no neighbors
    itself.

    An undirected graph is represented as a directed graph where for
    each edge (U,V) there is a symmetric edge (V,U).

    An edge (U,V) is represented as the term U-V.

    A vertex can be any term.  Two vertices are distinct iff they are
    not identical (==).

    A path is represented as a list of vertices.  
    No vertex can appear twice in a path.
*/

:- module(ugraphs, [
	vertices_edges_to_ugraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose/2,
	neighbors/3,
	neighbours/3,
	complement/2,
	compose/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_ugraph/3,
	min_tree/3,
	clique/3,
	independent_set/3,
	coloring/3,
	colouring/3
   ]).
:- use_module(library(ordsets),
	  [ ord_del_element/3,
	    ord_add_element/3,
	    ord_subset/2,
	    ord_union/3,
	    ord_union/4,
	    ord_disjoint/2,
	    ord_intersection/3
	  ]).


:- use_module(library(lists), [
	append/3,
	member/2,
	reverse/2
   ]).

:- use_module(library(assoc), [
	list_to_assoc/2,
	ord_list_to_assoc/2,
	get_assoc/3,
	get_assoc/5
   ]).

/*:- use_module(random, [
	random/1
   ]).
*/
%   vertices_edges_to_ugraph(+Vertices, +Edges, -Graph)
%   is true if Vertices is a list of vertices, Edges is a list of edges,
%   and Graph is a graph built from Vertices and Edges.  Vertices and
%   Edges may be in any order.  The vertices mentioned in Edges do not
%   have to occur explicitly in Vertices.  Vertices may be used to
%   specify vertices that are not connected to any edges.

vertices_edges_to_ugraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	sort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([], []).
edges_vertices([From-To|Edges], [From,To|Vertices]) :-
	edges_vertices(Edges, Vertices).

group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges(Edges, Vertex, Neibs, RestEdges),
	group_edges(Vertices, RestEdges, G).

group_edges([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges(Edges, V, Neibs, RestEdges).
group_edges(Edges, _, [], Edges).



%   vertices(+Graph, -Vertices)
%   unifies Vertices with the vertices in Graph.

vertices([], []).
vertices([Vertex-_|Graph], [Vertex|Vertices]) :- vertices(Graph, Vertices).



%   edges(+Graph, -Edges)
%   unifies Edges with the edges in Graph.

edges([], []).
edges([Vertex-Neibs|G], Edges) :-
	edges(Neibs, Vertex, Edges, MoreEdges),
	edges(G, MoreEdges).

edges([], _, Edges, Edges).
edges([Neib|Neibs], Vertex, [Vertex-Neib|Edges], MoreEdges) :-
	edges(Neibs, Vertex, Edges, MoreEdges).



%   add_vertices(+Graph1, +Vertices, -Graph2)
%   is true if Graph2 is Graph1 with Vertices added to it.

add_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	vertex_units(Vs, Graph1),
	graph_union(Graph0, Graph1, Graph).



%   del_vertices(+Graph1, +Vertices, -Graph2)
%   is true if Graph2 is Graph1 with Vertices and all edges to and from
%   Vertices removed from it.

del_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	graph_del_vertices(Graph0, Vs, Vs, Graph).



%   add_edges(+Graph1, +Edges, -Graph2) 
%   is true if Graph2 is Graph1 with Edges and their "to" and "from"
%   vertices added to it.

add_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_union(Graph0, Graph1, Graph).



%   del_edges(+Graph1, +Edges, -Graph2)
%   is true if Graph2 is Graph1 with Edges removed from it.

del_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_difference(Graph0, Graph1, Graph).



vertex_units([], []).
vertex_units([V|Vs], [V-[]|Us]) :- vertex_units(Vs, Us).


graph_union(G0, [], G) :- !, G = G0.
graph_union([], G, G).
graph_union([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_union(C, V1, N1, G1, V2, N2, G2, G).

graph_union(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_union(G1, [V2-N2|G2], G).
graph_union(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	ord_union(N1, N2, N),
	graph_union(G1, G2, G).
graph_union(>, V1, N1, G1, V2, N2, G2, [V2-N2|G]) :-
	graph_union([V1-N1|G1], G2, G).



graph_difference(G0, [], G) :- !, G = G0.
graph_difference([], _, []).
graph_difference([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_difference(C, V1, N1, G1, V2, N2, G2, G).

graph_difference(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_difference(G1, [V2-N2|G2], G).
graph_difference(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	ord_subtract(N1, N2, N),
	graph_difference(G1, G2, G).
graph_difference(>, V1, N1, G1, _, _, G2, G) :-
	graph_difference([V1-N1|G1], G2, G).


graph_del_vertices(G1, [], Set, G) :- !,
	graph_del_vertices(G1, Set, G).
graph_del_vertices([], _, _, []).
graph_del_vertices([V1-N1|G1], [V2|Vs], Set, G) :-
	compare(C, V1, V2),
	graph_del_vertices(C, V1, N1, G1, V2, Vs, Set, G).

graph_del_vertices(<, V1, N1, G1, V2, Vs, Set, [V1-N|G]) :-
	ord_subtract(N1, Set, N),
	graph_del_vertices(G1, [V2|Vs], Set, G).
graph_del_vertices(=, _, _, G1, _, Vs, Set, G) :-
	graph_del_vertices(G1, Vs, Set, G).
graph_del_vertices(>, V1, N1, G1, _, Vs, Set, G) :-
	graph_del_vertices([V1-N1|G1], Vs, Set, G).

graph_del_vertices([], _, []).
graph_del_vertices([V1-N1|G1], Set, [V1-N|G]) :-
	ord_subtract(N1, Set, N),
	graph_del_vertices(G1, Set, G).



%   transpose(+Graph, -Transpose)
%   is true if Transpose is the graph computed by replacing each edge
%   (u,v) in Graph by its symmetric edge (v,u).  It can only be used
%   one way around.  The cost is O(N log N).

transpose(Graph, Transpose) :-
	transpose_edges(Graph, TEdges, []),
	sort(TEdges, TEdges2),
	vertices(Graph, Vertices),
	group_edges(Vertices, TEdges2, Transpose).

transpose_edges([]) --> [].
transpose_edges([Vertex-Neibs|G]) -->
        transpose_edges(Neibs, Vertex),
	transpose_edges(G).

transpose_edges([], _) --> [].
transpose_edges([Neib|Neibs], Vertex) --> [Neib-Vertex],
	transpose_edges(Neibs, Vertex).


%   neighbours(+Vertex, +Graph, -Neighbors)
%   neighbors(+Vertex, +Graph, -Neighbors)
%   is true if Vertex is a vertex in Graph and Neighbors are its neighbors.

neighbours(Vertex, Graph, Neighbors) :-
	neighbors(Vertex, Graph, Neighbors).

neighbors(V, [V0-Neighbors|_], Neighbors) :- V0==V, !.
neighbors(V, [_|Graph], Neighbors) :- neighbors(V, Graph, Neighbors).



%   complement(+Graph, -Complement)
%   Complement is the complement graph of Graph, i.e. the graph that has
%   the same vertices as Graph but only the edges that are not in Graph.

complement(Graph, Complement) :-
	vertices(Graph, Vertices),
	complement(Graph, Vertices, Complement).

complement([], _, []).
complement([V-Neibs|Graph], Vertices, [V-Others|Complement]) :-
	ord_add_element(Neibs, V, Neibs1),
	ord_subtract(Vertices, Neibs1, Others),
	complement(Graph, Vertices, Complement).



%   compose(+G1, +G2, -Composition)
%   computes Composition as the composition of two graphs, which need
%   not have the same set of vertices.

compose(G1, G2, Composition) :-
	vertices(G1, V1),
	vertices(G2, V2),
	ord_union(V1, V2, V),
	compose(V, G1, G2, Composition).

compose([], _, _, []).
compose([V0|Vertices], [V-Neibs|G1], G2, [V-Comp|Composition]) :- V==V0, !,
	compose1(Neibs, G2, [], Comp),
	compose(Vertices, G1, G2, Composition).
compose([V|Vertices], G1, G2, [V-[]|Composition]) :-
	compose(Vertices, G1, G2, Composition).

compose1([V1|Vs1], [V2-N2|G2], SoFar, Comp) :- !,
	compare(Rel, V1, V2),
	compose1(Rel, V1, Vs1, V2, N2, G2, SoFar, Comp).
compose1(_, _, Comp, Comp).

compose1(<, _, Vs1, V2, N2, G2, SoFar, Comp) :-
	compose1(Vs1, [V2-N2|G2], SoFar, Comp).
compose1(>, V1, Vs1, _, _, G2, SoFar, Comp) :-
	compose1([V1|Vs1], G2, SoFar, Comp).
compose1(=, V1, Vs1, V1, N2, G2, SoFar, Comp) :-
	ord_union(N2, SoFar, Next),
	compose1(Vs1, G2, Next, Comp).



%   transitive_closure(+Graph, -Closure) 
%   computes Closure as the transitive closure of Graph in O(N^3) time.

transitive_closure(Graph, Closure) :-
	warshall(Graph, Graph, Closure).

warshall([], Closure, Closure).
warshall([V-_|G], E, Closure) :-
	neighbors(V, E, Y),
	warshall(E, V, Y, NewE),
	warshall(G, NewE, Closure).

warshall([], _, _, []).
warshall([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	ord_subset([V], Neibs), !,
	ord_del_element(Y, X, Y1),
	ord_union(Neibs, Y1, NewNeibs),
	warshall(G, V, Y, NewG).
warshall([X-Neibs|G], V, Y, [X-Neibs|NewG]) :-
	warshall(G, V, Y, NewG).



%   symmetric_closure(+Graph, -Closure) 
%   computes Closure as the symmetric closure of Graph, i.e.  for each
%   edge (u,v) in Graph, add its symmetric edge (v,u).  Approx O(N log N)
%   time.  This is useful for making a directed graph undirected.

symmetric_closure(Graph, Closure) :-
	transpose(Graph, Transpose),
	symmetric_closure(Graph, Transpose, Closure).

symmetric_closure([], [], []).
symmetric_closure([V-Neibs1|Graph], [V-Neibs2|Transpose], [V-Neibs|Closure]) :-
	ord_union(Neibs1, Neibs2, Neibs),
	symmetric_closure(Graph, Transpose, Closure).



%   top_sort(+Graph, -Sorted)
%   finds a topological ordering of a Graph and returns the ordering
%   as a list of Sorted vertices.  Fails iff no ordering exists, i.e.
%   iff the graph contains cycles.  Approx O(N log N) time.

top_sort(Graph, Sorted) :-
	fanin_counts(Graph, Counts),
	get_top_elements(Counts, Top, 0, I),
	ord_list_to_assoc(Counts, Map),
	top_sort(Top, I, Map, Sorted).

top_sort([], 0, _, []).
top_sort([V-VN|Top0], I, Map0, [V|Sorted]) :-
	dec_counts(VN, I, J, Map0, Map, Top0, Top),
	top_sort(Top, J, Map, Sorted).

dec_counts([], I, I, Map, Map, Top, Top).
dec_counts([N|Ns], I, K, Map0, Map, Top0, Top) :-
	get_assoc(N, Map0, NN-C0, Map1, NN-C),
	C is C0-1,
	(C=:=0 -> J is I-1, Top1 = [N-NN|Top0]; J = I, Top1 = Top0),
	dec_counts(Ns, J, K, Map1, Map, Top1, Top).

get_top_elements([], [], I, I).
get_top_elements([V-(VN-C)|Counts], Top0, I, K) :-
	(C=:=0 -> J = I, Top0 = [V-VN|Top1]; J is I+1, Top0 = Top1),
	get_top_elements(Counts, Top1, J, K).

fanin_counts(Graph, Counts) :-
	transpose_edges(Graph, Edges0, []),
	keysort(Edges0, Edges),
	fanin_counts(Graph, Edges, Counts).

fanin_counts([], [], []).
fanin_counts([V-VN|Graph], Edges, [V-(VN-C)|Counts]) :-
	fanin_counts(Edges, V, 0, C, Edges1),
	fanin_counts(Graph, Edges1, Counts).

fanin_counts([V-_|Edges0], V0, C0, C, Edges) :-
	V==V0, !,
	C1 is C0+1,
	fanin_counts(Edges0, V0, C1, C, Edges).
fanin_counts(Edges, _, C, C, Edges).


%   max_path(+V1, +V2, +Graph, -Path, -Cost)
%   is true if Path is a list of vertices constituting a longest path
%   of cost Cost from V1 to V2 in Graph, there being no cyclic paths from
%   V1 to V2.  Takes O(N^2) time.

max_path(Initial, Final, Graph, Path, Cost) :-
	transpose(Graph, TGraph),
	max_path_init(Initial, Final, Graph, TGraph, TGraph2, Order),
	max_path_init(TGraph2, Val0),
	max_path(Order, TGraph2, Val0, Val),
	max_path_select(Val, Path, Cost).

max_path_init(Initial, Final, Graph, TGraph, TGraph2, Order) :-
	reachable(Initial, Graph, InitialReachable),
	reachable(Final, TGraph, FinalReachable),
	ord_intersection(InitialReachable, FinalReachable, Reachable),
	subgraph(TGraph, Reachable, TGraph2),
	top_sort(TGraph2, Order).

max_path_init([], []).
max_path_init([V-_|G], [V-([]-0)|Val]) :- max_path_init(G, Val).

max_path_select([V-(Longest-Max)|Val], Path, Cost) :-
	max_path_select(Val, V, Longest, Path, Max, Cost).

max_path_select([], V, Path, [V|Path], Cost, Cost).
max_path_select([V1-(Path1-Cost1)|Val], V2, Path2, Path, Cost2, Cost) :-
	(   Cost1>Cost2 -> 
	    max_path_select(Val, V1, Path1, Path, Cost1, Cost)
	;   max_path_select(Val, V2, Path2, Path, Cost2, Cost)
	).

max_path([], _, Val, Val).
max_path([V|Order], Graph, Val0, Val) :-
	neighbors(V, Graph, Neibs),
	neighbors(V, Val0, Item),
	max_path_update(Neibs, V-Item, Val0, Val1),
	max_path(Order, Graph, Val1, Val).

%% [MC] 3.8.6: made determinate
max_path_update([], _, Val, Val).
max_path_update([N|Neibs], Item, [Item0|Val0], Val) :-
	Item0 = V0-(_-Cost0),
	N==V0, !,
	Item = V-(Path-Cost),
	Cost1 is Cost+1,
	(   Cost1>Cost0 -> Val = [V0-([V|Path]-Cost1)|Val1]
	;   Val = [Item0|Val1]
	),
	max_path_update(Neibs, Item, Val0, Val1).
max_path_update(Neibs, Item, [X|Val0], [X|Val]) :-
	Neibs = [_|_],
	max_path_update(Neibs, Item, Val0, Val).

subgraph([], _, []).
subgraph([V-Neibs|Graph], Vs, [V-Neibs1|Subgraph]) :-
	ord_subset([V], Vs), !,
	ord_intersection(Neibs, Vs, Neibs1),
	subgraph(Graph, Vs, Subgraph).
subgraph([_|Graph], Vs, Subgraph) :-
	subgraph(Graph, Vs, Subgraph).



%   min_path(+V1, +V2, +Graph, -Path, -Length)
%   is true if Path is a list of vertices constituting a shortest path
%   of length Length from V1 to V2 in Graph.  Takes O(N^2) time.

min_path(Initial, Final, Graph, Path, Length) :-
	min_path([[Initial]|Q], Q, [Initial], Final, Graph, Rev),
	reverse(Rev, Path),
	length(Path, N),
	Length is N-1.

min_path(Head0, Tail0, Closed0, Final, Graph, Rev) :-
	Head0 \== Tail0,
	Head0 = [Sofar|Head],
	Sofar = [V|_],
	(   V==Final -> Rev = Sofar
	;   neighbors(V, Graph, Neibs),
	    ord_union(Closed0, Neibs, Closed, Neibs1),
	    enqueue(Neibs1, Sofar, Tail0, Tail),
	    min_path(Head, Tail, Closed, Final, Graph, Rev)
	).

enqueue([], _) --> [].
enqueue([V|Vs], Sofar) --> [[V|Sofar]], enqueue(Vs, Sofar).



%   min_paths(+Vertex, +Graph, -Tree)
%   is true if Tree is a tree of all the shortest paths from Vertex to
%   every other vertex in Graph.  This is the single-source shortest
%   paths problem.  The algorithm is straightforward.

min_paths(Vertex, Graph, Tree) :-
	min_paths([Vertex], Graph, [Vertex], List),
	keysort(List, Tree).

min_paths([], _, _, []).
min_paths([Q|R], Graph, Reach0, [Q-New|List]) :-
	neighbors(Q, Graph, Neibs),
	ord_union(Reach0, Neibs, Reach, New),
	append(R, New, S),
	min_paths(S, Graph, Reach, List).



%   path(+Vertex, +Graph, -Path)
%   is given a Graph and a Vertex of that Graph, and returns a maximal
%   Path rooted at Vertex, enumerating more Paths on backtracking.

path(Initial, Graph, Path) :-
	path([Initial], [], Graph, Path).

path(Q, Not, Graph, Path) :-
	Q = [Qhead|_],
	neighbors(Qhead, Graph, Neibs),
	ord_subtract(Neibs, Not, Neibs1),
	(   Neibs1 = [] -> reverse(Q, Path)
	;   ord_add_element(Not, Qhead, Not1),
	    member(N, Neibs1),
	    path([N|Q], Not1, Graph, Path)
	).



%   reduce(+Graph, -Reduced)
%   is true if Reduced is the reduced graph for Graph. The vertices of
%   the reduced graph are the strongly connected components of Graph.
%   There is an edge in Reduced from u to v iff there is an edge in
%   Graph from one of the vertices in u to one of the vertices in v. A
%   strongly connected component is a maximal set of vertices where
%   each vertex has a path to every other vertex.
%   Algorithm from "Algorithms" by Sedgewick, page 482, Tarjan's algorithm.
%   Approximately linear in the maximum of arcs and nodes (O(N log N)).

reduce(Graph, Reduced) :-
	strong_components(Graph, SCCS, Map),
	reduced_vertices_edges(Graph, Vertices, Map, Edges, []),
	sort(Vertices, Vertices1),
	sort(Edges, Edges1),
	group_edges(Vertices1, Edges1, Reduced),
	sort(SCCS, Vertices1).

strong_components(Graph, SCCS, A) :-
	nodeinfo(Graph, Nodeinfo, Vertices), 
	ord_list_to_assoc(Nodeinfo, A0), 
	visit(Vertices, 0, _, A0, A, 0, _, [], _, SCCS, []).

visit([], Min, Min, A, A, I, I, Stk, Stk) --> [].
visit([V|Vs], Min0, Min, A0, A, I, M, Stk0, Stk) -->
	{get_assoc(V, A0, node(Ns,J,Eq), A1, node(Ns,K,Eq))},
	(   {J>0} ->
	    {J=K, J=Min1, A1=A3, I=L, Stk0=Stk2}
	;   {K is I+1},
	    visit(Ns, K, Min1, A1, A2, K, L, [V|Stk0], Stk1),
	    (   {K>Min1} -> {A2=A3, Stk1=Stk2}
	    ;   pop(V, Eq, A2, A3, Stk1, Stk2, [])
	    )
	),
	{Min2 is min(Min0,Min1)},
	visit(Vs, Min2, Min, A3, A, L, M, Stk2, Stk).

pop(V, Eq, A0, A, [V1|Stk0], Stk, SCC0) -->
	{get_assoc(V1, A0, node(Ns,_,Eq), A1, node(Ns,16'100000,Eq))},
	(   {V==V1} -> [SCC], {A1=A, Stk0=Stk, sort([V1|SCC0], SCC)}
	;   pop(V, Eq, A1, A, Stk0, Stk, [V1|SCC0])
	).

nodeinfo([], [], []).
nodeinfo([V-Ns|G], [V-node(Ns,0,_)|Nodeinfo], [V|Vs]) :-
	nodeinfo(G, Nodeinfo, Vs).

reduced_vertices_edges([], [], _) --> [].
reduced_vertices_edges([V-Neibs|Graph], [V1|Vs], Map) -->
	{get_assoc(V, Map, N), N=node(_,_,V1)},
	reduced_edges(Neibs, V1, Map),
	reduced_vertices_edges(Graph, Vs, Map).

reduced_edges([], _, _) --> [].
reduced_edges([V|Vs], V1, Map) -->
	{get_assoc(V, Map, N), N=node(_,_,V2)},
	({V1==V2} -> []; [V1-V2]),
	reduced_edges(Vs, V1, Map).


%   reachable(+Vertex, +Graph, -Reachable)
%   is given a Graph and a Vertex of that Graph, and returns the set
%   of vertices that are Reachable from that Vertex.  Takes O(N^2)
%   time.

reachable(Initial, Graph, Reachable) :-
	reachable([Initial], Graph, [Initial], Reachable).

reachable([], _, Reachable, Reachable).
reachable([Q|R], Graph, Reach0, Reachable) :-
	neighbors(Q, Graph, Neighbors),
	ord_union(Reach0, Neighbors, Reach1, New),
	append(R, New, S),
	reachable(S, Graph, Reach1, Reachable).



%   random_ugraph(+P, +N, -Graph)
%   where P is a probability, unifies Graph with a random graph of N
%   vertices where each possible edge is included with probability P.

random_ugraph(P, N, Graph) :-
	(   float(P), P >= 0.0, P =< 1.0 -> true
	;   prolog:illarg(domain(float,between(0.0,1.0)),
	                  random_ugraph(P,N,Graph), 1)
	),
	(   integer(N), N >= 0 -> true
	;   prolog:illarg(domain(integer,>=(0)),
	                  random_ugraph(P,N,Graph), 2)
	),
	random_ugraph(0, N, P, Graph).

random_ugraph(N, N, _, Graph) :- !, Graph = [].
random_ugraph(I, N, P, [J-List|Graph]) :-
	J is I+1,
	random_neighbors(N, J, P, List, []),
	random_ugraph(J, N, P, Graph).

random_neighbors(0, _, _, S0, S) :- !, S = S0.
random_neighbors(N, J, P, S0, S) :-
	(   N==J -> S1 = S
	;   random(X), X > P -> S1 = S
	;   S1 = [N|S]
	),
	M is N-1,
	random_neighbors(M, J, P, S0, S1).

random(X) :-				% JW: was undefined.  Assume
	X is random(10000)/10000.	% we need 0<=X<=1


%   min_tree(+Graph, -Tree, -Cost)
%   is true if Tree is a spanning tree of an *undirected* Graph with
%   cost Cost, if it exists.  Using a version of Prim's algorithm.

min_tree([V-Neibs|Graph], Tree, Cost) :-
	length(Graph, Cost),
	prim(Cost, Neibs, Graph, [V], Edges),
	vertices_edges_to_ugraph([], Edges, Tree).	

%% [MC] 3.8.6: made determinate
prim(0, [], _, _, []) :- !.
prim(I, [V|Vs], Graph, Reach, [V-W,W-V|Edges]) :-
	neighbors(V, Graph, Neibs),
	ord_subtract(Neibs, Reach, Neibs1),
	ord_subtract(Neibs, Neibs1, [W|_]),
	ord_add_element(Reach, V, Reach1),
	ord_union(Vs, Neibs1, Vs1),
	J is I-1,
	prim(J, Vs1, Graph, Reach1, Edges).



%   clique(+Graph, +K, -Clique)
%   is true if Clique is a maximal clique (complete subgraph) of N
%   vertices of an *undirected* Graph, where N>=K.  Adapted from
%   Algorithm 457, "Finding All Cliques of an Undirected Graph [H]",
%   Version 1, by Coen Bron and Joep Kerbosch, CACM vol. 6 no. 9 pp.
%   575-577, Sept. 1973.

clique(Graph, K, Clique) :-
	(   integer(K), K >= 0 -> true
	;   prolog:illarg(domain(integer,>=(0)),
	                  clique(Graph,K,Clique), 2)
	),
	J is K-1,
	prune(Graph, [], J, Graph1),
	clique1(Graph1, J, Clique).

clique1([], J, []) :- J < 0.
clique1([C-Neibs|Graph], J, [C|Clique]) :-
	neighbor_graph(Graph, Neibs, C, Vs, Graph1),
	J1 is J-1,
	prune(Graph1, Vs, J1, Graph2),
	clique1(Graph2, J1, Clique).
clique1([C-Neibs|Graph], J, Clique) :-
	prune(Graph, [C], J, Graph2),
	clique1(Graph2, J, Clique),
	\+ ord_subset(Clique, Neibs).

neighbor_graph([], _, _, [], []).
neighbor_graph([V0-Neibs0|Graph0], [V|Vs], W, Del, [V-Neibs|Graph]) :-
	V0==V, !,
	ord_del_element(Neibs0, W, Neibs),
	neighbor_graph(Graph0, Vs, W, Del, Graph).
neighbor_graph([V-_|Graph0], Vs, W, [V|Del], Graph) :-
	neighbor_graph(Graph0, Vs, W, Del, Graph).

prune(Graph0, [], K, Graph) :- K =< 0, !, Graph = Graph0.
prune(Graph0, Vs0, K, Graph) :-
	prune(Graph0, Vs0, K, Graph1, Vs1),
	(   Vs1==[] -> Graph = Graph1
	;   prune(Graph1, Vs1, K, Graph)
	).

prune([], _, _, [], []).
prune([V-Ns0|Graph0], Vs1, K, [V-Ns|Graph], Vs2) :-
	ord_disjoint([V], Vs1),
	ord_subtract(Ns0, Vs1, Ns),
	length(Ns, I),
	I >= K, !,
	prune(Graph0, Vs1, K, Graph, Vs2).
prune([V-_|Graph0], Vs1, K, Graph, Vs2) :-
	(   ord_disjoint([V], Vs1) -> Vs2 = [V|Vs3]
	;   Vs2 = Vs3
	),
	prune(Graph0, Vs1, K, Graph, Vs3).



%   independent_set(+Graph, +K, -Set)
%   is true if Set is a maximal independent (unconnected) set of N
%   vertices of an *undirected* Graph, where N>=K.

independent_set(Graph, K, Set) :-
	complement(Graph, Complement),
	clique(Complement, K, Set).



%   colouring(+Graph, +K, -Coloring)
%   coloring(+Graph, +K, -Coloring)
%   is true if Coloring is a mapping from vertices to colors 1..N of
%   an *undirected* Graph such that all edges have distinct end colors,
%   where N=<K.  Adapted from "New Methods to Color the Vertices of a
%   Graph", by D. Brelaz, CACM vol. 4 no. 22 pp. 251-256, April 1979.
%   Augmented with ideas from Matula's smallest-last ordering.

colouring(Graph, K, Coloring) :-
	coloring(Graph, K, Coloring).

coloring(Graph, K, Coloring) :-
	(   integer(K), K >= 0 -> true
	;   prolog:illarg(domain(integer,>=(0)),
	                  coloring(Graph,K,Coloring), 2)
	),
	color_map(Graph, Coloring),
	color_map(Graph, Graph1, Coloring, Coloring),
	coloring(Graph1, K, 0, [], Stack),
	color_stack(Stack).

coloring([], _, _, Stk0, Stk) :- !, Stk0 = Stk.
coloring(Graph, K, InUse, Stk0, Stk) :-
	select_vertex(Graph, K, Compare, -, 0+0, V-Ns),
	graph_del_vertices(Graph, [V], [V], Graph1),
	(   Compare = < ->
	    coloring(Graph1, K, InUse, [V-Ns|Stk0], Stk)
	;   M is min(K,InUse+1),
	    vertex_color(Ns, 1, M, V),
	    add_color(Graph1, Ns, V, Graph2),
	    InUse1 is max(V,InUse),
	    coloring(Graph2, K, InUse1, Stk0, Stk)
	).

%   select_vertex(+Graph, +K, -Comp, +Pair0, +Rank, -Pair)
%   return any vertex with degree<K right away (Comp = <) else return
%   vertex with max. saturation degree (Comp = >=), break ties using
%   max. degree

select_vertex([], _, >=, Pair, _, Pair).
select_vertex([V-Neibs|Graph], K, Comp, Pair0, Rank0, Pair) :-
	evaluate_vertex(Neibs, 0, Rank),
	(   Rank < K -> Comp = <, Pair = V-Neibs
	;   Rank @> Rank0 ->
	    select_vertex(Graph, K, Comp, V-Neibs, Rank, Pair)
	;   select_vertex(Graph, K, Comp, Pair0, Rank0, Pair)
	).

evaluate_vertex([V|Neibs], Deg, Rank) :- var(V), !,
	Deg1 is Deg+1,
	evaluate_vertex(Neibs, Deg1, Rank).
evaluate_vertex(Neibs, Deg, Sat+Deg) :-
	prolog:length(Neibs, 0, Sat).

add_color([], _, _, []).
add_color([V-Neibs|Graph], [W|Vs], C, [V-Neibs1|Graph1]) :- V==W, !,
	ord_add_element(Neibs, C, Neibs1),
	add_color(Graph, Vs, C, Graph1).
add_color([Pair|Graph], Vs, C, [Pair|Graph1]) :-
	add_color(Graph, Vs, C, Graph1).

vertex_color([V|Vs], I, M, Color) :- V@<I, !,
	vertex_color(Vs, I, M, Color).
vertex_color([I|Vs], I, M, Color) :- !,
	I<M, J is I+1, vertex_color(Vs, J, M, Color).
vertex_color(_, I, _, I).
vertex_color(Vs, I, M, Color) :-
	I<M, J is I+1, vertex_color(Vs, J, M, Color).

color_stack([]).
color_stack([V-Neibs|Stk]) :- sort(Neibs, Set), color_stack(Set, 1, V, Stk).

color_stack([I|Is], I, V, Stk) :- !, J is I+1, color_stack(Is, J, V, Stk).
color_stack(_, V, V, Stk) :- color_stack(Stk).

color_map([], []).
color_map([V-_|Graph], [V-_|Coloring]) :- color_map(Graph, Coloring).

color_map([], [], [], _).
color_map([V-Ns|Graph], [C-Ns1|Graph1], [V-C|Cols], Coloring) :-
	map_colors(Ns, Coloring, Ns1),
	color_map(Graph, Graph1, Cols, Coloring).

map_colors([], _, []).
map_colors(Ns, Coloring, Ns1) :-
	Ns = [X|_],
	Coloring = [V-_|_],
	compare(C, X, V),
	map_colors(C, Ns, Coloring, Ns1).

map_colors(=, [_|Xs], [_-Y|Coloring], [Y|Ns1]) :-
	map_colors(Xs, Coloring, Ns1).
map_colors(>, Ns, [_|Coloring], Ns1) :-
	map_colors(Ns, Coloring, Ns1).
