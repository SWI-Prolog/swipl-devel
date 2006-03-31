%   $Id$
%   File   : GRAPHS.PL
%   Author : R.A.O'Keefe
%   Updated: 20 March 1984
%   Purpose: Graph-processing utilities.

%
% adapted to support some of the functionality of the SICStus ugraphs library
% by Vitor Santos Costa.
%
 
% 
% Ported from YAP 5.0.1 to SWI-Prolog by Jan Wielemaker. 
%
% As the original code was distributed in the public domain and YAP
% under the Perl artistic license the code can be used with SWI-Prolog
% applications without consequences to the overall system or proprietary
% code linked to SWI-Prolog
% 

/*  The S-representation of a graph is a list of (vertex-neighbours)
    pairs, where the pairs are in standard order (as produced by
    keysort) and the neighbours of each vertex are also in standard
    order (as produced by sort).  This form is convenient for many
    calculations.
 */

:- module(ugraphs,
	  [ add_edges/3,		% +Graph, +Edges, -NewGraph
	    add_vertices/3,		% +Graph, +Vertices, -NewGraph
	    complement/2,		% +Graph, -NewGraph
	    compose/3,			% +LeftGraph, +RightGraph, -NewGraph
	    del_edges/3,		% +Graph, +Edges, -NewGraph
	    del_vertices/3,		% +Vertices, +Graph, -NewGraph
	    edges/2,			% +Graph, -Edges
	    neighbors/3,		% +Vertex, +Graph, -Vertices
	    neighbours/3,		% +Vertex, +Graph, -Vertices
	    reachable/3,		% +Vertex, +Graph, -Vertices
	    top_sort/2,			% +Graph, -Sort
	    top_sort/3,			% +Graph, -Sort0, -Sort
	    transitive_closure/2,	% +Graph, -Closure
	    transpose/2,		% +Graph, -NewGraph
	    vertices/2,			% +Graph, -Vertices
	    vertices_edges_to_ugraph/3	% +Vertices, +Edges, -Graph
	  ]).

:- use_module(library(lists), [
	append/3,
	member/2
   ]).

:- use_module(library(ordsets), [
	ord_add_element/3,
	ord_subtract/3,
	ord_union/3,
	ord_union/4
   ]).


/*
 
:- public
	p_to_s_graph/2,
	s_to_p_graph/2, % edges
	s_to_p_trans/2,
	p_member/3,
	s_member/3,
	p_transpose/2,
	s_transpose/2,
	compose/3,
	top_sort/2,
	vertices/2,
	warshall/2.

:- mode
	vertices(+, -),
	p_to_s_graph(+, -),
	    p_to_s_vertices(+, -),
	    p_to_s_group(+, +, -),
		p_to_s_group(+, +, -, -),
	s_to_p_graph(+, -),
	    s_to_p_graph(+, +, -, -),
	s_to_p_trans(+, -),
	    s_to_p_trans(+, +, -, -),
	p_member(?, ?, +),
	s_member(?, ?, +),
	p_transpose(+, -),
	s_transpose(+, -),
	    s_transpose(+, -, ?, -),
		transpose_s(+, +, +, -),
	compose(+, +, -),
	    compose(+, +, +, -),
		compose1(+, +, +, -),
		    compose1(+, +, +, +, +, +, +, -),
	top_sort(+, -),
	    vertices_and_zeros(+, -, ?),
	    count_edges(+, +, +, -),
		incr_list(+, +, +, -),
	    select_zeros(+, +, -),
	    top_sort(+, -, +, +, +),
		decr_list(+, +, +, -, +, -),
	warshall(+, -),
	    warshall(+, +, -),
		warshall(+, +, +, -).

*/
 
 
%	vertices(+S_Graph, -Vertices)
%   
%	Strips off the  neighbours  lists   of  an  S-representation  to
%	produce  a  list  of  the  vertices  of  the  graph.  (It  is  a
%	characteristic of S-representations that *every* vertex appears,
%	even if it has no neighbours.)
 
vertices([], []) :- !.
vertices([Vertex-_|Graph], [Vertex|Vertices]) :-
	vertices(Graph, Vertices).
 
vertices_edges_to_ugraph(Vertices, Edges, Graph) :-
	sort(Edges, EdgeSet),
	p_to_s_vertices(EdgeSet, IVertexBag),
	append(Vertices, IVertexBag, VertexBag),
	sort(VertexBag, VertexSet),
	p_to_s_group(VertexSet, EdgeSet, Graph).


add_vertices(Graph, Vertices, NewGraph) :-
	msort(Vertices, V1),
	add_vertices_to_s_graph(V1, Graph, NewGraph).
	
add_vertices_to_s_graph(L, [], NL) :- !,
	add_empty_vertices(L, NL).
add_vertices_to_s_graph([], L, L) :- !.
add_vertices_to_s_graph([V1|VL], [V-Edges|G], NGL) :-
	compare(Res, V1, V),
	add_vertices_to_s_graph(Res, V1, VL, V, Edges, G, NGL).

add_vertices_to_s_graph(=, _, VL, V, Edges, G, [V-Edges|NGL]) :-
	add_vertices_to_s_graph(VL, G, NGL).
add_vertices_to_s_graph(<, V1, VL, V, Edges, G, [V1-[]|NGL]) :-
	add_vertices_to_s_graph(VL, [V-Edges|G], NGL).
add_vertices_to_s_graph(>, V1, VL, V, Edges, G, [V-Edges|NGL]) :-
	add_vertices_to_s_graph([V1|VL], G, NGL).

add_empty_vertices([], []).
add_empty_vertices([V|G], [V-[]|NG]) :-
	add_empty_vertices(G, NG).

%
% unmark a set of vertices plus all edges leading to them.
%
del_vertices(Vertices, Graph, NewGraph) :-
	sort(Vertices, V1),		% JW: was msort
	(   V1 = []
	->  Graph = NewGraph
	;   del_vertices(Graph, V1, V1, NewGraph)
	).

del_vertices(G, [], V1, NG) :- !,
	del_remaining_edges_for_vertices(G, V1, NG).
del_vertices([], _, _, []).
del_vertices([V-Edges|G], [V0|Vs], V1, NG) :-  
	compare(Res, V, V0),
	split_on_del_vertices(Res, V,Edges, [V0|Vs], NVs, V1, NG, NGr),
	del_vertices(G, NVs, V1, NGr).

del_remaining_edges_for_vertices([], _, []).
del_remaining_edges_for_vertices([V0-Edges|G], V1, [V0-NEdges|NG]) :-
	ord_subtract(Edges, V1, NEdges),
	del_remaining_edges_for_vertices(G, V1, NG).

split_on_del_vertices(<, V, Edges, Vs, Vs, V1, [V-NEdges|NG], NG) :-
	ord_subtract(Edges, V1, NEdges).
split_on_del_vertices(>, V, Edges, [_|Vs], Vs, V1, [V-NEdges|NG], NG) :-
	ord_subtract(Edges, V1, NEdges).
split_on_del_vertices(=, _, _, [_|Vs], Vs, _, NG, NG).

add_edges(Graph, Edges, NewGraph) :-
	p_to_s_graph(Edges, G1),
	graph_union(Graph, G1, NewGraph).

%	graph_union(+Set1, +Set2, ?Union)
%   
%	Is true when Union is the union of Set1 and Set2. This code is a
%	copy of set union

graph_union(Set1, [], Set1) :- !.
graph_union([], Set2, Set2) :- !.
graph_union([Head1-E1|Tail1], [Head2-E2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	graph_union(Order, Head1-E1, Tail1, Head2-E2, Tail2, Union).

graph_union(=, Head-E1,  Tail1, _-E2,     Tail2, [Head-Es|Union]) :-
	ord_union(E1, E2, Es),
	graph_union(Tail1, Tail2, Union).
graph_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	graph_union(Tail1, [Head2|Tail2], Union).
graph_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	graph_union([Head1|Tail1], Tail2, Union).

del_edges(Graph, Edges, NewGraph) :-
	p_to_s_graph(Edges, G1),
	graph_subtract(Graph, G1, NewGraph).

%	graph_subtract(+Set1, +Set2, ?Difference)
%	
% 	Is based on ord_subtract

graph_subtract(Set1, [], Set1) :- !.
graph_subtract([], _, []).
graph_subtract([Head1-E1|Tail1], [Head2-E2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	graph_subtract(Order, Head1-E1, Tail1, Head2-E2, Tail2, Difference).

graph_subtract(=, H-E1,     Tail1, _-E2,     Tail2, [H-E|Difference]) :-
	ord_subtract(E1,E2,E),
	graph_subtract(Tail1, Tail2, Difference).
graph_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	graph_subtract(Tail1, [Head2|Tail2], Difference).
graph_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
	graph_subtract([Head1|Tail1], Tail2, Difference).



edges(Graph, Edges) :- 
	s_to_p_graph(Graph, Edges).

p_to_s_graph(P_Graph, S_Graph) :-
	sort(P_Graph, EdgeSet),
	p_to_s_vertices(EdgeSet, VertexBag),
	sort(VertexBag, VertexSet),
	p_to_s_group(VertexSet, EdgeSet, S_Graph).
 
 
p_to_s_vertices([], []).
p_to_s_vertices([A-Z|Edges], [A,Z|Vertices]) :-
	p_to_s_vertices(Edges, Vertices).
 
 
p_to_s_group([], _, []).
p_to_s_group([Vertex|Vertices], EdgeSet, [Vertex-Neibs|G]) :-
	p_to_s_group(EdgeSet, Vertex, Neibs, RestEdges),
	p_to_s_group(Vertices, RestEdges, G).
 
 
p_to_s_group([V1-X|Edges], V2, [X|Neibs], RestEdges) :- V1 == V2, !,
	p_to_s_group(Edges, V2, Neibs, RestEdges).
p_to_s_group(Edges, _, [], Edges).
 
 
 
s_to_p_graph([], []) :- !.
s_to_p_graph([Vertex-Neibs|G], P_Graph) :-
	s_to_p_graph(Neibs, Vertex, P_Graph, Rest_P_Graph),
	s_to_p_graph(G, Rest_P_Graph).
 
 
s_to_p_graph([], _, P_Graph, P_Graph) :- !.
s_to_p_graph([Neib|Neibs], Vertex, [Vertex-Neib|P], Rest_P) :-
	s_to_p_graph(Neibs, Vertex, P, Rest_P).
 
 
transitive_closure(Graph, Closure) :-
	warshall(Graph, Graph, Closure).
 
warshall([], Closure, Closure) :- !.
warshall([V-_|G], E, Closure) :-
	memberchk(V-Y, E),	%  Y := E(v)
	warshall(E, V, Y, NewE),
	warshall(G, NewE, Closure).
 
 
warshall([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	memberchk(V, Neibs),
	!,
	ord_union(Neibs, Y, NewNeibs),
	warshall(G, V, Y, NewG).
warshall([X-Neibs|G], V, Y, [X-Neibs|NewG]) :- !,
	warshall(G, V, Y, NewG).
warshall([], _, _, []).
 
 
 
transpose(S_Graph, Transpose) :-
	s_transpose(S_Graph, Base, Base, Transpose).
 
s_transpose([], [], Base, Base) :- !.
s_transpose([Vertex-Neibs|Graph], [Vertex-[]|RestBase], Base, Transpose) :-
	s_transpose(Graph, RestBase, Base, SoFar),
	transpose_s(SoFar, Neibs, Vertex, Transpose).
 
transpose_s([Neib-Trans|SoFar], [Neib|Neibs], Vertex,
		[Neib-[Vertex|Trans]|Transpose]) :- !,
	transpose_s(SoFar, Neibs, Vertex, Transpose).
transpose_s([Head|SoFar], Neibs, Vertex, [Head|Transpose]) :- !,
	transpose_s(SoFar, Neibs, Vertex, Transpose).
transpose_s([], [], _, []).
 
 
%	compose(G1, G2, Composition)
%   
%	Calculates the composition of two S-form  graphs, which need not
%	have the same set of vertices.
 
compose(G1, G2, Composition) :-
	vertices(G1, V1),
	vertices(G2, V2),
	ord_union(V1, V2, V),
	compose(V, G1, G2, Composition).
 
 
compose([], _, _, []) :- !.
compose([Vertex|Vertices], [Vertex-Neibs|G1], G2,
	[Vertex-Comp|Composition]) :- !,
	compose1(Neibs, G2, [], Comp),
	compose(Vertices, G1, G2, Composition).
compose([Vertex|Vertices], G1, G2, [Vertex-[]|Composition]) :-
	compose(Vertices, G1, G2, Composition).
 
 
compose1([V1|Vs1], [V2-N2|G2], SoFar, Comp) :-
	compare(Rel, V1, V2), !,
	compose1(Rel, V1, Vs1, V2, N2, G2, SoFar, Comp).
compose1(_, _, Comp, Comp).
 
 
compose1(<, _, Vs1, V2, N2, G2, SoFar, Comp) :- !,
	compose1(Vs1, [V2-N2|G2], SoFar, Comp).
compose1(>, V1, Vs1, _, _, G2, SoFar, Comp) :- !,
	compose1([V1|Vs1], G2, SoFar, Comp).
compose1(=, V1, Vs1, V1, N2, G2, SoFar, Comp) :-
	ord_union(N2, SoFar, Next),
	compose1(Vs1, G2, Next, Comp).
 
 
top_sort(Graph, Sorted) :-
	vertices_and_zeros(Graph, Vertices, Counts0),
	count_edges(Graph, Vertices, Counts0, Counts1),
	select_zeros(Counts1, Vertices, Zeros),
	top_sort(Zeros, Sorted, Graph, Vertices, Counts1).
 
top_sort(Graph, Sorted0, Sorted) :-
	vertices_and_zeros(Graph, Vertices, Counts0),
	count_edges(Graph, Vertices, Counts0, Counts1),
	select_zeros(Counts1, Vertices, Zeros),
	top_sort(Zeros, Sorted, Sorted0, Graph, Vertices, Counts1).
 
 
vertices_and_zeros([], [], []) :- !.
vertices_and_zeros([Vertex-_|Graph], [Vertex|Vertices], [0|Zeros]) :-
	vertices_and_zeros(Graph, Vertices, Zeros).
 
 
count_edges([], _, Counts, Counts) :- !.
count_edges([_-Neibs|Graph], Vertices, Counts0, Counts2) :-
	incr_list(Neibs, Vertices, Counts0, Counts1),
	count_edges(Graph, Vertices, Counts1, Counts2).
 
 
incr_list([], _, Counts, Counts) :- !.
incr_list([V1|Neibs], [V2|Vertices], [M|Counts0], [N|Counts1]) :- V1 == V2, !,
	N is M+1,
	incr_list(Neibs, Vertices, Counts0, Counts1).
incr_list(Neibs, [_|Vertices], [N|Counts0], [N|Counts1]) :-
	incr_list(Neibs, Vertices, Counts0, Counts1).
 
 
select_zeros([], [], []) :- !.
select_zeros([0|Counts], [Vertex|Vertices], [Vertex|Zeros]) :- !,
	select_zeros(Counts, Vertices, Zeros).
select_zeros([_|Counts], [_|Vertices], Zeros) :-
	select_zeros(Counts, Vertices, Zeros).
 
 
 
top_sort([], [], Graph, _, Counts) :- !,
	vertices_and_zeros(Graph, _, Counts).
top_sort([Zero|Zeros], [Zero|Sorted], Graph, Vertices, Counts1) :-
	graph_memberchk(Zero-Neibs, Graph),
	decr_list(Neibs, Vertices, Counts1, Counts2, Zeros, NewZeros),
	top_sort(NewZeros, Sorted, Graph, Vertices, Counts2).
 
top_sort([], Sorted0, Sorted0, Graph, _, Counts) :- !,
	vertices_and_zeros(Graph, _, Counts).
top_sort([Zero|Zeros], [Zero|Sorted], Sorted0, Graph, Vertices, Counts1) :-
	graph_memberchk(Zero-Neibs, Graph),
	decr_list(Neibs, Vertices, Counts1, Counts2, Zeros, NewZeros),
	top_sort(NewZeros, Sorted, Sorted0, Graph, Vertices, Counts2).
 
graph_memberchk(Element1-Edges, [Element2-Edges2|_]) :- Element1 == Element2, !,
	Edges = Edges2.
graph_memberchk(Element, [_|Rest]) :-
        graph_memberchk(Element, Rest).

 
decr_list([], _, Counts, Counts, Zeros, Zeros) :- !.
decr_list([V1|Neibs], [V2|Vertices], [1|Counts1], [0|Counts2], Zi, Zo) :-
	V1 == V2, !,
	decr_list(Neibs, Vertices, Counts1, Counts2, [V2|Zi], Zo).
decr_list([V1|Neibs], [V2|Vertices], [N|Counts1], [M|Counts2], Zi, Zo) :-
	V1 == V2, !,
	M is N-1,
	decr_list(Neibs, Vertices, Counts1, Counts2, Zi, Zo).
decr_list(Neibs, [_|Vertices], [N|Counts1], [N|Counts2], Zi, Zo) :-
	decr_list(Neibs, Vertices, Counts1, Counts2, Zi, Zo).
 
 
%	neighbors(+Vertex, +Graph, -Neigbours)
%	neighbours(+Vertex, +Graph, -Neigbours)
%	
%	Neigbours is a sorted list of the neighbours of Vertex in Graph.
 
neighbors(Vertex, Graph, Neig) :-
	neighbours(Vertex, Graph, Neig).

neighbours(V,[V0-Neig|_],Neig) :-
	V == V0, !.
neighbours(V,[_|G],Neig) :- 
	neighbours(V,G,Neig).


%
% Simple two-step algorithm. You could be smarter, I suppose.
%
complement(G, NG) :-
	vertices(G,Vs),
	complement(G,Vs,NG).

complement([], _, []).
complement([V-Ns|G], Vs, [V-INs|NG]) :-
	ord_add_element(Ns,V,Ns1),
	ord_subtract(Vs,Ns1,INs),
	complement(G, Vs, NG).



reachable(N, G, Rs) :-
	reachable([N], G, [N], Rs).

reachable([], _, Rs, Rs).
reachable([N|Ns], G, Rs0, RsF) :-
	neighbours(N, G, Nei),
	ord_union(Rs0, Nei, Rs1, D),
	append(Ns, D, Nsi),
	reachable(Nsi, G, Rs1, RsF).

