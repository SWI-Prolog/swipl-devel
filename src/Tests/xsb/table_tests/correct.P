% module shared_graph. /* Shape-graph with sharing information */

% export var_edge(bff).
% export sel_edge(bfff).
% export is_shared(bf).

:- import member/2 from basics.

/* Auxiliary functions ----------------------------------------------- */

singleton(X,NX) :- create_set(Temp), add_elem(X,Temp,NX).

del_elem(X,NX,NY) :- singleton(X,Xset), difference(NX,Xset,NY).

:- table transitively_points_to/3.

transitively_points_to(U, NX, Z) :- var_edge(U, Z, NX).
transitively_points_to(U, NX, Z) :- sel_edge(U, NY, _, NX),
               		            transitively_points_to(U, NY, Z).

disjoint_or_equal(X,Y) :- set_equal(X, Y).
disjoint_or_equal(X,Y) :- inter(X, Y, []).

rename_var_to_var(NT,X,Y,NZ) :- member(Y,NT), add_elem(X,NT,NZ).
rename_var_to_var(NT,_,Y,NZ) :- not member(Y,NT), set_equal(NT, NZ).

:- table var_edge/3, sel_edge/4, is_shared/2.

/* Explicit Initializations */
var_edge(V, X, NX) :- init_var_edge(V, X, NX). 
sel_edge(V, NX1,Sel,NX2) :- init_sel_edge(V, NX1, Sel, NX2).
is_shared(V, NX) :- init_is_shared(V, NX).

/* Edges with no pointer manipulation */
var_edge(V,X,NX) :- identity(U, V), var_edge(U, X, NX).
sel_edge(V,NX1,Sel,NX2) :- identity(U, V), sel_edge(U, NX1, Sel, NX2).
is_shared(V,NX) :- identity(U, V), is_shared(U, NX).

/* x := nil ------------------------------------------------------- */

var_edge(V, Y, NYP) :-
    assign_nil_to_var(U,V,X),
    var_edge(U, Y ,NY),
    Y\==X,
    del_elem(X, NY, NYP).
sel_edge(V, NY1P, Sel, NY2P) :-
    assign_nil_to_var(U,V,X),
    sel_edge(U, NY1, Sel, NY2),
    transitively_points_to(U, NY1, Y1), 
    Y1 \== X,
    transitively_points_to(U, NY2, Y2), 
    Y2 \== X,
    del_elem(X, NY1, NY1P),
    del_elem(X, NY2, NY2P).
is_shared(V, NYP) :-	
    assign_nil_to_var(U,V,X),
    is_shared(U, NY),
    transitively_points_to(U, NY, Y), 
    Y \== X,
    del_elem(X, NY, NYP).

/* x.sel_0 := nil ----------------------------------------------------- */
var_edge(V, Z, NZ) :-
    assign_nil_to_sel(U,V,_,_Sel0),
    var_edge(U, Z, NZ).

sel_edge(V, NZ1, Sel, NZ2) :-
    assign_nil_to_sel(U,V,_,Sel0),
    sel_edge(U, NZ1, Sel, NZ2),
    Sel \== Sel0.
sel_edge(V, NZ1, Sel, NZ2) :-
    assign_nil_to_sel(U,V,X,_Sel0),
    sel_edge(U, NZ1, Sel, NZ2),
    not member(X,NZ1).

is_shared(V, NZ) :-	
    assign_nil_to_sel(U,V,_,_Sel0),
    is_shared(U, NZ),
    sel_edge(V, NZ1, _, NZ),
    sel_edge(V, NZ2, _, NZ),
    inter(NZ1, NZ2, []).
is_shared(V, NZ) :-	
    assign_nil_to_sel(U,V,_,_Sel0),
    is_shared(U, NZ),
    sel_edge(V, NW, Sel1, NZ),
    sel_edge(V, NW, Sel2, NZ),
    Sel1 \== Sel2.

/*  x := new  ------------------------------------ */

var_edge(V, Z, NZ) :-
    assign_new_to_var(U,V,_),
    var_edge(U, Z, NZ).
var_edge(V, X, NX) :-	/* Create the new var_edge from X */
    assign_new_to_var(_,V,X),
    singleton(X,NX).
sel_edge(V, NY, Sel, NZ) :-
    assign_new_to_var(U,V,_),
    sel_edge(U, NY, Sel, NZ).
is_shared(V, NZ) :-	
    assign_new_to_var(U,V,_),
    is_shared(U, NZ).

/* x := y --------------------------- */

var_edge(V, Z, NZP) :-
    assign_var_to_var(U,V,X,Y),
    var_edge(U, Z, NZ),
    rename_var_to_var(NZ,X,Y,NZP).
var_edge(V, X, NXY) :-
    assign_var_to_var(U,V,X,Y),
    var_edge(U, Y, NY),
    rename_var_to_var(NY,X,Y,NXY).
sel_edge(V, NZ1P, Sel, NZ2P) :-
    assign_var_to_var(U,V,X,Y),
    sel_edge(U, NZ1, Sel, NZ2),
    rename_var_to_var(NZ1,X,Y,NZ1P),
    rename_var_to_var(NZ2,X,Y,NZ2P).
is_shared(V, NZP) :-
    assign_var_to_var(U,V,X,Y),
    is_shared(U, NZ),
    rename_var_to_var(NZ,X,Y,NZP).

/* x := y.sel0 ---------------------------- */

var_edge(V, Z, NZ) :-
    assign_sel_to_var(U,V,_X,_Y,_Sel0),
    var_edge(U, Z, NZ).
var_edge(V, X, NXZ) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    add_elem(X,NZ,NXZ).
var_edge(V, Z, NXZ) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    add_elem(X,NZ,NXZ),
    var_edge(U, Z, NZ).
/* old -> old */
sel_edge(V, NZ1, Sel, NZ2) :-
    assign_sel_to_var(U,V,_X,Y,_Sel0),
    sel_edge(U, NZ1, Sel, NZ2),
    not member(Y,NZ1).
/* old -> old */
sel_edge(V, NZ1, Sel, NZ2) :-
    assign_sel_to_var(U,V,_X,_Y,Sel0),
    sel_edge(U, NZ1, Sel, NZ2),
    Sel \== Sel0.
/* old -> new */
sel_edge(V, NY, Sel0, NXZ) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    add_elem(X,NZ,NXZ),
    not set_equal(NY, NZ).
%    NY \== NZ. 
/* new -> old */
sel_edge(V, NXZ, Sel, NT) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    add_elem(X,NZ,NXZ),
    sel_edge(U, NZ, Sel, NT),
    not set_equal(NZ, NY),
%    NZ \== NY,
    disjoint_or_equal(NXZ, NT). /* Can be replaced by NZ <> NT or NZ = {} */
/* new -> old */
sel_edge(V, NXZ, Sel, NT) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    add_elem(X,NZ,NXZ),
    sel_edge(U, NZ, Sel, NT),
    Sel \== Sel0,
    disjoint_or_equal(NXZ, NT). /* Can be replaced by NZ <> NT or NZ = {} */
/* new -> new */
sel_edge(V, NXY, Sel0, NXY1) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NY),
    add_elem(X,NY,NXY),
    set_equal(NXY, NXY1).
/* new -> new */
sel_edge(V, NXZ, Sel, NXZ1) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    is_shared(U, NZ),
    sel_edge(U, NZ, Sel, NZ),
    add_elem(X,NZ,NXZ),
    set_equal(NXZ, NXZ1).
/* old -> new */
sel_edge(V, NT, Sel, NXZ) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    is_shared(U, NZ),
    add_elem(X,NZ,NXZ),
    sel_edge(U, NT, Sel, NZ),
    not member(Y,NT),
    disjoint_or_equal(NT, NXZ). /* Can be replaced by NT <> NZ or NT = {} */
/* old -> new */
sel_edge(V, NT, Sel, NXZ) :-
    assign_sel_to_var(U,V,X,Y,Sel0),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    is_shared(U, NZ),
    add_elem(X,NZ,NXZ),
    sel_edge(U, NT, Sel, NZ),
    Sel \== Sel0,
    disjoint_or_equal(NT, NXZ). /* Can be replaced by NT <> NZ or NT = {} */
is_shared(V, NZ) :-	
    assign_sel_to_var(U,V,_X,_Y,_Sel0),
    is_shared(U, NZ).
is_shared(V, NXZ) :-	
    assign_sel_to_var(U,V,X,Y,Sel0),
    is_shared(U,NZ),
    var_edge(U, Y, NY),
    sel_edge(U, NY, Sel0, NZ),
    add_elem(X,NZ,NXZ).

/* x.sel0 := y ------------------------------------------- */

var_edge(V, Z, NZ) :-
    assign_var_to_sel(U,V,_X,_Sel0,_Y),
    var_edge(U, Z, NZ).
/* old edges */
sel_edge(V, NZ, Sel, NT) :-
    assign_var_to_sel(U,V,_X,_Sel0,_Y),
    sel_edge(U, NZ, Sel, NT).
/* new edges */
sel_edge(V, NX, Sel0, NY) :-
    assign_var_to_sel(U,V,X,Sel0,Y),
    var_edge(U, X, NX),
    var_edge(U, Y, NY),
    disjoint_or_equal(NX, NY).
is_shared(V, NZ) :-	
    assign_var_to_sel(U,V,_X,_Sel0,_Y),
    is_shared(U, NZ).
is_shared(V, NY) :-	
    assign_var_to_sel(U,V,_X,_Sel0,Y),
    var_edge(U, Y, NY),
    sel_edge(U, _, _, NY).

