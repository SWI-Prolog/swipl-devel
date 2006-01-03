/*  $Id$

    Part of CPL(R) (Constraint Logic Programming over Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2004, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is part of Leslie De Koninck's master thesis, supervised
    by Bart Demoen and daily advisor Tom Schrijvers.  It is based on CLP(Q,R)
    by Christian Holzbaur for SICStus Prolog and distributed under the
    license details below with permission from all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


% attribute = (type(_),strictness(_),lin(_),order(_),class(_),forward(_),
%		nonzero,target,keep_indep,keep)

:- module(itf3,
	[
	    dump_linear/4,
	    dump_nonzero/4
	]).
:- use_module(bv,
	[
	    deref/2,
	    detach_bounds_vlv/5,
	    dump_var/6,
	    dump_nz/5,
	    solve/1,
	    solve_ord_x/3
	]).
:- use_module(nf,
	[
	    nf/2
	]).
:- use_module(store,
	[
	    add_linear_11/3,
	    indep/2,
	    nf_coeff_of/3
	]).
:- use_module(class,
	[
	    class_drop/2
	]).

this_linear_solver(clpr).

%
% Parametrize the answer presentation mechanism
% (toplevel,compiler/debugger ...)
%
:- dynamic presentation_context/1.

% replaces the old presentation context by a new one.
presentation_context(Old,New) :-
	clause(presentation_context(Current), _),
  	!,
  	Current = Old,
  	retractall(presentation_context(_)),
  	assert(presentation_context(New)).
presentation_context(toplevel,New) :- 
	assert(presentation_context(New)). %default

%
% attribute_goal( V, V:Atts) :- get_atts( V, Atts).
%
attribute_goal(V,Goal) :-
	presentation_context(Cont,Cont),
	dump_linear(V,Cont,Goals,Gtail),
	dump_nonzero(V,Cont,Gtail,[]),
	l2wrapped(Goals,Goal).

l2wrapped([],true).
l2wrapped([X|Xs],Conj) :-
	(   Xs = [],
	    wrap(X,Conj)
	;   Xs = [_|_],
	    wrap(X,Xw),
	    Conj = (Xw,Xc),
	    l2wrapped(Xs,Xc)
	).

%
% Tests should be pulled out of the loop ...
%
wrap(C,W) :-
	prolog_flag(typein_module,Module),
	this_linear_solver(Solver),
	(   Module == Solver
	->  W = {C}
	;   predicate_property(Module:{_},imported_from(Solver))
	->  W = {C}
	;   W = Solver:{C}
	).

dump_linear(V,Context) -->
	{
	    get_attr(V,itf3,(type(Type),_,lin(Lin),_)),
	    !,
	    Lin = [I,_|H]
	},
	%
	% This happens if not all target variables can be made independent
	% Example: examples/option.pl:
	% | ?- go2(S,W).
	%
	% W = 21/4,
	% S>=0,
	% S<50 ? ;
	%
	% W>5,
	% S=221/4-W,		  this line would be missing !!!
	% W=<21/4
	%
	(   {
		Type=t_none
	    ;	get_attr(V,itf3,(_,_,_,_,_,_,_,n,_))
	    }
	->  []
	;   dump_v(Context,t_none,V,I,H)
	),
  	(   {
		Type=t_none,
		get_attr(V,itf3,(_,_,_,_,_,_,_,n,_))
	    }
	->  % nonzero produces such
	    []
	;   dump_v(Context,Type,V,I,H)
	).
dump_linear(_,_) --> [].

dump_v(toplevel,Type,V,I,H) --> dump_var(Type,V,I,H).
% dump_v(compiler,Type,V,I,H) --> compiler_dump_var(Type,V,I,H).

dump_nonzero(V,Cont) -->
	{
	    get_attr(V,itf3,(_,_,lin(Lin),_,_,_,nonzero,_)),
	    !,
	    Lin = [I,_|H]
	},
	dump_nz(Cont,V,H,I).
dump_nonzero(_,_) --> [].

dump_nz(toplevel,V,H,I) --> dump_nz(V,H,I).
% dump_nz(compiler,V,H,I) --> compiler_dump_nz(V,H,I).

numbers_only(Y) :- 
	var(Y),
	!.
numbers_only(Y) :-
	number(Y),
	!.
numbers_only(Y) :-
	this_linear_solver(Solver),
	(   Solver == clpr
	->  What = 'a real number'
	;   Solver == clpq
	->  What = 'a rational number'
	),
	raise_exception(type_error(_X = Y,2,What,Y)).

attr_unify_hook((n,n,n,n,n,n,n,_),_) :- !.
attr_unify_hook((_,_,_,_,_,forward(F),_),Y) :-
	!,
	fwd_deref(F,Y).
attr_unify_hook((Ty,St,Li,Or,Cl,_,No,_),Y) :-
	numbers_only(Y),
	verify_nonzero(No,Y),
	verify_type(Ty,St,Y,Later,[]),
	verify_lin(Or,Cl,Li,Y),
	call_list(Later).

% call_list(List)
%
% Calls all the goals in List.

call_list([]).
call_list([G|Gs]) :-
	call(G),
	call_list(Gs).

%%%%
fwd_deref(X,Y) :-
	nonvar(X),
	X = Y.
fwd_deref(X,Y) :-
	var(X),
	(   get_attr(X,itf3,(_,_,_,_,_,forward(F),_))
	->  fwd_deref(F,Y)
	;   X = Y
	).

% verify_nonzero(Nonzero,Y)
%
% if Nonzero = nonzero, then verify that Y is not zero
% (if possible, otherwise set Y to be nonzero)

verify_nonzero(nonzero,Y) :-
	!,
	(   var(Y)
	->  (   get_attr(Y,itf3,(A,B,C,D,E,F,_,H))
	    ->  put_attr(Y,itf3,(A,B,C,D,E,F,nonzero,H))
	    ;   put_attr(Y,itf3,(n,n,n,n,n,n,nonzero,n,n,n))
	    )
	;   (   Y < -1.0e-10
	    ->	true
	    ;	Y > 1.0e-10
	    )
	).
verify_nonzero(_,_). % X is not nonzero

% verify_type(type(Type),strictness(Strict),Y,[OL|OLT],OLT)
%
% if possible verifies whether Y satisfies the type and strictness of X
% if not possible to verify, then returns the constraints that follow from
% the type and strictness

verify_type(type(Type),strictness(Strict),Y) -->
	!,
	verify_type2(Y,Type,Strict).
verify_type(_,_,_) --> [].

verify_type2(Y,TypeX,StrictX) -->
	{var(Y)},
	!,
	verify_type_var(TypeX,Y,StrictX).
verify_type2(Y,TypeX,StrictX) -->
	{verify_type_nonvar(TypeX,Y,StrictX)}.

% verify_type_nonvar(Type,Nonvar,Strictness)
%
% verifies whether the type and strictness are satisfied with the Nonvar

verify_type_nonvar(t_none,_,_).
verify_type_nonvar(t_l(L),Value,S) :- ilb(S,L,Value).
verify_type_nonvar(t_u(U),Value,S) :- iub(S,U,Value).
verify_type_nonvar(t_lu(L,U),Value,S) :-
	ilb(S,L,Value),
	iub(S,U,Value).
verify_type_nonvar(t_L(L),Value,S) :- ilb(S,L,Value).
verify_type_nonvar(t_U(U),Value,S) :- iub(S,U,Value).
verify_type_nonvar(t_Lu(L,U),Value,S) :-
	ilb(S,L,Value),
	iub(S,U,Value).
verify_type_nonvar(t_lU(L,U),Value,S) :-
	ilb(S,L,Value),
	iub(S,U,Value).

% ilb(Strict,Lower,Value) & iub(Strict,Upper,Value)
%
% check whether Value is satisfiable with the given lower/upper bound and
% strictness.
% strictness is encoded as follows:
% 2 = strict lower bound
% 1 = strict upper bound
% 3 = strict lower and upper bound
% 0 = no strict bounds

ilb(S,L,V) :-
	S /\ 2 =:= 0,
	!,
	L - V < 1.0e-10. % non-strict
ilb(_,L,V) :- L - V < -1.0e-10. % strict

iub(S,U,V) :-
	S /\ 1 =:= 0,
	!,
	V - U < 1.0e-10. % non-strict
iub(_,U,V) :- V - U < -1.0e-10. % strict

%
% Running some goals after X=Y simplifies the coding. It should be possible
% to run the goals here and taking care not to put_atts/2 on X ...
%

% verify_type_var(Type,Var,Strictness,[OutList|OutListTail],OutListTail)
%
% returns the inequalities following from a type and strictness satisfaction
% test with Var

verify_type_var(t_none,_,_) --> [].
verify_type_var(t_l(L),Y,S) --> llb(S,L,Y).
verify_type_var(t_u(U),Y,S) --> lub(S,U,Y).
verify_type_var(t_lu(L,U),Y,S) -->
	llb(S,L,Y),
	lub(S,U,Y).
verify_type_var(t_L(L),Y,S) --> llb(S,L,Y).
verify_type_var(t_U(U),Y,S) --> lub(S,U,Y).
verify_type_var(t_Lu(L,U),Y,S) -->
	llb(S,L,Y),
	lub(S,U,Y).
verify_type_var(t_lU(L,U),Y,S) -->
	llb(S,L,Y),
	lub(S,U,Y).

% llb(Strict,Lower,Value,[OL|OLT],OLT) and lub(Strict,Upper,Value,[OL|OLT],OLT)
%
% returns the inequalities following from the lower and upper bounds and the
% strictness see also lb and ub
llb(S,L,V) -->
	{S /\ 2 =:= 0},
	!,
	[{L =< V}].
llb(_,L,V) --> [{L < V}].

lub(S,U,V) -->
	{S /\ 1 =:= 0},
	!,
	[{V =< U}].
lub(_,U,V) -->	[{V < U}].

%
% We used to drop X from the class/basis to avoid trouble with subsequent
% put_atts/2 on X. Now we could let these dead but harmless updates happen.
% In R however, exported bindings might conflict, e.g. 0 \== 0.0
%
% If X is indep and we do _not_ solve for it, we are in deep shit
% because the ordering is violated.
%
verify_lin(order(OrdX),class(Class),lin(LinX),Y) :-
	!,
	(   indep(LinX,OrdX)
	->  detach_bounds_vlv(OrdX,LinX,Class,Y,NewLinX),
	    % if there were bounds, they are requeued already
	    class_drop(Class,Y),
	    nf(-Y,NfY),
	    deref(NfY,LinY),
	    add_linear_11(NewLinX,LinY,Lind),
	    (   nf_coeff_of(Lind,OrdX,_)
	    ->	% X is element of Lind
		solve_ord_x(Lind,OrdX,Class)
	    ;	solve(Lind)	% X is gone, can safely solve Lind
	    )
	;   class_drop(Class,Y),
	    nf(-Y,NfY),
	    deref(NfY,LinY),
	    add_linear_11(LinX,LinY,Lind),
	    solve(Lind)
	).
verify_lin(_,_,_,_).