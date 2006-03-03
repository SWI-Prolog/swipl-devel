/*  

    Part of CLP(Q) (Constraint Logic Programming over Rationals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

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

:- module(itf_q,
	[
	    do_checks/8
	]).
:- use_module(bv_q,
	[
	    deref/2,
	    detach_bounds_vlv/5,
	    solve/1,
	    solve_ord_x/3
	]).
:- use_module(nf_q,
	[
	    nf/2
	]).
:- use_module(store_q,
	[
	    add_linear_11/3,
	    indep/2,
	    nf_coeff_of/3
	]).
:- use_module('../clpqr/class',
	[
	    class_drop/2
	]).

do_checks(Y,Ty,St,Li,Or,Cl,No,Later) :-
	numbers_only(Y),
	verify_nonzero(No,Y),
	verify_type(Ty,St,Y,Later,[]),
	verify_lin(Or,Cl,Li,Y),
	maplist(call,Later).

numbers_only(Y) :-
	(   var(Y)
	;   rational(Y) 
	;   throw(type_error(_X = Y,2,'a rational number',Y))
	),
	!.

% verify_nonzero(Nonzero,Y)
%
% if Nonzero = nonzero, then verify that Y is not zero
% (if possible, otherwise set Y to be nonzero)

verify_nonzero(nonzero,Y) :-
	(   var(Y)
	->  (   get_attr(Y,itf,Att)
	    ->  setarg(8,Att,nonzero)
	    ;   put_attr(Y,itf,t(clpq,n,n,n,n,n,n,nonzero,n,n,n))
	    )
	;   Y =\= 0
	).
verify_nonzero(n,_). % X is not nonzero

% verify_type(type(Type),strictness(Strict),Y,[OL|OLT],OLT)
%
% if possible verifies whether Y satisfies the type and strictness of X
% if not possible to verify, then returns the constraints that follow from
% the type and strictness

verify_type(type(Type),strictness(Strict),Y) -->
	verify_type2(Y,Type,Strict).
verify_type(n,n,_) --> [].

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
	L =< V. % non-strict
ilb(_,L,V) :- L < V. % strict

iub(S,U,V) :-
	S /\ 1 =:= 0,
	!,
	V =< U. % non-strict
iub(_,U,V) :- V < U. % strict

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
	[clpq:{L =< V}].
llb(_,L,V) --> [clpq:{L < V}].

lub(S,U,V) -->
	{S /\ 1 =:= 0},
	!,
	[clpq:{V =< U}].
lub(_,U,V) -->	[clpq:{V < U}].

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