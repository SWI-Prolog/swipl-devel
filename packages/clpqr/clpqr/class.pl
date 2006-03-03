/*  $Id$

    Part of CLP(Q,R) (Constraint Logic Programming over Rationals and Reals)

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

:- module(class,
	[
	    class_allvars/2,
	    class_new/5,
	    class_drop/2,
	    class_basis/2,
	    class_basis_add/3,
	    class_basis_drop/2,
	    class_basis_pivot/3,
	    class_get_clp/2,
	    class_get_prio/2,
	    class_put_prio/2,
	    ordering/1,
	    arrangement/2
	]).

:- use_module(ordering,
	[
	    combine/3,
	    ordering/1,
	    arrangement/2
	]).
:- use_module(library(lists),
	      [ append/3
	      ]).
	
% called when two classes are unified: the allvars lists are appended to eachother, as well as the basis
% lists. 
% 
% note: La=[A,B,...,C|Lat], Lb=[D,E,...,F|Lbt], so new La = [A,B,...,C,D,E,...,F|Lbt]

attr_unify_hook(class(CLP,La,Lat,ABasis,PrioA),Y) :-
	!,
	var(Y),
	get_attr(Y,class,class(CLP,Lb,Lbt,BBasis,PrioB)),
	Lat = Lb,
	append(ABasis,BBasis,CBasis),
	combine(PrioA,PrioB,PrioC),
	put_attr(Y,class,class(CLP,La,Lbt,CBasis,PrioC)).
attr_unify_hook(_,_).

class_new(Class,CLP,All,AllT,Basis) :-
	put_attr(Su,class,class(CLP,All,AllT,Basis,[])),
	Su = Class.

class_get_prio(Class,Priority) :- 
	get_attr(Class,class,class(_,_,_,_,Priority)).

class_get_clp(Class,CLP) :-
	get_attr(Class,class,class(CLP,_,_,_,_)).

class_put_prio(Class,Priority) :- 
	get_attr(Class,class,class(CLP,All,AllT,Basis,_)),
	put_attr(Class,class,class(CLP,All,AllT,Basis,Priority)).

class_drop(Class,X) :-
	get_attr(Class,class,class(CLP,Allvars,Tail,Basis,Priority)),
	delete_first(Allvars,X,NewAllvars),
	delete_first(Basis,X,NewBasis),	
	put_attr(Class,class,class(CLP,NewAllvars,Tail,NewBasis,Priority)).

class_allvars(Class,All) :- get_attr(Class,class,class(_,All,_,_,_)).

% class_basis(Class,Basis)
%
% Returns the basis of class Class.

class_basis(Class,Basis) :- get_attr(Class,class,class(_,_,_,Basis,_)).

% class_basis_add(Class,X,NewBasis)
%
% adds X in front of the basis and returns the new basis

class_basis_add(Class,X,NewBasis) :-
	NewBasis = [X|Basis],
	get_attr(Class,class,class(CLP,All,AllT,Basis,Priority)),
	put_attr(Class,class,class(CLP,All,AllT,NewBasis,Priority)).

% class_basis_drop(Class,X)
%
% removes the first occurence of X from the basis (if exists)

class_basis_drop(Class,X) :-
	get_attr(Class,class,class(CLP,All,AllT,Basis0,Priority)),
	delete_first(Basis0,X,Basis),
	Basis0 \== Basis,   % anything deleted ?
	!,
	put_attr(Class,class,class(CLP,All,AllT,Basis,Priority)).
class_basis_drop(_,_).

% class_basis_pivot(Class,Enter,Leave)
%
% removes first occurence of Leave from the basis and adds Enter in front of the basis

class_basis_pivot(Class,Enter,Leave) :-
	get_attr(Class,class,class(CLP,All,AllT,Basis0,Priority)),
	delete_first(Basis0,Leave,Basis1),
	put_attr(Class,class,class(CLP,All,AllT,[Enter|Basis1],Priority)).

% delete_first(Old,Element,New)
%
% removes the first occurence of Element from Old and returns the result in New
%
% note: test via syntactic equality, not unifiability

delete_first(L,_,Res) :-
	var(L),
	!,
	Res = L.
delete_first([],_,[]).
delete_first([Y|Ys],X,Res) :-
	(   X==Y
	->  Res = Ys
	;   Res = [Y|Tail],
	    delete_first(Ys,X,Tail)
	).
