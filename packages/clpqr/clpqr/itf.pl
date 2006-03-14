/*  

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


% attribute = t(CLP,type(_),strictness(_),lin(_),order(_),class(_),forward(_),
%		nonzero,target,keep_indep,keep)

:- module(itf,
	[
	    dump_linear/3,
	    dump_nonzero/3,
	    clp_type/2
	]).

clp_type(Var,Type) :- 
	(   get_attr(Var,itf,Att)
	->  arg(1,Att,Type)
	;   get_attr(Var,geler,Att)
	->  arg(1,Att,Type)
	).

dump_linear(V) -->
	{
	    get_attr(V,itf,Att),
	    arg(1,Att,CLP),
	    arg(2,Att,type(Type)),
	    arg(4,Att,lin(Lin)),
	    !,
	    Lin = [I,_|H]
	},
	(   {
		Type=t_none
	    ;	arg(9,Att,n)
	    }
	->  []
	;   dump_v(CLP,t_none,V,I,H)
	),
  	(   {
		Type=t_none,
		arg(9,Att,n) % attribute should not have changed by dump_v...
	    }
	->  % nonzero produces such
	    []
	;   dump_v(CLP,Type,V,I,H)
	).
dump_linear(_) --> [].

dump_v(clpq,Type,V,I,H) --> bv_q:dump_var(Type,V,I,H).
dump_v(clpr,Type,V,I,H) --> bv_r:dump_var(Type,V,I,H).

dump_nonzero(V) -->
	{
	    get_attr(V,itf,Att),
	    arg(1,Att,CLP),
	    arg(4,Att,lin(Lin)),
	    arg(8,Att,nonzero),
	    !,
	    Lin = [I,_|H]
	},
	dump_nz(CLP,V,H,I).
dump_nonzero(_) --> [].

dump_nz(clpq,V,H,I) --> bv_q:dump_nz(V,H,I).
dump_nz(clpr,V,H,I) --> bv_r:dump_nz(V,H,I).

attr_unify_hook(t(CLP,n,n,n,n,n,n,n,_,_,_),Y) :- 
	!,
	(   get_attr(Y,itf,AttY),
	    \+ arg(1,AttY,CLP)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',Y),context(_)))
	;   true
	).
attr_unify_hook(t(CLP,Ty,St,Li,Or,Cl,_,No,_,_,_),Y) :-
	(   get_attr(Y,itf,AttY),
	    \+ arg(1,AttY,CLP)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',Y),context(_)))
	;   true
	),	
	do_checks(CLP,Y,Ty,St,Li,Or,Cl,No,Later),
	maplist(call,Later).

do_checks(clpq,Y,Ty,St,Li,Or,Cl,No,Later) :-
	itf_q:do_checks(Y,Ty,St,Li,Or,Cl,No,Later).
do_checks(clpr,Y,Ty,St,Li,Or,Cl,No,Later) :-
	itf_r:do_checks(Y,Ty,St,Li,Or,Cl,No,Later).