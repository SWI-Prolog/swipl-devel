/*  $Id$

    Part of CPL(R) (Constraint Logic Programming over Reals)

    Author:        Leslie De Koninck
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
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

:- module(arith_r, 
	[
	    arith_eps/1,
	    arith_normalize/2,
	    integerp/1,
	    integerp/2
	]).

arith_eps(1.0e-10).				% for Monash #zero expansion 1.0e-12
eps(1.0e-10,-1.0e-10).

arith_normalize(X,Norm) :- 
	var(X),
	!,
	raise_exception(instantiation_error(arith_normalize(X,Norm),1)).
arith_normalize(rat(N,D),Norm) :- rat_float(N,D,Norm).
arith_normalize(X,Norm) :- 
	number(X),
	Norm is float(X).

integerp(X) :-
	floor(/*float?*/X)=:=X.

integerp(X,I) :-
	floor(/*float?*/X)=:=X,
	I is integer(X).

% copied from arith.pl.

rat_float(Nx,Dx,F) :-
	limit_encoding_length(Nx,Dx,1023,Nxl,Dxl),
	F is Nxl / Dxl.

limit_encoding_length(0,D,_,0,D) :- !.	% msb ...
limit_encoding_length(N,D,Bits,Nl,Dl) :-
	Shift is min(max(msb(abs(N)),msb(D))-Bits,
	min(msb(abs(N)),msb(D))),
	Shift > 0,
	!,
	Ns is N>>Shift,
	Ds is D>>Shift,
	Gcd is gcd(Ns,Ds),
	Nl is Ns//Gcd,
	Dl is Ds//Gcd.
limit_encoding_length(N,D,_,N,D).
