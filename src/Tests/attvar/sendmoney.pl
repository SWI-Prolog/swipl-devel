/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Scrijvers
    E-mail:        tom.schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004, K.U.Leuven

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(sendmoney,
	  [ sendmoney/0
	  ]).
:- use_module(library('clp/bounds')).

sendmoney :-
	send(X),
	X == 
	[    [9, 5, 6, 7],
	     [1, 0, 8, 5],
	  [1, 0, 6, 5, 2]
	].

send([[S,E,N,D],  [M,O,R,E],  [M,O,N,E,Y]])  :-
	Digits  = [S,E,N,D,M,O,R,Y],
	Carries = [C1,C2,C3,C4],
	Digits  in 0..9,
	Carries in 0..1,
	
	M                #=              C4,
	O  +  10  *  C4  #=  M  +  S  +  C3,
	N  +  10  *  C3  #=  O  +  E  +  C2,
	E  +  10  *  C2  #=  R  +  N  +  C1,
	Y  +  10  *  C1  #=  E  +  D,
	
	M  #>=  1,
	S  #>=  1,
	all_different(Digits),
	label(Digits).
