/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

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

:- module(chr_debug,
	  [ chr_show_store/1		% +Module
	  ]).
:- use_module(chr(chr_runtime)).
:- use_module(library(lists)).
:- set_prolog_flag(generate_debug_info, false).


%	chr_show_store(+Module)
%	
%	Prints all suspended constraints of module   Mod to the standard
%	output.

chr_show_store(Mod) :-
	(
		Mod:'$enumerate_suspensions'(Susp),
		arg(6,Susp,C),
		writeln(C),
		fail
	;
		true
	).
