/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_flags, [
			 set_prolog_flag/2,
			 current_prolog_flag/2,
			 prolog_flag/3,
			 push_prolog_flag/2,
			 pop_prolog_flag/1,
			 
			 set_ciao_flag/2,
			 current_ciao_flag/2,
			 ciao_flag/3,
			 push_ciao_flag/2,
			 pop_ciao_flag/1
			
			 % prompt/2,
			 % gc/0, nogc/0, fileerrors/0, nofileerrors/0
			],
	    [assertions, nortchecks, isomodes, define_flag]).

%% Migrated from Ciao to SWI-Prolog

:- data old_flag/2.

:- doc(push_prolog_flag(Flag, NewValue), "Same as
   @pred{set_prolog_flag/2}, but storing current value of @var{Flag} to
   restore it with @pred{pop_prolog_flag/1}.").


:- pred push_prolog_flag(+atm, +term) => atm * term.

push_prolog_flag(Flag, NewValue) :-
	nonvar(Flag),
	prolog_flag(Flag, OldValue, NewValue),
	asserta_fact(old_flag(Flag, OldValue)).

:- doc(pop_prolog_flag(Flag), "Restore the value of @var{Flag}
   previous to the last non-canceled @pred{push_prolog_flag/2} on it.").

:- pred push_ciao_flag(Flag, NewValue)
	+ equiv(push_prolog_flag(Flag, NewValue)).

push_ciao_flag(Flag, NewValue) :- push_prolog_flag(Flag, NewValue).

:- pred pop_prolog_flag(+atm) => atm.

pop_prolog_flag(Flag) :-
	nonvar(Flag),
	retract_fact(old_flag(Flag, OldValue)),
	!, % to avoid removal on backtracking --EMM
	prolog_flag(Flag, _, OldValue).

:- pred pop_ciao_flag(Flag) + equiv(pop_prolog_flag(Flag)).

pop_ciao_flag(Flag) :- pop_prolog_flag(Flag).


:- pred set_ciao_flag(FlagName, Value)
	+ equiv(set_prolog_flag(FlagName, Value)).

set_ciao_flag(FlagName, Value) :- set_prolog_flag(FlagName, Value).


:- pred current_ciao_flag(FlagName, Value)
	+ equiv(current_prolog_flag(FlagName, Value)).

current_ciao_flag(FlagName, Value) :- current_prolog_flag(FlagName, Value).


:- pred ciao_flag(Flag, Old, New) + equiv(prolog_flag(Flag, Old, New)).

ciao_flag(Flag, Old, New) :- prolog_flag(Flag, Old, New).
