/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sicstus,
	  [ (block)/1,			% +Heads
	    (public)/1,			% +Heads
	    use_module/3,		% ?Module, ?File, +Imports
	    bb_put/2,			% :Key, +Value
	    bb_get/2,			% :Key, -Value
	    bb_delete/2,		% :Key, -Value
	    bb_update/3,		% :Key, -Old, +New

	    op(1150, fx, (block)),
	    op(1150, fx, (public))
	  ]).

:- use_module(sicstus/block).

:- multifile
	system:goal_expansion/2.

% SICStus use_module/1 does not require the target to be a module.

system:goal_expansion(use_module(File), load_files(File, [if(changed)])).

%%	use_module(+Module, -File, +Imports) is det.
%%	use_module(-Module, +File, +Imports) is det.
%
%	This predicate can be used to import   from a named module while
%	the file-location of the module is unknown   or to get access to
%	the module-name loaded from a file.

use_module(Module, File, Imports) :-
	ground(File), !,
	absolute_file_name(File, Path,
			   [ file_type(prolog),
			     access(read)
			   ]),
	use_module(Path, Imports),
	module_property(Module, file(Path)).
use_module(Module, Path, Imports) :-
	atom(Module), !,
	module_property(Module, file(Path)),
	use_module(Path, Imports).

%%	public(+Heads)
%
%	Seems to be used in e.g., :- public user:help_pred/3.

public(_).

		 /*******************************
		 *	       BB_*		*
		 *******************************/

:- meta_predicate
	bb_put(:, +),
	bb_get(:, -),
	bb_delete(:, -),
	bb_update(:, -, +).

system:goal_expansion(bb_put(Key, Value), nb_setval(Atom, Value)) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_get(Key, Value), nb_current(Atom, Value)) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_delete(Key, Value),
		      (	  nb_current(Atom, Value),
			  nb_delete(Atom)
		      )) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_update(Key, Old, New),
		      (	  nb_current(Atom, Old),
			  nb_setval(Atom, New)
		      )) :-
	bb_key(Key, Atom).

bb_key(Module:Key, Atom) :-
	atom(Module), !,
	atomic(Key),
	atomic_list_concat([Module, Key], :, Atom).
bb_key(Key, Atom) :-
	atomic(Key),
	prolog_load_context(module, Module),
	atomic_list_concat([Module, Key], :, Atom).

%%	bb_put(:Name, +Value) is det.
%%	bb_get(:Name, -Value) is semidet.
%%	bb_delete(:Name, -Value) is semidet.
%%	bb_update(:Name, -Old, +New) is semidet.
%
%	SICStus compatible blackboard routines. The implementations only
%	deal with cases where the module-sensitive   key  is unknown and
%	meta-calling. Simple cases are  directly   mapped  to SWI-Prolog
%	non-backtrackable global variables.

bb_put(Key, Value) :-
	bb_key(Key, Name),
	nb_setval(Name, Value).
bb_get(Key, Value) :-
	bb_key(Key, Name),
	nb_current(Name, Value).
bb_delete(Key, Value) :-
	bb_key(Key, Name),
	nb_current(Name, Value),
	nb_delete(Name).
bb_update(Key, Old, New) :-
	bb_key(Key, Name),
	nb_current(Name, Old),
	nb_setval(Name, New).
