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

	    if/3,			% :If, :Then, :Else

	    use_module/3,		% ?Module, ?File, +Imports

	    bb_put/2,			% :Key, +Value
	    bb_get/2,			% :Key, -Value
	    bb_delete/2,		% :Key, -Value
	    bb_update/3,		% :Key, -Old, +New

	    call_residue/2,		% :Goal, -Residue

	    prolog_flag/3,		% +Flag, -Old, +New
	    version/0,
	    version/1,			% +Message

	    op(1150, fx, (block)),
	    op(1150, fx, (public))
	  ]).

:- use_module(sicstus/block).
:- use_module(library(occurs)).
:- use_module(library(debug)).

/** <module> SICStus compatibility library

This library is intended to be activated   using  the directive below in
files that are designed for use with  SICStus Prolog. The changes are in
effect until the end of the file and in each file loaded from this file.

    ==
    :- expects_dialect(sicstus).
    ==

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package.
*/

:- multifile
	system:goal_expansion/2.


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_sicstus_library
%
%	Pushes searching for dialect/sicstus in   front of every library
%	directory that contains such as sub-directory.

push_sicstus_library :-
	(   absolute_file_name(library(dialect/sicstus), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, sicstus))),
	    fail
	;   true
	).


:- push_sicstus_library.


		 /*******************************
		 *	      OPERATORS		*
		 *******************************/

%	declare all operators globally

system:goal_expansion(op(Pri,Ass,Name),
		      op(Pri,Ass,user:Name)) :-
	\+ qualified(Name),
	prolog_load_context(dialect, sicstus).

qualified(Var) :- var(Var), !, fail.
qualified(_:_).


		 /*******************************
		 *	      CONTROL		*
		 *******************************/

:- meta_predicate
	if(0,0,0).

system:goal_expansion(if(If,Then,Else),
		      (If *-> Then ; Else)) :-
	\+ (sub_term(X, [If,Then,Else]), X == !).

%%	if(:If, :Then, :Else)
%
%	Same  as  SWI-Prolog  soft-cut  construct.   Normally,  this  is
%	translated using goal-expansion. If either term contains a !, we
%	use meta-calling for full compatibility (i.e., scoping the cut).

if(If, Then, Else) :-
	(   If
	*-> Then
	;   Else
	).


		 /*******************************
		 *	     MODULES		*
		 *******************************/

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


		 /*******************************
		 *  COROUTINING & CONSTRAINTS	*
		 *******************************/

%%	call_residue(:Goal, -Residue) is nondet.
%
%	Residue is a list of VarSet-Goal.  Note that this implementation
%	is   incomplete.   Please   consult     the   documentation   of
%	call_residue_vars/2 for known issues.

:- meta_predicate
	call_residue(0, -).

call_residue(Goal, Residue) :-
	call_residue_vars(Goal, Vars),
	(   Vars == []
	->  Residue = []
	;   copy_term(Vars, _AllVars, Goals),
	    phrase(vars_by_goal(Goals), Residue)
	).

vars_by_goal((A,B)) --> !,
	vars_by_goal(A),
	vars_by_goal(B).
vars_by_goal(Goal) -->
	{ term_attvars(Goal, AttVars),
	  sort(AttVars, VarSet)
	},
	[ VarSet-Goal ].


		 /*******************************
		 *	       FLAGS		*
		 *******************************/

%%	prolog_flag(+Flag, -Old, +New) is semidet.
%
%	Query and set a Prolog flag. Use the debug/1 topic =prolog_flag=
%	to find the flags accessed using this predicate.

prolog_flag(Flag, Old, New) :-
	debug(prolog_flag, 'prolog_flag(~q, ~q, ~q)', [Flag, Old, New]),
	current_prolog_flag(Flag, Old),
	set_prolog_flag(Flag, New).

:- dynamic
	version_msg/1.

%%	version is det.
%
%	Print welcome message.
%
%	@tbd	This should be merged into the message-system

version :-
	'$welcome',
	forall(version_msg(Msg),
	       print_message(banner, format('~w', [Msg]))).

%%	version(+Message) is det.
%
%	Add message to version/0

version(Message) :-
	assertz(version_msg(Message)).


		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

% Provide (#)/2 as arithmetic function.  Ideally, we should be able to
% bind multiple names to built-in functions.  This is rather slow.  We
% could also consider adding # internally, but not turning it into an
% operator.

:- op(500, yfx, #).

:- arithmetic_function((#)/2).

#(X,Y,R) :-
	R is xor(X,Y).
