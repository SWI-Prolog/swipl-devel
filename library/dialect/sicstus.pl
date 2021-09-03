/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2014, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sicstus,
	  [ (block)/1,			% +Heads

	    if/3,			% :If, :Then, :Else

	    use_module/3,		% ?Module, ?File, +Imports

	    bb_put/2,			% :Key, +Value
	    bb_get/2,			% :Key, -Value
	    bb_delete/2,		% :Key, -Value
	    bb_update/3,		% :Key, -Old, +New

	    is_mutable/1,		% @Term
	    create_mutable/2,		% ?Value, -Mutable
	    get_mutable/2,		% ?Value, +Mutable
	    update_mutable/2,		% ?Value, !Mutable

	    sicstus_is_readable_stream/1, % +Stream
	    read_line/1,		% -Codes
	    read_line/2,		% +Stream, -Codes

	    trimcore/0,

%	    call_residue/2,		% :Goal, -Residue

	    prolog_flag/3,		% +Flag, -Old, +New

	    statistics/2,		% ?Key, ?Value

	    op(1150, fx, (block)),
	    op(1150, fx, (mode)),
	    op(900, fy, (spy)),
	    op(900, fy, (nospy))
	  ]).

:- use_module(sicstus/block).
:- use_module(library(occurs)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(arithmetic)).


/** <module> SICStus 3 compatibility library

This library is intended to be activated   using  the directive below in
files that are designed for use with   SICStus Prolog 3. The changes are
in effect until the end of the file   and  in each file loaded from this
file.

    ==
    :- expects_dialect(sicstus).
    ==

This library only provides  compatibility  with   version  3  of SICStus
Prolog.     For     SICStus     Prolog      4     compatibility,     use
library(dialect/sicstus4) instead.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package.
*/

% SICStus built-in operators that SWI doesn't declare by default.
:- op(1150, fx, user:(mode)).
:- op(900, fy, user:(spy)).
:- op(900, fy, user:(nospy)).

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


in_sicstus_dialect :-
	(   prolog_load_context(dialect, sicstus)
	->  true
	;   prolog_load_context(dialect, sicstus4)
	).


		 /*******************************
		 *	      OPERATORS		*
		 *******************************/

%	declare all operators globally

user:goal_expansion(op(Pri,Ass,Name),
		    op(Pri,Ass,user:Name)) :-
	\+ qualified(Name),
	in_sicstus_dialect.

qualified(Var) :- var(Var), !, fail.
qualified(_:_).

% Import all operators from a module, even when using an explicit list
% of imports. This simulates the SICStus behavior, where operators are
% not module-sensitive and don't need to be listed in import lists.

user:goal_expansion(use_module(Module,Imports),
		    use_module(Module,[op(_,_,_)|Imports])) :-
	in_sicstus_dialect,
	% Prevent infinite recursion.
	\+ memberchk(op(_,_,_),Imports).

%%	setup_dialect
%
%	Further dialect initialization.
%
%	Currently this disables quoting when printing atoms,
%	which SWI does by default, but SICStus doesn't.
%	This globally modifies the print_write_options Prolog flag,
%	so this change also affects code that doesn't request
%	SICStus compatibility.

setup_dialect :-
	current_prolog_flag(print_write_options, Options),
	(   selectchk(quoted(true), Options, OptionsNoQuoted)
	->  set_prolog_flag(print_write_options, OptionsNoQuoted)
	;   true
	).


		 /*******************************
		 *	      CONTROL		*
		 *******************************/

:- meta_predicate
	if(0,0,0).

system:goal_expansion(if(If,Then,Else),
		      (If *-> Then ; Else)) :-
	in_sicstus_dialect,
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
		 *	  LIBRARY MODULES	*
		 *******************************/

%%	rename_module(?SICStusModule, ?RenamedSICSTusModule) is nondet.
%
%	True if RenamedSICSTusModule is the  name   that  we use for the
%	SICStus native module SICStusModule. We do  this in places where
%	the module-name conflicts. All explicitely   qualified goals are
%	mapped to the SICStus equivalent of the module.

:- multifile
	rename_module/2.

system:goal_expansion(M:Goal, SicstusM:Goal) :-
	atom(M),
	rename_module(M, SicstusM),
	prolog_load_context(dialect, sicstus).


		 /*******************************
		 *	     MODULES		*
		 *******************************/

% SICStus use_module/1 does not require the target to be a module.

system:goal_expansion(use_module(File), load_files(File, [if(changed)])) :-
	prolog_load_context(dialect, sicstus).

%%	use_module(+Module, -File, +Imports) is det.
%%	use_module(-Module, +File, +Imports) is det.
%
%	This predicate can be used to import   from a named module while
%	the file-location of the module is unknown   or to get access to
%	the module-name loaded from a file.
%
%	If both Module and File are  given,   we  use  Module and try to
%	unify File with the absolute  canonical   path  to the file from
%	which Module was loaded. However, we   succeed regardless of the
%	success of this unification.

use_module(Module, File, Imports) :-
	atom(Module), !,
	module_property(Module, file(Path)),
	use_module(Path, Imports),
	ignore(File = Path).
use_module(Module, File, Imports) :-
	ground(File), !,
	absolute_file_name(File, Path,
			   [ file_type(prolog),
			     access(read)
			   ]),
	use_module(Path, Imports),
	module_property(Module, file(Path)).
use_module(Module, _, _Imports) :-
	instantiation_error(Module).


		 /*******************************
		 *	 FOREIGN RESOURCES      *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SICStus uses foreign_resource(Name, Functions) and predicate definitions
similar to Quintus. qpforeign can generate  the   glue  code that can be
linked with swipl-ld. This  part  of   the  emulation  merely  skips the
declarations and Maps load_foreign_resource   to load_foreign_resource/2
from library(qpforeign).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

system:term_expansion(
	   (:- load_foreign_resource(Base)),
	   (:- initialization(load_foreign_resource(M:Base, Source), now))) :-
	prolog_load_context(source, Source),
	prolog_load_context(module, M).
system:term_expansion(
	   (:- module(Name, Exports, Options)),
	   [ (:- module(Name, Exports))
	   | Declarations
	   ]) :-
	(prolog_load_context(dialect, sicstus) ; prolog_load_context(dialect, sicstus4)),
	phrase(sicstus_module_decls(Options), Declarations).

sicstus_module_decls([]) --> [].
sicstus_module_decls([H|T]) -->
	sicstus_module_decl(H),
	sicstus_module_decls(T).

sicstus_module_decl(hidden(true)) --> !,
	[(:- set_prolog_flag(generate_debug_info, false))].
sicstus_module_decl(_) -->
	[].


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
		 *	     MUTABLES		*
		 *******************************/

%%	is_mutable(@Term) is det.
%
%	True if Term is bound to a mutable term.
%
%	@compat sicstus

is_mutable(Term) :-
	nonvar(Term),
	functor(Term, '$mutable', 2).

%%	create_mutable(?Value, -Mutable) is det.
%
%	Create a mutable term with the given initial Value.
%
%	@compat sicstus

create_mutable(Value, '$mutable'(Value,_)).

%%	get_mutable(?Value, +Mutable) is semidet.
%
%	True if Value unifies with the current value of Mutable.
%
%	@compat sicstus

get_mutable(Value, '$mutable'(Value,_)).

%%	update_mutable(?Value, !Mutable) is det.
%
%	Set the value of Mutable to Value.  The old binding is
%	restored on backtracking.
%
%	@see setarg/3.
%	@compat sicstus

update_mutable(Value, Mutable) :-
	functor(Mutable, '$mutable', 2), !,
	setarg(1, Mutable, Value).
update_mutable(_, Mutable) :-
	type_error(mutable, Mutable).


		 /*******************************
		 *	   LINE READING		*
		 *******************************/

%%	read_line(-Codes) is det.
%%	read_line(+Stream, -Codes) is det.
%
%	Read a line from the given or  current input. The line read does
%	_not_ include the line-termination character. Unifies Codes with
%	=end_of_file= if the end of the input is reached.
%
%	@compat sicstus
%	@see	The SWI-Prolog primitive is read_line_to_codes/2.

read_line(Codes) :-
    read_line_to_codes(current_input, Codes).

read_line(Stream, Codes) :-
    read_line_to_codes(Stream, Codes).

% Emulate the SICStus behavior of at_end_of_stream, which silently fails
% instead of blocking if reading from the stream would block.
% Also fails silently if Stream is not actually a valid stream.

sicstus_is_readable_stream(Stream) :-
	is_stream(Stream),
	stream_property(Stream, end_of_stream(not)).

user:goal_expansion(at_end_of_stream(Stream), \+ sicstus_is_readable_stream(Stream)) :-
	in_sicstus_dialect.

user:goal_expansion(at_end_of_stream, \+ sicstus_is_readable_stream(current_input)) :-
	in_sicstus_dialect.


		 /*******************************
		 *  COROUTINING & CONSTRAINTS	*
		 *******************************/

/* This is more complicated.  Gertjan van Noord decided to use
   copy_term/3 in Alpino.

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
*/

%%	trimcore is det.
%
%	Trims the stacks and releases unused heap memory to the
%	operating system where possible. Other tasks of the SICStus
%	trimcore/0 are automatically scheduled by SWI-Prolog.

trimcore :-
	trim_stacks,
	trim_heap.


		 /*******************************
		 *	       FLAGS		*
		 *******************************/

:- use_module(library(quintus), [prolog_flag/2 as quintus_flag]).

%%	prolog_flag(+Flag, -Old, +New) is semidet.
%
%	Query and set a Prolog flag. Use the debug/1 topic =prolog_flag=
%	to find the flags accessed using this predicate.

prolog_flag(Flag, Old, New) :-
	debug(prolog_flag, 'prolog_flag(~q, ~q, ~q)', [Flag, Old, New]),
	sicstus_flag(Flag, Old),
	set_prolog_flag(Flag, New).

:- public sicstus_flag/2.

sicstus_flag(host_type, HostType) :- !,
	% Not a perfect emulation. SWI's arch flag only contains the
	% architecture and OS family (e. g. 'x86_64-darwin'),
	% but SICStus host_type also contains the OS version number
	% (e. g. 'x86_64-darwin-15.6.0').
	% But this works well enough for code that just checks the
	% architecture/OS part and not the exact version.
	current_prolog_flag(arch, HostType).
sicstus_flag(system_type, Type) :- !,
	(   current_prolog_flag(saved_program, true)
	->  Type = runtime
	;   Type = development
	).
sicstus_flag(Name, Value) :-
	quintus_flag(Name, Value).

% Replace all current_prolog_flag/2 and prolog_flag/2 calls with
% sicstus_flag/2. prolog_flag/2 can also be autoloaded from
% library(quintus) - this goal expansion ensures that sicstus_flag/2
% takes priority when SICStus emulation is active.

user:goal_expansion(Goal, sicstus:sicstus_flag(Name, Value)) :-
	nonvar(Goal),
	(Goal = current_prolog_flag(Name, Value) ; Goal = prolog_flag(Name, Value)),
	in_sicstus_dialect.


% As of SICStus 3.2.11, the following statistics/2 keys are still missing:
% * choice

statistics(heap, Stats) :- !, system:statistics(program, Stats).
statistics(garbage_collection, [Count, Freed, Time]) :- !,
	% Remove fourth list element (SWI extension).
	system:statistics(garbage_collection, [Count, Freed, Time|_]).
statistics(atoms, [H|T]) :- !,
	% SWI natively provides two different values under the atoms key:
	% the number of atoms as a single integer,
	% and a Quintus/SICStus-compatible list of atom usage statistics.
	% Which value is returned when calling statistics(atoms, X)
	% depends on the value of X before the call:
	% if X is unbound, the single integer is returned,
	% but if X is already bound to a (usually non-ground) list,
	% the list of statistics is returned instead.

	% Here we just force the list to be returned in all cases
	% if SICStus emulation is active, by forcing the second argument
	% to be bound to a list.
	system:statistics(atoms, [H|T]).

statistics(Keyword, Value) :- system:statistics(Keyword, Value).


		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

% Provide (#)/2 as arithmetic function.  Ideally, we should be able to
% bind multiple names to built-in functions.  This is rather slow.  We
% could also consider adding # internally, but not turning it into an
% operator.

:- op(500, yfx, user:(#)).

:- arithmetic_function(user:(#)/2).

user:(#(X,Y,R)) :-
	R is xor(X,Y).


		 /*******************************
		 *	       HACKS		*
		 *******************************/

%%	prolog:'$breaklevel'(-BreakLevel, Unknown)
%
%	Query the current break-level

prolog:'$breaklevel'(BreakLevel, _) :-
	current_prolog_flag(break_level, BreakLevel), !.
prolog:'$breaklevel'(0, _).
