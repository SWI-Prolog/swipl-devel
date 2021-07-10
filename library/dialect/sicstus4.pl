/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020-2021, SWI-Prolog Solutions b.v.
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

:- module(sicstus4,
	  [ nonmember/2,		% ?Element, ?List
	    statistics/2,		% ?Key, ?Value
	    op(1100, xfy, (do))
	  ]).
:- reexport(sicstus,
	    [ (block)/1,
	      if/3,
	      use_module/3,
	      bb_put/2,
	      bb_get/2,
	      bb_delete/2,
	      bb_update/3,
	      is_mutable/1 as mutable,
	      create_mutable/2,
	      get_mutable/2,
	      update_mutable/2,
	      sicstus_is_readable_stream/1,
	      read_line/1,
	      read_line/2,
	      trimcore/0,
	      prolog_flag/3,
	      op(1150, fx, (block)),
	      op(1150, fx, (mode)),
	      op(900, fy, (spy)),
	      op(900, fy, (nospy))
	    ]).

/** <module> SICStus 4 compatibility library

This library is intended to be activated using the directive below in
files that are designed for use with SICStus Prolog 4. The changes are in
effect until the end of the file and in each file loaded from this file.

    ==
    :- expects_dialect(sicstus4).
    ==

This library only provides compatibility with version 4 of SICStus Prolog.
For SICStus Prolog 3 compatibility, use library(dialect/sicstus) instead.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion. Please contribute to this package.
*/

% Note: Although the do operator is declared here, do loops currently
% aren't emulated by library(dialect/sicstus4).
:- op(1100, xfy, user:(do)).


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_sicstus4_library
%
%	Pushes searching for dialect/sicstus4 in front of every library
%	directory that contains such as sub-directory.

push_sicstus4_library :-
	(   absolute_file_name(library(dialect/sicstus4), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, sicstus4))),
	    fail
	;   true
	).


:- push_sicstus4_library.


%%	setup_dialect
%
%	Further dialect initialization.
%
%	sicstus4 currently performs the same initialization
%	as the sicstus (SICStus 3) dialect.

setup_dialect :- sicstus:setup_dialect.


		 /*******************************
		 *	  LIBRARY MODULES	*
		 *******************************/

%%	rename_module(?SICStus4Module, ?RenamedSICStus4Module) is nondet.
%
%	True if RenamedSICStus4Module is the  name  that  we use for the
%	SICStus 4 native module SICStus4Module. We do this in places where
%	the module-name conflicts. All explicitly qualified goals are
%	mapped to the SICStus 4 equivalent of the module.

:- multifile
	rename_module/2.

system:goal_expansion(M:Goal, SicstusM:Goal) :-
	atom(M),
	rename_module(M, SicstusM),
	prolog_load_context(dialect, sicstus4).


% SICStus 4 copy_term/2 behaves like SWI copy_term_nat/2.
user:goal_expansion(copy_term(Term, Copy), copy_term_nat(Term, Copy)) :-
	prolog_load_context(dialect, sicstus4).


%%	nonmember(?Element, ?List) is semidet.
%
%	True if Element doesn't unify with any element in List.
%	No variables are bound.

nonmember(Element, List) :- \+ memberchk(Element, List).

% As of SICStus 4.6.0, the following statistics/2 keys are still missing:
% Introduced in SICStus 4.0:
% * defragmentation
% Introduced in SICStus 4.1:
% * choice_used, choice_free
% * defrag_count, defrag_time
% * dpgc_count, dpgc_time
% * memory_culprit
% * memory_buckets
% Introduced in SICStus 4.3:
% * jit_count, jit_time

statistics(total_runtime, Stats) :- !, system:statistics(runtime, Stats).
% The following keys were introduced with SICStus Prolog 4.1.
statistics(memory_used, BytesUsed) :- !, system:statistics(memory, [BytesUsed, _]).
statistics(memory_free, BytesFree) :- !, system:statistics(memory, [_, BytesFree]).
statistics(global_stack_used, BytesUsed) :- !, system:statistics(global_stack, [BytesUsed, _]).
statistics(global_stack_free, BytesFree) :- !, system:statistics(global_stack, [_, BytesFree]).
statistics(local_stack_used, BytesUsed) :- !, system:statistics(local_stack, [BytesUsed, _]).
statistics(local_stack_free, BytesFree) :- !, system:statistics(local_stack, [_, BytesFree]).
statistics(trail_used, BytesUsed) :- !, system:statistics(trail, [BytesUsed, _]).
statistics(trail_free, BytesFree) :- !, system:statistics(trail, [_, BytesFree]).
statistics(atoms_used, BytesUsed) :- !, system:statistics(atom_space, BytesUsed).
statistics(atoms_nbused, CountUsed) :- !, system:statistics(atoms, CountUsed).
statistics(atoms_nbfree, CountFree) :- !, CountFree = 0.
statistics(ss_global, Count) :- !, system:statistics(stack_shifts, [Count, _, _]).
statistics(ss_local, Count) :- !, system:statistics(stack_shifts, [_, Count, _]).
statistics(ss_time, Time) :- !, system:statistics(stack_shifts, [_, _, Time]).
statistics(gc_count, Count) :- !, system:statistics(garbage_collection, [Count, _, _|_]).
statistics(gc_freed, BytesFreed) :- !, system:statistics(garbage_collection, [_, BytesFreed, _|_]).
statistics(gc_time, Time) :- !, system:statistics(garbage_collection, [_, _, Time|_]).
statistics(agc_count, Count) :- !, system:statistics(atom_garbage_collection, [Count, _, _]).
statistics(agc_freed, BytesFreed) :- !, system:statistics(atom_garbage_collection, [_, BytesFreed, _]).
statistics(agc_time, Time) :- !, system:statistics(atom_garbage_collection, [_, _, Time]).
statistics(dcgc_count, Count) :- !, system:statistics(clause_garbage_collection, [Count, _, _]).
statistics(dcgc_time, Time) :- !, system:statistics(clause_garbage_collection, [_, _, Time]).

:- use_module(sicstus, [statistics/2 as sicstus3_statistics]).
statistics(Keyword, Value) :- sicstus3_statistics(Keyword, Value).

% Provide (\)/2 as arithmetic function.  Ideally, we should be able to
% bind multiple names to built-in functions.  This is rather slow.  We
% could also consider adding \ internally, but not turning it into an
% operator.

:- arithmetic_function(user:(\)/2).

user:(\(X,Y,R)) :-
	R is xor(X,Y).
