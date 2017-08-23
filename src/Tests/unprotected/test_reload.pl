/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_reload,
	  [ test_reload/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(readutil)).

/** <module> Test reloading code

These tests test incremental  compilation   as  introduced in SWI-Prolog
7.3.12.
*/

test_reload :-
	run_tests([ reconsult
		  ]).

%%	reload(+File, +Version) is det.
%
%	Reload file in given Version, but  pretending we are loading the
%	same file, such that Prolog considers this a _reconsult_.

reload(File, Version) :-
	file_version_source(File, Version, Src),
	atomic_list_concat([File, '.pl'], FileID),
	setup_call_cleanup(
	    open_string(Src, In),
	    load_files(FileID, [stream(In)]),
	    close(In)).

file_version_source(File, Version, Src) :-
	source_file(file_version_source(_, _, _), MyFile),
	file_directory_name(MyFile, MyDir),
	atomic_list_concat([MyDir, '/reload/', File, '.pl'], SrcFile),
	read_file_to_string(SrcFile, SrcString, []),
	split_string(SrcString, "%%%%", "%\n\r", Versions),
	nth1(Version, Versions, SrcContent),
	(   sub_string(SrcContent, _, _, _, ":- module(")
	->  Src = SrcContent
	;   format(string(Src),
		   ":- module(~q, []).~n~n~s~n", [File, SrcContent])
	).


		 /*******************************
		 *	       TESTS		*
		 *******************************/

:- begin_tests(reconsult).

test(simple, Clauses0 =@= Clauses1) :-	% no changes
	reload(simple, 1),
	clauses(simple, Clauses0),
	reload(simple, 1),
	clauses(simple, Clauses1).
test(add_clause, all(X == [a,b,c])) :-
	reload(add_clause, 1),
	reload(add_clause, 2),
	add_clause:p1(X).
test(del_clause, all(X == [a,c])) :-
	reload(del_clause, 1),
	reload(del_clause, 2),
	del_clause:p1(X).
test(del_clause_a, all(X == [b,c])) :-
	reload(del_clause_a, 1),
	reload(del_clause_a, 2),
	del_clause_a:p1(X).
test(del_clause_z, all(X == [a,b])) :-
	reload(del_clause_z, 1),
	reload(del_clause_z, 2),
	del_clause_z:p1(X).
test(replace_clause, all(X == [b])) :-
	reload(replace_clause, 1),
	reload(replace_clause, 2),
	replace_clause:p(X).
test(add_pred, Clauses == [p1,p2]) :-
	reload(add_pred, 1),
	reload(add_pred, 2),
	clauses(add_pred, Clauses).
test(del_pred, Clauses == [p1]) :-
	reload(del_pred, 1),
	assertion(del_pred:p2),
	reload(del_pred, 2),
	clauses(del_pred, Clauses),
	catch(del_pred:p2, Error, true),
	assertion(subsumes_term(error(existence_error(_,_),_), Error)).
test(add_attr_end) :-
	reload(add_attr_end, 1),
	reload(add_attr_end, 2),
	assertion(predicate_property(add_attr_end:p, public)).
test(del_attr_end) :-
	reload(del_attr_end, 1),
	assertion(predicate_property(del_attr_end:p, public)),
	reload(del_attr_end, 2),
	assertion(\+ predicate_property(del_attr_end:p, public)).
test(del_dynamic) :-				% dynamic remains
	reload(del_dynamic, 1),
	reload(del_dynamic, 2),
	assertion(predicate_property(del_dynamic:p(_), dynamic)).
test(add_thread_local) :-
	reload(add_thread_local, 1),
	collect_messages(reload(add_thread_local, 2),
			 Messages),
	assertion(error_in_messages(
		      permission_error(modify,static_procedure,
				       add_thread_local:p/1),
		      Messages)).
test(del_discontiguous) :-
	reload(del_discontiguous, 1),
	collect_messages(reload(del_discontiguous, 2),
			 Messages),
	assertion(memberchk(warning-discontiguous(_,_), Messages)).
test(add_meta_predicate) :-
	reload(add_meta_predicate, 1),
	assertion(\+ add_meta_predicate:p(x)),
	reload(add_meta_predicate, 2),
	assertion(add_meta_predicate:p(x)).
test(del_meta_predicate) :-
	reload(del_meta_predicate, 1),
	assertion(del_meta_predicate:p(x)),
	reload(del_meta_predicate, 2),
	assertion(\+ del_meta_predicate:p(x)).
test(del_export, Exports == [p1/0]) :-
	reload(del_export, 1),
	reload(del_export, 2),
	module_property(del_export, exports(Exports)).
test(reload_maplist) :-
	reload(reload_maplist, 1),
	assertion(reload_maplist:square_list([1,2,3], [1,16,81])),
	reload(reload_maplist, 1),
	assertion(reload_maplist:square_list([1,2,3], [1,16,81])).
test(mod_dynamic) :-
	reload(mod_dynamic, 1),
	assertion(mod_dynamic:p(x)),
	reload(mod_dynamic, 2),
	assertion(forall(mod_dynamic:p(X), mod_dynamic:p(X))).
test(goal_expansion) :-
	reload(goal_expansion, 1),
	reload(goal_expansion, 1).

:- end_tests(reconsult).


		 /*******************************
		 *	      UTIL		*
		 *******************************/

file_version_terms(File, Version, Terms) :-
	file_version_source(File, Version, Src),
	setup_call_cleanup(
	    open_string(Src, In),
	    read_stream_to_terms(In, Terms),
	    close(In)).

read_stream_to_terms(In, Terms) :-
	read_term(In, Term0, []),
	read_stream_to_terms(Term0, In, Terms).

read_stream_to_terms(end_of_file, _, []) :- !.
read_stream_to_terms(Term0, In, [Term0|Terms]) :-
	read_term(In, Term1, []),
	read_stream_to_terms(Term1, In, Terms).

%%	clauses(+Module, -Clauses) is det.
%
%	True if Clauses is a sorted list of Clauses in Module.

clauses(Module, Clauses) :-
	findall(Clause, module_clause(Module, Clause), Clauses0),
	msort(Clauses0, Clauses).

module_clause(Module, Clause) :-
	Head = Module:Plain,
	predicate_property(Head, number_of_clauses(_)),
	\+ predicate_property(Head, imported_from(_)),
	Module:clause(Plain, Body),
	(   Body == true
	->  Clause = Plain
	;   Clause = (Plain :- Body)
	).

:- thread_local	message/2.
:- meta_predicate collect_messages(0,-).

collect_messages(Goal, Messages) :-
	setup_call_cleanup(
	    asserta((user:thread_message_hook(Term, Level, _Lines) :-
		      assert(message(Level, Term))), Ref),
	    call(Goal),
	    erase(Ref)),
	findall(Level-Term, retract(message(Level, Term)), Messages).

error_in_messages(Error, Messages) :-
	memberchk(error-error(Error,_), Messages).

:- if(current_predicate(prolog_debug/1)).

debug :-
	prolog_debug(msg_reconsult),
	prolog_debug(msg_reconsult_pred),
	prolog_debug(msg_reconsult_clause),
	prolog_debug(msg_reconsult_module).

nodebug :-
	prolog_nodebug(msg_reconsult),
	prolog_nodebug(msg_reconsult_pred),
	prolog_nodebug(msg_reconsult_clause),
	prolog_nodebug(msg_reconsult_module).

:- endif.
