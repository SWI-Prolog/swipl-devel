/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_w3c_test,
	  [ process_manifest/0,
	    process_manifest/1,
	    run_tests/0,		% run all tests
	    run/0,			% run selected test
	    show/1,			% RDF diagram for File
	    run_test/1			% run a single test
	  ]).

					% get libraries locally
:- asserta(user:file_search_path(library, '.')).

:- use_module(rdf).			% our RDF parser
:- use_module(rdf_ntriples).			% read .nt files
:- load_files([ library(pce),
		library(toolbar),
		library(pce_report),
		rdf_diagram,
		library('emacs/emacs')
	      ],
	      [ silent(true)
	      ]).

:- dynamic
	verbose/0.
%verbose.

set_verbose :-
	verbose, !.
set_verbose :-
	assert(verbose).

:- dynamic
	rdf/3.

ns(test,
   'http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#').

local('http://www.w3.org/2000/10/rdf-tests/rdfcore/',
      'W3Ctests/').

process_manifest :-
	process_manifest('W3Ctests/Manifest.rdf').
process_manifest(Manifest) :-
	retractall(rdf(_,_,_)),
	load_rdf(Manifest, Triples),
	assert_triples(Triples).

assert_triples([]).
assert_triples([rdf(S, P, O)|T]) :-
	canonise(S, Subject),
	canonise(P, Predicate),
	canonise(O, Object),
	assert(rdf(Subject, Predicate, Object)),
	assert_triples(T).

canonise(NS:Name, N:Name) :-
	ns(N, NS), !.
canonise(Absolute, N:Name) :-
	atom(Absolute),
	ns(N, NS),
	atom_concat(NS, Name, Absolute), !.
canonise(X, X).
	

run_tests :-
	process_manifest,
	start_tests,
	(   rdf(About, rdf:type, test:Type),
	    \+ rdf(About, test:status, literal('OBSOLETE')),
	    test_type(Type),
%	    once(run_test(About)),		% Should not be needed
	    run_test(About),
	    fail
	;   true
	), !,
	report_results.

test_type('PositiveParserTest').
%test_type('NegativeParserTest').

run_test(Test) :-
	rdf(Test, test:inputDocument, In),
	local_file(In, InFile),
	exists_file(InFile),
	(   load_rdf(InFile, RDF,
		     [ base_uri(In),
		       expand_foreach(true)
		     ])
	->  true
	;   RDF = []
	),
	Data = [ source(InFile),
		 result(RDF),
		 norm(NT),
		 substitutions(Substitions)
	       ],
					% there may be alternative output
					% documents
	(   rdf(Test, test:outputDocument, Out),
	    local_file(Out, NTFile),
	    load_rdf_ntriples(NTFile, NT),
	    feedback('Comparing to ~w~n', [NTFile]),
	    compare_triples(RDF, NT, Substitions)
	->  test_result(pass, Test, Data)
					% if all fails, display the first
	;   rdf(Test, test:outputDocument, Out),
	    local_file(Out, NTFile),
	    load_rdf_ntriples(NTFile, NT),
	    Substitions = [],
	    test_result(fail, Test, Data)
	).


local_file(URL, File) :-
	local(URLPrefix, FilePrefix),
	atom_concat(URLPrefix, Base, URL), !,
	atom_concat(FilePrefix, Base, File).


		 /*******************************
		 *	       GUI		*
		 *******************************/

:- pce_begin_class(w3c_rdf_test_gui, frame).

initialise(F, Show:chain) :->
	send_super(F, initialise, 'W3C RDF test suite results'),
	send(F, append, new(B, browser)),
	send(B, hor_stretch, 100),
	send(B, hor_shrink, 100),
	(   send(Show, member, source)
	->  new(V, emacs_view(height := 3)),
	    send(V, name, text)
	;   true
	),
	(   send(Show, member, result)
	->  new(R, rdf_diagram),
	    send(R, name, result),
	    send(R, label, 'Result')
	;   true
	),
	(   send(Show, member, norm)
	->  new(N, rdf_diagram),
	    send(N, name, norm),
	    send(N, label, 'Norm')
	;   true
	),
	stack_windows([V,R,N], _, W),
	(   nonvar(W)
	->  send(W, right, B)
	;   true
	),
	send(new(D, tool_dialog(F)), above, B),
	send(new(report_dialog), below, B),
	send(F, fill_menu, D),
	send(F, fill_browser, B).

stack_windows([], L, L).
stack_windows([H|T], W0, W) :-
	var(H), !,
	stack_windows(T, W0, W).
stack_windows([H|T], W0, W) :-
	var(W0), !,
	stack_windows(T, H, W).
stack_windows([H|T], WL, W) :-
	send(H, below, WL),
	stack_windows(T, H, W).

fill_menu(F, D:tool_dialog) :->
	send_list(D,
		  [ append(menu_item(exit, message(F, destroy)),
			   file)
		  ]).

fill_browser(_F, B:browser) :->
	send(B, style, pass, style(colour := dark_green)),
	send(B, style, fail, style(colour := red)),
	send(B?image, recogniser,
	     handler(ms_right_down,
		     and(message(B, selection,
				 ?(B, dict_item, @event)),
			 new(or)))),
	send(B, popup, new(P, popup)),
	send(B, select_message, message(@arg1, run)),
	send_list(P, append,
		  [ menu_item(run,
			      message(@arg1, run)),
		    menu_item(edit,
			      message(@arg1, edit_test)),
		    gap,
		    menu_item(show_result,
			      message(@arg1, show_triples, result)),
		    menu_item(show_norm,
			      message(@arg1, show_triples, norm)),
		    gap,
		    menu_item(discussion,
			      message(@arg1, open_url, discussion),
			      condition :=
			      message(@arg1, has_url, discussion)),
		    menu_item(approval,
			      message(@arg1, open_url, approval),
			      condition :=
			      message(@arg1, has_url, approval)),
		    gap,
		    menu_item(copy_test_uri,
			      message(@arg1, copy_test_uri))
		  ]).


test_result(F, Result:{pass,fail}, Test:name, Data:prolog) :->
	"Test failed"::
	get(F, member, browser, B),
	(   get(B, member, Test, Item)
	->  send(Item, object, prolog(Data)),
	    send(Item, style, Result)
	;   send(B, append,
		 rdf_test_item(Test, @default, prolog(Data), Result))
	).

clear(F) :->
	get(F, member, browser, B),
	send(B, clear).

summarise(F) :->
	get(F, member, browser, Browser),
	new(Pass, number(0)),
	new(Fail, number(0)),
	send(Browser?members, for_all,
	     if(@arg1?style == pass,
		message(Pass, plus, 1),
		message(Fail, plus, 1))),
	send(F, report, status, '%d tests succeeded; %d failed',
	     Pass, Fail).

:- pce_end_class(w3c_rdf_test_gui).

:- pce_begin_class(rdf_test_item, dict_item).


edit_test(Item) :->
	"Edit input document of test"::
	get(Item, object, List),
	member(source(InFile), List),
	edit(file(InFile)).

show_triples(Item, Set:{result,norm}) :->
	"Show result of our parser"::
	get(Item, key, Test),
	get(Item, object, List),
	Term =.. [Set,Triples],
	member(Term, List),
	send(Item, show_diagram(Triples,
				string('%s for %s', Set?label_name, Test))).

show_diagram(_Item, Triples:prolog, Label:name) :->
	"Show diagram for triples"::
	new(D, rdf_diagram(Label)),
	send(new(report_dialog), below, D),
	send(D, triples, Triples),
	send(D, open).

open_url(Item, Which:name) :->
	"Open associated URL in browser"::
	get(Item, key, Test),
	rdf(Test, test:Which, URL),
	www_open_url(URL).

has_url(Item, Which:name) :->
	"Test if item has URL"::
	get(Item, key, Test),
	rdf(Test, test:Which, _URL).

run(Item) :->
	"Re-run the test"::
	get(Item, key, Test),
	run_test(Test),
	send(Item, show).

copy_test_uri(Item) :->
	"Copy URI of test to clipboard"::
	get(Item, key, Test),
	send(@display, copy, Test).

show(Item) :->
	"Show source, result and norm diagrams"::
	get(Item?image, frame, Frame),
	get(Item, object, List),
	(   get(Frame, member, result, Result)
	->  member(result(RTriples), List),
	    send(Result, triples, RTriples)
	;   true
	),
	(   get(Frame, member, norm, Norm)
	->  member(norm(NTriples), List),
	    send(Norm, triples, NTriples)
	;   true
	),
	(   get(Frame, member, text, View)
	->  member(source(File), List),
	    send(View, text_buffer, new(TB, emacs_buffer(File))),
					% scroll to RDF text
	    (   member(Pattern, [':RDF', 'RDF']),
		get(TB, find, 0, Pattern, Start),
		get(TB, scan, Start, line, 0, start, BOL)
	    ->  send(View, scroll_to, BOL, 1)
	    ;   true
	    )
	;   true
	).
%	member(substitutions(Substitutions), List),
%	send(Result, copy_layout, Norm, Substitutions),
	
:- pce_end_class(rdf_test_item).


:- pce_global(@rdf_test_gui, make_rdf_test_gui).

make_rdf_test_gui(Ref) :-
	send(new(Ref, w3c_rdf_test_gui(chain(source,result))), open).


test_result(Result, Test, Data) :-
	send(@rdf_test_gui, test_result, Result, Test, Data),
	(   Result == fail, verbose
	->  member(result(Our), Data),
	    length(Our, OurLength),
	    format('~N** Our Triples (~w)~n', OurLength),
	    pp(Our),
	    member(norm(Norm), Data),
	    length(Norm, NormLength),
	    format('~N** Normative Triples (~w)~n', NormLength),
	    pp(Norm)
	;   true
	).
	    


start_tests :-
	send(@rdf_test_gui, clear).

report_results :-
	send(@rdf_test_gui, summarise).

run :-
	set_verbose,
	get(@rdf_test_gui, member, browser, B),
	get(B, selection, DI),
	get(DI, key, Test),
	run_test(Test).


		 /*******************************
		 *	     SHOW A FILE	*
		 *******************************/


show(File) :-
	rdf_diagram_from_file(File).
	

		 /*******************************
		 *	     COMPARING		*
		 *******************************/

%	compare_triples(+PlRDF, +NTRDF, -Substitions)
%
%	Compare two models and if they are equal, return a list of
%	PlID = NTID, mapping NodeID elements.


compare_triples(A, B, Substitutions) :-
	compare_list(A, B, [], Substitutions).

compare_list([], [], S, S).
compare_list(L1, L2, S0, S) :-
	take_bag(L1, B1, E1, R1), !,
	take_bag(L2, B2, E2, R2),
	compare_field(B1, B2, S0, S1),
	compare_bags(E1, E2, S1, S2),
	compare_list(R1, R2, S2, S).
compare_list([H1|T1], In2, S0, S) :-
	select(H2, In2, T2),
	compare_triple(H1, H2, S0, S1), % put(.), flush_output,
	compare_list(T1, T2, S1, S).

compare_triple(rdf(Subj1,P1,O1), rdf(Subj2, P2, O2), S0, S) :-
	compare_field(Subj1, Subj2, S0, S1),
	compare_field(P1, P2, S1, S2),
	compare_field(O1, O2, S2, S).

compare_field(X, X, S, S) :- !.
compare_field(literal(X), xml(X), S, S) :- !. % TBD
compare_field(rdf:Name, Atom, S, S) :-
	atom(Atom),
	rdf_parser:rdf_name_space(NS),
	atom_concat(NS, Name, Atom), !.
compare_field(NS:Name, Atom, S, S) :-
	atom(Atom),
	atom_concat(NS, Name, Atom), !.
compare_field(X, node(Id), S, S) :-
	memberchk(X=Id, S), !.
compare_field(X, node(Id), S, [X=Id|S]) :-
	\+ memberchk(X=_, S),
	atom(X),
	generated_prefix(Prefix),
	sub_atom(X, 0, _, _, Prefix), !,
	feedback('Assume ~w = ~w~n', [X, node(Id)]).

generated_prefix(Prefix) :-
	rdf_truple:anon_base(Prefix).

%	compare_bags(+Members1, +Members2, +S0, -S)
%	
%	Order of _1, _2, etc. are not relevant in BadID reification. Are
%	they in general?  Anyway, we'll normalise the order of the bags

compare_bags([], [], S, S).
compare_bags([E1|T1], M, S0, S) :-
	select(E2, M, T2),
	compare_field(E1, E2, S0, S1),
	compare_bags(T1, T2, S1, S).

take_bag(Triples, Bag, Elems, RestTriples) :-
	select(rdf(Bag, Type, BagClass), Triples, T1),
	compare_field(rdf:type, Type, [], []),
	compare_field(rdf:'Bag', BagClass, [], []),
	bag_members(T1, Bag, Elems, RestTriples).
	
bag_members([], _, [], []).
bag_members([rdf(Bag, IsElm, E)|T], Bag, [E|ET], Rest) :-
	member_prop(IsElm), !,
	bag_members(T, Bag, ET, Rest).
bag_members([T0|T], Bag, Elems, [T0|R]) :-
	bag_members(T, Bag, Elems, R).
	
member_prop(rdf:Name) :-
	atom_codes(Name, [0'_|Codes]),
	number_codes(_N, Codes), !.
member_prop(Prop) :-
	atom(Prop),
	rdf_parser:rdf_name_space(NS),
	atom_concat(NS, Name, Prop),
	atom_codes(Name, [0'_|Codes]),
	number_codes(_N, Codes), !.


%	feedback(+Format, +Args)
%	
%	Print if verbose

feedback(Fmt, Args) :-
	verbose, !,
	format(user_error, Fmt, Args).
feedback(_, _).
