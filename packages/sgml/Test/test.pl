/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(sgml_test,
	  [ test/1,			% +File
	    testdir/1,			% +Dir
	    pass/1,			% +File
	    show/1,			% +File
	    test/0
	  ]).

:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(foreign, '..')).
:- use_module(library(sgml)).


test :-
	testdir(.).

testdir(Dir) :-
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Files),
	checklist(dotest, Files).

dotest(File) :-
	file_name_extension(_, Ext, File),
	memberchk(Ext, [sgml, xml, html]), !,
	test(File).
dotest(_).

test(File) :-
	format('Test ~w ... ', [File]),
	flush_output,
	load_file(File, Term),
	ground(Term),			% make sure
	okfile(File, OkFile),
	(   exists_file(OkFile)
	->  load_prolog_file(OkFile, TermOk, ErrorsOk),
	    (	compare_dom(Term, TermOk)
	    ->	format('ok')
	    ;	format('WRONG'),
	        format('~NOK:~n'),
		pp(TermOk),
		format('~NANSWER:~n'),
		pp(Term)
	    ),
	    error_terms(Errors),
	    (	compare_errors(Errors, ErrorsOk)
	    ->	true
	    ;	format(' [Different errors]')
%		pp(ErrorsOk),
%		format('**************~n'),
%		pp(Errors)
	    ),
	    nl
	;   show_errors,
	    format('Loaded, no validating data~n'),
	    pp(Term)
	).

show(File) :-
	load_file(File, Term),
	pp(Term).

pass(File) :-
	load_file(File, Term),
	okfile(File, OkFile),
	open(OkFile, write, Fd),
	format(Fd, '~q.~n', [Term]),
	(   error_terms(Errors)
	->  format(Fd, '~q.~n', [Errors])
	;   true
	),
	close(Fd).

:- dynamic
	error/3.
:- multifile
	user:message_hook/3.

user:message_hook(Term, Kind, Lines) :-
	Term = sgml(_,_,_,_),
	assert(error(Term, Kind, Lines)).

show_errors :-
	(   error(_Term, Kind, Lines),
	    atom_concat(Kind, ': ', Prefix),
	    print_message_lines(user_error, Prefix, Lines),
	    fail
	;   true
	).

error_terms(Errors) :-
	findall(Term, error(Term, _, _), Errors).

compare_errors([], []).
compare_errors([sgml(_Parser1, _File1, Line, Msg)|T0],
	       [sgml(_Parser2, _File2, Line, Msg)|T]) :-
	compare_errors(T0, T).

load_file(File, Term) :-
	load_pred(Ext, Pred),
	file_name_extension(_, Ext, File), !,
	retractall(error(_,_,_)),
	call(Pred, File, Term).
load_file(Base, Term) :-
	load_pred(Ext, Pred),
	file_name_extension(Base, Ext, File),
	exists_file(File), !,
	retractall(error(_,_,_)),
	call(Pred, File, Term).


load_pred(sgml,	load_sgml_file).
load_pred(xml,	load_xml_file).
load_pred(html,	load_html_file).

okfile(File, OkFile) :-
	file_name_extension(Base, _, File),
	file_directory_name(Base, Dir),
	concat_atom([Dir, '/ok/', Base, '.ok'], OkFile).

load_prolog_file(File, Term, Errors) :-
	open(File, read, Fd),
	read(Fd, Term),
	(   read(Fd, Errors),
	    Errors \== end_of_file
	->  true
	;   Errors = []
	),
	close(Fd).

compare_dom([], []) :- !.
compare_dom([H1|T1], [H2|T2]) :- !,
	compare_dom(H1, H2),
	compare_dom(T1, T2).
compare_dom(X, X) :- !.
compare_dom(element(Name, A1, Content1),
	    element(Name, A2, Content2)) :-
	compare_attributes(A1, A2),
	compare_dom(Content1, Content2).

compare_attributes(A1, A2) :-
	sort(A1, L1),
	sort(A2, L2),
	L1 == L2.

	    
	
