/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/


:- module(isocomp,
	  [ iso_check_file/1,		% +File
	    iso_check_directory/1,	% +Directory
	    iso_check_application/1	% +LoadFile
	  ]).

		 /*******************************
		 *	     SETTINGS		*
		 *******************************/

:- dynamic
	setting/2,			% Name, Value
	file_queue/1,			% +Spec
	file_done/1,			% Path
	application_search_path/2.

set(Attribute, Value) :-
	retractall(setting(Attribute, _)),
	assert(setting(Attribute, Value)).

reset :-
	retractall(setting(_,_)),
	set(character_escapes, false).

		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

iso_check_directory(Dir) :-
	destroy_editors,
	atom_concat(Dir, '/*.pl', Pattern),
	expand_file_name(Pattern, Files),
	forall(member(File, Files),
	       iso_check_file(File)).

iso_check_file(Spec) :-
	retractall(file_done(_)),
	check_file(Spec).

check_file(Spec) :-
	reset,
	strip_module(Spec, _, File),
	absolute_file_name(File,
			   [ file_type(prolog),
			     access(read)
			   ], Path),
	(   file_done(Path)
	->  true
	;   format('Checking file ~w ... ', [Path]), flush,
	    clean_editor(Path),
	    set(file, Path),
	    open(Path, read, Fd),
	    check(Fd),
	    close(Fd),
	    format('ok~n', []),
	    assert(file_done(Path))
	).

iso_check_application(File) :-
	retractall(file_queue(_)),
	retractall(file_done(_)),
	retractall(application_search_path(_)),
	destroy_editors,
	check_file(File),
	check_file_queue.

check_file_queue :-
	retract(file_queue(File)), !,
	check_file(File),
	check_file_queue.
check_file_queue.

load_goal(Goal) :-
	load_goal_files(Goal, Spec),
	assert_queue(Spec).

load_goal_files(use_module(Spec),    Spec).
load_goal_files(use_module(Spec, _), Spec).
load_goal_files(ensure_loaded(Spec), Spec).
load_goal_files(consult(Spec),	     Spec).
load_goal_files(load_files(Spec, _), Spec).
load_goal_files([],		     []).
load_goal_files([H|T],		     [H|T]).

assert_queue([]) :- !.
assert_queue([H|T]) :- !,
	assert_queue(H),
	assert_queue(T).
assert_queue(H) :-
	atom(H),
	\+ is_absolute_file_name(H),
	setting(file, Path),
	file_directory_name(Path, Dir),
	concat_atom([Dir, H], /, Local),
	absolute_file_name(Local,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   AbsPath), !,
	assert(file_queue(AbsPath)).
assert_queue(H) :-
	assert(file_queue(H)).

user:file_search_path(Spec, Path) :-
	application_search_path(Spec, Path).


		 /*******************************
		 *	    CHECK LOOP		*
		 *******************************/

check(Fd) :-
	read_source_term(Fd, Term, Pos),
	check(Term, Pos, Fd).

check((:- module(Module, _Public)), _, Fd) :-
	set(module, Module),
	read_source_term(Fd, Term, Pos),
	check2(Term, Pos, Fd).
check(Term, Pos, Fd) :-
	gensym(check_module, ModuleId),
	set(module, ModuleId),
	check2(Term, Pos, Fd).

check2(end_of_file, _, _) :- !.
check2(Term, Pos, Fd) :-
	handle(Term, Pos) ->
	read_source_term(Fd, Term2, Pos2),
	check2(Term2, Pos2, Fd).

handle((:- Directive), Pos) :- !,	% directives
	directive(Directive, Pos).
handle((?- Directive), Pos) :- !,
	directive(Directive, Pos).
handle(file_search_path(A,B), _) :-	% maintain search-path
	assert(application_search_path(A,B)),
	fail.
handle(user:file_search_path(A,B), Pos) :-
	handle(file_search_path(A, B), Pos).
handle(file_search_path(A,B) :- Body, _) :-
	assert(application_search_path(A,B) :- Body),
	fail.
handle(user:file_search_path(A,B) :- Body, _) :-
	assert(application_search_path(A,B) :- Body),
	fail.
					% clauses
handle((_Head :- Body), term_position(_,_,_,_,[_,Pos])) :-
	body(Body, Pos).
handle((_Head --> Body), term_position(_,_,_,_,[_,Pos])) :-
	dcg_body(Body, Pos).
handle(:->(_Head, Body), term_position(_,_,_,_,[_,Pos])) :-
	body(Body, Pos).
handle(:<-(_Head, Body), term_position(_,_,_,_,[_,Pos])) :-
	body(Body, Pos).
handle(_Fact, _) :- !.

meta(','(:, :)).
meta(';'(:, :)).
meta('->'(:, :)).
meta('*->'(:, :)).
meta('\+'(:)).
meta(findall(+, :, -)).
meta(setof(+, :, -)).
meta(not(:)).
meta(::(+, :)).				% XPCE method comment

body(Var, _) :-
	var(Var), !.
body(Meta, term_position(_,_,_,_,ArgPos)) :-
	functor(Meta, Name, Arity),
	functor(Templ, Name, Arity),
	meta(Templ), !,
	meta_body(1, Meta, Templ, ArgPos).
body(get0(_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, get_byte, iso).
body(get0(_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, get_byte, iso).
body(put(_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, put_byte, iso).
body(put(_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, put_byte, iso).
body(atom_chars(_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, atom_codes, codes).
body(number_chars(_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, number_codes, codes).
body(feature(_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, current_prolog_flag, iso).
body(set_feature(_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, set_prolog_flag, iso).
body(concat(_,_,_), term_position(_,_,FF,FT,_)) :-
	change(FF-FT, atom_concat, iso).
body(dup_stream(_,_), term_position(_,_,FF,FT,_)) :-
	warn(FF-FT, dup_stream).
body(op(_,_,Scope), Pos) :-
	setting(module, Module),
	Module \== user,		% global anyway
	Scope \= _:_,			% already aware
	warn(Pos, op).
body('$argv'(_), term_position(_,_,FF,FT0,_)) :-
	FT is FT0+1,
	change(FF-FT, 'current_prolog_flag(argv, ', argv).
body(Goal, _) :-
	load_goal(Goal).
body(_, _).
	
meta_body(_, _, _, []) :-  !.
meta_body(N, Meta, Templ, [P|T]) :- 
	(   arg(N, Templ, :)
	->  arg(N, Meta, A),
	    body(A, P)
	;   true
	),
	NN is N + 1,
	meta_body(NN, Meta, Templ, T).


		 /*******************************
		 *	     DGC-RULES 		*
		 *******************************/

dcg_body(Var, _) :-
	var(Var), !.
dcg_body({}(Body), Pos) :- !,
	dcg_body(Body, Pos).
dcg_body(Meta, term_position(_,_,_,_,ArgPos)) :-
	functor(Meta, Name, Arity),
	functor(Templ, Name, Arity),
	dcg_meta(Templ), !,
	dcg_meta_body(1, Meta, Templ, ArgPos).
dcg_body(_, _).

dcg_meta_body(_, _, _, []) :-  !.
dcg_meta_body(N, Meta, Templ, [P|T]) :- 
	(   arg(N, Templ, :)
	->  arg(N, Meta, A),
	    dcg_body(A, P)
	;   true
	),
	NN is N + 1,
	dcg_meta_body(NN, Meta, Templ, T).

dcg_meta(','(:, :)).
dcg_meta(';'(:, :)).
dcg_meta('->'(:, :)).
dcg_meta('*->'(:, :)).
dcg_meta('\+'(:)).


		 /*******************************
		 *	     DIRECTIVES		*
		 *******************************/

directive(op(A,B,C), _) :-
	setting(module, Module),
	op(A, B, Module:C),
	fail.				% divert to body/2
directive(pce_begin_class(_,_), _Pos) :-
	setting(module, Module),
	push_operators(Module,
		       [ op(1200, xfx, :->)
		       , op(1200, xfx, :<-)
		       , op(1190, xfx, ::)
		       , op(100,  xf,  *)
		       , op(125,  xf,  ?)
		       , op(150,  xf,  ...)
		       , op(100,  xfx, ..)
		       ]).
directive(pce_begin_class(_,_,_), Pos) :- !,
	directive(pce_begin_class(_,_), Pos).
directive(pce_end_class, _Pos) :-
	setting(module, Module),
	pop_operators(Module).
directive(set_feature(character_escapes, Value), Pos) :- !,
	directive(set_prolog_flag(character_escapes, Value), Pos).
directive(set_prolog_flag(character_escapes, Value), Pos) :- !,
	change(Pos, '', character_escapes),
	set(character_escapes, Value).
directive(Code, Pos) :-
	body(Code, Pos).


		 /*******************************
		 *	      READING		*
		 *******************************/

read_source_term(Fd, Term, Pos) :-
	(   setting(module, Module)
	->  true
	;   Module = user
	),
	seek(Fd, 0, current, Here),
	catch(read_term(Fd, Term1,
			[ subterm_positions(Pos1),
			  module(Module),
			  character_escapes(false)
			]),
	      _E1, true),
	seek(Fd, Here, bof, _),
	catch(read_term(Fd, Term2,
			[ subterm_positions(Pos2),
			  module(Module),
			  character_escapes(true)
			]),
	      _E2, true),
	(   Term1 =@= Term2
	->  Term = Term1,
	    Pos = Pos1
	;   report_difference(Term1, Pos1, Term2, Pos2),
	    Term = Term1		% for further processing
	).

report_difference(T1, _P1, T2, _P2) :-
	T1 =@= T2, !.
report_difference({T1}, brace_term_position(_, _, P1),
		  {T2}, brace_term_position(_, _, P2)) :- !,
	report_difference(T1, P1, T2, P2).
report_difference(L1, list_position(_, _, E1, T1),
		  L2, list_position(_, _, E2, T2)) :- !,
	list_difference(L1, E1, T1, L2, E2, T2).
report_difference(T1, term_position(_, _, _, _, A1),
		  T2, term_position(_, _, _, _, A2)) :- !,
	arg_difference(1, T1, A1, T2, A2).
report_difference(T1, P1, T2, _P2) :-
	atom(T1),
	(   setting(character_escapes, true)
	->  T = T2			% this is what it should be
	;   T = T1
	),
	sformat(ISO, '~W', [T, [quoted(true), character_escapes(true)]]),
	change(P1, ISO, iso_atom).
report_difference(T1, P1, T2, _P2) :-
	arg(1, P1, S1),
	arg(2, P1, E1),
	format('Change ~p --> ~p at ~d-~d~n',
	       [ T1, T2, S1, E1 ]).

list_difference([], _, _, [], _, _) :- !.
list_difference([H1|L1], [P1|PT1], T1, [H2|L2], [P2|PT2], T2) :- !,
	report_difference(H1, P1, H2, P2),
	list_difference(L1, PT1, T1, L2, PT2, T2).
list_difference(L1, _, P1, L2, _, P2) :- !,
	report_difference(L1, P1, L2, P2).


arg_difference(_, _, [], _, []) :- !.
arg_difference(N, T1, [A1|R1], T2, [A2|R2]) :-
	arg(N, T1, AT1),
	arg(N, T2, AT2),
	report_difference(AT1, A1, AT2, A2),
	NN is N+1,
	arg_difference(NN, T1, R1, T2, R2).


		 /*******************************
		 *	   OPERATOR UTIL	*
		 *******************************/

:- dynamic
        operator_stack/2.

push_operators(Module, New) :-
        undo_operators(New, Module, Undo),
        set_ops(New),
        asserta(operator_stack(Undo)).

pop_operators(Module) :-
        retract(operator_stack(Module, Undo)), !,
        set_ops(Undo).

set_ops([]) :- !.
set_ops([op(A,B,C)|T]) :- !,
	op(A,B,C),
        set_ops(T).

undo_operators([], _, []).
undo_operators([O0|T0], M, [U0|T]) :-
        undo_operator(O0, M, U0),
        undo_operators(T0, M, T).

undo_operator(op(_P, T, N), M, op(OP, OT, N)) :-
        current_op(OP, OT, M:N),
        same_op_type(T, OT), !.
undo_operator(op(P, T, [H|R]), M, [OH|OT]) :- !,
        undo_operator(op(P, T, H), M, OH),
        undo_operator(op(P, T, R), M, OT).
undo_operator(op(_, _, []), _, []) :- !.
undo_operator(op(_P, T, N), _, op(0, T, N)).
        
same_op_type(T, OT) :-
        op_type(T, Type),
        op_type(OT, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).
op_type(xf,  postfix).
op_type(yf,  postfix).


		 /*******************************
		 *	     GUI STUFF		*
		 *******************************/

:- use_module(library(pce)).

:- dynamic
	editor/2.			% Path, Frame

message(iso_atom) -->
	"Quoted atom with ISO escape characters".
message(codes) -->
	"The *_chars functions translated to one-character atoms".
message(iso) -->
	"Replaced by ISO compliant predicate".
message(argv) -->
	"Replaced by documented equivalent".
message(character_escapes) -->
	"In ISO-Prolog, character_escapes are always on".
message(op) -->
	"From SWI-Prolog 3.3.0, operators are local to the module".
message(dup_stream) -->
	"dup_stream/2 is no longer supported.  Please check release-notes".

style(iso_atom,		 iso_atom,	    style(colour := red)).
style(iso,		 iso,		    style(colour := red)).
style(argv,		 argv,		    style(colour := red)).
style(codes,		 codes,		    style(colour := red)).
style(character_escapes, character_escapes, style(colour := red)).
style(op, 		 warn,		    style(colour := blue)).
style(dup_stream,	 error,		    style(colour := red)).
style(done,		 done,		    style(colour := dark_blue)).

change(Pos, To, Message) :-
	make_fragment(Pos, Fragment),
	send(Fragment, to, To),
	phrase(message(Message), Text),
	style(Message, Style, _),
	send(Fragment, comment, string(Text)),
	send(Fragment, style, Style).

warn(Pos, Message) :-
	make_fragment(Pos, Fragment),
	phrase(message(Message), Text),
	style(Message, Style, _),
	send(Fragment, comment, string(Text)),
	send(Fragment, style, Style).

make_fragment(Pos, Fragment) :-
	setting(file, Path),
	(   get(@editor_list, editor, Path, Frame)
	->  true
	;   new(Frame, change_frame(Path))
	),
	arg(1, Pos, From),
	arg(2, Pos, To),
	get(Frame, editor, View),
	new(Fragment, change_fragment(View, From, To-From)),
	send(Frame, fragment, Fragment).


		 /*******************************
		 *	     CLASSES		*
		 *******************************/

:- use_module(library(pce_report)).
:- use_module(library(hyper)).
:- use_module(library(toolbar)).

resource(save,	  image, image('16x16/save.xpm')).
resource(replace, image, image('16x16/redo.xpm')).

:- pce_begin_class(change_fragment, fragment,
		   "Indicate suggested change").

variable(to,	  string, both,	"Proposed new text").
variable(comment, string, both,	"Comment for change").
variable(undo,	  string, both, "Text for undo").

identify(F) :->
	get(F, comment, Comment),
	get(F, to, To),
	send(F, report, status,
	     'Change to \'%s\' (%s)', To, Comment).

replace(F) :->
	"Replace with <-to"::
	get(F, to, To),
	To \== @nil,
	get(F, string, Undo),
	send(F, undo, Undo),
	send(F, string, To),
	send(F, style, done).

:- pce_end_class.

:- pce_begin_class(change_editor, view,
		   "Show change-proposals").

initialise(V) :->
	send_super(V, initialise),
	forall(style(_, Id, Term),
	       send(V, style, Id, Term)).

:- pce_global(@change_editor_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, clicked, @arg1)))).

fragment(V, Ev:event, Fragment:change_fragment) :<-
	"Find fragment from event"::
	get(V?image, index, Ev, Index),
	    get(V?text_buffer, find_fragment,
		message(@arg1, overlap, Index),
		Fragment).

event(V, Ev:event) :->
	(   get(V, image, Image),
	    send(Ev, inside, Image)
	->  (   send(@change_editor_recogniser, event, Ev)
	    ->  true
	    ;   (   get(V, fragment, Ev, Fragment)
		->  send(Fragment, identify)
		;   true
		),
		send_super(V, event, Ev)
	    )
	;   send_super(V, event, Ev)
	).
		
clicked(V, Ev:event) :->
	"Replace text with proposed alternative"::
	send(V, selection, 0, 0),
	(   get(V, fragment, Ev, Fragment),
	    get(Fragment, to, To),
	    To \== @nil
	->  send(Fragment, replace),
	    (	get(Fragment, next, Next),
		Next \== @nil
	    ->	send(V?frame, select_fragment, Next)
	    ;	true
	    )
	;   get(V?image, index, Ev, Index),
	    send(V, caret, Index)
	).


:- pce_end_class.


:- pce_begin_class(change_frame, frame,
		   "Entire change-editor application").

initialise(F, File:file) :->
	get(File, absolute_path, Path),
	send_super(F, initialise, Path),
	send(F, done_message, message(F, quit)),
	send(F, append, new(B, browser)),
	send(B, select_message,
	     message(F, goto, @arg1)),
	forall(style(_, Id, Term),
	       send(B, style, Id, Term)),
	send(new(V, change_editor), right, B),
	send(V, load, File),
	send(new(D, dialog), above, B),
	send(D, pen, 0),
	send(D, gap, size(0, 5)),
	send(F, fill_dialog),
	send(new(report_dialog), below, B),
	send(@editor_list, append, F).

editor(F, Editor:view) :<-
	"The view of the editor"::
	get(F, member, change_editor, Editor).

browser(F, B:browser) :<-
	"The fragment browser"::
	get(F, member, browser, B).

fill_dialog(F) :->
	get(F, member, dialog, D),
	send(D, append, new(TB, tool_bar(F))),
	send_list(TB, append,
		  [ tool_button(save,
				resource(save),
				save),
		    gap,		% skip a little
		    tool_button(replace,
				resource(replace),
				replace)
		  ]).
			    
:- pce_group(fragment).

fragment(F, Fragment:fragment) :->
	get(F, browser, Browser),
	get(Fragment, string, String),
	send(Browser, append, new(DI, dict_item(String, @default, Fragment))),
	new(_, partof_hyper(Fragment, DI, dict_item, fragment)).

select_fragment(F, Fragment:fragment) :->
	"Select fragment in browser and view"::
	get(Fragment, hypered, dict_item, DI),
	send(F?browser, selection, DI),
	get(F, editor, View),
	send(View, normalise, Fragment?start, Fragment?end),
	send(View, selection, Fragment?start, Fragment?end),
	send(Fragment, identify).

select_first_fragment(F) :->
	get(F, editor, View),
	(   get(View?text_buffer, first_fragment, Fragment)
	->  send(F, select_fragment, Fragment)
	;   true
	).

goto(F, DI:dict_item) :->
	"Switch to the indicated item"::
	get(DI, hypered, fragment, Fragment),
	send(F, select_fragment, Fragment).

replace(F) :->
	"Replace fragment with selected object"::
	get(F, browser, Browser),
	get(Browser, selection, DI),
	get(DI, object, Fragment),
	send(Fragment, replace),
	(   get(Fragment, next, Next)
	->  send(F, select_fragment, Next)
	;   true
	).

:- pce_group(file).

save(F) :->
	get(F, editor, View),
	send(View, save_buffer),
	send(F, quit).

quit(F) :->
	(   get(F?editor, modified, @on)
	->  (   send(@display, confirm, 'Save changes?')
	    ->	send(F, save)
	    ;	send(@display, confirm, 'Quit, discarding changes?')
	    )
	;   true
	),
	send(F, destroy).

:- pce_end_class.

:- pce_begin_class(editor_list, browser,
		   "Show relevant editors").

:- pce_global(@editor_list, make_editor_list).

make_editor_list(EL) :-
	send(new(EL, editor_list), open).

initialise(EL) :->
	send_super(EL, initialise, 'Files with conflicts'),
	send(EL, width, 60),
	send(EL, select_message,
	     message(EL, open_editor, @arg1)).

append(EL, F:change_frame) :->
	get(F, label, Path),
	send_super(EL, append, new(DI, dict_item(Path, @default, F))),
	new(_, partof_hyper(F, DI, dict_item, editor)).

open_editor(_, DI:dict_item) :->
	get(DI, hypered, Editor),
	send(Editor, expose),
	send(Editor, select_first_fragment).

editor(EL, Path:name, Editor:change_frame) :<-
	get(EL, member, Path, DI),
	get(DI, object, Editor).

:- pce_end_class.



destroy_editors :-
	(   object(@editor_list)
	->  send(@editor_list?dict, for_all,
		 message(@arg1?object, quit))
	;   true
	).

clean_editor(Path) :-
	(   object(@editor_list),
	    get(@editor_list, editor, Path, Editor)
	->  get(Editor, editor, View),
	    send(View?text_buffer, for_all_fragments,
		 message(@arg1, free))
	;   true
	).
