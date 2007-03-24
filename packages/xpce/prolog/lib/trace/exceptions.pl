:- module(prolog_trace_exception,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(toolbar)).
:- use_module(library(tabular)).
:- use_module(library(pce_tick_box)).
:- use_module(library(pce_report)).
:- use_module(library(pce_util)).
:- use_module(library(help_message)).

/** <module> Program exception handling

@author	Jan Wielemaker
*/

:- multifile
	prolog:general_exception/2.


:- pce_global(@prolog_exception_window,
	      new(prolog_trace_exception)).

:- dynamic
	exception/4,			% Name, Term, NotCaught, Caught
	installed/1.			% ClauseRef

exception(ex1, error(_, _), true, false).

:- dynamic
	user:prolog_exception_hook/4.


exception_hook(Ex, Ex, _Frame, Catcher) :-
	register(Ex),
	exception(_, Ex, NotCaught, Caught),
	(   Caught == true
	->  true
	;   Catcher == none,
	    NotCaught == true
	),
	trace, fail.


%%	install_exception_hook
%
%	Make sure our handler is the first of the hook predicate.

install_exception_hook :-
	installed(Ref),
	(   nth_clause(_, I, Ref)
	->  I == 1, !			% Ok, we are the first
	;   retractall(installed(Ref)),
	    erase(Ref),			% Someone before us!
	    fail
	).
install_exception_hook :-
	asserta((user:prolog_exception_hook(Ex, Out, Frame, Catcher) :-
			exception_hook(Ex, Out, Frame, Catcher)), Ref),
	assert(installed(Ref)).
		 

%%	register(Ex) is det.
%
%	Register the new exception with the GUI. We execute this code is
%	the GUI thread as  it  modifyes   the  GUI.  Due to asynchronous
%	execution we need to check  the   an  already  defined exception
%	twice.

register(Ex) :-
	general_exception(Ex, Gen),
	(   defined_exception(Gen)
	->  true
	;   in_pce_thread(register_exception(Gen))
	).


register_exception(Ex) :-
	(   defined_exception(Ex)
	->  true
	;   new_exception(Ex, last)
	).


%%	defined_exception(@Ex) is semidet.
%
%	True if we already have a rule for this exception

defined_exception(Ex) :-
	(   exception(_, Old, _, _),
	    Old =@= Ex
	->  true
	).


%%	general_exception(+ExIn, -ExOut) is det.
%
%	Remove specific aspects of an exception, so   we do not get very
%	long lists of them. This can  be   extended  by  the user and/or
%	libraries  by  adding  rules   for    the   multifile  predicate
%	prolog:general_exception/2.

general_exception(E, E) :-
	var(E), !.
general_exception(E0, E) :-
	prolog:general_exception(E0, E), !.
general_exception(error(E0, _), error(E, _)) :- !,
	general_exception(E0, E).
general_exception(permission_error(Type, Action, _), permission_error(Type, Action, _)) :- !.
general_exception(existence_error(Type, Object, _), existence_error(Type, Object, _)) :- !.
general_exception('$stream'(_), '$stream'(_)) :- !.
general_exception('$socket'(_), '$socket'(_)) :- !.
general_exception(C0, C) :-
	compound(C0), !,
	C0 =.. [Name|Args0],
	maplist(general_exception, Args0, Args),
	C =.. [Name|Args].
general_exception(E, E).


%%	new_exception(+Term, +Where) is det.
%%	new_exception(+Term, +NotCaught, +Caught, +Where) is det.
%
%	Add a new exception

new_exception(Ex, Where) :-
	(   \+ Ex = error(_,_)
	->  new_exception(Ex, false, false, Where)
	;   new_exception(Ex, true,  false, Where)
	).

new_exception(Ex, NotCaught, Caught, Where) :-
	(   repeat, 
	    gensym(ex, Name),
	    \+ exception(Name, _, _, _)
	->  (   Where == first
	    ->	asserta(exception(Name, Ex, NotCaught, Caught))
	    ;	assertz(exception(Name, Ex, NotCaught, Caught))
	    )
	),
	send(@prolog_exception_window, refresh).


%%	update_exception(+Name, +Term, +NotCaught, +Caught)
%
%	Update  given  exception  in  exception/4.    As  the  order  is
%	maintained by the clauses  we   must  unfortunately  retract and
%	re-assert all clauses. Luckily there are few.

update_exception(Name, Ex, NotCaught, Caught) :-
	T = exception(_,_,_,_),
	findall(T, retract(T), List),
	replace(exception(Name, Ex, NotCaught, Caught), List, New),
	maplist(assert, New).

replace(E, [H0|T], [E|T]) :-
	arg(1, E, Name),
	arg(1, H0, Name), !.
replace(E, [H|T0], [H|T]) :- !,
	replace(E, T0, T).
replace(_, [], []).


		 /*******************************
		 *	       GUI		*
		 *******************************/

:- pce_begin_class(prolog_trace_exception, persistent_frame,
		   "Configure tracing exceptions").

initialise(F) :->
	send_super(F, initialise, 'Manage exception debugging'),
	send(F, append, new(D, tool_dialog(F))),
	send(F, create_menu),
	send(new(W, dialog), below, D),
	send(W, scrollbars, vertical),
	send(W, ver_stretch, 100),
	send(W, ver_shrink, 100),
	send(W, name, main),
	send(W, display, new(T, prolog_exception_table)),
	send(W, resize_message, message(T, table_width, @arg2?width - 2)),
	send(new(report_dialog), below, W),
	install_exception_hook.
	     
create_menu(F) :->
	get(F, member, tool_dialog, D),
	send(D, append, new(File, popup(file))),
	send(D, append, new(Exceptions, popup(exceptions))),
	send(D, append, new(Debug, popup(debug))),
	send_list(File, append,
		  [ menu_item(exit, message(F, destroy))
		  ]),
	send_list(Exceptions, append,
		  [ menu_item(clear_all,
			      message(F, clear_all),
			      end_group := @on),
		    menu_item('New (error, first)',
			      message(F, new, prolog(error(_,_)))),
		    menu_item('New (general, first)',
			      message(F, new)),
		    menu_item('New (error, last)',
			      message(F, new, prolog(error(_,_)), last)),
		    menu_item('New (general, last)',
			      message(F, new, @default, last))
		  ]),
	send_list(Debug, append,
		  [ menu_item(debug_mode,
			      message(F, debug_mode, @on)),
		    menu_item(nodebug_mode,
			      message(F, debug_mode, @off))
		  ]).


table(F, Table:tabular) :<-
	"Get the table"::
	get(F, member, main, W),
	get(W, member, prolog_exception_table, Table).

new(_F, Term:[prolog], Where0:[{first,last}]) :->
	"Define new exception"::
	default(Where0, first, Where),
	(   Term == @default
	->  new_exception(_, Where)
	;   new_exception(Term, Where)
	).

clear_all(F) :->
	"Remove all exceptions"::
	retractall(exception(_,_,_,_)),
	send(F, refresh).

refresh(F) :->
	get(F, table, T),
	send(T, refresh).

debug_mode(_F, OnOff:bool) :->
	"Switch debug mode on/off"::
	(   OnOff == @on
	->  in_pce_thread(tdebug)
	;   in_pce_thread(tnodebug)
	).

:- pce_end_class(prolog_trace_exception).


:- pce_begin_class(prolog_exception_table, tabular,
		   "Show Prolog exceptions").

initialise(T) :->
	send_super(T, initialise),
	send(T, rules, all),
	send(T, cell_spacing, -1),
	BG = (background := khaki1),
	send(T, append, 'Exception',        bold, left,   BG),
	send(T, append, 'Trace not caught', bold, center, BG),
	send(T, append, 'Trace always',     bold, center, BG),
	send(T, next_row),
	send(T, append_error, ex1, error(_,_), true, false), % one for proper layout
	send(T, compute),
	set_column_rubber(T, 1, 100, 100, @off),
	set_column_rubber(T, 2, 0, 0, @on),
	set_column_rubber(T, 3, 0, 0, @on),
	set_column_rubber(T, 4, 0, 0, @on),
	send(T, refresh).

set_column_rubber(T, N, Stretch, Shrink, Fixed) :-
	get(T, column, N, C),
	send(C, rubber, new(R, rubber(1, Stretch, Shrink))),
	(   Fixed == @on
	->  send(C, fixed, @on),
	    send(R, natural, C?width)
	;   true
	).

clear(T) :->
	send(T, delete_rows, 2, @default).

refresh(T) :->
	send(T, clear),
	forall(exception(Name, Error, NotCaught, Caught),
	       send(T, append_error, Name, Error, NotCaught, Caught)),
	install_exception_hook.		% make sure!
	

append_error(T, Name:name, Error:prolog, Trace:[bool], WhenCaught:[bool]) :->
	get(T, current, point(_, RowN)),
	get(T, row, RowN, @on, Row),
	send(Row, name, Name),
	send(T, append, new(TI, prolog_term_item(exception, Error))),
	send(T, append, new(Tr, tick_box(trace_not_caught, Trace)),
	     halign := center, valign := bottom),
	send(T, append, new(Ca, tick_box(trace_caught, WhenCaught)),
	     halign := center, valign := bottom),
	new(D, figure),
	send(D, format, new(Fmt, format(vertical, 1, @on))),
	send(Fmt, row_sep, 1),
	send_list(D, display,
		  [ pte_img_button(up),
		    pte_img_button(down),
		    pte_img_button(delete)
		  ]),
	send(T, append, D, valign := center),
	send_list([TI, Tr, Ca], show_label, @off),
	send_list([Tr, Ca], auto_align, @off),
	send(TI, message, message(T, modify, Tr, @arg1)),
	send(Tr, message, message(T, modify, Tr, @arg1)),
	send(Ca, message, message(T, modify, Ca, @arg1)),
	send(T, next_row),
	(   get(T, window, W)
	->  send(W, keyboard_focus, TI)
	;   true
	).

:- pce_group(actions).

modify(T, TB:tick_box, _Val:any) :->
	"One of the tick-boxes changed"::
	get(TB, layout_interface, Cell),
	get(Cell, row, RowI),
	get(T, row, RowI, Row),
	get(Cell, table, Table),
	get(Table, cell, 1, RowI, TermCell),
	get(Table, cell, 2, RowI, NotCaughtCell),
	get(Table, cell, 3, RowI, CaughtCell),
	get(TermCell?image, selection, Exception),
	get(NotCaughtCell?image, selection, PceNotCaught),
	get(CaughtCell?image, selection, PceCaught),
	prolog_pce_bool(NotCaught, PceNotCaught),
	prolog_pce_bool(Caught, PceCaught),
	get(Row, name, Name),
	update_exception(Name, Exception, NotCaught, Caught).

alter(T, RowI:int, Action:{up,down,delete}) :->
	get(T, row, RowI, Row),
	get(Row, name, Name),
	E = exception(_,_,_,_),
	findall(E, E, List),
	(   Action == delete
	->  select(exception(Name, _, _, _), List, New)
	;   Action == up
	->  up(Name, List, New)
	;   Action == down
	->  down(Name, List, New)
	),
	retractall(exception(_,_,_,_)),
	maplist(assert, New),
	send(T, refresh).

up(Name, [E0,E1|T], [E1, E0|T]) :-
	arg(1, E1, Name), !.
up(Name, [H|T0], [H|T]) :- !,
	up(Name, T0, T).
up(_, [], []).

down(Name, [E0,E1|T], [E1, E0|T]) :-
	arg(1, E0, Name), !.
down(Name, [H|T0], [H|T]) :- !,
	down(Name, T0, T).
down(_, [], []).


prolog_pce_bool(true, @on).
prolog_pce_bool(false, @off).

:- pce_end_class(prolog_exception_table).


resource(up,     image, image('16x16/up.xpm')).
resource(down,   image, image('16x16/down.xpm')).
resource(delete, image, image('16x16/delete.xpm')).

:- pce_begin_class(pte_img_button, button).

initialise(B, Name:name) :->
	send_super(B, initialise, Name,
		   message(B?device?device, alter, B?row, Name)),
	send(B, label, image(resource(Name))),
	send(B, help_message, tag, Name?capitalise).

row(B, Row:int) :<-
	"Row in table"::
	get(B?device?layout_interface, row, Row).

:- pce_end_class(pte_img_button).


		 /*******************************
		 *	    TERM ITEM		*
		 *******************************/

:- pce_begin_class(prolog_term_item, text_item,
		   "Item for entering a Prolog term").

initialise(TI, Name:name, Term:prolog, Message:[code]*) :->
	send_super(TI, initialise, Name, @default, Message),
	send(TI, selection, Term).

selection(TI, Term:prolog) :->
	send(TI, slot, selection, prolog(Term)),
	\+ \+ ( numbervars(Term, 0, _, [singletons(true)]),
		format(string(S), '~W', [Term, [ quoted(true),
						 numbervars(true)
					       ]]),
		send(TI, string, S)).

selection(TI, Term:prolog) :<-
	get(TI, slot, selection, Term).


typed(TI,  Ev:'event|event_id') :->
	"Update related term"::
	send_super(TI, typed, Ev),
	get(TI, string, S),
	object(S, string(Text)),
	(   catch(atom_to_term(Text, Term, _), _, fail)
	->  send(TI?value_text, colour, black),
	    (   get(TI, slot, selection, Old),
	        Old =@= Term
	    ->	true
	    ;	send(TI, slot, selection, prolog(Term)),
		(   get(TI, message, Msg),
		    send(Msg, instance_of, code)
		->  send(Msg, forward, prolog(Term))
		;   true
		)
	    )
	;   send(TI?value_text, colour, red)
	).

:- pce_end_class(prolog_term_item).
