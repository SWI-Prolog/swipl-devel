/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(man_search, []).
:- use_module(library(pce)).
:- require([ absolute_file_name/3
	   , append/3
	   , default/3
	   , ignore/1
	   , concat_atom/2
	   ]).


		 /*******************************
		 *  PAUSE INDICATING TEXT-ITEM	*
		 *******************************/

:- pce_begin_class(man_pause_item, text_item,
		   "Item that sends message after pause").

variable(pause_message,	code*, both, "Message if pause has elapsed").
variable(timer,		timer, get, "Timer for pause").
variable(last_typed,	int,   get, "Time of last type action").

initialise(I, Name:[name], Def:[any|function], Msg:[code]*, PMsg:[code]*) :->
	default(PMsg, @nil, PauseMsg),
	send(I, send_super, initialise, Name, Def, Msg),
	send(I, slot, pause_message, PauseMsg),
	send(I, slot, last_typed, 0),
	send(I, slot, timer, timer(0.5, message(I, timeout))).


unlink(I) :->
	"Stop associated <-timer"::
	get(I, timer, Timer),
	send(Timer, stop).


event(I, Ev:event) :->
	(   send(Ev, is_a, focus)
	->  get(I, timer, Timer),
	    (	send(Ev, is_a, deactivate_keyboard_focus)
	    ->	send(Timer, stop)
	    ;	send(Ev, is_a, activate_keyboard_focus)
	    ->  send(Timer, start)
	    ;	true
	    )
	;   true
	),
	send(I, send_super, event, Ev).


timeout(I) :->
	get(I, pause_message, Msg),
	Msg \== @nil,
	get(@pce, mclock, Now),
	get(I, last_typed, Last),
	Now - Last > 1500,
	get(I, modified, @on),
	get(I, selection, Selection),
	\+ get(Selection, size, 0),
	send(I, modified, @off),
	send(I?display, busy_cursor),
	ignore(send(Msg, forward, Selection)),
	send(I?display, busy_cursor, @nil).
	
	
typed(I, Id:event_id) :->
	send(I, slot, last_typed, @pce?mclock),
	send(I, send_super, typed, Id),
	send(I?frame, typed_in_search_item, Id, I?value_text?string).

:- pce_end_class.

		 /*******************************
		 *	  INDEX DATABASE	*
		 *******************************/

make_index(_, Ref) :-
	object(Ref), !.
make_index(IV, @Ref) :-
	absolute_file_name(pce('/man/reference/index'),
			   [ extensions([obj]),
			     access(read),
			     file_errors(fail)
			   ],
			   IndexFile), !,
	send(IV, report, progress, 'Loading index ...'),
	get(file(IndexFile), object, Obj),
	send(IV, report, done),
	send(Obj, name_reference, Ref).
make_index(IV, @Ref) :-
	absolute_file_name(pce('/man/reference'),
			   [ file_type(directory),
			     access(write),
			     file_errors(fail)
			   ],
			   IndexDir), !,
	concat_atom([IndexDir, /, 'index.obj'], IndexFile),
	send(@display, confirm,
	     '%s\n%s %s',
	     'Cannot find PCE manual index file.',
	     'Create', IndexFile),
	send(IV, busy_cursor),
	get(new(man_index_manager), make_index, IndexFile, TmpTable),
	send(IV, busy_cursor, @nil),
	send(TmpTable, name_reference, Ref),
	send(@display, inform,
	     '%s\n%s\n%s\n%s',
	     'Creating the manual index has loaded the entire manual',
	     'and introduced considerable fragmentation in XPCE''s',
	     'memory management.  If you are tight on memory, please',
	     'quit and restart XPCE').
make_index(_, Ref) :-
	send(@display, inform,
	     'Sorry, no manual index and no permission to write one.'),
	new(Ref, chain_table).


		 /*******************************
		 *	      THE FRAME		*
		 *******************************/

:- pce_begin_class(man_search_tool, man_frame,
		   "Search tool for the manual index").

initialise(IV, Manual:[man_manual]) :->
	default(Manual, @manual, TheManual),
	send(IV, send_super, initialise, TheManual, 'PCE Manual --- Search'),
	send(IV, append, new(D, dialog)),
	send(D, append, button(quit, message(IV, quit))),
	send(D, append, button(help, message(IV, help))),
	send(D, append, new(B, button(search_for))),
	send(D, append, new(S, man_pause_item(search, '')), right),
	send(S, length, 40),
	send(S, show_label, @off),
	send(S, pause_message, message(IV, pause, @arg1)),
	send(D, append, label(reporter), right),
	send(B, message, message(IV, search, S?selection)),
	new(WL, browser),
	send(WL, name, wordlist),
	new(WLMsg, message(S, selection, create(string, '<%s>', @arg1?key))),
	send(WL, select_message,
	     and(WLMsg, message(IV, pause, S?selection))),
	send(WL, open_message,
	     and(WLMsg, message(IV, search, S?selection))),
	send(new(H, man_summary_browser(man_summary, size(90, 15))),
	     right, WL),
	send(WL, above, D),
	send(H, name, hitlist),
	send(D, default_button, B),
	send(IV, input_window, D),
	send(IV, open),
	send(IV, wait),
	make_index(IV, @man_index),
	send(IV, report, progress, 'Extracting word-list ...'),
	word_list(@man_index, List),
	send(WL, members, List),
	send(IV, report, progress, done).


unlink(IV) :->
	ignore(send(IV?manual, search_patterns, @nil)),
	send(IV, send_super, unlink).


typed_in_search_item(IV, Id:event_id, Current:char_array) :->
	"Handle typing in the search string"::
	get(IV, member, wordlist, WL),
	(   get(Current, size, 0)
	->  send(WL, cancel_search),
	    get(IV, member, hitlist, List),
	    send(List, clear),
	    get(IV, manual, Manual),
	    send(Manual, search_patterns, @nil)
	;   send(Current, suffix, ' ')
	->  send(WL, cancel_search)
	;   Id == 13			% return
	->  true
	;   get(WL, key_binding, KB),
	    get(KB, function, Id, F),
	    F \== alert
	->  send(WL, typed, Id)
	;   true
	).


search(IV, Spec:name) :->
	"Perform the actual search"::
	(   parse_search_spec(Spec, Term)
	->  execute_search(Term, @man_index, CardsIds),
	    search_patterns(Term, Patterns),
	    send(IV?manual, search_patterns, Patterns),
	    send(IV, show_cards, CardsIds)
	;   send(IV, report, warning, 'Bad search expression'),
	    fail
	).


pause(IV, Spec:char_array) :->
	"Timeout pause, show number of matches"::
	(   parse_search_spec(Spec, Term)
	->  execute_search(Term, @man_index, CardsIds),
	    send(IV, report, status, '%d matches', CardsIds?size)
	;   send(IV, report, status, 'Incomplete expression')
	).


show_cards(IV, Cards:chain) :->
	get(IV, member, hitlist, Browser),
	get(Cards, map, ?(@prolog, object_from_id, @arg1), Objects),
	send(Browser, members, Objects).
	    

object_from_id(Id, Obj) :-
	atom_codes(Id, Chars),
	phrase(id(Obj), Chars), !.
object_from_id(Id, _) :-
	format('Cannot parse card id "~w"~n', [Id]),
	fail.

id(Obj) -->				% methods
	"M.", string(C), ".", sendget(SG), ".", string(N), eos, !,
	{ atom_codes(ClassName, C),
	  atom_codes(MName, N),
	  get(@pce, convert, ClassName, class, Class),
	  get(Class, SG, MName, Obj)
	}.
id(Obj) -->				% variable
	"V.", string(C), ".", string(N), eos, !,
	{ atom_codes(ClassName, C),
	  atom_codes(VarName, N),
	  get(@pce, convert, ClassName, class, Class),
	  get(Class, instance_variable, VarName, Obj)
	}.
id(Obj) -->				% resource
	"R.", string(C), ".", string(N), eos, !,
	{ atom_codes(ClassName, C),
	  atom_codes(ResName, N),
	  get(@pce, convert, ClassName, class, Class),
	  get(Class, class_variable, ResName, Obj)
	}.
id(Obj) -->				% classes
	"C.", string(C), eos, !,
	{ atom_codes(ClassName, C),
	  get(@pce, convert, ClassName, class, Obj)
	}.
id(Obj) -->				% plain cards
	"$", string(MS), "$", string(I), eos, !,
	{ atom_codes(Module, MS),
	  name(CardId, I),
	  get(@manual, space, ManSpace),
	  get(ManSpace, module, Module, @on, M),
	  get(M, card, CardId, Obj)
	}.

sendget(send_method) --> "S".
sendget(get_method) --> "G".

string(S, In, Rest) :-
	append(S, Rest, In).

eos([], []).

		 /*******************************
		 *	    SEARCH STUFF	*
		 *******************************/

parse_search_spec(Spec, Term) :-
	atom_codes(Spec, Chars),
	phrase(wordlist(List), Chars), !,
	phrase(search_specification(Term), List), !.

search_patterns(Term, Patterns) :-
	new(Patterns, chain),
	extract_search_patterns(Term, Patterns).

extract_search_patterns(word(W), Ch) :-
	(   atom_codes(W, [0'@|_])
	->  new(Re, regex(string('%s\\\\b', W)))
	;   new(Re, regex(string('\\\\b%s\\\\b', W)))
	),
	send(Ch, append, Re),
	send(Re, ignore_case, @on).
extract_search_patterns(prefix(W), Ch) :-
	(   atom_codes(W, [0'@|_])
	->  new(Re, regex(string('%s', W)))
	;   new(Re, regex(string('\\\\b%s', W)))
	),
	send(Ch, append, Re),
	send(Re, ignore_case, @on).
extract_search_patterns(not(_), _) :- !.
extract_search_patterns(and(S1, S2), Ch) :-
	extract_search_patterns(S1, Ch),
	extract_search_patterns(S2, Ch).
extract_search_patterns(or(S1, S2), Ch) :-
	extract_search_patterns(S1, Ch),
	extract_search_patterns(S2, Ch).


		 /*******************************
		 *	  MAKE WORD-LIST	*
		 *******************************/

wordlist([H|T]) -->
	blanks,
	word(H), !,
	wordlist(T).
wordlist([]) -->
	blanks.

blanks -->
	blank, !,
	blanks.
blanks -->
	[].

blank -->
	[C], { C =< 32 }.

singleton(0'(, '(').
singleton(0'), ')').
singleton(0'<, '<').
singleton(0'>, '>').

word(W) -->
	[C],
	{ singleton(C, W) }, !.
word(W) -->
	wordchar(C0),
	wordchars(CT),
	{ atom_codes(W, [C0|CT]) }.

wordchars([H|T]) -->
	wordchar(H), !,
	wordchars(T).
wordchars([]) -->
	[].

wordchar(C) -->
	[C],
	{ C > 32,
	  \+ singleton(C, _)
	}.

		 /*******************************
		 *	      COMBINE		*
		 *******************************/

search_specification(Simple) -->
	simple_search(Simple).
search_specification(Compound) -->
	simple_search(Left),
	infix(Connective), !,
	search_specification(Right),
	{ Compound =.. [Connective, Left, Right] }.
search_specification(and(S1, S2)) -->
	simple_search(S1),
	search_specification(S2).

simple_search(not(S)) -->
	[ not ], !,
	simple_search(S), !.		% not takes shortest expression
simple_search(S) -->
	['('], !,
	search_specification(S),
	[')'].
simple_search(word(W)) -->
	['<',W,'>'], !.
simple_search(prefix(W)) -->
	[ W ],
	{ \+ singleton(_, W) }.
	
infix(and)  --> [and].
infix(or)   --> [or].


		 /*******************************
		 *	     AND DO IT		*
		 *******************************/
      
execute_search(word(W), DB, Result) :-
	get(DB, member, W, Result).
execute_search(prefix(W), DB, Result) :-
	word_list(DB, List),
	get(List, complete_name, W, Tuple),
	get(Tuple, first, Words),
	new(Result, chain),
	send(Words, for_all,
	     message(Result, merge, ?(DB, member, @arg1))),
	send(Result, unique).
execute_search(and(not(S1), S2), DB, Result) :-
	execute_search(S1, DB, R1),
	execute_search(S2, DB, R2),
	get(R2, copy, Result),
	send(Result, subtract, R1),
	send(R1, done),
	send(R2, done).
execute_search(and(S1, not(S2)), DB, Result) :-
	execute_search(and(not(S2), S1), DB, Result).
execute_search(and(S1, S2), DB, Result) :- !,
	execute_search(S1, DB, R1),
	execute_search(S2, DB, R2),
	get(R1, intersection, R2, Result),
	send(R1, done),
	send(R2, done).
execute_search(or(S1, S2), DB, Result) :-
	execute_search(S1, DB, R1),
	execute_search(S2, DB, R2),
	get(R1, union, R2, Result),
	send(R1, done),
	send(R2, done).

word_list(HT, Words) :-
	get(HT, attribute, word_list, Words), !.
word_list(HT, Words) :-
	send(HT, attribute, word_list, new(Words, chain)),
	send(HT, for_all, message(Words, append, @arg1)),
	send(Words, sort).

:- pce_end_class.
