/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_prolog_mode, []).
:- use_module(library(pce)).
:- require([ make/0
	   , absolute_file_name/3
	   , auto_call/1
	   , chain_list/2
	   , concat/3
	   , concat_atom/2
	   , default/3
	   , forall/2
	   , ignore/1
	   , list_to_set/2
	   , member/2
	   , memberchk/2
	   , seek/4
	   , strip_module/3
	   ]).
pce_ifhostproperty(prolog(quintus),
		   (:- use_module(library(strings), [concat_chars/2]))).

:- emacs_begin_mode(prolog, language,
		    "Mode for editing XPCE/Prolog sources",
					% BINDINGS
	[ indent_line		       = key('TAB'),
	  indent_clause		       = key('\eq'),
	  insert_if_then_else	       = key('(') + key(';') + key('>'),

	  prolog_manual		       = button(prolog),
	  break_at		       = key('\C-cb') + button(prolog),
	  check_clause		       = key('\C-c\C-s') + button(prolog),
	  insert_full_stop	       = key(.),
	  find_definition	       = key('\e.') + button(prolog),
	  make			       = key('\C-cRET') + button(prolog),
	  compile_buffer	       = key('\C-c\C-b') + button(prolog),
	  consult_selection	       = button(compile) + button(prolog),
	  source_file		       = button(prolog,
						@prolog?source_file_chain),

	  forward_clause	       = key('\ee'),
	  backward_clause	       = key('\ea'),
	  backward_predicate	       = key('\e['),
	  forward_predicate	       = key('\e]'),

	  manpce		       = key('\C-c?') + button(pce),
	  editpce		       = key('\C-ce') + button(pce),
	  tracepce		       = key('\C-ct') + button(pce),
	  breakpce 		       = button(pce),
	  spypce		       = button(pce),
	  what_class		       = key('\C-cw') + button(pce),
	  pce_insert_require_directive = key('\C-c\C-r') + button(pce),
	  pce_check_require_directives = button(pce)
	],
					% SYNTAX TABLE
	[ '"'  = string_quote('"'),
	  '''' = string_quote(''''),
	  $    = symbol,
	  @    = symbol,
	  '%'  = comment_start,
	  '\n' + comment_end,
	  '/'  + comment_start('*'),
	  '*'  + comment_end('/')
	]).
		 
:- send(@class, attribute, outline_regex_list,
	chain(regex(string('\\(^\\w+.*:<?->?\\)\\([^.]+\\.\\(\\s *\n\\)*\\)\\s ')))).


source_file_chain(Ch) :-
	new(Ch, chain),
	forall(user_source_file(X), send(Ch, append, X)),
	send(Ch, sort).

user_source_file(F) :-
	source_file(F),
	\+ (lib_dir(D), concat(D, _, F)).

ignore_paths_from(library).
ignore_paths_from(pce_boot).

lib_dir(D) :-
	ignore_paths_from(Category),
	user:file_search_path(Category, X),
	expand_path(X, D0),
	absolute_file_name(D0, D).	% canonise

expand_path(X, X) :-
	atomic(X), !.
expand_path(Term, D) :-
	Term =.. [New, Sub],
	user:file_search_path(New, D0),
	expand_path(D0, D1),
	concat_atom([D1, /, Sub], D).


:- pce_global(@prolog_neck_regex,
	      new(regex(':-\|:->\|:<-\|-->'))).
:- pce_global(@prolog_full_stop,
	      new(regex('[^-#$&*+./:<=>?@\^`~]\.\($\|\s \)'))).

indent_line(E) :->
	"Indent current line (Prolog indentation)"::
	send(E, beginning_of_text_on_line),
	(   send(E, indent_if_then_else)
	;   send(E, indent_expression_line)
	;   send(E, indent_clause_line)
	;   send(E, align_line, 8)
	).
	

beginning_of_clause(E, Start:int, BOP:int) :<-
	"Find start of predicate"::
	new(Here, number(Start)),
	get(E, text_buffer, TB),
	repeat,
	    (	(   send(Here, less_equal, 0)
		;   \+ send(@prolog_full_stop, search, TB, Here, 0)
		)
	    ->	!,
		get(TB, skip_comment, 0, BOP)
	    ;   get(@prolog_full_stop, register_start, SReg),
		get(@prolog_full_stop, register_end, P0),
		send(Here, value, SReg),
		get(TB, skip_comment, P0, BOP),
		BOP =< Start,
		get(TB, scan_syntax, 0, BOP, code)
	    ).


beginning_of_if_then_else(E, Pos:int) :<-
	"Beginning of if-then-else construct"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	pce_catch_error(mismatched_bracket,
			get(TB, matching_bracket, Caret, ')', Pos)),
	get(TB, character, Pos-1, Before),
	\+ send(E?syntax, has_syntax, Before, word),
	Before \== 0'?,				% '?(' for xpce
	get(E, beginning_of_clause, Pos, BegOfPred),
	get(TB, scan_syntax, BegOfPred, Pos, code).


indent_if_then_else(E) :->
	"Indent subclause in an (if->then;else)"::
	get(E, beginning_of_if_then_else, OpenPos),
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(E, scan, Caret, term, -1, end, EndOfPreviousTerm),
	get(E, skip_comment, EndOfPreviousTerm, Glue),
	(   send(regex('\,'), match, TB, Glue)
	->  get(TB, scan, Caret, line, -1, start, StartOfPrevLine),
	    get(regex('\s *\(->\|;\)\s *'), match, TB, StartOfPrevLine, L),
	    get(E, column, L+StartOfPrevLine, PrevExprCol),
	    send(E, align_line, PrevExprCol)
	;   get(E, column, OpenPos, OpenCol),
	    send(E, align_line, OpenCol)
	).
	

indent_clause_line(E) :->
	"Indent current line according to clause"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(E, scan, Caret, term, -1, end, EndOfPreviousTerm),
	get(E, skip_comment, EndOfPreviousTerm, Glue),
	(   send(regex('\.'), match, TB, Glue)		% new clause
	->  send(E, align_line, 0)
	;   send(regex('\,'), match, TB, Glue)	  	% Next subclause
	->  send(E, align_with_previous_line)
	;   send(@prolog_neck_regex, match, TB, Glue) 	% First subclause
	->  send(E, align_line, 8)
	;   send(E, align_with_previous_line)
	).


insert_if_then_else(E, Times:[int], Char:char) :->
	"Indent after typing (, > or ;"::
	send(E, insert_self, Times, Char),
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	(   get(regex('\s *\((\|->\|;\)'), match, TB, SOL, L),
	    Caret =:= SOL + L,
	    get(E, beginning_of_if_then_else, OpenPos)
	->  get(E, text_buffer, TB),
	    get(TB, scan, Caret, line, 0, start, SOL),
	    (   (   send(regex('\s *\((\|->\|;\)$'), match, TB, SOL, Caret)
		;   Caret =:= 1 + OpenPos
		)
	    ->  get(E, column, OpenPos, Col),
		send(E, align, Col+4)
	    ;   true
	    )
	;   true
	).


indent_clause(E) :->
	"Indent current clause"::
	get(E, text_buffer, TB),
	get(E, beginning_of_clause, E?caret, Start),
	send(E, caret, Start),
	repeat,
	    send(E, next_line),
	    send(E, indent_line),
	    get(E, caret, Caret),
	    get(regex('.*\S.\.'), match, TB, Caret, Size),
	    End is Caret + Size,
	    get(TB, scan_syntax, Start, End, code), !,
 	send(E, forward_char, Size),
	send(E, electric_caret, Start).


		 /*******************************
		 *          COMPILATION		*
		 *******************************/

make(E) :->				% SWI-Prolog specific
	"Run `make/0' in the Prolog window"::
	send(@emacs, save_some_buffers),
	make,
	send(E, report, status, 'Make done').

compile_buffer(E) :->
	"Save current buffer and (re)consult its file"::
	get(E?text_buffer, file, File),
	(   send(File, instance_of, file)
	->  send(E, save_if_modified),
	    get(File, name, Path),
	    consult(user:Path),
	    send(E, report, status, '%s compiled', Path)
	;   send(E, report, error,
		 'Buffer is not connected to a file')
	).


		/********************************
		*       FINDING PREDICATES	*
		********************************/

find_definition(E) :->
	"Find definition of predicate"::
	get(E, caret, Caret),
	(   get(E, name_and_arity, Caret, tuple(Name, Arity))
	->  true
	;   Name = '',
	    Arity = @default
	),
	(   Arity == @default
	->  Def = Name
	;   Def = string('%s/%d', Name, Arity)
	),
	get(E?frame, prompt, 'Name/Arity', Def, NameAndArity),
	new(NameAndArityRegex, regex('\s *\(.+\)/\(\sd+\)\s *$')),
	(   send(NameAndArityRegex, match, NameAndArity)
	->  get(NameAndArityRegex, register_value, NameAndArity, 1, NameStr),
	    get(NameAndArityRegex, register_value, NameAndArity, 2, ArityStr),
	    get(type(name), check, NameStr, PredName),
	    get(type(int), check, ArityStr, PredArity)
	;   get(NameAndArity, value, PredName)
	),

	find_predicate(PredName, PredArity, Preds),
	(   Preds = []
	->  ignore(PredArity = '?'),
	    send(E, report, warning,
		 'Cannot find %s/%s', PredName, PredArity)
	;   Preds = [Pred]
	->  (	locate_predicate(Pred, Buffer, Index)
	    ->	send(Buffer, open),
		send(Buffer?editors?head, caret, Index)
	    ;	send(E, report, warning,
		     'Cannot locate %s/%d', PredName, PredArity)
	    )
	;   mark_predicates(Preds, NameAndArity)
	).

	
%	Finding predicates (SWI-Prolog specific)

find_predicate(Name, Arity, Preds) :-
	(   integer(Arity)
	->  functor(Head, Name, Arity)
	;   true
	),
	findall(H, find_predicate_(Name, Head, H), Hs),
	list_to_set(Hs, Preds).			% remove duplicates

find_predicate_(Name, Head, TheModule:Head) :-
	current_predicate(Name, Module:Head),
	(   predicate_property(Module:Head, imported_from(TheModule))
	->  true
	;   TheModule = Module
	).

%	Marking predicates (SWI-Prolog specific)

mark_predicates(List, Label) :-
	new(L, emacs_hit_list(string('Predicates for %s', Label))),
	forall(member(Pred, List),
	       (locate_predicate(Pred, Buffer, CharIndex),
		send(L, append_hit, Buffer, CharIndex))).


locate_predicate(Head, Buffer, CharIndex) :-
	source_file(Head, Path),
	new(Buffer, emacs_buffer(Path)),
	(   strip_module(Head, _Module, PlainHead),
	    functor(PlainHead, Name, Arity),
	    locate(Buffer, Name, Arity, CharIndex)
	->  true
	;   predicate_property(Head, line_count(Line)),
	    get(Buffer, scan, 0, line, Line, start, CharIndex)
	).


locate(Buffer, Name, Arity, CharIndex) :-
	new(Idx, number(0)),
	new(Regex, regex('')),
	get(Regex, quote, Name, QName),
	(   Arity == 0
	->  send(Regex, pattern, string('\n%s\\b', QName))
	;   send(Regex, pattern, string('\n%s(', QName))
	),
	repeat,
	    (	get(Regex, search, Buffer, Idx, Hit)
	    ->	Start is Hit + 1,
		send(Idx, value, Start),
		get(Buffer, name_and_arity, Start, tuple(_Name, Arity)),
		!,
		CharIndex = Start
	    ;	!,
	        functor(Head, Name, Arity),
		predicate_property(_:Head, line_count(Line)),
		get(Buffer, scan, 0, line, Line-1, start, CharIndex)
	    ).


locate(E, Name, Arity) :->
	"Locate definition of predicate"::
	get(E, text_buffer, TB),
	locate(TB, Name, Arity, Index),
	send(E, caret, Index).

what_class(E, ClassName:name) :<-
	"Find current XPCE class"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	new(BG, regex(':-\s *pce_begin_class(\(\w+\)')),
	get(BG, search, TB, Caret, 0, BeginClass),
	(   get(regex(':-\s *pce_end_class\s *.'), search,
		TB, Caret, 0, EndClass)
	->  EndClass < BeginClass
	;   true
	),
	get(BG, register_value, TB, 1, name, ClassName).

what_class(E) :->
	"Display current class"::
	(   get(E, what_class, ClassName)
	->  send(E, report, inform, 'Caret is in XPCE class "%s"', ClassName)
	;   send(E, report, inform,
		 'Not between :- pce_begin_class and :- pce_end_class')
	).


source_file(E, F:file) :->
	"Switch to named source_file"::
	send(E, find_file, F).



		 /*******************************
		 *	   COMPILATION		*
		 *******************************/

consult_region(M, From:[int], To:[int]) :->
	"Consult region between indices"::
	default(From, M?mark, F),
	default(To, M?caret, T),
	get(T-F, value, S),
	(   S >= 0
	->  Start = F, Size = S
	;   Start = T, Size is -S
	),
	new(File, file),		% temporary file
	send(File, open, write),
	send(File, append, ?(M, contents, Start, Size)),
	send(File, newline),		% make sure it ends with a newline
	send(File, close),
	get(File, name, TmpNam),
	consult(user:TmpNam),
	send(M, report, status, 'Region consulted'),
	send(File, remove).
	

consult_selection(M) :->
	"Consult selected text"::
	get(M, selection, point(From, To)),
	send(M, consult_region, From, To).


		 /*******************************
		 *	       PCE		*
		 *******************************/

pce_insert_require_directive(M) :->
	"Insert :-require/1 directive"::
	send(M, save_if_modified),
	get(M, file, File),
	get(File, name, Name),
	auto_call(pce_require(Name, Directive, Message)),
	send(M, insert, Directive),
	(   Message \== ''
	->  send(M, report, status, Message)
	;   true
	).


pce_check_require_directives(M, Dir:directory) :->
	"Mark :- require's that are out-of-date"::
	get(Dir, files, '.*\.pl$', PlFiles),
	send(PlFiles, for_some,
	     message(M, pce_check_require, ?(Dir, file, @arg1))),
	get(Dir, directories, SubDirs),
	send(SubDirs, for_some,
	     message(M, pce_check_require_directives,
		     ?(Dir, directory, @arg1))).


no_check(library(pce)).
no_check(library('xref/common')).
no_check(library('xref/mkcommon')).
no_check(library('xref/quintus')).
no_check(library('xref/sicstus')).

do_not_check(File) :-
	  no_check(Spec),
	  absolute_file_name(Spec, [access(read), extensions([pl])], Expanded),
	  send(File, same, Expanded).

pce_check_require(M, File:file) :->
	  "Open of there is no :- require"::
	  (   do_not_check(File)
	  ->  true
	  ;   get(File, name, Name),
	      send(M, report, status, 'Checking %s', Name),
	      send(M, synchronise),
	      auto_call(pce_require(Name, _Directive, Message)),
	      (   send(Message, sub, 'up-to-date')
	      ->  true
	      ;   new(B, emacs_buffer(File)),
		  (   get(regex('^:-\s *require('), search, B, Index)
		  ->  true
		  ;   Index = 0
		  ),
		  send(@emacs_mark_list, append_hit, B, Index)
	      ),
	      send(M, report, done)
	  ).

	    
		 /*******************************
		 *	       DROP		*
		 *******************************/
	
preview_drop(M, Obj:object*) :->
	"Preview the upcomming drop action"::
	(   Obj == @nil
	->  send(M, report, status, '')
	;   get(Obj, get_method, prolog_source, tuple(_, Method))
	->  (	get(Method, summary, Summary), Summary \== @nil
	    ->	send(M, report, status, 'Drop to include %s', Summary)
	    ;   send(M, report, status,
		     'Please drop to include source at caret')
	    )
	;   send(M, send_super, preview_drop, Obj)
	).

drop(M, Obj:object) :->
	"Import source-code from object"::
	(   send(Obj, has_get_method, prolog_source)
	->  send(M, insert, Obj?prolog_source),
	    send(M, mark_undo),
	    send(M, report, status, 'Source included')
	;   send(M, send_super, drop, Obj)
	).

		 /*******************************
		 *	  SYNTAX CHECKING	*
		 *******************************/

error_at_location(M, Caret:int) :->
	"Goto error at location"::
	send(M, caret, Caret),
	send(M, check_clause).


symbol_chars("-#$&*+./:<=>?@\^`~").


symbol_char(C) :-
	symbol_chars(Symbols),
	memberchk(C, Symbols).


insert_full_stop(M, Arg:[int]) :->
	"Check clause after typing '.'"::
	send(M, insert_self, Arg, 0'.),
	(   Arg == @default,
	    get(M, caret, Caret),
	    get(M, character, Caret-2, Prev),
	    \+ symbol_char(Prev),
	    get(M, scan_syntax, 0, Caret, code)
	->  send(M, check_clause)
	;   true
	).

alternate_syntax(prolog,    true,
			    true).
alternate_syntax(pce_class, pce_expansion:push_compile_operators,
			    pce_expansion:pop_compile_operators).

:- dynamic
	syntax_error/1.

check_clause(M, From:[int], End:int) :<-
	"Check clause, returning the end of it"::
	send(M, style, singleton, style(bold := @on)),
        (   From == @default
	->  get(M, caret, C),
	    get(M, beginning_of_clause, C, Start),
	    ignore(send(M, electric_caret, Start)),
	    Verbose = true
	;   Start = From,
	    Verbose = fail
	),
	get(M, text_buffer, TB),
	pce_open(TB, read, Fd),
	read_term_from_stream(Fd, Start, T, Error, S, P),
	close(Fd),
	(   Error == none
	->  unmark_singletons(M, P),
	    (   S == []
	    ->  (   Verbose
		->  send(M, report, status, 'Clause checked')
		;   true
		)
	    ;   mark_singletons(M, T, S, P),
		replace_singletons(M, P)
	    ),
	    arg(2, P, E0),
	    get(TB, find, E0, '.', 1, end, End)
	;   Error = EPos:Msg,
	    send(M, caret, EPos),
	    send(M, report, warning, 'Syntax error: %s', Msg),
	    fail
	).

read_term_from_stream(Fd, Start, T, Error, S, P) :-
	retractall(syntax_error(_)),
	alternate_syntax(_Name, Setup, Restore),
	Setup,
	seek(Fd, Start, bof, _),
	read_with_errors(Fd, Start, T, Error, S, P),
	Restore,
	(   Error == none
	->  true
	;   assert(syntax_error(Error)),
	    fail
	), !.
read_term_from_stream(_, _, _, Error, _, _) :-
	setof(E, retract(syntax_error(E)), Es),
	last(Error, Es).

pce_ifhostproperty(prolog(swi),
(read_with_errors(Fd, _Start, T, Error, Singletons, TermPos) :-
	read_term(Fd, T, [ syntax_errors(Error0),
			   singletons(Singletons),
			   subterm_positions(TermPos)
			 ]),
	pl_error_message(Error0, Error))).
pce_ifhostproperty(prolog(quintus),
(read_with_errors(Fd, Start, T, Error, Singletons, TermPos) :-
	on_exception(syntax_error(_G, _Pos,
				  Message,
				  Pre, Post, _),
		     read_term(Fd, [ syntax_errors(error),
				     singletons(Singletons),
				     subterm_positions(TermPos)
				   ], T),
		     qp_error_message(Message, Start, Pre, Post, Error)),
	(var(Error) -> Error = none ; true))).

pl_error_message(none, none) :- !.
pl_error_message('$stream_position'(EP, _, _):Msg, EP:Msg).

pce_ifhostproperty(prolog(quintus),
(qp_error_message(Msg, Start, Pre, Post, EP:TheMsg) :-
	length(Pre, EP0),
	(   EP0 > 10
	->  length(PreM, 10),
	    append(_, PreM, Pre)
	;   PreM = Pre
	),
	length(Post, PL),
	(   PL > 10
	->  length(PosM, 10),
	    append(PosM, _, Post)
	;   PosM = Post
	),
	(   Msg == ''
	->  MsgChars0 = ''
	;   atom_chars(Msg, MsgChars0)
	),
	concat_chars([ MsgChars0,
		       "between `..", PreM, "' and `", PosM, "..'"
		     ],
		     MsgChars),
	atom_chars(TheMsg, MsgChars),
	EP is EP0 + Start)).


last(X, [X]).
last(X, [_|T]) :-
	last(X, T).

check_clause(M, From:[int]) :->
	"Check syntax of clause"::
	get(M, check_clause, From, _).


unmark_singletons(M, P) :-
	arg(1, P, Start),
	arg(2, P, End),
	new(Pt, point(Start, End)),
	get(M, find_all_fragments, message(@arg1, overlap, Pt), Frags),
	send(Frags, for_all, message(@arg1, free)).

mark_singletons(M, T, S, A-Z) :-
	var(T),
	member_var(T, S), !,
	get(M, text_buffer, TB),
	new(_, fragment(TB, A, Z-A, singleton)).
mark_singletons(_, _, _, _-_) :- !.
mark_singletons(_, _, _, list_position(_, _, [], none)) :- !.
mark_singletons(M, T, S, list_position(_, _, [], Tail)) :- !,
	mark_singletons(M, T, S, Tail).
mark_singletons(M, [H|T], S, list_position(A, Z, [E|ET], Tail)) :- !,
	mark_singletons(M, H, S, E),
	mark_singletons(M, T, S, list_position(A, Z, ET, Tail)).
mark_singletons(_, _, _, string_position(_,_)) :- !.
mark_singletons(M, {T}, S, brace_term_position(_, _, P)) :- !,
	mark_singletons(M, T, S, P).
mark_singletons(M, T, S, term_position(_,_,_,_,Args)) :-
	mark_arg_singletons(M, T, S, 1, Args).

mark_arg_singletons(_, _, _, _, []) :- !.
mark_arg_singletons(M, T, S, N, [H|L]) :-
	arg(N, T, A),
	mark_singletons(M, A, S, H),
	NN is N + 1,
	mark_arg_singletons(M, T, S, NN, L).

member_var(V, [_=V2|_]) :-
	V == V2, !.
member_var(V, [_|T]) :-
	member_var(V, T).

replace_singletons(M, P) :-
	arg(1, P, Start),
	arg(2, P, End),
	new(Pt, point(Start, End)),
	get(M, find_all_fragments, message(@arg1, overlap, Pt), Frags),
	send(M, attribute, singletons, Frags),
	get(M, caret, C),
	send(M, mark, C),
	send(M, focus_function, '_replace_singletons'),
	prepare_replace_singletons(M).

'_replace_singletons'(M, Id:event_id) :->
	get(M, attribute, singletons, Frags),
	get(Frags, delete_head, Frag),
	(   Id == 0'y
	->  send(Frag, insert, 0, '_'),
	    send(Frag, free)
	;   Id == 0'_
	->  send(Frag, string, '_'),
	    send(Frag, free)
	;   Id == 0'n
	->  true
	),
	(   send(Frags, empty)
	->  send(M, focus_function, @nil),
	    send(M, selection, 0, 0),
	    send(M, caret, M?mark),
	    send(M, report, status, '')
	;   prepare_replace_singletons(M)
	).

prepare_replace_singletons(M) :-
	get(M, attribute, singletons, Frags),
	get(Frags, head, F0),
	get(F0, start, S),
	get(F0, end, E),
	send(M, selection, S, E),
	send(M, caret, E),
	send(M, report, status,
	     'Replace singleton? (''y'' --> _Name, ''_'' --> _, ''n'')').

pce_ifhostproperty(prolog(swi),		% should become built-in
(seek(Fd, Pos, bof, Old) :-
	stream_position(Fd,
			'$stream_position'(Old, _, _),
			'$stream_position'(Pos, 0, 0)))).

		 /*******************************
		 *       SOURCE DEBUGGER	*
		 *******************************/

break_at(M) :->
	"Set a Prolog break-point at this location"::
	send(M, save_buffer),
	get(M, text_buffer, TB),
	get(TB, file, File),
	(   source_file(Source),
	    send(File, same, Source)
	->  get(M, caret, Caret),
	    get(M, line_number, M?caret, Line),
	    user:break_at(Source, Line, Caret) % for now!
	;   send(M, report, error, 'Source file is not loaded')
	).


		 /*******************************
		 *	      HELP		*
		 *******************************/

pce_ifhostproperty(prolog(swi),
[ (make_prolog_help_topic :-
	object(@prolog_help_topic_type), !),
  (make_prolog_help_topic :-
	new(Topics, quote_function(@prolog?help_topics)),
	new(@prolog_help_topic_type,
	    type(prolog_help_topic, value_set, Topics))),

  (:- initialization make_prolog_help_topic),

  (help_topics(@prolog_help_topics) :-
	object(@prolog_help_topics), !),
  (help_topics(@prolog_help_topics) :-
	setof(Name, prolog_help_topic(Name), Names),
	chain_list(Chain, [''|Names]),
	send(Chain, name_reference, prolog_help_topics)),

  (do_help(M, What) :-
	(   What == ''
	->  help
	;   help(What)
	))
],
[ (do_help(M, What) :-
	send(M, report, warning, 'No manual interface for this Prolog'))
]).

prolog_manual(M, What:prolog_help_topic) :->
	"Display section from the Prolog manual"::
	do_help(M, What).


		 /*******************************
		 *	  CLAUSE FWD/BWD	*
		 *******************************/

forward_clause(M, Start:int, EOC:int) :<-
	"Find end of first clause after Start"::
	new(Here, number(Start)),
	repeat,
	(   send(@prolog_full_stop, search, M, Here)
	->  get(@prolog_full_stop, register_start, 1, Stop),
	    (   get(M, scan_syntax, 0, Stop, code)
	    ->	!,
	        EOC = Stop
	    ;	send(Here, value, Stop),
		fail
	    )
	;   !,
	    fail
	).

at_start_of_clause(M, Pos:[int]) :->
	"Succeeds if this is the start of a clause"::
	(   Pos == @default
	->  get(M, caret, C)
	;   C = Pos
	),
	get(M, scan, C, word, 0, start, SOW),
	SOW == C,
	(   send(M, looking_at, ':-', C)
	;   get(M, text_buffer, TB),
	    get(TB, scan, C, term, 1, end, TE),
	    get(TB, skip_comment, TE, Neck),
	    send(M, looking_at, ':-\|-->\|:<-\|\.', Neck)
	).

backward_clause(M, Start:int, BOC:int) :<-
	"Find start of clause or previous clause"::
	(   send(M, at_start_of_clause, Start)
	->  From is Start - 1
	;   From is Start
	),
	get(M, beginning_of_clause, From, BOC).

forward_clause(M, Arg:[int]) :->
	"Go forwards by <arg> clauses"::
	default(Arg, 1, Times),
	get(M, caret, Caret),
	(   Times > 0
	->  do_n_times(Times, M, forward_clause, Caret, Pos)
	;   NegTimes is -Times,
	    do_n_times(NegTimes, M, backward_clause, Caret, Pos)
	),
	send(M, caret, Pos).

backward_clause(M, Arg:[int]) :->
	"Go backwards by <arg> clauses"::
	default(Arg, 1, Times),
	Forward is -Times,
	send(M, forward_clause, Forward).

do_n_times(0, _, _, Pos, Pos) :- !.
do_n_times(N, M, Sel, Here, End) :-
	get(M, Sel, Here, Pos), !,
	NN is N - 1,
	do_n_times(NN, M, Sel, Pos, End).
do_n_times(_, _, _, Pos, Pos).


		 /*******************************
		 *	 PREDICATE FWD/BWD	*
		 *******************************/

at_start_of_predicate(M, Start:[int]) :->
	(   Start == @default
	->  get(M, caret, P0)
	;   P0 = Start
	),
	send(M, at_start_of_clause, P0),
	get(M, name_and_arity, P0, tuple(Name, Arity)),
	(   get(M, backward_clause, P0, P1)
	->  \+ get(M, name_and_arity, P1, tuple(Name, Arity))
	;   true
	).
	    

backward_predicate(M, P0:int, BPred:int) :<-
	"Find start of this/previous predicate"::
	(   send(M, at_start_of_predicate, P0)
	->  P1 is P0-1
	;   P1 is P0
	),
	get(M, beginning_of_clause, P1, BOC),
	(   get(M, name_and_arity, BOC, tuple(Name, Arity))
	->  new(BP, number(BOC)),	% clause
	    repeat,
		(   get(M, backward_clause, BP, BPC),
		    send(BP, larger, BPC)
		->  (   get(M, name_and_arity, BPC, tuple(Name, Arity))
		    ->  send(BP, value, BPC),
			fail
		    ;   !,
		        get(BP, value, BPred)
		    )
		;   !,
		    fail
		)
	;   get(M, backward_clause, P0-1, P2),
	    (	get(M, name_and_arity, P2, tuple(Name, Arity))
	    ->	get(M, backward_predicate, P2+1, BPred)
	    ;	BPred = P2
	    )
	).

forward_predicate(M, P0:int, End:int) :<-
	"Find end of predicate"::
	get(M, forward_clause, P0, EOC),
	get(M, backward_clause, EOC, BOC),
	(   get(M, name_and_arity, BOC, tuple(Name, Arity))
	->  new(Here, number(EOC)),
	    repeat,
		get(M, skip_comment, Here, BONC),
		(   get(M, name_and_arity, BONC, tuple(Name, Arity))
		->  get(M, forward_clause, BONC, EONC),
		    send(Here, value, EONC),
		    fail
		;   !,
		    get(Here, value, End)
		)
	;   End = EOC
	).


forward_predicate(M, Arg:[int]) :->
	"Move forwards by <arg> predicates"::
	default(Arg, 1, Times),
	get(M, caret, P0),
	do_n_times(Times, M, forward_predicate, P0, P),
	send(M, caret, P).

backward_predicate(M, Arg:[int]) :->
	"Move backwards by <arg> predicates"::
	default(Arg, 1, Times),
	get(M, caret, P0),
	do_n_times(Times, M, backward_predicate, P0, P),
	send(M, caret, P).

:- emacs_end_mode.

