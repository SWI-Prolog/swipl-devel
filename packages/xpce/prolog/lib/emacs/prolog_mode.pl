/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_prolog_mode, []).
:- use_module(library(pce)).
:- require([ make/0
	   , forall/2
	   , list_to_set/2
	   , member/2
	   , strip_module/3
	   ]).


:- initialization
	new(KB, key_binding(prolog, language)),
	send(KB, function, 'TAB',      indent_line),
	send(KB, function, '\eq',      indent_clause),
	send(KB, function, '\e.',      find_definition),
	send(KB, function, '(',        insert_if_then_else),
	send(KB, function, ';',        insert_if_then_else),
	send(KB, function, '>',        insert_if_then_else),
	send(KB, function, '\C-cRET',  make),
	send(KB, function, '\C-c\C-r', pce_insert_require_directive),
	send(KB, function, '\C-c\C-b', compile_buffer).

:- initialization
	new(X, syntax_table(prolog)),
	send(X, syntax, '"',  string_quote, '"'),
	send(X, syntax, '''', string_quote, ''''),
	send(X, syntax, $,    symbol),
	send(X, syntax, @,    symbol),

	send(X, syntax,     '%',  comment_start),
	send(X, add_syntax, '\n', comment_end),
	send(X, add_syntax, '/',  comment_start, '*'),
	send(X, add_syntax, '*',  comment_end, '/').

:- initialization
	new(MM, emacs_mode_menu(prolog, language)),

	send(MM, append, help, manpce),

	send(MM, append, browse, find_definition),

	send(MM, append, compile, make),
	send(MM, append, compile, compile_buffer),
	send(MM, append, compile, consult_selection),

	send(MM, append, prolog, make),
	send(MM, append, prolog, compile_buffer),
	send(MM, append, prolog, consult_selection),
	send(MM, append, prolog, find_definition),

	send(MM, append, pce, manpce),
	send(MM, append, pce, editpce),
	send(MM, append, pce, pce_insert_require_directive).

   
:- pce_begin_class(emacs_prolog_mode, emacs_language_mode).


:- pce_global(@prolog_neck_regex,
	      new(regex(':-\|-->\|:->\|:<-'))).
:- pce_global(@prolog_full_stop,
	      new(regex('\S.\.\($\|\s \)'))).

indent_line(E) :->
	"Indent current line (Prolog indentation)"::
	send(E, beginning_of_text_on_line),
	(   send(E, indent_if_then_else)
	;   send(E, indent_expression_line)
	;   send(E, indent_clause_line)
	;   send(E, align_line, 8)
	).
	

beginning_of_clause(E, Here:number, BOP:int) :<-
	"Find start of predicate"::
	get(E, text_buffer, TB),
	repeat,
	    (	(   send(Here, less_equal, 0)
		;   \+ send(@prolog_full_stop, search, TB, Here, 0)
		)
	    ->	!, fail
	    ;   send(Here, value, @prolog_full_stop?register_start),
		get(@prolog_full_stop, register_end, P0),
		get(TB, skip_comment, P0, BOP),
		get(TB, scan, BOP, term, 1, end, P1),
		get(TB, skip_comment, P1, P2),
		send(@prolog_neck_regex, match, TB, P2)
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
	\+ send(TB, in_string, Pos, BegOfPred),
	\+ send(TB, in_comment, Pos, BegOfPred).


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
	get(E, beginning_of_if_then_else, OpenPos),
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	(   (   send(regex('\s *\((\|->\|;\)$'), match, TB, SOL, Caret)
	    ;   Caret =:= 1 + OpenPos
	    )
	->  get(E, column, OpenPos, Col),
	    send(E, align, Col+4)
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
	    \+ send(TB, in_string, End, Start),
	    \+ send(TB, in_comment, End, Start), !,
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
	send(E, save_if_modified),
	get(E?text_buffer, file, File),
	get(File, name, Path),
	[user:Path],
	send(E, report, status, '%s compiled', Path).


		/********************************
		*       FINDING PREDICATES	*
		********************************/

find_definition(E) :->
	"Find definition of predicate"::
	get(E, caret, Caret),
	get(E, name_and_arity, Caret, tuple(Name, Arity)),
	get(E?frame, prompt,
	    'Name/Arity', string('%s/%d', Name, Arity), NameAndArity),
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
	->  send(E, report, warning,
		 'Cannot find %s/%d', PredName, PredArity)
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
	get(string('/tmp/xpce-consult-%d', @pce?pid), value, TmpNam),
	new(File, file(TmpNam)),
	send(File, open, write),
	send(File, append, ?(M, contents, Start, Size)),
	send(File, close),
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
	ensure_loaded(library(pce_require)),
	Goal = pce_require(Name, Directive, Message), % fool xref :-)
	Goal,
	send(M, insert, Directive),
	(   Message \== ''
	->  send(M, report, status, Message)
	;   true
	).


pce_require_dir(M) :->
	"Open buffers that need require"::
	get(M, directory, Dir),
	get(Dir, files, '.*\.pl$', PlFiles),
	get(PlFiles, map, ?(Dir?path, append, /, @arg1), FileNames),
	send(FileNames, for_all, message(M, pce_check_require, @arg1)).


pce_check_require(M, File:file) :->
	"Open of there is no :- require"::
	get(File, name, Name),
	send(M, report, status, 'Checking %s', Name),
	seeing(Old), see(Name),
	repeat,
	    read(Term),
	    (	Term == end_of_file
	    ->	!, Open = true
	    ;	Term = (:- require(_))
	    ->	!, Open = fail
	    ;	fail
	    ),
	seen, see(Old),
	(   Open
	->  new(B, emacs_buffer(File)),
	    send(B, open)
	;   true
	).
	    
		 /*******************************
		 *	       DROP		*
		 *******************************/
	
preview_drop(M, Obj:object*) :->
	"Preview the upcomming drop action"::
	(   Obj == @nil
	->  send(M, report, status, '')
	;   get(Obj, get_method, prolog_source, tuple(_, Method)),
	    (	get(Method, summary, Summary), Summary \== @nil
	    ->	send(M, report, status, 'Drop to include %s', Summary)
	    ;   send(M, report, status,
		     'Please drop to include source at caret')
	    )
	).

drop(M, Obj:object) :->
	"Import source-code from object"::
	send(Obj, has_get_method, prolog_source),
	send(M, insert, Obj?prolog_source),
	send(M, mark_undo),
	send(M, report, status, 'Source included').

:- pce_end_class.

