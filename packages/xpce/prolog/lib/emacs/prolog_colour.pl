/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(emacs_prolog_colours, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- use_module(prolog_xref).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
User extension hooks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	term_colours/2,
	goal_classification/2.

:- emacs_extend_mode(prolog,
		     [ colourise_or_recenter = key('\\C-l')
		     ]).

class_variable(auto_colourise_size_limit, int, 50000,
	       "Auto-colourise if buffer is smaller then this").

% :- start_emacs, send(@emacs, kind, user).

colourise_clause(M) :->
	"Colourise the current clause"::
	send(M, setup_styles),
	get(M, caret, C),
	get(M, beginning_of_clause, C, Start),
	get(M, text_buffer, TB),
	pce_open(TB, read, Fd),
	seek(Fd, Start, bof, _),
	colourise(TB, Fd),
	close(Fd).

setup_styles(M) :->
	"Associate defined syntax-styles"::
	(   get(M, attribute, styles_initialised, @on)
	->  true
	;   send(M, reload_styles),
	    send(M, attribute, styles_initialised, @on)
	).

reload_styles(M) :->
	"Force reloading the styles"::
	retractall(style_name(_,_)),
	(   style(Class, Name, Style),
	    send(M, style, Name, Style),
	    assert(style_name(Class, Name)),
	    fail
	;   true
	).

colourise_term(M, Term:prolog, TermPos:prolog) :->
	"Colourise the given term"::
	send(M, setup_styles),
	get(M, text_buffer, TB),
	arg(1, TermPos, From),
	arg(2, TermPos, To),
	send(M, remove_syntax_fragments, From, To),
	send(M, colourise_comments, From, To),
	colourise_term(Term, TB, TermPos).

colourise_buffer(M) :->
	"Do cross-referencing and colourising of the whole buffer"::
	OldTime is cputime,
	new(Class, class(emacs_colour_fragment)),
	get(Class, no_created, OldCreated),

	send_super(M, colourise_buffer),
	send(M, setup_styles),

	get(M, text_buffer, TB),
	send(M, report, progress, 'Cross-referencing buffer ...'),
	xref_source(TB),
	send(M, report, progress, 'Colourising buffer ...'),
	colourise_text_buffer(TB),
	send(M, colourise_comments),
	Used is cputime - OldTime,
	get(Class, no_created, NewCreated),
	Created is NewCreated - OldCreated,
	send(M, report, done,
	     'done, %.2f seconds, %d fragments', Used, Created).

colourise_comments(M, From:[int], To:[int]) :->
	get(M, text_buffer, TB),
	send(TB, for_all_comments,
	     message(@prolog, colour_item, comment, TB, @arg1, @arg2),
	     From, To).

colourise_or_recenter(M) :->
	"Colour according to syntax and recenter"::
	(   send(M, colourisation_up_to_date)
	->  send(M, recenter)
	;   send(M, colourise_buffer)
	).

		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

colourise_text_buffer(TB) :-
	pce_open(TB, read, Fd),
	(   peek_char(Fd, #)		% skip #! script line
	->  skip(Fd, 10)
	;   true
	),
	'$style_check'(Old, Old),
	repeat,
	    catch(read_term(Fd, Term,
			    [ subterm_positions(TermPos),
			      character_escapes(true)
			    ]),
		  _E,
		  (   %print_message(error, _E),
		      fail
		  )),
	    once(colourise_term(Term, TB, TermPos)),
	    fix_operators(Term),
	    Term == end_of_file, !,
	'$style_check'(_, Old),
	close(Fd).

fix_operators((:-style_check(X))) :- !,
	style_check(X).
fix_operators((:-Directive)) :- !,
	asserta(user:message_hook(_,_,_), Ref),
	ignore(xref_expand((:-Directive), _)),
	erase(Ref).
fix_operators(_).


%	colourise(+TB, +Stream)
%
%	Read next term from the text_buffer and  colourise the syntax

colourise(TB, Fd) :-
	character_count(Fd, Pos),
	emacs_prolog_mode:read_term_from_stream(Fd, Pos,
						Term,
						Error,
						_Singletons,
						TermPos),
	(   Error == none
	->  colourise_term(Term, TB, TermPos)
	;   show_syntax_error(TB, Error)
	).
	
show_syntax_error(TB, Pos:_Message) :-
	get(TB, scan, Pos, line, 0, start, BOL),
	get(TB, scan, Pos, line, 0, end, EOL),
	colour_item(syntax_error, TB, BOL-EOL).

colourise_term(Term, TB, Pos) :-
	term_colours(Term, FuncSpec-ArgSpecs), !,
	Pos = term_position(_,_,FF,FT,ArgPos),
	specified_item(FuncSpec, Term, TB, FF-FT),
	specified_items(ArgSpecs, Term, TB, ArgPos).
colourise_term((Head :- Body), TB,
	       term_position(F,T,FF,FT,[HP,BP])) :- !,
	colour_item(clause,	    TB,	F-T),
	colour_item(neck(clause),   TB,	FF-FT),
	colourise_clause_head(Head, TB,	HP),
	colourise_body(Body,	    TB,	BP).
colourise_term((Head --> Body), TB,			% TBD: expansion!
	       term_position(F,T,FF,FT,[HP,BP])) :- !,
	colour_item(grammar_rule,	TB, F-T),
	colour_item(neck(grammar_rule),	TB, FF-FT),
	colourise_dcg_head(Head,	TB, HP),
	colourise_dcg(Body,		TB, BP).
colourise_term(:->(Head, Body), TB,
	       term_position(F,T,FF,FT,[HP,BP])) :- !,
	colour_item(method,		TB, F-T),
	colour_item(neck(method(send)),	TB, FF-FT),
	colour_method_head(send(Head),	TB, HP),
	colourise_body(Body,		TB, BP).
colourise_term(:<-(Head, Body), TB,
	       term_position(F,T,FF,FT,[HP,BP])) :- !,
	colour_item(method,	       TB, F-T),
	colour_item(neck(method(get)), TB, FF-FT),
	colour_method_head(get(Head),  TB, HP),
	colourise_body(Body,	       TB, BP).
colourise_term((:- Directive), TB, Pos) :- !,
	arg(1, Pos, F),
	arg(2, Pos, T),
	get(TB, scan, T, line, 0, end, EOL),
	To is EOL+1,
	colour_item(directive, TB, F-To),
	arg(5, Pos, [ArgPos]),
	colourise_body(Directive, TB, ArgPos).
colourise_term(end_of_file, _, _) :- !.
colourise_term(Fact, TB, Pos) :- !,
	colour_item(clause, TB,	Pos),
	colourise_clause_head(Fact, TB, Pos).

colourise_dcg_head(Head, TB, Pos) :-
	functor_position(Pos, FPos, _),
	Head =.. List,
	append(List, [_,_], List1),
	TheHead =.. List1,
	classify_head(TB, TheHead, Class),
	colour_item(head(Class), TB, FPos),
	colourise_term_args(Head, TB, Pos).

colourise_clause_head(Head, TB, Pos) :-
	nonvar(Head),
	head_colours(Head, ClassSpec-ArgSpecs), !,
	functor_position(Pos, FPos, ArgPos),
	(   ClassSpec == classify
	->  classify_head(TB, Head, Class)
	;   Class = ClassSpec
	),
	colour_item(head(Class), TB, FPos),
	specified_items(ArgSpecs, Head, TB, ArgPos).
colourise_clause_head(Head, TB, Pos) :-
	functor_position(Pos, FPos, _),
	classify_head(TB, Head, Class),
	colour_item(head(Class), TB, FPos),
	colourise_term_args(Head, TB, Pos).

colour_method_head(SGHead, TB, Pos) :-
	arg(1, SGHead, Head),
	functor(SGHead, SG, _),
	functor_position(Pos, FPos, _),
	colour_item(method(SG), TB, FPos),
	colourise_term_args(Head, TB, Pos).

functor_position(term_position(_,_,FF,FT,ArgPos), FF-FT, ArgPos) :- !.
functor_position(Pos, Pos, []).

%	colourise_body(+Body, +TB, +Pos)
%	
%	Breaks down to colourise_goal/3.

colourise_body(Body, TB, Pos) :-
	colour_item(body, TB, Pos),
	colourise_goals(Body, TB, Pos).

colourise_goals(Body, TB, term_position(_,_,_,_,ArgPos)) :-
	body_compiled(Body), !,
	colourise_subgoals(ArgPos, 1, Body, TB).
colourise_goals(::(_Comment, Body), TB,	% XPCE <Comment>::Body construct
	       term_position(_,_,_,_,[CommentPos, BodyPos])) :- !,
	colour_item(pce(comment), TB, CommentPos),
	colourise_goals(Body, TB, BodyPos).
colourise_goals(Goal, TB, Pos) :-
	colourise_goal(Goal, TB, Pos).

colourise_subgoals([], _, _, _).
colourise_subgoals([Pos|T], N, Body, TB) :-
	arg(N, Body, Arg),
	colourise_goals(Arg, TB, Pos),
	NN is N + 1,
	colourise_subgoals(T, NN, Body, TB).

%	colourise_dcg(+Body, +TB, +Pos)
%	
%	Breaks down to colourise_dcg_goal/3.

colourise_dcg(Body, TB, Pos) :-
	colour_item(dcg, TB, Pos),
	colourise_dcg_goals(Body, TB, Pos).

colourise_dcg_goals({Body}, TB,	brace_term_position(F,T,Arg)) :- !,
	colour_item(dcg(plain), TB, F-T),
	colourise_goals(Body, TB, Arg).
colourise_dcg_goals(List, TB, Pos) :-
	is_list(List), !,
	colour_item(dcg(list), TB, Pos),
	colourise_term_args(List, TB, Pos).
colourise_dcg_goals(Body, TB, term_position(_,_,_,_,ArgPos)) :-
	body_compiled(Body), !,
	colourise_dcg_subgoals(ArgPos, 1, Body, TB).
colourise_dcg_goals(Goal, TB, Pos) :-
	colourise_dcg_goal(Goal, TB, Pos),
	colourise_term_args(Goal, TB, Pos).

colourise_dcg_subgoals([], _, _, _).
colourise_dcg_subgoals([Pos|T], N, Body, TB) :-
	arg(N, Body, Arg),
	colourise_dcg_goals(Arg, TB, Pos),
	NN is N + 1,
	colourise_dcg_subgoals(T, NN, Body, TB).

%	colourise_dcg_goal(+Goal, +TB, +Pos).

colourise_dcg_goal(Goal, TB, TermPos) :-
	compound(Goal), !,
	Goal =.. List,
	append(List, [_,_], List2),
	TheGoal =.. List2,
	colourise_goal(TheGoal, TB, TermPos).
colourise_dcg_goal(Goal, TB, Pos) :-
	colourise_term_args(Goal, TB, Pos).


%	colourise_goal(+Goal, +TB, +Pos).

colourise_goal(Goal, TB, Pos) :-
	nonvar(Goal),
	goal_colours(Goal, ClassSpec-ArgSpecs), !,
	functor_position(Pos, FPos, ArgPos),
	(   ClassSpec == classify
	->  goal_classification(TB, Goal, Class)
	;   Class = ClassSpec
	),
	colour_item(goal(Class), TB, FPos),
	specified_items(ArgSpecs, Goal, TB, ArgPos).
colourise_goal(Goal, TB, Pos) :-
	goal_classification(TB, Goal, Class),
	(   Pos = term_position(_,_,FF,FT,_ArgPos)
	->  FPos = FF-FT
	;   FPos = Pos
	),
	colour_item(goal(Class), TB, FPos),
	colourise_term_args(Goal, TB, Pos).

%	colourise_files(+Arg, +TB, +Pos)
%
%	Colourise the argument list of one of the file-loading predicates.

colourise_files(List, TB, list_position(_,_,Elms,_)) :- !,
	colourise_file_list(List, TB, Elms).
colourise_files(Spec0, TB, Pos) :-
	strip_module(Spec0, _, Spec),
	(   catch(xref_source_file(Spec, Path, TB), _, fail)
	->  colour_item(file(Path), TB, Pos)
	;   colour_item(nofile, TB, Pos)
	).

colourise_file_list([], _, _).
colourise_file_list([H|T], TB, [PH|PT]) :-
	colourise_files(H, TB, PH),
	colourise_file_list(T, TB, PT).

%	colourise_class(ClassName, TB, Pos)
%
%	Colourise an XPCE class.  Dealing with not-loaded classes is not
%	very easy as classes cannot easily be resolved using local
%	cross-referencing.

colourise_class(ClassName, TB, Pos) :-
	get(@classes, member, ClassName, Class), !,
	(   get(Class, creator, built_in)
	->  colour_item(class(built_in, ClassName), TB, Pos)
	;   colour_item(class(user, ClassName), TB, Pos)
	).
colourise_class(ClassName, TB, Pos) :-
	pce_prolog_class(ClassName), !,
	colour_item(class(user, ClassName), TB, Pos).
colourise_class(ClassName, TB, Pos) :-
	colour_item(class(unknown, ClassName), TB, Pos).

%	colourise_term_args(+Term, +TB, +Pos)
%
%	colourise head/body principal terms.

colourise_term_args(Term, TB,
		    term_position(_,_,_,_,ArgPos)) :- !,
	colourise_term_args(ArgPos, 1, Term, TB).
colourise_term_args(_, _, _).

colourise_term_args([], _, _, _).
colourise_term_args([Pos|T], N, Term, TB) :-
	arg(N, Term, Arg),
	colourise_term_arg(Arg, TB, Pos),
	NN is N + 1,
	colourise_term_args(T, NN, Term, TB).

colourise_term_arg(Var, TB, Pos) :-			% variable
	var(Var), !,
	colour_item(var, TB, Pos).
colourise_term_arg(Atom, TB, Pos) :-			% single quoted atom
	atom(Atom),
	arg(1, Pos, From),
	get(TB, character, From, 39), !, 	
	colour_item(quoted_atom, TB, Pos).
colourise_term_arg(List, TB, list_position(_, _, Elms, Tail)) :- !,
	colourise_list_args(Elms, Tail, List, TB, classify).	% list
colourise_term_arg(Compound, TB, Pos) :- 		% compound
	compound(Compound), !,
	colourise_term_args(Compound, TB, Pos).
colourise_term_arg(_, TB, string_position(F, T)) :- !,	% string
	colour_item(string, TB, F-T).
colourise_term_arg(_Arg, _TB, _Pos) :-
	true.
	
colourise_list_args([HP|TP], Tail, [H|T], TB, How) :-
	specified_item(How, H, TB, HP),
	colourise_list_args(TP, Tail, T, TB, How).
colourise_list_args([], none, _, _, _) :- !.
colourise_list_args([], TP, T, TB, How) :-
	specified_item(How, T, TB, TP).


%	colour_item(+Class, +TB, +Pos)
%
%	colourise region if a style is defined for this class.

colour_item(Class, TB, Pos) :-
	style_name(Class, Name), !,
	arg(1, Pos, F),
	arg(2, Pos, T),
	L is T - F,
	new(Fragment, emacs_colour_fragment(TB, F, L, Name)),
	message(Class, Fragment).
colour_item(_, _, _).
	
colour_item(Class, TB, F, T) :-
	colour_item(Class, TB, F-T).

:- pce_global(@prolog_mode_file_popup,
	      make_prolog_mode_file_popup).
:- pce_global(@prolog_mode_class_popup,
	      make_prolog_mode_class_popup).

make_prolog_mode_file_popup(G) :-
	new(G, popup(file_options)),
	send_list(G, append,
		  [ menu_item(open,
			      message(@emacs, open_file, @arg1?message)),
		    menu_item(open_other_window,
			      message(@emacs, open_file, @arg1?message, @on))
		  ]).

make_prolog_mode_class_popup(G) :-
	new(G, popup(file_options)),
	new(Class, @arg1?context),
	M = @emacs_mode,
	send_list(G, append,
		  [ menu_item(edit,
			      message(M, edit_class, Class),
			      condition := message(M, is_user_class, Class)),
		    menu_item(edit_other_window,
			      message(M, edit_class, Class, @on),
			      condition := message(M, is_user_class, Class)),
		    gap,
		    menu_item(manual,
			      message(M, manpce, Class))
		  ]).

is_user_class(_, ClassName:name) :->
	get(@pce, convert, ClassName, class, Class),
	\+ get(Class, creator, built_in).

edit_class(Mode, ClassName:name, NewWindow:[bool]) :->
	"Open XPCE class [in new window]"::
	(   get(@pce, convert, ClassName, class, Class)
	->  (   get(Class, creator, built_in)
	    ->	manpce(ClassName)
	    ;	ensure_loaded(library(edit)),
	        prolog_edit:locate(class(ClassName), Location),
		memberchk(file(File), Location),
		memberchk(line(Line), Location),
		send(@emacs, goto_source_location,
		     source_location(File, Line), NewWindow)
	    )
	;   send(Mode, report, error, 'Class %s doesn''t exist', ClassName)
	).

message(file(Path), F) :- !,
	send(F, message, Path),
	send(F, attribute, popup, @prolog_mode_file_popup).
message(class(_, Class), F) :- !,
	send(F, context, Class),
	send(F, attribute, popup, @prolog_mode_class_popup).
message(_, _).


		 /*******************************
		 *	  CONFIGURATION		*
		 *******************************/

%	body_compiled(+Term)
%
%	Succeeds if term is a construct handled by the compiler.

body_compiled((_,_)).
body_compiled((_->_)).
body_compiled((_*->_)).
body_compiled((_;_)).
body_compiled(\+_).

goal_classification(TB, Goal, How) :-
	xref_defined(TB, Goal, How), !.
goal_classification(_TB, Goal, Class) :-
	goal_classification(Goal, Class), !.
goal_classification(_TB, _Goal, undefined).

goal_classification(Goal, built_in) :-
	built_in_predicate(Goal), !.
goal_classification(Goal, autoload) :-			% SWI-Prolog
	functor(Goal, Name, Arity),
	'$in_library'(Name, Arity), !.
goal_classification(Goal, global) :-	% SWI-Prolog
	current_predicate(_, user:Goal), !.
goal_classification(SS, expanded) :-	% XPCE (TBD)
	functor(SS, send_super, A),
	A >= 2, !.
goal_classification(SS, expanded) :-	% XPCE (TBD)
	functor(SS, get_super, A), 
	A >= 3, !.

classify_head(TB, Goal, exported) :-
	xref_exported(TB, Goal), !.
classify_head(TB, Goal, unreferenced) :-
	\+ xref_called(TB, Goal), !.
classify_head(TB, Goal, How) :-
	xref_defined(TB, Goal, How), !.
classify_head(_TB, Goal, built_in) :-
	built_in_predicate(Goal), !.
classify_head(_TB, _Goal, undefined).

built_in_predicate(Goal) :-
	predicate_property(system:Goal, built_in), !.
built_in_predicate(module(_, _)).

%	Specify colours for individual goals.  Currently used only to
%	highlight file references, so we can jump to them and are indicated
%	on missing files.

goal_colours(use_module(_),	     built_in-[file]).
goal_colours(use_module(_,_),	     built_in-[file,classify]).
goal_colours(consult(_),	     built_in-[file]).
goal_colours(include(_),	     built_in-[file]).
goal_colours(ensure_loaded(_),	     built_in-[file]).
goal_colours(pce_autoload(_,_),	     classify-[classify,file]).
goal_colours(pce_image_directory(_), classify-[file]).
goal_colours(new(_, _),		     built_in-[classify,pce_new]).
goal_colours(send_list(_,_,_),	     built_in-pce_arg_list).
goal_colours(send(_,_),		     built_in-[pce_arg,pce_selector]).
goal_colours(get(_,_,_),	     built_in-[pce_arg,pce_selector,pce_arg]).
goal_colours(send_super(_,_),	     built_in-[pce_arg,pce_selector]).
goal_colours(get_super(_,_),	     built_in-[pce_arg,pce_selector,pce_arg]).
goal_colours(Pce,		     built_in-pce_arg) :-
	compound(Pce),
	functor(Pce, Functor, _),
	pce_functor(Functor).

pce_functor(send).
pce_functor(get).
pce_functor(send_super).
pce_functor(get_super).


		 /*******************************
		 *	  SPECIFIC HEADS	*
		 *******************************/

head_colours(file_search_path(_,_), hook-[identifier,classify]).
head_colours(library_directory(_),  hook-[file]).
head_colours(message_hook(_,_,_),   hook-[classify,classify,classify]).
head_colours(portray(_),	    hook-[classify]).
head_colours(resource(_,_,_),	    hook-[identifier,classify,file]).
head_colours(term_expansion(_,_),   hook-[classify,classify]).
head_colours(M:H, Colours) :-
	M == user,
	head_colours(H, HC),
	HC = hook - _, !,
	Colours = hook - [ hook, HC ].
head_colours(M:_,		    built_in-[module(M),head]).


		 /*******************************
		 *	       STYLES		*
		 *******************************/

%	style(+Pattern, -Style)
%
%	Define the style used for the given pattern.

style(goal(built_in),	  style(colour	   := blue)).
style(goal(imported),	  style(colour	   := blue)).
style(goal(autoload),	  style(colour	   := blue)).
style(goal(global),	  style(colour	   := navy_blue)).
style(goal(undefined),	  style(colour	   := red)).
style(goal(dynamic),	  style(colour	   := magenta)).
style(goal(multifile),	  style(colour	   := navy_blue)).
style(goal(expanded),	  style(colour	   := blue,
				underline  := @on)).

style(head(exported),	  style(bold	   := @on, colour := blue)).
style(head(local(_)),	  style(bold	   := @on)).
style(head(unreferenced), style(bold	   := @on, colour := red)).
style(head(hook),	  style(colour	   := blue,
				underline  := @on)).
style(comment,		  style(colour	   := dark_green)).

style(directive,	  style(background := grey90)).
style(syntax_error,	  style(background := red)).
style(method(_),	  style(bold       := @on)).

style(var,		  style(colour	   := red4)).
style(unbound,		  style(bold	   := @on, colour := red)).
style(quoted_atom,        style(colour	   := navy_blue)).
style(string,		  style(colour	   := navy_blue)).
style(nofile,		  style(colour	   := red)).
style(file(_),		  style(colour	   := blue,
				underline  := @on)).
style(class(built_in,_),  style(colour	   := blue,
				underline  := @on)).
style(class(user,_),	  style(underline  := @on)).
style(class(_,_),	  style(underline  := @on)).

style(identifier,	  style(bold       := @on)).
style(expanded,		  style(colour	   := blue,
				underline  := @on)).
style(hook,		  style(colour	   := blue,
				underline  := @on)).
style(error,		  style(background := orange)).

:- dynamic
	style_name/2.

style(Class, Name, Style) :-
	style(Class, Style),
	copy_term(Class, Copy),
	numbervars(Copy, 0, _),
	term_to_atom(Copy, Name).


%	term_colours(+Term, -FunctorColour, -ArgColours)
%
%	Define colourisation for specific terms.

term_colours((prolog:message(_) --> _),
	     expanded - [ expanded - [ expanded,
				       expanded - [ identifier
						  ]
				     ],
			  classify
			]).

%	XPCE rules

term_colours(variable(_, _, _, _),
	     expanded - [ identifier,
			  classify,
			  classify,
			  comment
			]).
term_colours(variable(_, _, _),
	     expanded - [ identifier,
			  classify,
			  atom
			]).
term_colours(handle(_, _, _),
	     expanded - [ classify,
			  classify,
			  classify
			]).
term_colours(handle(_, _, _, _),
	     expanded - [ classify,
			  classify,
			  classify,
			  classify
			]).
term_colours(class_variable(_,_,_,_),
	     expanded - [ identifier,
			  pce(type),
			  pce(default),
			  comment
			]).
term_colours(class_variable(_,_,_),
	     expanded - [ identifier,
			  pce(type),
			  pce(default),
			  comment
			]).
term_colours(delegate_to(_),
	     expanded - [ classify
			]).
term_colours((:- pce_begin_class(_, _, _)),
	     expanded - [ expanded - [ identifier,
				       pce_new,
				       comment
				     ]
			]).
term_colours((:- pce_begin_class(_, _)),
	     expanded - [ expanded - [ identifier,
				       pce_new
				     ]
			]).
term_colours((:- pce_extend_class(_)),
	     expanded - [ expanded - [ identifier
				     ]
			]).
term_colours((:- pce_end_class),
	     expanded - [ expanded
			]).
term_colours(:- pce_end_class(_),
	     expanded - [ expanded - [ identifier
				     ]
			]).
term_colours(:- use_class_template(_),
	     expanded - [ expanded - [ classify
				     ]
			]).
term_colours((:- emacs_begin_mode(_,_,_,_,_)),
	     expanded - [ expanded - [ identifier,
				       classify,
				       classify,
				       classify,
				       classify
				     ]
			]).
term_colours((:- emacs_extend_mode(_,_)),
	     expanded - [ expanded - [ identifier,
				       classify
				     ]
			]).
term_colours((:- pce_group(_)),
	     expanded - [ expanded - [ identifier
				     ]
			]).
term_colours((:- pce_global(_, _)),
	     expanded - [ expanded - [ identifier,
				       classify
				     ]
			]).
term_colours((:- emacs_end_mode),
	     expanded - [ expanded
			]).
term_colours(pce_ifhostproperty(_,_),
	     expanded - [ classify,
			  classify
			]).
term_colours((_,_),
	     error - [ classify,
		       classify
		     ]).

specified_item(_, Var, TB, Pos) :-
	var(Var), !,
	colourise_term_arg(Var, TB, Pos).
					% generic classification
specified_item(classify, Term, TB, Pos) :- !,
	colourise_term_arg(Term, TB, Pos).
					% classify as head
specified_item(head, Term, TB, Pos) :- !,
	colourise_clause_head(Term, TB, Pos).
					% classify as body
specified_item(body, Term, TB, Pos) :- !,
	colourise_body(Term, TB, Pos).
					% files
specified_item(file, Term, TB, Pos) :- !,
	colourise_files(Term, TB, Pos).
					% XPCE new argument
specified_item(pce_new, Term, TB, Pos) :- !,
	(   atom(Term)
	->  colourise_class(Term, TB, Pos)
	;   compound(Term)
	->  functor(Term, Class, _),
	    Pos = term_position(_,_,FF, FT, ArgPos),
	    colourise_class(Class, TB, FF-FT),
	    specified_items(pce_arg, Term, TB, ArgPos)
	;   colourise_term_arg(Term, TB, Pos)
	).
					% Generic XPCE arguments
specified_item(pce_arg, new(X), TB,
	       term_position(_,_,_,_,[ArgPos])) :- !,
	specified_item(pce_new, X, TB, ArgPos).
specified_item(pce_arg, new(X, T), TB,
	       term_position(_,_,_,_,[P1, P2])) :- !,
	colourise_term_arg(X, TB, P1),
	specified_item(pce_new, T, TB, P2).
specified_item(pce_arg, @Ref, TB, Pos) :- !,
	colourise_term_arg(@Ref, TB, Pos).
specified_item(pce_arg, Term, TB, Pos) :-
	compound(Term), !,
	specified_item(pce_new, Term, TB, Pos).
specified_item(pce_arg, Term, TB, Pos) :-
	colourise_term_arg(Term, TB, Pos).
					% List of XPCE arguments
specified_item(pce_arg_list, List, TB, list_position(_,_,Elms,Tail)) :- !,
	colourise_list_args(Elms, Tail, List, TB, pce_arg).
specified_item(pce_arg_list, Term, TB, Pos) :- !,
	specified_item(pce_arg, Term, TB, Pos).
					% XPCE selector
specified_item(pce_selector, Term, TB,
	       term_position(_,_,_,_,ArgPos)) :- !,
	specified_items(pce_arg, Term, TB, ArgPos).
specified_item(pce_selector, Term, TB, Pos) :-
	colourise_term_arg(Term, TB, Pos).
					% Nested specification
specified_item(FuncSpec-ArgSpecs, Term, TB,
	       term_position(_,_,FF,FT,ArgPos)) :- !,
	specified_item(FuncSpec, Term, TB, FF-FT),
	specified_items(ArgSpecs, Term, TB, ArgPos).
					% Specified
specified_item(Class, _, TB, Pos) :-
	colour_item(Class, TB, Pos).

%	specified_items(+Spec, +T, +TB, +PosList)

specified_items(Specs, Term, TB, PosList) :-
	is_list(Specs), !,
	specified_arglist(Specs, 1, Term, TB, PosList).
specified_items(Spec, Term, TB, PosList) :-
	specified_argspec(PosList, Spec, 1, Term, TB).


specified_arglist([], _, _, _, _).
specified_arglist([S0|ST], N, T, TB, [P0|PT]) :-
	arg(N, T, Term),
	specified_item(S0, Term, TB, P0),
	NN is N + 1,
	specified_arglist(ST, NN, T, TB, PT).

specified_argspec([], _, _, _, _).
specified_argspec([P0|PT], Spec, N, T, TB) :-
	arg(N, T, Term),
	specified_item(Spec, Term, TB, P0),
	NN is N + 1,
	specified_argspec(PT, Spec, NN, T, TB).

:- emacs_end_mode.

