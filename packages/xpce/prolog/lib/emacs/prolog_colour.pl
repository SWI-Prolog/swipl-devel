/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
	    assert(style_name(Class, Name)),
	    Style \== @default,
	    send(M, style, Name, Style),
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
	statistics(runtime, _),
	new(Class, class(emacs_colour_fragment)),
	get(Class, no_created, @on, OldCreated),

	send_super(M, colourise_buffer),
	send(M, setup_styles),
	send(M, xref_buffer),
	send(M, report, progress, 'Colourising buffer ...'),
	do_colourise_buffer(M),
	send(M, colourise_comments),
	statistics(runtime, [_,UsedMilliSeconds]),
	Used is UsedMilliSeconds/1000,
	get(Class, no_created, @on, NewCreated),
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

xref_buffer(M, Always:[bool]) :->
	"Run the cross-referencer on buffer"::
	get(M, text_buffer, TB),
	get(TB, generation, G),
	(   (   Always == @on
	    ->  true
	    ;   get(TB, xref_generation, GRef),
		GRef \== G
	    )
	->  send(M, report, progress, 'Cross-referencing buffer ...'),
	    xref_source(TB),
	    send(TB, xref_generation, G),
	    send(M, report, done)
	;   true
	).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

do_colourise_buffer(M) :-
	get(M, text_buffer, TB),
	pce_open(TB, read, Fd),
	(   peek_char(Fd, #)		% skip #! script line
	->  skip(Fd, 10)
	;   true
	),
	'$style_check'(Old, Old),
	repeat,
	    catch(read_term(Fd, Term,
			    [ subterm_positions(TermPos),
			      singletons(Singletons),
			      character_escapes(true)
			    ]),
		  _E,
		  (   %print_message(error, _E),
		      fail
		  )),
	    fix_operators(Term),
	    (	colourise_term(Term, TB, TermPos),
		send(M, mark_singletons, Term, Singletons, TermPos)
	    ->	true
	    ;	print_message(warning,
			      format('Failed to colourise ~p~n', [Term]))
	    ),
	    Term == end_of_file, !,
	'$style_check'(_, Old),
	close(Fd).

%	fix_operators(+Term)
%	
%	Fix flags that affect the syntax, such as operators and some
%	style checking options.

fix_operators((:-style_check(X))) :- !,
	style_check(X).
fix_operators((:-module($(Name),_))) :-
	atom(Name),
	style_check(+dollar),
	fail.				% allow for other expansions
fix_operators('$:-'(_)) :- !,		% deal with swi('boot/init.pl')
	style_check(+dollar).
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
	colourise_body(Body, Head,  TB,	BP).
colourise_term((Head --> Body), TB,			% TBD: expansion!
	       term_position(F,T,FF,FT,[HP,BP])) :- !,
	colour_item(grammar_rule,	TB, F-T),
	colour_item(neck(grammar_rule),	TB, FF-FT),
	colourise_extended_head(Head, 2, TB, HP),
	colourise_dcg(Body, Head,	TB, BP).
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

colourise_extended_head(Head, N, TB, Pos) :-
	functor_position(Pos, FPos, _),
	Head =.. List,
	length(Extra, N),
	append(List, Extra, List1),
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

%	colourise_extern_head(+Head, +Module, +TB, +Pos)
%	
%	Colourise the head specified as Module:Head. Normally used for
%	adding clauses to multifile predicates in other modules.

colourise_extern_head(Head, M, TB, Pos) :-
	functor_position(Pos, FPos, _),
	colour_item(head(extern(M)), TB, FPos),
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
	colourise_body(Body, [], TB, Pos).

colourise_body(Body, Origin, TB, Pos) :-
	colour_item(body, TB, Pos),
	colourise_goals(Body, Origin, TB, Pos).

colourise_goals(Body, Origin, TB, term_position(_,_,_,_,ArgPos)) :-
	body_compiled(Body), !,
	colourise_subgoals(ArgPos, 1, Body, Origin, TB).
colourise_goals(::(_Comment, Body), Origin, TB,% XPCE <Comment>::Body construct
	       term_position(_,_,_,_,[CommentPos, BodyPos])) :- !,
	colour_item(pce(comment), TB, CommentPos),
	colourise_goals(Body, Origin, TB, BodyPos).
colourise_goals(Goal, Origin, TB, Pos) :-
	colourise_goal(Goal, Origin, TB, Pos).

colourise_subgoals([], _, _, _, _).
colourise_subgoals([Pos|T], N, Body, Origin, TB) :-
	arg(N, Body, Arg),
	colourise_goals(Arg, Origin, TB, Pos),
	NN is N + 1,
	colourise_subgoals(T, NN, Body, Origin, TB).

%	colourise_dcg(+Body, +Head, +TB, +Pos)
%	
%	Breaks down to colourise_dcg_goal/3.

colourise_dcg(Body, Head, TB, Pos) :-
	colour_item(dcg, TB, Pos),
	dcg_extend(Head, Origin),
	colourise_dcg_goals(Body, Origin, TB, Pos).

colourise_dcg_goals(Var, _, TB, Pos) :-
	var(Var), !,
	colour_item(goal(meta,Var), TB, Pos).
colourise_dcg_goals({Body}, Origin, TB,	brace_term_position(F,T,Arg)) :- !,
	colour_item(dcg(plain), TB, F-T),
	colourise_goals(Body, Origin, TB, Arg).
colourise_dcg_goals([], _, TB, Pos) :- !,
	colour_item(dcg(list), TB, Pos).
colourise_dcg_goals(List, _, TB, Pos) :-
	List = [_|_], !,
	colour_item(dcg(list), TB, Pos),
	colourise_term_args(List, TB, Pos).
colourise_dcg_goals(Body, Origin, TB, term_position(_,_,_,_,ArgPos)) :-
	body_compiled(Body), !,
	colourise_dcg_subgoals(ArgPos, 1, Body, Origin, TB).
colourise_dcg_goals(Goal, Origin, TB, Pos) :-
	colourise_dcg_goal(Goal, Origin, TB, Pos),
	colourise_term_args(Goal, TB, Pos).

colourise_dcg_subgoals([], _, _, _, _).
colourise_dcg_subgoals([Pos|T], N, Body, Origin, TB) :-
	arg(N, Body, Arg),
	colourise_dcg_goals(Arg, Origin, TB, Pos),
	NN is N + 1,
	colourise_dcg_subgoals(T, NN, Body, Origin, TB).

dcg_extend(Term, Goal) :-
	callable(Term),
	Term =.. List,
	append(List, [_,_], List2),
	Goal =.. List2.

%	colourise_dcg_goal(+Goal, +Origin, +TB, +Pos).

colourise_dcg_goal(!, Origin, TB, TermPos) :- !,
	colourise_goal(!, Origin, TB, TermPos).
colourise_dcg_goal(Goal, Origin, TB, TermPos) :-
	dcg_extend(Goal, TheGoal), !,
	colourise_goal(TheGoal, Origin, TB, TermPos).
colourise_dcg_goal(Goal, _, TB, Pos) :-
	colourise_term_args(Goal, TB, Pos).


%	colourise_goal(+Goal, +Origin, +TB, +Pos)
%	
%	Colourise access to a single goal.

colourise_goal(Goal, Origin, TB, Pos) :-
	nonvar(Goal),
	goal_colours(Goal, ClassSpec-ArgSpecs), !, % specified
	functor_position(Pos, FPos, ArgPos),
	(   ClassSpec == classify
	->  goal_classification(TB, Goal, Origin, Class)
	;   Class = ClassSpec
	),
	colour_item(goal(Class, Goal), TB, FPos),
	specified_items(ArgSpecs, Goal, TB, ArgPos).
colourise_goal(Module:Goal, _Origin, TB, term_position(_,_,_,_,[PM,PG])) :- !,
	colour_item(module(Module), TB, PM),
	(   PG = term_position(_,_,FF,FT,_)
	->  FP = FF-FT
	;   FP = PG
	),
	colour_item(goal(extern(Module), Goal), TB, FP),
	colourise_goal_args(Goal, TB, PG).
colourise_goal(Goal, Origin, TB, Pos) :-
	goal_classification(TB, Goal, Origin, Class),
	(   Pos = term_position(_,_,FF,FT,_ArgPos)
	->  FPos = FF-FT
	;   FPos = Pos
	),
	colour_item(goal(Class, Goal), TB, FPos),
	colourise_goal_args(Goal, TB, Pos).

%	colourise_goal_args(+Goal, +TB, +Pos)
%	
%	Colourise the arguments to a goal. This predicate deals with
%	meta- and database-access predicates.

colourise_goal_args(Goal, TB, term_position(_,_,_,_,ArgPos)) :-
	meta_args(Goal, MetaArgs), !,
	colourise_meta_args(1, Goal, MetaArgs, TB, ArgPos).
colourise_goal_args(Goal, TB, Pos) :-
	colourise_term_args(Goal, TB, Pos).

colourise_meta_args(_, _, _, _, []) :- !.
colourise_meta_args(N, Goal, MetaArgs, TB, [P0|PT]) :-
	arg(N, Goal, Arg),
	arg(N, MetaArgs, MetaSpec),
	(   expand_meta(MetaSpec, Arg, Expanded)
	->  colourise_goal(Expanded, [], TB, P0) % TBD: recursion
	;   colourise_term_arg(Arg, TB, P0)
	),
	NN is N + 1,
	colourise_meta_args(NN, Goal, MetaArgs, TB, PT).

%	meta_args(+Goal, -ArgSpec)
%	
%	Return a copy of Goal, where each meta-argument is an integer
%	representing the number of extra arguments. The non-meta
%	arguments are unbound variables.
%	
%	E.g. meta_args(maplist(foo,x,y), X) --> X = maplist(2,_,_)
%	
%	NOTE: this could be cached if performance becomes an issue.

meta_args(Goal, VarGoal) :-
	xref_meta(Goal, _),
	functor(Goal, Name, Arity),
	functor(VarGoal, Name, Arity),
	xref_meta(VarGoal, MetaArgs),
	instantiate_meta(MetaArgs).

instantiate_meta([]).
instantiate_meta([H|T]) :-
	(   var(H)
	->  H = 0
	;   H = V+N
	->  V = N
	),
	instantiate_meta(T).

%	expand_meta(+MetaSpec, +Goal, -Expanded)
%	
%	Add extra arguments to the goal if the meta-specifier is an
%	integer (see above).

expand_meta(MetaSpec, Goal, Goal) :-
	MetaSpec == 0.
expand_meta(MetaSpec, Goal, Expanded) :-
	integer(MetaSpec),
	callable(Goal), !,
	length(Extra, MetaSpec),
	Goal =.. List0,
	append(List0, Extra, List),
	Expanded =.. List.

%	colourise_db(+Arg, +TB, +Pos)
%	
%	Colourise database modification calls (assert/1, retract/1 and
%	friends.

colourise_db((Head:-_Body), TB, term_position(_,_,_,_,[HP,_])) :- !,
	colourise_db(Head, TB, HP).
colourise_db(Module:Head, TB, term_position(_,_,_,_,[MP,HP])) :- !,
	colour_item(module(Module), TB, MP),
	(   atom(Module),
	    xref_module(Module, TB)
	->  colourise_db(Head, TB, HP)
	;   true			% TBD: Modifying in other module
	).
colourise_db(Head, TB, Pos) :-
	colourise_goal(Head, '<db-change>', TB, Pos).


%	colourise_files(+Arg, +TB, +Pos)
%
%	Colourise the argument list of one of the file-loading predicates.

colourise_files(List, TB, list_position(_,_,Elms,_)) :- !,
	colourise_file_list(List, TB, Elms).
colourise_files(M:Spec, TB, term_position(_,_,_,_,[MP,SP])) :- !,
	colour_item(module(M), TB, MP),
	colourise_files(Spec, TB, SP).
colourise_files(Var, TB, P) :-
	var(Var), !,
	colour_item(var, TB, P).
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


%	colourise_exports(+List, +TB, +Pos)
%	
%	Colourise the module export-list (or any other list holding
%	terms of the form Name/Arity referring to predicates).

colourise_exports([], _, _) :- !.
colourise_exports(List, TB, list_position(_,_,ElmPos,Tail)) :- !,
	(   Tail == none
	->  true
	;   colour_item(type_error(list), TB, Tail)
	),
	colourise_exports2(List, TB, ElmPos).
colourise_exports(_, TB, Pos) :-
	colour_item(type_error(list), TB, Pos).

colourise_exports2([G0|GT], TB, [P0|PT]) :- !,
	colourise_declaration(G0, TB, P0),
	colourise_exports2(GT, TB, PT).
colourise_exports2(_, _, _).

%	colourise_declarations(+Term, +TB, +Pos)
%	
%	Colourise the Name/Arity lists of dynamic, multifile, etc
%	declarations.

colourise_declarations((Head,Tail), TB,
		       term_position(_,_,_,_,[PH,PT])) :- !,
	colourise_declaration(Head, TB, PH),
	colourise_declarations(Tail, TB, PT).
colourise_declarations(Last, TB, Pos) :-
	colourise_declaration(Last, TB, Pos).

colourise_declaration($(Name)/Arity, TB, Pos) :-
	style_check(?dollar), !,	% deal with system boot-files
	atom_concat($, Name, TheName),
	colourise_declaration(TheName/Arity, TB, Pos).
colourise_declaration(Name/Arity, TB, Pos) :-
	atom(Name), integer(Arity), !,
	functor(Goal, Name, Arity),
	goal_classification(TB, Goal, [], Class),
	colour_item(goal(Class, Goal), TB, Pos).
colourise_declaration(Module:Name/Arity, TB,
		      term_position(_,_,_,_,[PM,PG])) :-
	atom(Module), atom(Name), integer(Arity), !,
	colour_item(module(M), TB, PM),
	functor(Goal, Name, Arity),
	colour_item(goal(extern(M), Goal), TB, PG).
colourise_declaration(_, TB, Pos) :-
	colour_item(type_error(name_arity), TB, Pos).


%	colour_item(+Class, +TB, +Pos)
%
%	colourise region if a style is defined for this class.

colour_item(Class, TB, Pos) :-
	style_name(Class, Name), !,
	arg(1, Pos, F),
	arg(2, Pos, T),
	L is T - F,
	make_fragment(Class, TB, F, L, Name).
colour_item(_, _, _).
	
colour_item(Class, TB, F, T) :-
	colour_item(Class, TB, F-T).

%	make_fragment(+Class, +TB, +From, +Len, +StyleName)
%	
%	Actually create the fragment.

make_fragment(goal(Class, Goal), TB, F, L, Style) :-
	callable(Goal), !,
	new(Fragment, emacs_goal_fragment(TB, F, L, Style)),
	functor(Goal, Name, Arity),
	send(Fragment, name, Name),
	send(Fragment, arity, Arity),
	(   Class =.. [ClassName,Context],
	    atomic(Context)
	->  send(Fragment, classification, ClassName),
	    send(Fragment, context, Context)
	;   functor(Class, ClassName, _),
	    send(Fragment, classification, ClassName)
	).
make_fragment(class(Type, Class), TB, F, L, Style) :-
	atom(Class), !,
	new(Fragment, emacs_class_fragment(TB, F, L, Style)),
	send(Fragment, classification, Type),
	send(Fragment, referenced_class, Class).
make_fragment(Class, TB, F, L, Style) :-
	new(Fragment, emacs_prolog_fragment(TB, F, L, Style)),
	functor(Class, Classification, Arity),
	send(Fragment, classification, Classification),
	(   Arity == 1,
	    arg(1, Class, Context),
	    atomic(Context)
	->  send(Fragment, context, Context)
	;   true
	).

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

%	goal_classification(+TB, +Goal, +Origin, -Class)
%	
%	Classify Goal appearing in TB and called from a clause with head
%	Origin.

goal_classification(_, Goal, _, meta) :-
	var(Goal), !.
goal_classification(_, Goal, Origin, recursion) :-
	callable(Goal),
	functor(Goal, Name, Arity),
	functor(Origin, Name, Arity), !.
goal_classification(TB, Goal, _, How) :-
	xref_defined(TB, Goal, How), !.
goal_classification(_TB, Goal, _, Class) :-
	goal_classification(Goal, Class), !.
goal_classification(_TB, _Goal, _, undefined).

%	goal_classification(+Goal, -Class)
%	
%	Multifile hookable classification for non-local goals.

goal_classification(Goal, built_in) :-
	built_in_predicate(Goal), !.
goal_classification(Goal, autoload) :-	% SWI-Prolog
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

goal_colours(module(_,_),	     built_in-[identifier,exports]).
goal_colours(use_module(_),	     built_in-[file]).
goal_colours(use_module(_,_),	     built_in-[file,classify]).
goal_colours(dynamic(_),	     built_in-[predicates]).
goal_colours(multifile(_),	     built_in-[predicates]).
goal_colours(volatile(_),	     built_in-[predicates]).
goal_colours(consult(_),	     built_in-[file]).
goal_colours(include(_),	     built_in-[file]).
goal_colours(ensure_loaded(_),	     built_in-[file]).
goal_colours(load_files(_,_),	     built_in-[file,classify]).
% Database access
goal_colours(assert(_),		     built_in-[db]).
goal_colours(asserta(_),	     built_in-[db]).
goal_colours(assertz(_),	     built_in-[db]).
goal_colours(assert(_,_),	     built_in-[db,classify]).
goal_colours(asserta(_,_),	     built_in-[db,classify]).
goal_colours(assertz(_,_),	     built_in-[db,classify]).
goal_colours(retract(_),	     built_in-[db]).
goal_colours(retractall(_),	     built_in-[db]).
goal_colours(clause(_,_),	     built_in-[db,classify]).
goal_colours(clause(_,_,_),	     built_in-[db,classify,classify]).
% XPCE stuff
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
head_colours(goal_expansion(_,_),   hook-[classify,classify]).
head_colours(prolog_trace_interception(_,_,_,_),
				    hook-classify).
head_colours(M:H, Colours) :-
	M == user,
	head_colours(H, HC),
	HC = hook - _, !,
	Colours = hook - [ hook, HC ].
head_colours(M:_,		    meta-[module(M),extern(M)]).


		 /*******************************
		 *	       STYLES		*
		 *******************************/

%	style(+Pattern, -Style)
%
%	Define the style used for the given pattern.

style(goal(built_in,_),	  style(colour	   := blue)).
style(goal(imported(_),_),style(colour	   := blue)).
style(goal(autoload,_),	  style(colour	   := blue)).
style(goal(global,_),	  style(colour	   := navy_blue)).
style(goal(undefined,_),  style(colour	   := red)).
style(goal(dynamic,_),	  style(colour	   := magenta)).
style(goal(multifile,_),  style(colour	   := navy_blue)).
style(goal(expanded,_),	  style(colour	   := blue,
				underline  := @on)).
style(goal(extern(_),_),  style(colour	   := blue,
				underline  := @on)).
style(goal(recursion,_),  style(underline  := @on)).
style(goal(meta,_),	  style(colour	   := red4)). % same as var
style(goal(local(_),_),	  @default).

style(head(exported),	  style(bold	   := @on, colour := blue)).
style(head(extern(_)),	  style(bold	   := @on, colour := blue)).
style(head(dynamic),	  style(bold	   := @on, colour := magenta)).
style(head(multifile),	  style(bold	   := @on, colour := navy_blue)).
style(head(unreferenced), style(bold	   := @on, colour := red)).
style(head(hook),	  style(underline  := @on, colour := blue)).
style(head(meta),	  @default).
style(head(_),	  	  style(bold	   := @on)).

style(comment,		  style(colour	   := dark_green)).

style(directive,	  style(background := grey90)).
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
style(type_error(_),	  style(background := orange)).
style(syntax_error,	  style(background := red)).

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
			  pce(default)
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
	     expanded - [ expanded - [ pce_new
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
term_colours((:- pce_global(_, new(_))),
	     expanded - [ expanded - [ identifier,
				       pce_arg
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
					% expanded head (DCG=2, ...)
specified_item(head(+N), Term, TB, Pos) :- !,
	colourise_extended_head(Term, N, TB, Pos).
					% M:Head
specified_item(extern(M), Term, TB, Pos) :- !,
	colourise_extern_head(Term, M, TB, Pos).
					% classify as body
specified_item(body, Term, TB, Pos) :- !,
	colourise_body(Term, TB, Pos).
					% assert/retract arguments
specified_item(db, Term, TB, Pos) :- !,
	colourise_db(Term, TB, Pos).
					% files
specified_item(file, Term, TB, Pos) :- !,
	colourise_files(Term, TB, Pos).
					% [Name/Arity, ...]
specified_item(exports, Term, TB, Pos) :- !,
	colourise_exports(Term, TB, Pos).
					% Name/Arity, ...
specified_item(predicates, Term, TB, Pos) :- !,
	colourise_declarations(Term, TB, Pos).
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
	compound(Term),
	Term \= [_|_], !,
	specified_item(pce_new, Term, TB, Pos).
specified_item(pce_arg, Term, TB, Pos) :- !,
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
					% Nested for {...}
specified_item(FuncSpec-[ArgSpec], {Term}, TB,
	       brace_term_position(F,T,ArgPos)) :- !,
	specified_item(FuncSpec, {Term}, TB, F-T),
	specified_item(ArgSpec, Term, TB, ArgPos).
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


		 /*******************************
		 *	   GOAL FRAGMENT	*
		 *******************************/

:- pce_begin_class(emacs_goal_fragment, emacs_colour_fragment,
		   "Fragment for a goal in PceEmacs Prolog mode").

variable(name,		 name,	both, "Name of the predicate").
variable(arity,		 int,	both, "Arity of the predicate").
variable(classification, name,	both, "XREF classification").
variable(context,	 any*,	both, "Classification argument").

:- pce_group(popup).

popup(_GF, Popup:popup) :<-
	"Return popup menu"::
	Popup = @prolog_mode_goal_popup.

:- pce_global(@prolog_mode_goal_popup,
	      make_prolog_mode_goal_popup).

%	make_prolog_mode_goal_popup(-Popup)
%	
%	Create the popup and define actions for handling the right-menu
%	on goals.

make_prolog_mode_goal_popup(G) :-
	new(G, popup(goal_actions)),
	Fragment = @arg1,
	new(HasSource, message(Fragment, has_source)),
	new(HasListing, message(Fragment, has_listing)),
	send_list(G, append,
		  [ menu_item(edit,
			      message(Fragment, edit),
			      condition := HasSource),
		    menu_item(edit_other_window,
			      message(Fragment, edit, @on),
			      condition := HasSource),
		    gap,
		    menu_item(listing,
			      message(Fragment, listing),
			      condition := HasListing),
		    gap,
		    menu_item(documentation,
			      message(Fragment, documentation))
		  ]).


module(F, Module:name) :<-
	"Module for Module:Goal references"::
	get(F, classification, extern),
	get(F, context, Module),
	Module \== @nil.

predicate(F, Pred:prolog_predicate) :<-
	"Get referenced predicate"::
	get(F, name, Name),
	get(F, arity, Arity),
	(   get(F, module, Module)
	->  Spec = Name/Arity
	;   Spec = Module:Name/Arity
	),
	new(Pred, prolog_predicate(Spec)).

head(F, Head:prolog) :<-
	"Return goal-head"::
	get(F, name, Name),
	get(F, arity, Arity),
	functor(Head0, Name, Arity),
	(   get(F, module, M)
	->  Head = M:Head0
	;   Head = Head0
	).

loaded_specifier(F, TheHead:prolog) :<-
	"Get predicate specifier for loaded predicate"::
	get(F, head, Head),
	(   Head = _:_
	->  TheHead = Head
	;   get(F, text_buffer, TB),
	    xref_module(TB, M)
	->  TheHead = M:Head
	;   TheHead = _:Head
	),
	current_predicate(_, TheHead).

has_source(F) :->
	"Test if there is source available"::
	get(F, text_buffer, TB),
	get(F, head, Head),
	(   xref_defined(TB, Head, local(_Line))
	->  true
	;   xref_defined(TB, Head, imported(_From))
	->  true
	;   get(prolog_predicate(Head), source, _)
	).


%	->edit
%	
%	Find the predicate and invoke ->find_definition on the
%	@emacs_mode, which is the mode object of the current editor.

edit(F, NewWindow:[bool]) :->
	"Open Prolog predicate [in new window]"::
	get(F, predicate, Pred),
	send(@emacs_mode, find_definition, Pred, NewWindow).


%	->listing
%	
%	List the predicate in an XPCE buffer

listing(F) :->
	"Generate a listing"::
	get(F, loaded_specifier, Spec),
	new(Tmp, emacs_buffer(@nil, string('*Listing for %N*', F))),
	pce_open(Tmp, write, Out),
	telling(Old), set_output(Out),
	ignore(listing(Spec)),
	tell(Old),
	close(Out),
	send(Tmp, modified, @off),
%	send(Tmp, mode, prolog),
	send(Tmp, open).


has_listing(F) :->
	"Test if we can make a listing"::
	get(F, loaded_specifier, Spec),
	predicate_property(Spec, number_of_clauses(N)),
	N > 0.


documentation(F) :->
	"Invoke Prolog help-system"::
	send(F?predicate, help).


print_name(F, PN:string) :<-
	"Return [Module:]Name/Arity"::
	get(F, name, Name),
	get(F, arity, Arity),
	(   get(F, module, Module)
	->  new(PN, string('%s:%s/%d', Module, Name, Arity))
	;   new(PN, string('%s/%d', Name, Arity))
	).


identify(F) :->
	"Tell the user about the predicate"::
	get(F, text_buffer, TB),
	get(F, classification, Class),
	(   get(F, context, Context),
	    Context \== @nil
	->  Id =.. [Class, Context]
	;   Id = Class
	),
	identify_pred(Id, F, Report),
	send(TB, report, status, Report).

%	identify_pred(+XrefClass, +Fragment, -Summary)
%	
%	Generate an identifying description for the predicate.

identify_pred(built_in, F, Summary) :-	% SWI-Prolog documented built-in
	get(F, predicate, Pred),
	get(Pred, summary, Summary0), !,
	new(Summary, string('%N: %s', Pred, Summary0)).
identify_pred(built_in, F, Summary) :-
	get(F, head, Head),
	predicate_property(system:Head, foreign), !,
	new(Summary, string('%N: Built-in foreign predicate', F)).
identify_pred(built_in, F, Summary) :-
	get(F, name, Name),
	sub_atom(Name, 0, _, _, $), !,
	new(Summary, string('%N: SWI-Prolog private built-in', F)).
identify_pred(autoload, F, Summary) :-	% Autoloaded predicates
	get(F, name, Name),
	get(F, arity, Arity),
	'$find_library'(_Module, Name, Arity, _LoadModule, Library), !,
	absolute_file_name(Library,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   Source),
	new(Summary, string('%N: autoload from %s', F, Source)).
identify_pred(local(Line), F, Summary) :-	% Local predicates
	new(Summary, string('%N: locally defined at line %d', F, Line)).
identify_pred(imported(From), F, Summary) :-
	new(Summary, string('%N: imported from %s', F, From)).
identify_pred(recursion, _, 'Recursive reference') :- !.
identify_pred(dynamic, F, Summary) :-
	get(F, loaded_specifier, Spec),
	(   predicate_property(Spec, number_of_clauses(N))
	->  new(Summary, string('%N: dynamic predicate with %d clauses',
				prolog_predicate(Spec),
				N))
	;   new(Summary, string('%N: dynamic predicate', F))
	).
identify_pred(Class, _, ClassName) :-
	term_to_atom(Class, ClassName).

:- pce_end_class(emacs_goal_fragment).


		 /*******************************
		 *	  CLASS FRAGMENT	*
		 *******************************/

:- pce_begin_class(emacs_class_fragment, emacs_colour_fragment,
		   "Represent an XPCE class in PceEmacs Prolog mode").


variable(classification,   name, both, "XREF classification").
variable(referenced_class, name, both, "Name of referenced class").

:- pce_group(popup).

popup(_GF, Popup:popup) :<-
	"Return popup menu"::
	Popup = @prolog_mode_class_popup.

:- pce_global(@prolog_mode_class_popup,
	      make_prolog_mode_class_popup).

make_prolog_mode_class_popup(G) :-
	new(G, popup(class_actions)),
	Fragment = @arg1,
	send_list(G, append,
		  [ menu_item(edit,
			      message(Fragment, edit),
			      condition := message(Fragment, user_class)),
		    menu_item(edit_other_window,
			      message(Fragment, edit, @on),
			      condition := message(Fragment, user_class)),
		    gap,
		    menu_item(documentation,
			      message(Fragment, documentation))
		  ]).

user_class(F) :->
	"Test if referenced class is non-builtin"::
	get(F, referenced_class, ClassName),
	get(@pce, convert, ClassName, class, Class),
	\+ get(Class, creator, built_in).

edit(F, NewWindow:[bool]) :->
	"Open XPCE class [in new window]"::
	get(F, referenced_class, ClassName),
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
	;   send(F, report, error, 'Class %s doesn''t exist', ClassName)
	).

documentation(F) :->
	"Open XPCE manual"::
	get(F, referenced_class, ClassName),
	manpce(ClassName).

identify(F) :->
	"Provide identification"::
	get(F, referenced_class, ClassName),
	(   get(@pce, convert, ClassName, class, Class)
	->  get(F, classification, Classification),
	    (   get(Class, summary, Summary)
	    ->	true
	    ;	Summary = '<no summary>'
	    ),
	    send(F, report, status,
		     string('XPCE %s class %s: %s',
			    Classification, ClassName, Summary))
	;   send(F, report, status, 'Class %s doesn''t exist', ClassName)
	).


:- pce_end_class(emacs_class_fragment).


		 /*******************************
		 *      GENERIC FRAGMENTS	*
		 *******************************/

:- pce_begin_class(emacs_prolog_fragment, emacs_colour_fragment,
		   "Colour fragment in Prolog mode").

variable(classification, name,	both, "XREF classification").
variable(context,	 any*,	both, "Classification argument").

:- pce_group(popup).

popup(F, Popup:popup) :<-
	get(F, context, Context),
	Context \== @nil,
	(   get(F, classification, file)
	;   get(F, classification, module)
	),
	Popup = @prolog_mode_file_popup.

:- pce_global(@prolog_mode_file_popup,
	      make_prolog_mode_file_popup).

make_prolog_mode_file_popup(G) :-
	new(G, popup(file_actions)),
	send_list(G, append,
		  [ menu_item(open,
			      message(@emacs, open_file, @arg1?file)),
		    menu_item(open_other_window,
			      message(@emacs, open_file, @arg1?file, @on))
		  ]).

file(F, File:name) :<-
	"Return associated file"::
	get(F, context, Context),
	(   get(F, classification, file)
	->  File = Context
	;   get(F, classification, module)
	->  current_module(Context, File)
	).


identify(F) :->
	"Identify in status window"::
	get(F, classification, Class),
	(   get(F, context, Context),
	    Context \== @nil
	->  Term =.. [Class, Context]
	;   Term = Class
	),
	identify_fragment(Term, F, Summary), !,
	send(F, report, status, Summary).

identify_fragment(var,  _, 'Variable').
identify_fragment(file(Path), _, Summary) :-
	new(Summary, string('File %s', Path)).
identify_fragment(type_error(Type), _, Summary) :-
	new(Summary, string('Type error: argument must be a %s', Type)).
identify_fragment(module(Module), _, Summary) :-
	current_module(Module, Path),
	new(Summary, string('Module %s loaded from %s', Module, Path)).
identify_fragment(method(send), _, 'XPCE send method').
identify_fragment(method(get), _, 'XPCE get method').
identify_fragment(head(unreferenced), _, 'Unreferenced predicate').
identify_fragment(head(exported), _, 'Exported (Public) predicate').
identify_fragment(Class, _, Summary) :-
	term_to_atom(Class, Summary).

:- pce_end_class.
		   
