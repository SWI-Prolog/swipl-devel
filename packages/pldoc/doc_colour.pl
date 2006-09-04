/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_colours,
	  [ colour_fragments/2,		% +Source, -Fragments
	    prolog_src_style/2		% ?Term, ?Style
	  ]).
:- use_module(library(prolog_xref)).
:- use_module(library(lists)).
:- use_module(library(operators)).
:- use_module(library(debug)).
:- use_module(doc_process).

/** <module> Source colouring support

This module is extracted from the   PceEmacs colouring code, providing a
reusable base for colouring source.  Issues:

	* Must this module keep the style info?

@bug	This module contains XPCE specific datastructures. Although there
	is no XPCE code here and it can thus execute in plain Prolog, this
	isn't very elegant.

@tbd	Make PceEmacs use this module
@tbd	Export call-back interface
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
User extension hooks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	style/2,
	identify/2,
	term_colours/2,
	goal_colours/2,
	goal_classification/2,
	pce_classify_class/3.

:- thread_local
	fragment/3.			% Start, Length, Class

%	XPCE operators

:- op(200, fy, @).
:- op(990, xfx, :=).

%%	colour_fragments(+In, -Fragments:list) is det.
%
%	Create a list of colour fragments from In.
%	
%	@param Fragments	List of fragment(Start, End, Class)
	
colour_fragments(Source, Fragments) :-
	F = fragment(_,_,_),
	retractall(F),
	prolog_canonical_source(Source, Src),
	xref_source(Src),
	process_source(Src, assert_fragment),
	findall(F, retract(F), Fragments0),
	sort(Fragments0, Fragments1),
	fragment_hierarchy(Fragments1, Fragments).
	
assert_fragment(Start, End, Class) :-
	assert(fragment(Start, End, Class)).


%%	fragment_hierarchy(+Fragments, -Hierarchy) is det.
%
%	Translate   list   of   fragment(Start,     End,   Class)   into
%	fragment(Start, End, Class, SubFragments).
%	
%	@tbd	Detect improper nesting.  How to handle?

fragment_hierarchy([], []).
fragment_hierarchy([fragment(S,E,C)|Rest0], [fragment(S,E,C,Sub)|Rest]) :-
	sub_fragments(Rest0, E, Sub, Rest1),
	fragment_hierarchy(Rest1, Rest).

sub_fragments([], _, [], []).
sub_fragments([F|R0], End, Sub, Rest) :-
	F = fragment(SF,EF,C),
	(   EF =< End
	->  Sub = [fragment(SF,EF,C,FSub)|RSub],
	    sub_fragments(R0, EF, FSub, R1),
	    sub_fragments(R1, End, RSub, Rest)
	;   Sub = [],
	    Rest = [F|R0]
	).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

%%	process_source(+Src, :Handler) is det.
%
%	Process input from Src, calling :Handler on identified
%	fragments.

process_source(Src, Handler) :-
	prolog_open_source(Src, Fd),
	b_setval(doc_colour_handler, Handler),
	call_cleanup(process_input(Fd, Src),
		     (	 prolog_close_source(Fd),
			 nb_delete(doc_colour_handler))).

process_input(In, Context) :-
	repeat,
	prolog_read_source_term(In, Term, _Expanded,
				[ comments(Comments),
				  subterm_positions(TermPos)
				]),
	(   Term == end_of_file
	->  !
	;   colourise_comments(Comments, Context),
	    colourise_term(Term, Context, TermPos)
	->  fail
	;   fail			% TBD: warning
	).

%%	colourise_comments(+Comments, +Context) is det.
%
%	Colourise  the  comments  we  have  found.  We  use  the  5.6.17
%	comment-hook of read to collect the comments realiably. Comments
%	are classified as one of =structured_comment= or =comment=.

colourise_comments([], _).
colourise_comments([H|T], Ctx) :-
	colourise_comment(H, Ctx),
	colourise_comments(T, Ctx).

colourise_comment(TermPos-Comment, Ctx) :-
	stream_position_data(char_count, TermPos, Start),
	atom_length(Comment, Len),
	End is Start + Len,
	(   is_structured_comment(Comment, _)
	->  colour_item(structured_comment, Ctx, Start-End)
	;   colour_item(comment, Ctx, Start-End)
	).

%%	colourise_term(+Term, +Context, +TermPos) is det.

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
	colourise_method_body(Body,	TB, BP).
colourise_term(:<-(Head, Body), TB,
	       term_position(F,T,FF,FT,[HP,BP])) :- !,
	colour_item(method,	       TB, F-T),
	colour_item(neck(method(get)), TB, FF-FT),
	colour_method_head(get(Head),  TB, HP),
	colourise_method_body(Body,    TB, BP).
colourise_term((:- Directive), TB, Pos) :- !,
	arg(1, Pos, F),
	arg(2, Pos, T0),
%	get(TB, scan, T, line, 0, end, EOL),
%	To is EOL+1,
	T is T0 + 1,			% TBD: include the '.'
	colour_item(directive, TB, F-T),
	arg(5, Pos, [ArgPos]),
	colourise_directive(Directive, TB, ArgPos).
colourise_term((?- Directive), TB, Pos) :- !,
	colourise_term((:- Directive), TB, Pos).
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

%%	colourise_extern_head(+Head, +Module, +TB, +Pos)
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

%%	functor_position(+Term, -FunctorPos, -ArgPosList)
%	
%	Get the position of a functor   and  its argument. Unfortunately
%	this goes wrong for lists, who have two `functor-positions'.

functor_position(term_position(_,_,FF,FT,ArgPos), FF-FT, ArgPos) :- !.
functor_position(list_position(F,_T,Elms,none), F-FT, Elms) :- !,
	FT is F + 1.
functor_position(Pos, Pos, []).


%%	colourise_directive(+Body, +TB, +Pos)
%	
%	Colourise the body of a directive.

colourise_directive(Body, TB, Pos) :-
	colourise_body(Body, TB, Pos).


%%	colourise_body(+Body, +TB, +Pos)
%	
%	Breaks down to colourise_goal/3.

colourise_body(Body, TB, Pos) :-
	colourise_body(Body, [], TB, Pos).

colourise_body(Body, Origin, TB, Pos) :-
	colour_item(body, TB, Pos),
	colourise_goals(Body, Origin, TB, Pos).

%%	colourise_method_body(+MethodBody, +TB, +Pos)
%	
%	Colourise the optional "comment":: as pce(comment) and proceed
%	with the body.

colourise_method_body(::(_Comment,Body), TB,
		      term_position(_F,_T,_FF,_FT,[CP,BP])) :- !,
	colour_item(comment, TB, CP),
	colourise_body(Body, TB, BP).
colourise_method_body(Body, TB, Pos) :-		% deal with pri(::) < 1000
	Body =.. [F,A,B],
	control_op(F), !,
	Pos = term_position(_F,_T,_FF,_FT,
			    [ AP,
			      BP
			    ]),
	colourise_method_body(A, TB, AP),
	colourise_body(B, TB, BP).
colourise_method_body(Body, TB, Pos) :-
	colourise_body(Body, TB, Pos).

control_op((,)).
control_op((;)).
control_op((->)).
control_op((*->)).

colourise_goals(Body, Origin, TB, term_position(_,_,_,_,ArgPos)) :-
	body_compiled(Body), !,
	colourise_subgoals(ArgPos, 1, Body, Origin, TB).
colourise_goals(Goal, Origin, TB, Pos) :-
	colourise_goal(Goal, Origin, TB, Pos).

colourise_subgoals([], _, _, _, _).
colourise_subgoals([Pos|T], N, Body, Origin, TB) :-
	arg(N, Body, Arg),
	colourise_goals(Arg, Origin, TB, Pos),
	NN is N + 1,
	colourise_subgoals(T, NN, Body, Origin, TB).

%%	colourise_dcg(+Body, +Head, +TB, +Pos)
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

%%	colourise_dcg_goal(+Goal, +Origin, +TB, +Pos).

colourise_dcg_goal(!, Origin, TB, TermPos) :- !,
	colourise_goal(!, Origin, TB, TermPos).
colourise_dcg_goal(Goal, Origin, TB, TermPos) :-
	dcg_extend(Goal, TheGoal), !,
	colourise_goal(TheGoal, Origin, TB, TermPos).
colourise_dcg_goal(Goal, _, TB, Pos) :-
	colourise_term_args(Goal, TB, Pos).


%%	colourise_goal(+Goal, +Origin, +TB, +Pos)
%	
%	Colourise access to a single goal.

					% Deal with list as goal (consult)
colourise_goal(Goal, _, TB, list_position(F,T,Elms,_)) :- !,
	FT is F + 1,
	AT is T - 1,
	colour_item(goal(built_in, Goal), TB, F-FT),
	colour_item(goal(built_in, Goal), TB, AT-T),
	colourise_file_list(Goal, TB, Elms).
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

%%	colourise_goal_args(+Goal, +TB, +Pos)
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

%%	meta_args(+Goal, -ArgSpec)
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

%%	expand_meta(+MetaSpec, +Goal, -Expanded)
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

%%	colourise_db(+Arg, +TB, +Pos)
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


%%	colourise_files(+Arg, +TB, +Pos)
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
	(   (   atom(TB)
	    ->	SourceId = TB
	    ;	TB = @SourceId
	    ),
	    catch(xref_source_file(Spec, Path, SourceId), _, fail)
	->  colour_item(file(Path), TB, Pos)
	;   colour_item(nofile, TB, Pos)
	).

colourise_file_list([], _, _).
colourise_file_list([H|T], TB, [PH|PT]) :-
	colourise_files(H, TB, PH),
	colourise_file_list(T, TB, PT).

%%	colourise_class(ClassName, TB, Pos)
%
%	Colourise   an   XPCE   class.   Relies     on   the   extension
%	pce_classify_class/3 to classify the XPCE class.

colourise_class(ClassName, TB, Pos) :-
	(   pce_classify_class(TB, ClassName, Classification)
	->  colour_item(class(Classification, ClassName), TB, Pos)
	;   true
	).

%%	colourise_term_args(+Term, +TB, +Pos)
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


%%	colourise_exports(+List, +TB, +Pos)
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


%%	colourise_imports(+List, +File, +TB, +Pos)
%	
%	Colourise import list from use_module/2, importing from File.

colourise_imports(List, File, TB, Pos) :-
	(   xref_public_list(File, _, Public, TB)
	->  true
	;   Public = []
	),
	colourise_imports(List, File, Public, TB, Pos).

colourise_imports([], _, _, _, _).
colourise_imports(List, File, Public, TB, list_position(_,_,ElmPos,Tail)) :- !,
	(   Tail == none
	->  true
	;   colour_item(type_error(list), TB, Tail)
	),
	colourise_imports2(List, File, Public, TB, ElmPos).
colourise_imports(_, _, _, TB, Pos) :-
	colour_item(type_error(list), TB, Pos).	

colourise_imports2([G0|GT], File, Public, TB, [P0|PT]) :- !,
	colourise_declaration(G0, TB, P0),
	colourise_imports2(GT, File, Public, TB, PT).
colourise_imports2(_, _, _, _, _).

%%	colourise_declarations(+Term, +TB, +Pos)
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
	atom(Name),
	style_check(?(dollar)), !,	% deal with system boot-files
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
colourise_declaration(op(_,_,_), TB, Pos) :-
	colour_item(exported_operator, TB, Pos).
colourise_declaration(_, TB, Pos) :-
	colour_item(type_error(export_declaration), TB, Pos).


%%	colour_item(+Class, +Source, +Pos)
%
%	colourise region if a style is defined for this class.

colour_item(Class, _Source, Pos) :-
	prolog_src_style(Class, _), !,
	arg(1, Pos, F),
	arg(2, Pos, T),
	b_getval(doc_colour_handler, Handler),
	call(Handler, F, T, Class).
colour_item(_, _, _).
	

		 /*******************************
		 *	  CONFIGURATION		*
		 *******************************/

%%	body_compiled(+Term)
%
%	Succeeds if term is a construct handled by the compiler.

body_compiled((_,_)).
body_compiled((_->_)).
body_compiled((_*->_)).
body_compiled((_;_)).
body_compiled(\+_).

%%	goal_classification(+TB, +Goal, +Origin, -Class)
%	
%	Classify Goal appearing in TB and called from a clause with head
%	Origin.  For directives Origin is [].

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

%%	goal_classification(+Goal, -Class)
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
classify_head(_TB, Goal, hook) :-
	xref_hook(Goal), !.
classify_head(TB, Goal, hook) :-
	xref_module(TB, M),
	xref_hook(M:Goal), !.
classify_head(TB, Goal, unreferenced) :-
	\+ (xref_called(TB, Goal, By), By \= Goal), !.
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
goal_colours(use_module(File,_),     built_in-[file,imports(File)]).
goal_colours(dynamic(_),	     built_in-[predicates]).
goal_colours(thread_local(_),	     built_in-[predicates]).
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
goal_colours(get_chain(_,_,_),	     built_in-[pce_arg,pce_selector,pce_arg]).
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
head_colours(resource(_,_,_),	    hook-[identifier,classify,file]).

head_colours(M:H, Colours) :-
	atom(M), callable(H),
	xref_hook(M:H), !,
	Colours = hook - [ hook, hook-classify ].
head_colours(M:H, Colours) :-
	M == user,
	head_colours(H, HC),
	HC = hook - _, !,
	Colours = hook - [ hook, HC ].
head_colours(M:_,		    meta-[module(M),extern(M)]).


		 /*******************************
		 *	       STYLES		*
		 *******************************/

%%	def_style(+Pattern, -Style)
%
%	Define the style used for the   given  pattern. Definitions here
%	can     be     overruled     by       defining     rules     for
%	emacs_prolog_colours:style/2

def_style(goal(built_in,_),	style(colour := blue)).
def_style(goal(imported(_),_),	style(colour := blue)).
def_style(goal(autoload,_), 	style(colour := navy_blue)).
def_style(goal(global,_),	style(colour := navy_blue)).
def_style(goal(undefined,_),	style(colour := red)).
def_style(goal(thread_local(_),_), style(colour := magenta,
				      underline:= @on)).
def_style(goal(dynamic(_),_), 	style(colour := magenta)).
def_style(goal(multifile(_),_),	style(colour := navy_blue)).
def_style(goal(expanded,_),	style(colour := blue,
				      underline := @on)).
def_style(goal(extern(_),_),	style(colour := blue,
				      underline := @on)).
def_style(goal(recursion,_),	style(underline := @on)).
def_style(goal(meta,_),		style(colour := red4)). % same as var
def_style(goal(foreign(_),_),	style(colour := darkturquoise)).
def_style(goal(local(_),_),	@default).
def_style(goal(constraint(_),_), style(colour := darkcyan)).

def_style(head(exported),	style(bold := @on, colour := blue)).
def_style(head(extern(_)),	style(bold := @on, colour := blue)).
def_style(head(dynamic),	style(bold := @on, colour := magenta)).
def_style(head(multifile),	style(bold := @on, colour := navy_blue)).
def_style(head(unreferenced),	style(bold := @on, colour := red)).
def_style(head(hook),	  	style(underline  := @on, colour := blue)).
def_style(head(meta),	  	@default).
def_style(head(constraint(_)),	style(bold := @on, colour := darkcyan)).
def_style(head(_),	  	style(bold := @on)).

def_style(comment,		style(colour := dark_green)).
def_style(structured_comment,	style(colour := dark_green)).

def_style(directive,	  	style(background := grey90)).
def_style(method(_),	  	style(bold := @on)).

def_style(var,		  	style(colour := red4)).
def_style(unbound,		style(bold := @on, colour := red)).
def_style(quoted_atom,        	style(colour := navy_blue)).
def_style(string,		style(colour := navy_blue)).
def_style(nofile,		style(colour := red)).
def_style(file(_),		style(colour := blue,
				      underline  := @on)).
def_style(class(built_in,_),	style(colour := blue,
				      underline := @on)).
def_style(class(library(_),_),	style(colour := navy_blue,
				      underline := @on)).
def_style(class(local(_,_,_),_), style(underline := @on)).
def_style(class(user(_),_),	style(underline := @on)).
def_style(class(user,_), 	style(underline := @on)).
def_style(class(undefined,_),	style(colour := red,
				      underline  := @on)).
def_style(prolog_data,		style(colour := blue,
				      underline  := @on)).

def_style(identifier, 		style(bold := @on)).
def_style(delimiter,		style(bold := @on)).
def_style(expanded,		style(colour := blue,
				      underline  := @on)).

def_style(hook,			style(colour := blue,
				      underline := @on)).

def_style(error,		style(background := orange)).
def_style(type_error(_),	style(background := orange)).
def_style(syntax_error,	  	style(background := red)).

:- dynamic
	style_name/2.

%%	prolog_src_style(?Class, ?Style) is nondet.
%
%	True if Style is the style to use for displaying an element
%	with the given classification.

prolog_src_style(Class, Style) :-
	style(Class, Style).		% user hook
prolog_src_style(Class, Style) :-
	def_style(Class, Style).	% system default

style(Class, Name, Style) :-
	prolog_src_style(Class, Style),
	copy_term(Class, Copy),
	numbervars(Copy, 0, _),
	term_to_atom(Copy, Name).


%%	term_colours(+Term, -FunctorColourArgColours)
%
%	Define colourisation for specific terms.

term_colours((?- Directive), Colours) :-
	term_colours((:- Directive), Colours).
term_colours((prolog:message(_) --> _),
	     expanded - [ expanded - [ expanded,
				       expanded - [ identifier
						  ]
				     ],
			  classify
			]).
term_colours((prolog:error_message(_) --> _),
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
term_colours((:- encoding(_)),
	     expanded - [ expanded - [ classify
				     ]
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
					% DCG goal in body
specified_item(dcg, Term, TB, Pos) :- !,
	colourise_dcg(Term, [], TB, Pos).
					% assert/retract arguments
specified_item(db, Term, TB, Pos) :- !,
	colourise_db(Term, TB, Pos).
					% files
specified_item(file, Term, TB, Pos) :- !,
	colourise_files(Term, TB, Pos).
					% [Name/Arity, ...]
specified_item(exports, Term, TB, Pos) :- !,
	colourise_exports(Term, TB, Pos).
					% [Name/Arity, ...]
specified_item(imports(File), Term, TB, Pos) :- !,
	colourise_imports(Term, File, TB, Pos).
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
specified_item(pce_arg, prolog(Term), TB,
	       term_position(_,_,FF,FT,[ArgPos])) :- !,
	colour_item(prolog_data, TB, FF-FT),
	colourise_term_arg(Term, TB, ArgPos).
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
specified_item(FuncSpec-ElmSpec, List, TB, list_position(F,T,ElmPos,TailPos)) :- !,
	FT is F + 1,
	AT is T - 1,
	colour_item(FuncSpec, TB, F-FT),
	colour_item(FuncSpec, TB, AT-T),
	specified_list(ElmSpec, List, TB, ElmPos, TailPos).
specified_item(Class, _, TB, Pos) :-
	colour_item(Class, TB, Pos).

%%	specified_items(+Spec, +T, +TB, +PosList)

specified_items(Specs, Term, TB, PosList) :-
	is_list(Specs), !,
	specified_arglist(Specs, 1, Term, TB, PosList).
specified_items(Spec, Term, TB, PosList) :-
	specified_argspec(PosList, Spec, 1, Term, TB).


specified_arglist([], _, _, _, _).
specified_arglist(_, _, _, _, []) :- !.			% Excess specification args
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


%%	specified_list(+Spec, +List, +TB, +PosList, TailPos)

specified_list([], [], _, [], _).
specified_list([HS|TS], [H|T], TB, [HP|TP], TailPos) :- !,
	specified_item(HS, H, TB, HP),
	specified_list(TS, T, TB, TP, TailPos).
specified_list(Spec, [H|T], TB, [HP|TP], TailPos) :-
	specified_item(Spec, H, TB, HP),
	specified_list(Spec, T, TB, TP, TailPos).
specified_list(_, _, _, [], none) :- !.
specified_list(Spec, Tail, TB, [], TailPos) :-
	specified_item(Spec, Tail, TB, TailPos).
