/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(prolog_colour,
	  [ prolog_colourise_stream/3,	% +Stream, +SourceID, :ColourItem
	    prolog_colourise_term/4,	% +Stream, +SourceID, :ColourItem, +Options
	    syntax_colour/2,		% +Class, -Attributes
	    syntax_message//1		% +Class
	  ]).
:- use_module(library(prolog_xref)).
:- use_module(library(predicate_options)).
:- use_module(library(prolog_source)).
:- use_module(library(lists)).
:- use_module(library(operators)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(record)).
:- if(exists_source(library(pce_meta))).
:- use_module(library(pce_meta)).
:- endif.

:- meta_predicate
	prolog_colourise_stream(+, +, 3),
	prolog_colourise_term(+, +, 3, +).

:- predicate_options(prolog_colourise_term/4, 4,
		     [ subterm_positions(-any)
		     ]).

/** <module> Prolog syntax colouring support.

This module defines reusable code to colourise Prolog source.

@tbd: The one-term version
*/


:- multifile
	style/2,			% +ColourClass, -Attributes
	message//1,			% +ColourClass
	term_colours/2,			% +SourceTerm, -ColourSpec
	goal_colours/2,			% +Goal, -ColourSpec
	directive_colours/2,		% +Goal, -ColourSpec
	goal_classification/2.		% +Goal, -Class


:- record
	colour_state(source_id,
		     module,
		     stream,
		     closure,
		     singletons).

%%	prolog_colourise_stream(+Stream, +SourceID, :ColourItem) is det.
%
%	Determine colour fragments for the data   on Stream. SourceID is
%	the  canonical  identifier  of  the  input    as  known  to  the
%	cross-referencer, i.e., as created using xref_source(SourceID).
%
%	ColourItem is a closure  that  is   called  for  each identified
%	fragment with three additional arguments:
%
%	  * The syntactical category
%	  * Start position (character offset) of the fragment
%	  * Length of the fragment (in characters).

prolog_colourise_stream(Fd, SourceId, ColourItem) :-
	make_colour_state([ source_id(SourceId),
			    stream(Fd),
			    closure(ColourItem)
			  ],
			  TB),
	setup_call_cleanup(
	    save_settings(TB, State),
	    colourise_stream(Fd, TB),
	    restore_settings(State)).

colourise_stream(Fd, TB) :-
	(   peek_char(Fd, #)		% skip #! script line
	->  skip(Fd, 10)
	;   true
	),
	repeat,
	    colour_state_module(TB, SM),
	    character_count(Fd, Start),
	    catch(read_term(Fd, Term,
			    [ subterm_positions(TermPos),
			      singletons(Singletons),
			      module(SM),
			      comments(Comments)
			    ]),
		  E,
		  read_error(E, TB, Fd, Start)),
	    fix_operators(Term, TB),
	    colour_state_singletons(TB, Singletons),
	    (	colourise_term(Term, TB, TermPos, Comments)
	    ->	true
	    ;	arg(1, TermPos, From),
	        print_message(warning,
			      format('Failed to colourise ~p at index ~d~n',
				     [Term, From]))
	    ),
	    Term == end_of_file, !.

save_settings(TB, state(Style, Esc, OSM)) :-
	(   source_module(TB, SM)
	->  true
	;   SM = prolog_colour_ops
	),
	'$set_source_module'(OSM, SM),
	colour_state_module(TB, SM),
	push_operators([]),
	current_prolog_flag(character_escapes, Esc),
	'$style_check'(Style, Style).

restore_settings(state(Style, Esc, OSM)) :-
	set_prolog_flag(character_escapes, Esc),
	'$style_check'(_, Style),
	pop_operators,
	'$set_source_module'(_, OSM).

%%	source_module(+State, -Module) is semidet.
%
%	True when Module is the module context   into  which the file is
%	loaded. This is the module of the file if File is a module file,
%	or the load context of  File  if   File  is  not included or the
%	module context of the file into which the file was included.

source_module(TB, Module) :-
	(   colour_state_source_id(TB, File),
	    atom(File)
	;   colour_state_stream(TB, Fd),
	    stream_property(Fd, file_name(File))
	),
	module_context(File, [], Module).

module_context(File, _, Module) :-
	source_file_property(File, module(Module)), !.
module_context(File, Seen, Module) :-
	source_file_property(File, included_in(File2, _Line)),
	\+ memberchk(File, Seen), !,
	module_context(File2, [File|Seen], Module).
module_context(File, _, Module) :-
	source_file_property(File, load_context(Module, _, _)).


%%	read_error(+Error, +TB, +Stream, +Start) is failure.
%
%	If this is a syntax error, create a syntax-error fragment.

read_error(Error, TB, Stream, Start) :-
	(   syntax_error(Error, Id, CharNo)
	->  message_to_string(error(syntax_error(Id), _), Msg),
	    character_count(Stream, End),
	    show_syntax_error(TB, CharNo:Msg, Start-End),
	    fail
	;   throw(Error)
	).

syntax_error(error(syntax_error(Id), stream(_S, _Line, _LinePos, CharNo)),
	     Id, CharNo).
syntax_error(error(syntax_error(Id), file(_S, _Line, _LinePos, CharNo)),
	     Id, CharNo).

%%	colour_item(+Class, +TB, +Pos) is det.

colour_item(Class, TB, Pos) :-
	arg(1, Pos, Start),
	arg(2, Pos, End),
	Len is End - Start,
	colour_state_closure(TB, Closure),
	call(Closure, Class, Start, Len).


%%	safe_push_op(+Prec, +Type, :Name, +State)
%
%	Define operators into the default source module and register
%	them to be undone by pop_operators/0.

safe_push_op(P, T, N0, State) :-
	colour_state_module(State, CM),
	strip_module(CM:N0, M, N),
	push_op(P, T, M:N),
	debug(colour, ':- ~w.', [op(P,T,M:N)]).

%%	fix_operators(+Term, +State) is det.
%
%	Fix flags that affect the  syntax,   such  as operators and some
%	style checking options. Src is the  canonical source as required
%	by the cross-referencer.

fix_operators((:- Directive), Src) :-
	ground(Directive),
	catch(process_directive(Directive, Src), _, true), !.
fix_operators(_, _).

process_directive(style_check(X), _) :- !,
	style_check(X).
process_directive(M:op(P,T,N), Src) :- !,
	process_directive(op(P,T,M:N), Src).
process_directive(op(P,T,N), Src) :- !,
	safe_push_op(P, T, N, Src).
process_directive(module(_Name, Export), Src) :- !,
	forall(member(op(P,A,N), Export),
	       safe_push_op(P,A,N, Src)).
process_directive(use_module(Spec), Src) :- !,
	catch(process_use_module(Spec, Src), _, true).
process_directive(Directive, Src) :-
	prolog_source:expand((:-Directive), Src, _).

%%	process_use_module(+Imports, +Src)
%
%	Get the exported operators from the referenced files.

process_use_module([], _) :- !.
process_use_module([H|T], Src) :- !,
	process_use_module(H, Src),
	process_use_module(T, Src).
process_use_module(File, Src) :-
	(   xref_public_list(File, Src,
			     [ exports(Exports),
			       silent(true),
			       path(Path)
			     ])
	->  forall(member(op(P,T,N), Exports),
		   safe_push_op(P,T,N,Src)),
	    colour_state_module(Src, SM),
	    (	member(Syntax/4, Exports),
		load_quasi_quotation_syntax(SM:Path, Syntax),
		fail
	    ;	true
	    )
	;   true
	).

%%	prolog_colourise_term(+Stream, +SourceID, :ColourItem, +Options)
%
%	Colourise    the    next     term      on     Stream.     Unlike
%	prolog_colourise_stream/3, this predicate assumes  it is reading
%	a single term rather than the   entire stream. This implies that
%	it cannot adjust syntax according to directives that preceed it.
%
%	Options:
%
%	  * subterm_positions(-TermPos)
%	  Return complete term-layout.  If an error is read, this is a
%	  term error_position(StartClause, EndClause, ErrorPos)

prolog_colourise_term(Stream, SourceId, ColourItem, Options) :-
	make_colour_state([ source_id(SourceId),
			    stream(Stream),
			    closure(ColourItem)
			  ],
			  TB),
	option(subterm_positions(TermPos), Options, _),
	findall(Op, xref_op(SourceId, Op), Ops),
	character_count(Stream, Start),
	(   source_module(TB, Module)
	->  true
	;   Module = prolog_colour_ops
	),
	read_source_term_at_location(
	    Stream, Term,
	    [ module(Module),
	      operators(Ops),
	      error(Error),
	      subterm_positions(TermPos),
	      singletons(Singletons),
	      comments(Comments)
	    ]),
	(   var(Error)
	->  colour_state_singletons(TB, Singletons),
	    colour_item(range, TB, TermPos),		% Call to allow clearing
	    colourise_term(Term, TB, TermPos, Comments)
	;   character_count(Stream, End),
	    TermPos = error_position(Start, End, Pos),
	    colour_item(range, TB, TermPos),
	    show_syntax_error(TB, Error, Start-End),
	    Error = Pos:_Message
	).

show_syntax_error(TB, Pos:Message, Range) :-
	End is Pos + 1,
	colour_item(syntax_error(Message, Range), TB, Pos-End).


singleton(Var, TB) :-
	colour_state_singletons(TB, Singletons),
	member_var(Var, Singletons).

member_var(V, [_=V2|_]) :-
	V == V2, !.
member_var(V, [_|T]) :-
	member_var(V, T).

%%	colourise_term(+Term, +TB, +Termpos, +Comments)
%
%	Colourise the next Term.
%
%	@bug	The colour spec is closed with =fullstop=, but the
%		position information does not include the full stop
%		location, so all we can do is assume it is behind the
%		term.

colourise_term(Term, TB, TermPos, Comments) :-
	colourise_comments(Comments, TB),
	(   Term == end_of_file
	->  true
	;   colourise_term(Term, TB, TermPos),
	    arg(2, TermPos, EndTerm),
	    Start is EndTerm + 1,
	    End is Start+1,
	    colour_item(fullstop, TB, Start-End)
	).

colourise_comments(-, _).
colourise_comments([], _).
colourise_comments([H|T], TB) :-
	colourise_comment(H, TB),
	colourise_comments(T, TB).

colourise_comment(Pos-Comment, TB) :-
	stream_position_data(char_count, Pos, Start),
	string_length(Comment, Len),
	End is Start + Len + 1,
	colour_item(comment, TB, Start-End).

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
colourise_term(((Head,RHC) --> Body), TB,
	       term_position(F,T,FF,FT,
			     [ term_position(_,_,_,_,[HP,RHCP]),
			       BP
			     ])) :- !,
	colour_item(grammar_rule,	TB, F-T),
	colour_item(dcg_right_hand_ctx, TB, RHCP),
	colourise_term_arg(RHC, TB, RHCP),
	colour_item(neck(grammar_rule),	TB, FF-FT),
	colourise_extended_head(Head, 2, TB, HP),
	colourise_dcg(Body, Head,	TB, BP).
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
	colour_item(directive, TB, Pos),
	arg(1, Pos, F),
	arg(2, Pos, T),
	colour_item(neck(directive), TB, F-T),
	arg(5, Pos, [ArgPos]),
	colourise_directive(Directive, TB, ArgPos).
colourise_term((?- Directive), TB, Pos) :- !,
	colourise_term((:- Directive), TB, Pos).
colourise_term(end_of_file, _, _) :- !.
colourise_term(Fact, TB, Pos) :- !,
	colour_item(clause, TB,	Pos),
	colourise_clause_head(Fact, TB, Pos).

%%	colourise_extended_head(+Head, +ExtraArgs, +TB, +Pos) is det.
%
%	Colourise a clause-head that  is   extended  by  term_expansion,
%	getting ExtraArgs more  arguments  (e.g.,   DCGs  add  two  more
%	arguments.

colourise_extended_head(Head, N, TB, Pos) :-
	extend(Head, N, TheHead),
	colourise_clause_head(TheHead, TB, Pos).

extend(M:Head, N, M:ExtHead) :-
	nonvar(Head), !,
	extend(Head, N, ExtHead).
extend(Head, N, ExtHead) :-
	callable(Head), !,
	Head =.. List,
	length(Extra, N),
	append(List, Extra, List1),
	ExtHead =.. List1.
extend(Head, _, Head).


colourise_clause_head(M:Head, TB, term_position(_,_,_,_,[MPos,HeadPos])) :-
	head_colours(M:Head, meta-[_, ClassSpec-ArgSpecs]), !,
	colour_item(module(M), TB, MPos),
	functor_position(HeadPos, FPos, ArgPos),
	(   ClassSpec == classify
	->  classify_head(TB, Head, Class)
	;   Class = ClassSpec
	),
	colour_item(head(Class, Head), TB, FPos),
	specified_items(ArgSpecs, Head, TB, ArgPos).
colourise_clause_head(Head, TB, Pos) :-
	head_colours(Head, ClassSpec-ArgSpecs), !,
	functor_position(Pos, FPos, ArgPos),
	(   ClassSpec == classify
	->  classify_head(TB, Head, Class)
	;   Class = ClassSpec
	),
	colour_item(head(Class, Head), TB, FPos),
	specified_items(ArgSpecs, Head, TB, ArgPos).
colourise_clause_head(Head, TB, Pos) :-
	functor_position(Pos, FPos, _),
	classify_head(TB, Head, Class),
	colour_item(head(Class, Head), TB, FPos),
	colourise_term_args(Head, TB, Pos).

%%	colourise_extern_head(+Head, +Module, +TB, +Pos)
%
%	Colourise the head specified as Module:Head. Normally used for
%	adding clauses to multifile predicates in other modules.

colourise_extern_head(Head, M, TB, Pos) :-
	functor_position(Pos, FPos, _),
	colour_item(head(extern(M), Head), TB, FPos),
	colourise_term_args(Head, TB, Pos).

colour_method_head(SGHead, TB, Pos) :-
	arg(1, SGHead, Head),
	functor(SGHead, SG, _),
	functor_position(Pos, FPos, _),
	colour_item(method(SG), TB, FPos),
	colourise_term_args(Head, TB, Pos).

%	functor_position(+Term, -FunctorPos, -ArgPosList)
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

colourise_directive((A,B), TB, term_position(_,_,_,_,[PA,PB])) :- !,
	colourise_directive(A, TB, PA),
	colourise_directive(B, TB, PB).
colourise_directive(Body, TB, Pos) :-
	nonvar(Body),
	directive_colours(Body, ClassSpec-ArgSpecs), !, % specified
	functor_position(Pos, FPos, ArgPos),
	(   ClassSpec == classify
	->  goal_classification(TB, Body, [], Class)
	;   Class = ClassSpec
	),
	colour_item(goal(Class, Body), TB, FPos),
	specified_items(ArgSpecs, Body, TB, ArgPos).
colourise_directive(Body, TB, Pos) :-
	colourise_body(Body, TB, Pos).


%	colourise_body(+Body, +TB, +Pos)
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
%
%	@tbd	Get this handled by a hook.

colourise_method_body(::(_Comment,Body), TB,
		      term_position(_F,_T,_FF,_FT,[CP,BP])) :- !,
	colour_item(comment, TB, CP),
	colourise_body(Body, TB, BP).
colourise_method_body(Body, TB, Pos) :-		% deal with pri(::) < 1000
	Body =.. [F,A,B],
	control_op(F), !,
	Pos = term_position(_F,_T,FF,FT,
			    [ AP,
			      BP
			    ]),
	colour_item(control, TB, FF-FT),
	colourise_method_body(A, TB, AP),
	colourise_body(B, TB, BP).
colourise_method_body(Body, TB, Pos) :-
	colourise_body(Body, TB, Pos).

control_op(',').
control_op((;)).
control_op((->)).
control_op((*->)).

colourise_goals(Body, Origin, TB, term_position(_,_,FF,FT,ArgPos)) :-
	body_compiled(Body), !,
	colour_item(control, TB, FF-FT),
	colourise_subgoals(ArgPos, 1, Body, Origin, TB).
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
	(   dcg_extend(Head, Origin)
	->  true
	;   Origin = Head
	),
	colourise_dcg_goals(Body, Origin, TB, Pos).

colourise_dcg_goals(Var, _, TB, Pos) :-
	var(Var), !,
	colour_item(goal(meta,Var), TB, Pos).
colourise_dcg_goals({Body}, Origin, TB,	brace_term_position(F,T,Arg)) :- !,
	colour_item(dcg(plain), TB, F-T),
	colourise_goals(Body, Origin, TB, Arg).
colourise_dcg_goals([], _, TB, Pos) :- !,
	colour_item(dcg(terminal), TB, Pos).
colourise_dcg_goals(List, _, TB, list_position(F,T,Elms,Tail)) :-
	List = [_|_], !,
	colour_item(dcg(terminal), TB, F-T),
	colourise_list_args(Elms, Tail, List, TB, classify).
colourise_dcg_goals(List, _, TB, string_position(F,T)) :-
	List = [_|_], !,
	colour_item(dcg(terminal), TB, F-T).
colourise_dcg_goals(Body, Origin, TB, term_position(_,_,_,_,ArgPos)) :-
	dcg_body_compiled(Body), !,	% control structures
	colourise_dcg_subgoals(ArgPos, 1, Body, Origin, TB).
colourise_dcg_goals(Goal, Origin, TB, Pos) :-
	colourise_dcg_goal(Goal, Origin, TB, Pos).

colourise_dcg_subgoals([], _, _, _, _).
colourise_dcg_subgoals([Pos|T], N, Body, Origin, TB) :-
	arg(N, Body, Arg),
	colourise_dcg_goals(Arg, Origin, TB, Pos),
	NN is N + 1,
	colourise_dcg_subgoals(T, NN, Body, Origin, TB).

dcg_extend(Term, _) :-
	var(Term), !, fail.
dcg_extend(M:Term, M:Goal) :-
	dcg_extend(Term, Goal).
dcg_extend(Term, Goal) :-
	callable(Term),
	Term =.. List,
	append(List, [_,_], List2),
	Goal =.. List2.

dcg_body_compiled(G) :-
	body_compiled(G), !.
dcg_body_compiled((_|_)).

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

					% Deal with list as goal (consult)
colourise_goal(Goal, _, TB, list_position(F,T,Elms,_)) :- !,
	FT is F + 1,
	AT is T - 1,
	colour_item(goal(built_in, Goal), TB, F-FT),
	colour_item(goal(built_in, Goal), TB, AT-T),
	colourise_file_list(Goal, TB, Elms, any).
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
	colourise_goal_args(Goal, Module, TB, PG).
colourise_goal(Op, _Origin, TB, Pos) :-
	nonvar(Op),
	Op = op(_,_,_), !,
	colourise_op_declaration(Op, TB, Pos).
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

colourise_goal_args(Goal, TB, Pos) :-
	(   colour_state_source_id(TB, SourceId),
	    xref_module(SourceId, Module)
	->  true
	;   Module = user
	),
	colourise_goal_args(Goal, Module, TB, Pos).

colourise_goal_args(Goal, M, TB, term_position(_,_,_,_,ArgPos)) :-
	meta_args(Goal, TB, MetaArgs), !,
	colourise_meta_args(1, Goal, M, MetaArgs, TB, ArgPos).
colourise_goal_args(Goal, M, TB, term_position(_,_,_,_,ArgPos)) :-
	colourise_goal_args(1, Goal, M, TB, ArgPos).
colourise_goal_args(_, _, _, _).		% no arguments

colourise_goal_args(_, _, _, _, []) :- !.
colourise_goal_args(N, Goal, Module, TB, [P0|PT]) :-
	colourise_option_arg(Goal, Module, N, TB, P0), !,
	NN is N + 1,
	colourise_goal_args(NN, Goal, Module, TB, PT).
colourise_goal_args(N, Goal, Module, TB, [P0|PT]) :-
	arg(N, Goal, Arg),
	colourise_term_arg(Arg, TB, P0),
	NN is N + 1,
	colourise_goal_args(NN, Goal, Module, TB, PT).


colourise_meta_args(_, _, _, _, _, []) :- !.
colourise_meta_args(N, Goal, Module, MetaArgs, TB, [P0|PT]) :-
	colourise_option_arg(Goal, Module, N, TB, P0), !,
	NN is N + 1,
	colourise_meta_args(NN, Goal, Module, MetaArgs, TB, PT).
colourise_meta_args(N, Goal, Module, MetaArgs, TB, [P0|PT]) :-
	arg(N, Goal, Arg),
	arg(N, MetaArgs, MetaSpec),
	colourise_meta_arg(MetaSpec, Arg, TB, P0),
	NN is N + 1,
	colourise_meta_args(NN, Goal, Module, MetaArgs, TB, PT).

colourise_meta_arg(MetaSpec, Arg, TB, Pos) :-
	expand_meta(MetaSpec, Arg, Expanded), !,
	colourise_goal(Expanded, [], TB, Pos). % TBD: recursion
colourise_meta_arg(MetaSpec, Arg, TB, Pos) :-
	MetaSpec == //, !,
	colourise_dcg_goals(Arg, //, TB, Pos).
colourise_meta_arg(_, Arg, TB, Pos) :-
	colourise_term_arg(Arg, TB, Pos).

%%	meta_args(+Goal, +TB, -ArgSpec) is semidet.
%
%	Return a copy of Goal, where   each  meta-argument is an integer
%	representing the number of extra arguments   or  the atom // for
%	indicating a DCG  body.  The   non-meta  arguments  are  unbound
%	variables.
%
%	E.g. meta_args(maplist(foo,x,y), X) --> X = maplist(2,_,_)
%
%	NOTE: this could be cached if performance becomes an issue.

meta_args(Goal, TB, VarGoal) :-
	colour_state_source_id(TB, SourceId),
	xref_meta(SourceId, Goal, _),
	functor(Goal, Name, Arity),
	functor(VarGoal, Name, Arity),
	xref_meta(SourceId, VarGoal, MetaArgs),
	instantiate_meta(MetaArgs).

instantiate_meta([]).
instantiate_meta([H|T]) :-
	(   var(H)
	->  H = 0
	;   H = V+N
	->  V = N
	;   H = //(V)
	->  V = (//)
	),
	instantiate_meta(T).

%%	expand_meta(+MetaSpec, +Goal, -Expanded) is semidet.
%
%	Add extra arguments to the goal if the meta-specifier is an
%	integer (see above).

expand_meta(MetaSpec, Goal, Goal) :-
	MetaSpec == 0.
expand_meta(MetaSpec, M:Goal, M:Expanded) :-
	atom(M), !,
	expand_meta(MetaSpec, Goal, Expanded).
expand_meta(MetaSpec, Goal, Expanded) :-
	integer(MetaSpec),
	callable(Goal), !,
	length(Extra, MetaSpec),
	Goal =.. List0,
	append(List0, Extra, List),
	Expanded =.. List.

%%	colourise_setof(+Term, +TB, +Pos)
%
%	Colourise the 2nd argument of setof/bagof

colourise_setof(Var^G, TB, term_position(_,_,FF,FT,[VP,GP])) :- !,
	colourise_term_arg(Var, TB, VP),
	colour_item(built_in, TB, FF-FT),
	colourise_setof(G, TB, GP).
colourise_setof(Term, TB, Pos) :-
	colourise_goal(Term, [], TB, Pos).

%	colourise_db(+Arg, +TB, +Pos)
%
%	Colourise database modification calls (assert/1, retract/1 and
%	friends.

colourise_db((Head:-_Body), TB, term_position(_,_,_,_,[HP,_])) :- !,
	colourise_db(Head, TB, HP).
colourise_db(Module:Head, TB, term_position(_,_,_,_,[MP,HP])) :- !,
	colour_item(module(Module), TB, MP),
	(   atom(Module),
	    colour_state_source_id(TB, SourceId),
	    xref_module(SourceId, Module)
	->  colourise_db(Head, TB, HP)
	;   colourise_db(Head, TB, HP)
	).
colourise_db(Head, TB, Pos) :-
	colourise_goal(Head, '<db-change>', TB, Pos).


%%	colourise_option_args(+Goal, +Module, +Arg:integer,
%			      +TB, +ArgPos) is semidet.
%
%	Colourise  predicate  options  for  the    Arg-th   argument  of
%	Module:Goal

colourise_option_arg(Goal, Module, Arg, TB, ArgPos) :-
	functor(Goal, Name, Arity),
	current_option_arg(Module:Name/Arity, Arg),
	current_predicate_options(Module:Name/Arity, Arg, OptionDecl),
	debug(emacs, 'Colouring option-arg ~w of ~p',
	      [Arg, Module:Name/Arity]),
	arg(Arg, Goal, Options),
	colourise_option(Options, Module, Goal, Arg, OptionDecl, TB, ArgPos).

colourise_option(Options0, Module, Goal, Arg, OptionDecl, TB, Pos0) :-
	strip_option_module_qualifier(Goal, Module, Arg, TB,
				      Options0, Pos0, Options, Pos),
	(   Pos = list_position(F, T, ElmPos, TailPos)
	->  colour_item(list, TB, F-T),
	    colourise_option_list(Options, OptionDecl, TB, ElmPos, TailPos)
	;   (   var(Options)
	    ;   Options == []
	    )
	->  colourise_term_arg(Options, TB, Pos)
	;   colour_item(type_error(list), TB, Pos)
	).

strip_option_module_qualifier(Goal, Module, Arg, TB,
			      M:Options, term_position(_,_,_,_,[MP,Pos]),
			      Options, Pos) :-
	predicate_property(Module:Goal, meta_predicate(Head)),
	arg(Arg, Head, :), !,
	colour_item(module(M), TB, MP).
strip_option_module_qualifier(_, _, _, _,
			      Options, Pos, Options, Pos).


colourise_option_list(_, _, _, [], none) :- !.
colourise_option_list(Tail, _, TB, [], TailPos) :- !,
	colourise_term_arg(Tail, TB, TailPos).
colourise_option_list([H|T], OptionDecl, TB, [HPos|TPos], TailPos) :-
	colourise_option(H, OptionDecl, TB, HPos),
	colourise_option_list(T, OptionDecl, TB, TPos, TailPos).

colourise_option(Opt, _, TB, Pos) :-
	var(Opt), !,
	colourise_term_arg(Opt, TB, Pos).
colourise_option(Opt, OptionDecl, TB, term_position(_,_,FF,FT,ValPosList)) :- !,
	functor(Opt, Name, Arity),
	functor(GenOpt, Name, Arity),
	(   memberchk(GenOpt, OptionDecl)
	->  colour_item(option_name, TB, FF-FT),
	    Opt =.. [Name|Values],
	    GenOpt =.. [Name|Types],
	    colour_option_values(Values, Types, TB, ValPosList)
	;   colour_item(no_option_name, TB, FF-FT)
	).
colourise_option(_, _, TB, Pos) :-
	colour_item(type_error(option), TB, Pos).

colour_option_values([], [], _, _).
colour_option_values([V0|TV], [T0|TT], TB, [P0|TP]) :-
	(   (   var(V0)
	    ;	is_of_type(T0, V0)
	    ;	T0 = list(_),
		member(E, V0),
		var(E)
	    )
	->  colourise_term_arg(V0, TB, P0)
	;   callable(V0),
	    (	T0 = callable
	    ->	N = 0
	    ;	T0 = (callable+N)
	    )
	->  colourise_meta_arg(N, V0, TB, P0)
	;   colour_item(type_error(T0), TB, P0)
	),
	colour_option_values(TV, TT, TB, TP).


%%	colourise_files(+Arg, +TB, +Pos, +Why)
%
%	Colourise the argument list of one of the file-loading predicates.
%
%	@param Why is one of =any= or =imported=

colourise_files(List, TB, list_position(F,T,Elms,_), Why) :- !,
	colour_item(list, TB, F-T),
	colourise_file_list(List, TB, Elms, Why).
colourise_files(M:Spec, TB, term_position(_,_,_,_,[MP,SP]), Why) :- !,
	colour_item(module(M), TB, MP),
	colourise_files(Spec, TB, SP, Why).
colourise_files(Var, TB, P, _) :-
	var(Var), !,
	colour_item(var, TB, P).
colourise_files(Spec0, TB, Pos, Why) :-
	strip_module(Spec0, _, Spec),
	(   colour_state_source_id(TB, Source),
	    prolog_canonical_source(Source, SourceId),
	    catch(xref_source_file(Spec, Path, SourceId, [silent(true)]),
		  _, fail)
	->  (   Why = imported,
	        \+ resolves_anything(TB, Path),
		exports_something(TB, Path)
	    ->	colour_item(file_no_depend(Path), TB, Pos)
	    ;	colour_item(file(Path), TB, Pos)
	    )
	;   colour_item(nofile, TB, Pos)
	).

colourise_file_list([], _, _, _).
colourise_file_list([H|T], TB, [PH|PT], Why) :-
	colourise_files(H, TB, PH, Why),
	colourise_file_list(T, TB, PT, Why).

resolves_anything(TB, Path) :-
	colour_state_source_id(TB, SourceId),
	xref_defined(SourceId, Head, imported(Path)),
	xref_called(SourceId, Head, _), !.

exports_something(TB, Path) :-
	colour_state_source_id(TB, SourceId),
	xref_defined(SourceId, _, imported(Path)), !.

%%	colourise_directory(+Arg, +TB, +Pos)
%
%	Colourise argument that should be an existing directory.

colourise_directory(Spec, TB, Pos) :-
	(   colour_state_source_id(TB, SourceId),
	    catch(xref_source_file(Spec, Path, SourceId,
				   [ file_type(directory),
				     silent(true)
				   ]),
		  _, fail)
	->  colour_item(directory(Path), TB, Pos)
	;   colour_item(nofile, TB, Pos)
	).

%%	colourise_langoptions(+Term, +TB, +Pos) is det.
%
%	Colourise the 3th argument of module/3

colourise_langoptions([], _, _) :- !.
colourise_langoptions([H|T], TB, list_position(PF,PT,[HP|TP],_)) :- !,
	colour_item(list, TB, PF-PT),
	colourise_langoptions(H, TB, HP),
	colourise_langoptions(T, TB, TP).
colourise_langoptions(Spec, TB, Pos) :-
	colourise_files(library(dialect/Spec), TB, Pos, imported).

%%	colourise_class(ClassName, TB, Pos)
%
%	Colourise an XPCE class.

colourise_class(ClassName, TB, Pos) :-
	colour_state_source_id(TB, SourceId),
	classify_class(SourceId, ClassName, Classification),
	colour_item(class(Classification, ClassName), TB, Pos).

%%	classify_class(+SourceId, +ClassName, -Classification).

classify_class(SourceId, Name, Class) :-
	xref_defined_class(SourceId, Name, Class), !.
:- if(current_predicate(classify_class/2)).
classify_class(_, Name, Class) :-
	classify_class(Name, Class).
:- endif.

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
	var(Var), Pos = _-_, !,
	(   singleton(Var, TB)
	->  colour_item(singleton, TB, Pos)
	;   colour_item(var, TB, Pos)
	).
colourise_term_arg(List, TB, list_position(F, T, Elms, Tail)) :- !,
	colour_item(list, TB, F-T),
	colourise_list_args(Elms, Tail, List, TB, classify).	% list
colourise_term_arg(_, TB, string_position(F, T)) :- !,	% string
	colour_item(string, TB, F-T).
colourise_term_arg(_, TB,
		   quasi_quotation_position(F,T,QQType,QQTypePos,CPos)) :- !,
	colourise_qq_type(QQType, TB, QQTypePos),
	functor(QQType, Type, _),
	colour_item(qq_content(Type), TB, CPos),
	arg(1, CPos, SE),
	SS is SE-2,
	FE is F+2,
	TS is T-2,
	colour_item(qq(open),  TB, F-FE),
	colour_item(qq(sep),   TB, SS-SE),
	colour_item(qq(close), TB, TS-T).
colourise_term_arg({Term}, TB, brace_term_position(F,T,Arg)) :- !,
	colour_item(brace_term, TB, F-T),
	colourise_term_arg(Term, TB, Arg).
colourise_term_arg(Compound, TB, Pos) :-		% compound
	compound(Compound), !,
	(   Pos = term_position(_F,_T,FF,FT,_ArgPos)
	->  colour_item(functor, TB, FF-FT)		% TBD: Infix/Postfix?
	;   true					% TBD: When is this
	),
	colourise_term_args(Compound, TB, Pos).
colourise_term_arg(Atom, TB, Pos) :-			% single quoted atom
	atom(Atom), !,
	(   Atom == []
	->  colour_item(empty_list, TB, Pos)
	;   colour_item(atom, TB, Pos)
	).
colourise_term_arg(Integer, TB, Pos) :-
	integer(Integer), !,
	colour_item(int, TB, Pos).
colourise_term_arg(Float, TB, Pos) :-
	integer(Float), !,
	colour_item(float, TB, Pos).
colourise_term_arg(_Arg, _TB, _Pos) :-
	true.

colourise_list_args([HP|TP], Tail, [H|T], TB, How) :-
	specified_item(How, H, TB, HP),
	colourise_list_args(TP, Tail, T, TB, How).
colourise_list_args([], none, _, _, _) :- !.
colourise_list_args([], TP, T, TB, How) :-
	specified_item(How, T, TB, TP).

%%	colourise_qq_type(+QQType, +TB, +QQTypePos)
%
%	Colouring the type part of a quasi quoted term

colourise_qq_type(QQType, TB, QQTypePos) :-
	functor_position(QQTypePos, FPos, _),
	colour_item(qq_type, TB, FPos),
	colourise_term_args(QQType, TB, QQTypePos).

qq_position(quasi_quotation_position(_,_,_,_,_)).


%	colourise_exports(+List, +TB, +Pos)
%
%	Colourise the module export-list (or any other list holding
%	terms of the form Name/Arity referring to predicates).

colourise_exports([], _, _) :- !.
colourise_exports(List, TB, list_position(F,T,ElmPos,Tail)) :- !,
	colour_item(list, TB, F-T),
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
	(   colour_state_source_id(TB, SourceId),
	    ground(File),
	    catch(xref_public_list(File, SourceId,
				   [ path(Path),
				     public(Public),
				     silent(true)
				   ] ), _, fail)
	->  true
	;   Public = [],
	    Path = (-)
	),
	colourise_imports(List, Path, Public, TB, Pos).

colourise_imports([], _, _, _, _).
colourise_imports(List, File, Public, TB, list_position(F,T,ElmPos,Tail)) :- !,
	colour_item(list, TB, F-T),
	(   Tail == none
	->  true
	;   colour_item(type_error(list), TB, Tail)
	),
	colourise_imports2(List, File, Public, TB, ElmPos).
colourise_imports(except(Except), File, Public, TB,
		  term_position(_,_,FF,FT,[LP])) :- !,
	colour_item(keyword(except), TB, FF-FT),
	colourise_imports(Except, File, Public, TB, LP).
colourise_imports(_, _, _, TB, Pos) :-
	colour_item(type_error(list), TB, Pos).

colourise_imports2([G0|GT], File, Public, TB, [P0|PT]) :- !,
	colourise_import(G0, File, TB, P0),
	colourise_imports2(GT, File, Public, TB, PT).
colourise_imports2(_, _, _, _, _).


colourise_import(PI as Name, File, TB, term_position(_,_,FF,FT,[PP,NP])) :-
	pi_to_term(PI, Goal), !,
	colour_item(goal(imported(File), Goal), TB, PP),
	functor(Goal, _, Arity),
	functor(NewGoal, Name, Arity),
	goal_classification(TB, NewGoal, [], Class),
	colour_item(goal(Class, NewGoal), TB, NP),
	colour_item(keyword(as), TB, FF-FT).
colourise_import(PI, File, TB, Pos) :-
	pi_to_term(PI, Goal),
	colour_state_source_id(TB, SourceID),
	(   \+ xref_defined(SourceID, Goal, imported(File))
	->  colour_item(undefined_import, TB, Pos)
	;   \+ xref_called(SourceID, Goal, _)
	->  colour_item(unused_import, TB, Pos)
	), !.
colourise_import(PI, _, TB, Pos) :-
	colourise_declaration(PI, TB, Pos).


%%	colourise_declarations(+Term, +TB, +Pos)
%
%	Colourise the Predicate indicator lists of dynamic, multifile, etc
%	declarations.

colourise_declarations(List, TB, list_position(F,T,Elms,none)) :- !,
	colour_item(list, TB, F-T),
	colourise_list_declarations(List, TB, Elms).
colourise_declarations((Head,Tail), TB,
		       term_position(_,_,_,_,[PH,PT])) :- !,
	colourise_declaration(Head, TB, PH),
	colourise_declarations(Tail, TB, PT).
colourise_declarations(Last, TB, Pos) :-
	colourise_declaration(Last, TB, Pos).

colourise_list_declarations([], _, []).
colourise_list_declarations([H|T], TB, [HP|TP]) :-
	colourise_declaration(H, TB, HP),
	colourise_list_declarations(T, TB, TP).

%%	colourise_declaration(+Decl, +TB, +Pos) is det.
%
%	Colourise declaration sequences as used  by module/2, dynamic/1,
%	etc.

colourise_declaration(PI, TB, term_position(F,T,FF,FT,[NamePos,ArityPos])) :-
	pi_to_term(PI, Goal), !,
	goal_classification(TB, Goal, [], Class),
	colour_item(predicate_indicator(Class, Goal), TB, F-T),
	colour_item(goal(Class, Goal), TB, NamePos),
	colour_item(predicate_indicator, TB, FF-FT),
	colour_item(arity, TB, ArityPos).
colourise_declaration(Module:PI, TB,
		      term_position(_,_,_,_,[PM,PG])) :-
	atom(Module), pi_to_term(PI, Goal), !,
	colour_item(module(M), TB, PM),
	colour_item(predicate_indicator(extern(M), Goal), TB, PG),
	PG = term_position(_,_,FF,FT,[NamePos,ArityPos]),
	colour_item(goal(extern, Goal), TB, NamePos),
	colour_item(predicate_indicator, TB, FF-FT),
	colour_item(arity, TB, ArityPos).
colourise_declaration(op(N,T,P), TB, Pos) :-
	colour_item(exported_operator, TB, Pos),
	colourise_op_declaration(op(N,T,P), TB, Pos).
colourise_declaration(_, TB, Pos) :-
	colour_item(type_error(export_declaration), TB, Pos).

pi_to_term(Name/Arity, Term) :-
	atom(Name), integer(Arity), Arity >= 0, !,
	functor(Term, Name, Arity).
pi_to_term(Name//Arity0, Term) :-
	atom(Name), integer(Arity0), Arity0 >= 0, !,
	Arity is Arity0 + 2,
	functor(Term, Name, Arity).

colourise_meta_declarations((Head,Tail), TB,
			    term_position(_,_,_,_,[PH,PT])) :- !,
	colourise_meta_declaration(Head, TB, PH),
	colourise_meta_declarations(Tail, TB, PT).
colourise_meta_declarations(Last, TB, Pos) :-
	colourise_meta_declaration(Last, TB, Pos).

colourise_meta_declaration(M:Head, TB,
			   term_position(_,_,_,_,
					 [ MP,
					   term_position(_,_,FF,FT,ArgPos)
					 ])) :- !,
	colour_item(module(M), TB, MP),
	colour_item(goal(extern(M,Head)), TB, FF-FT),
	Head =.. [_|Args],
	colourise_meta_args(Args, TB, ArgPos).
colourise_meta_declaration(Head, TB, term_position(_,_,FF,FT,ArgPos)) :- !,
	goal_classification(TB, Head, [], Class),
	colour_item(goal(Class, Head), TB, FF-FT),
	Head =.. [_|Args],
	colourise_meta_args(Args, TB, ArgPos).
colourise_meta_declaration([H|T], TB, list_position(LF,LT,[HP],TP)) :-
	colour_item(list, TB, LF-LT),
	colourise_meta_args([H,T], TB, [HP,TP]).

colourise_meta_args([], _, []).
colourise_meta_args([Arg|ArgT], TB, [PosH|PosT]) :-
	colourise_meta_arg(Arg, TB, PosH),
	colourise_meta_args(ArgT, TB, PosT).

colourise_meta_arg(Arg, TB, Pos) :-
	valid_meta_arg(Arg), !,
	colour_item(meta(Arg), TB, Pos).
colourise_meta_arg(_, TB, Pos) :-
	colour_item(error, TB, Pos).

valid_meta_arg(Var) :-
	var(Var), !, fail.
valid_meta_arg(:).
valid_meta_arg(*).
valid_meta_arg(//).
valid_meta_arg(^).
valid_meta_arg(?).
valid_meta_arg(+).
valid_meta_arg(-).
valid_meta_arg(I) :- integer(I), between(0,9,I).

%%	colourise_op_declaration(Op, TB, Pos) is det.

colourise_op_declaration(op(P,T,N), TB, term_position(_,_,FF,FT,[PP,TP,NP])) :-
	colour_item(goal(built_in, op(N,T,P)), TB, FF-FT),
	colour_op_priority(P, TB, PP),
	colour_op_type(T, TB, TP),
	colour_op_name(N, TB, NP).

colour_op_name(Name, TB, Pos) :-
	var(Name), !,
	colour_item(var, TB, Pos).
colour_op_name(Name, TB, Pos) :-
	atom(Name), !,
	colour_item(identifier, TB, Pos).
colour_op_name(_, TB, Pos) :-
	colour_item(error, TB, Pos).

colour_op_type(Type, TB, Pos) :-
	var(Type), !,
	colour_item(var, TB, Pos).
colour_op_type(Type, TB, Pos) :-
	op_type(Type), !,
	colour_item(op_type(Type), TB, Pos).
colour_op_type(_, TB, Pos) :-
	colour_item(error, TB, Pos).

colour_op_priority(Priority, TB, Pos) :-
	var(Priority), colour_item(var, TB, Pos).
colour_op_priority(Priority, TB, Pos) :-
	integer(Priority),
	between(0, 1200, Priority), !,
	colour_item(int, TB, Pos).
colour_op_priority(_, TB, Pos) :-
	colour_item(error, TB, Pos).

op_type(fx).
op_type(fy).
op_type(xf).
op_type(yf).
op_type(xfy).
op_type(xfx).
op_type(yfx).


%%	colourise_prolog_flag_name(+Name, +TB, +Pos)
%
%	Colourise the name of a Prolog flag

colourise_prolog_flag_name(Name, TB, Pos) :-
	atom(Name), !,
	(   current_prolog_flag(Name, _)
	->  colour_item(flag_name(Name), TB, Pos)
	;   colour_item(no_flag_name(Name), TB, Pos)
	).
colourise_prolog_flag_name(Name, TB, Pos) :-
	colourise_term(Name, TB, Pos).


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

%%	goal_classification(+TB, +Goal, +Origin, -Class)
%
%	Classify Goal appearing in TB and called from a clause with head
%	Origin.  For directives, Origin is [].

goal_classification(_, Goal, _, meta) :-
	var(Goal), !.
goal_classification(_, Goal, _, not_callable) :-
	\+ callable(Goal), !.
goal_classification(_, Goal, Origin, recursion) :-
	functor(Goal, Name, Arity),
	functor(Origin, Name, Arity), !.
goal_classification(TB, Goal, _, How) :-
	colour_state_source_id(TB, SourceId),
	xref_defined(SourceId, Goal, How),
	How \= public(_), !.
goal_classification(_TB, Goal, _, Class) :-
	goal_classification(Goal, Class), !.
goal_classification(TB, Goal, _, How) :-
	colour_state_module(TB, Module),
	atom(Module),
	Module \== prolog_colour_ops,
	predicate_property(Module:Goal, imported_from(From)), !,
	How = imported(From).
goal_classification(_TB, _Goal, _, undefined).

%	goal_classification(+Goal, -Class)
%
%	Multifile hookable classification for non-local goals.

goal_classification(Goal, built_in) :-
	built_in_predicate(Goal), !.
goal_classification(Goal, autoload) :-	% SWI-Prolog
	predicate_property(Goal, autoload(_)).
goal_classification(Goal, global) :-	% SWI-Prolog
	current_predicate(_, user:Goal), !.
goal_classification(SS, expanded) :-	% XPCE (TBD)
	functor(SS, send_super, A),
	A >= 2, !.
goal_classification(SS, expanded) :-	% XPCE (TBD)
	functor(SS, get_super, A),
	A >= 3, !.

classify_head(TB, Goal, exported) :-
	colour_state_source_id(TB, SourceId),
	xref_exported(SourceId, Goal), !.
classify_head(_TB, Goal, hook) :-
	xref_hook(Goal), !.
classify_head(TB, Goal, hook) :-
	colour_state_source_id(TB, SourceId),
	xref_module(SourceId, M),
	xref_hook(M:Goal), !.
classify_head(TB, Goal, Class) :-
	built_in_predicate(Goal),
	(   system_module(TB)
	->  (   predicate_property(system:Goal, iso)
	    ->	Class = def_iso
	    ;	goal_name(Goal, Name),
		\+ sub_atom(Name, 0, _, _, $)
	    ->	Class = def_swi
	    )
	;   (   predicate_property(system:Goal, iso)
	    ->  Class = iso
	    ;   Class = built_in
	    )
	).
classify_head(TB, Goal, unreferenced) :-
	colour_state_source_id(TB, SourceId),
	\+ (xref_called(SourceId, Goal, By), By \= Goal), !.
classify_head(TB, Goal, How) :-
	colour_state_source_id(TB, SourceId),
	(   xref_defined(SourceId, Goal, imported(From))
	->  How = imported(From)
	;   xref_defined(SourceId, Goal, How)
	), !.
classify_head(_TB, _Goal, undefined).

built_in_predicate(Goal) :-
	predicate_property(system:Goal, built_in), !.
built_in_predicate(module(_, _)).	% reserved expanded constructs
built_in_predicate(module(_, _, _)).
built_in_predicate(if(_)).
built_in_predicate(elif(_)).
built_in_predicate(else).
built_in_predicate(endif).

goal_name(_:G, Name) :- nonvar(G), !, goal_name(G, Name).
goal_name(G, Name) :- callable(G), functor(G, Name, _).

system_module(TB) :-
	colour_state_source_id(TB, SourceId),
	xref_module(SourceId, M),
	module_property(M, class(system)).


%	Specify colours for individual goals.

goal_colours(module(_,_),	     built_in-[identifier,exports]).
goal_colours(module(_,_,_),	     built_in-[identifier,exports,langoptions]).
goal_colours(use_module(_),	     built_in-[imported_file]).
goal_colours(use_module(File,_),     built_in-[file,imports(File)]).
goal_colours(reexport(_),	     built_in-[file]).
goal_colours(reexport(File,_),       built_in-[file,imports(File)]).
goal_colours(dynamic(_),	     built_in-[predicates]).
goal_colours(thread_local(_),	     built_in-[predicates]).
goal_colours(module_transparent(_),  built_in-[predicates]).
goal_colours(discontiguous(_),	     built_in-[predicates]).
goal_colours(multifile(_),	     built_in-[predicates]).
goal_colours(volatile(_),	     built_in-[predicates]).
goal_colours(public(_),		     built_in-[predicates]).
goal_colours(meta_predicate(_),	     built_in-[meta_declarations]).
goal_colours(consult(_),	     built_in-[file]).
goal_colours(include(_),	     built_in-[file]).
goal_colours(ensure_loaded(_),	     built_in-[file]).
goal_colours(load_files(_),	     built_in-[file]).
goal_colours(load_files(_,_),	     built_in-[file,classify]).
goal_colours(setof(_,_,_),	     built_in-[classify,setof,classify]).
goal_colours(bagof(_,_,_),	     built_in-[classify,setof,classify]).
goal_colours(predicate_options(_,_,_), built_in-[predicate,classify,classify]).
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
% misc
goal_colours(set_prolog_flag(_,_),   built_in-[prolog_flag_name,classify]).
goal_colours(current_prolog_flag(_,_), built_in-[prolog_flag_name,classify]).
% XPCE stuff
goal_colours(pce_autoload(_,_),	     classify-[classify,file]).
goal_colours(pce_image_directory(_), classify-[directory]).
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

head_colours(Var, _) :-
	var(Var), !,
	fail.
head_colours(M:H, Colours) :-
	M == user,
	head_colours(H, HC),
	HC = hook - _, !,
	Colours = meta-[module(user), HC ].
head_colours(M:H, Colours) :-
	atom(M), callable(H),
	xref_hook(M:H), !,
	Colours = meta-[module(M), hook-classify ].
head_colours(M:_, meta-[module(M),extern(M)]).


		 /*******************************
		 *	       STYLES		*
		 *******************************/

%%	def_style(+Pattern, -Style)
%
%	Define the style used for the   given  pattern. Definitions here
%	can     be     overruled     by       defining     rules     for
%	emacs_prolog_colours:style/2

def_style(goal(built_in,_),	   [colour(blue)]).
def_style(goal(imported(_),_),	   [colour(blue)]).
def_style(goal(autoload,_),	   [colour(navy_blue)]).
def_style(goal(global,_),	   [colour(navy_blue)]).
def_style(goal(undefined,_),	   [colour(red)]).
def_style(goal(thread_local(_),_), [colour(magenta), underline(true)]).
def_style(goal(dynamic(_),_),	   [colour(magenta)]).
def_style(goal(multifile(_),_),	   [colour(navy_blue)]).
def_style(goal(expanded,_),	   [colour(blue), underline(true)]).
def_style(goal(extern(_),_),	   [colour(blue), underline(true)]).
def_style(goal(recursion,_),	   [underline(true)]).
def_style(goal(meta,_),		   [colour(red4)]).
def_style(goal(foreign(_),_),	   [colour(darkturquoise)]).
def_style(goal(local(_),_),	   []).
def_style(goal(constraint(_),_),   [colour(darkcyan)]).
def_style(goal(not_callable,_),	   [background(orange)]).

def_style(option_name,		   [colour('#3434ba')]).
def_style(no_option_name,	   [colour(red)]).

def_style(head(exported,_),	   [colour(blue), bold(true)]).
def_style(head(public(_),_),	   [colour('#016300'), bold(true)]).
def_style(head(extern(_),_),	   [colour(blue), bold(true)]).
def_style(head(dynamic,_),	   [colour(magenta), bold(true)]).
def_style(head(multifile,_),	   [colour(navy_blue), bold(true)]).
def_style(head(unreferenced,_),	   [colour(red), bold(true)]).
def_style(head(hook,_),		   [colour(blue), underline(true)]).
def_style(head(meta,_),		   []).
def_style(head(constraint(_),_),   [colour(darkcyan), bold(true)]).
def_style(head(imported(_),_),	   [colour(darkgoldenrod4), bold(true)]).
def_style(head(built_in,_),	   [background(orange), bold(true)]).
def_style(head(iso,_),		   [background(orange), bold(true)]).
def_style(head(def_iso,_),	   [colour(blue), bold(true)]).
def_style(head(def_swi,_),	   [colour(blue), bold(true)]).
def_style(head(_,_),		   [bold(true)]).

def_style(module(_),		   [colour(dark_slate_blue)]).
def_style(comment,		   [colour(dark_green)]).

def_style(directive,		   [background(grey90)]).
def_style(method(_),		   [bold(true)]).

def_style(var,			   [colour(red4)]).
def_style(singleton,		   [bold(true), colour(red4)]).
def_style(unbound,		   [colour(red), bold(true)]).
def_style(quoted_atom,		   [colour(navy_blue)]).
def_style(string,		   [colour(navy_blue)]).
def_style(nofile,		   [colour(red)]).
def_style(file(_),		   [colour(blue), underline(true)]).
def_style(file_no_depend(_),	   [colour(blue), underline(true), background(pink)]).
def_style(directory(_),		   [colour(blue)]).
def_style(class(built_in,_),	   [colour(blue), underline(true)]).
def_style(class(library(_),_),	   [colour(navy_blue), underline(true)]).
def_style(class(local(_,_,_),_),   [underline(true)]).
def_style(class(user(_),_),	   [underline(true)]).
def_style(class(user,_),	   [underline(true)]).
def_style(class(undefined,_),	   [colour(red), underline(true)]).
def_style(prolog_data,		   [colour(blue), underline(true)]).
def_style(flag_name(_),		   [colour(blue)]).
def_style(no_flag_name(_),	   [colour(red)]).
def_style(unused_import,	   [colour(blue), background(pink)]).
def_style(undefined_import,	   [colour(red)]).

def_style(keyword(_),		   [colour(blue)]).
def_style(identifier,		   [bold(true)]).
def_style(delimiter,		   [bold(true)]).
def_style(expanded,		   [colour(blue), underline(true)]).
def_style(op_type(_),		   [colour(blue)]).

def_style(qq_type,		   [bold(true)]).
def_style(qq(_),		   [colour(blue), bold(true)]).
def_style(qq_content(_),	   [colour(red4)]).

def_style(hook,			   [colour(blue), underline(true)]).
def_style(dcg_right_hand_ctx,	   [background('#d4ffe3')]).

def_style(error,		   [background(orange)]).
def_style(type_error(_),	   [background(orange)]).
def_style(syntax_error(_,_),	   [background(orange)]).

%%	syntax_colour(?Class, ?Attributes) is nondet.
%
%	True when a range  classified  Class   must  be  coloured  using
%	Attributes.  Attributes is a list of:
%
%	  * colour(ColourName)
%	  * background(ColourName)
%	  * bold(Boolean)
%	  * underline(Boolean)
%
%	Attributes may be the empty list. This   is used for cases where
%	-for example- a  menu  is  associated   with  the  fragment.  If
%	syntax_colour/2 fails, no fragment is created for the region.

syntax_colour(Class, Attributes) :-
	(   style(Class, Attributes)		% user hook
	;   def_style(Class, Attributes)	% system default
	).


%%	term_colours(+Term, -FunctorColour, -ArgColours)
%
%	Define colourisation for specific terms.

term_colours((?- Directive), Colours) :-
	term_colours((:- Directive), Colours).
term_colours((prolog:Head --> _),
	     expanded - [ expanded - [ expanded,
				       expanded - [ identifier
						  ]
				     ],
			  dcg_body(prolog:Head)
			]) :-
	prolog_message_hook(Head).

prolog_message_hook(message(_)).
prolog_message_hook(error_message(_)).
prolog_message_hook(message_context(_)).
prolog_message_hook(message_location(_)).

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
term_colours((:- pce_end_class(_)),
	     expanded - [ expanded - [ identifier
				     ]
			]).
term_colours((:- use_class_template(_)),
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
	(   var(Var)
	;   qq_position(Pos)
	), !,
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
specified_item(dcg_body(Head), Term, TB, Pos) :- !,
	colourise_dcg(Term, Head, TB, Pos).
specified_item(setof, Term, TB, Pos) :- !,
	colourise_setof(Term, TB, Pos).
specified_item(meta(MetaSpec), Term, TB, Pos) :- !,
	colourise_meta_arg(MetaSpec, Term, TB, Pos).
					% DCG goal in body
specified_item(dcg, Term, TB, Pos) :- !,
	colourise_dcg(Term, [], TB, Pos).
					% assert/retract arguments
specified_item(db, Term, TB, Pos) :- !,
	colourise_db(Term, TB, Pos).
					% files
specified_item(file, Term, TB, Pos) :- !,
	colourise_files(Term, TB, Pos, any).
specified_item(imported_file, Term, TB, Pos) :- !,
	colourise_files(Term, TB, Pos, imported).
specified_item(langoptions, Term, TB, Pos) :- !,
	colourise_langoptions(Term, TB, Pos).

					% directory
specified_item(directory, Term, TB, Pos) :- !,
	colourise_directory(Term, TB, Pos).
					% [Name/Arity, ...]
specified_item(exports, Term, TB, Pos) :- !,
	colourise_exports(Term, TB, Pos).
					% [Name/Arity, ...]
specified_item(imports(File), Term, TB, Pos) :- !,
	colourise_imports(Term, File, TB, Pos).
					% Name/Arity, ...
specified_item(predicates, Term, TB, Pos) :- !,
	colourise_declarations(Term, TB, Pos).
					% Name/Arity
specified_item(predicate, Term, TB, Pos) :- !,
	colourise_declaration(Term, TB, Pos).
					% head(Arg, ...)
specified_item(meta_declarations, Term, TB, Pos) :- !,
	colourise_meta_declarations(Term, TB, Pos).
					% set_prolog_flag(Name, _)
specified_item(prolog_flag_name, Term, TB, Pos) :- !,
	colourise_prolog_flag_name(Term, TB, Pos).
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
specified_item(pce_arg, @(Ref), TB, Pos) :- !,
	colourise_term_arg(@(Ref), TB, Pos).
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
specified_item(pce_arg_list, List, TB, list_position(F,T,Elms,Tail)) :- !,
	colour_item(list, TB, F-T),
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
	colour_item(list, TB, F-T),
	FT is F + 1,
	AT is T - 1,
	colour_item(FuncSpec, TB, F-FT),
	colour_item(FuncSpec, TB, AT-T),
	specified_list(ElmSpec, List, TB, ElmPos, TailPos).
specified_item(Class, _, TB, Pos) :-
	colour_item(Class, TB, Pos).

%	specified_items(+Spec, +T, +TB, +PosList)

specified_items(Specs, Term, TB, PosList) :-
	is_list(Specs), !,
	specified_arglist(Specs, 1, Term, TB, PosList).
specified_items(Spec, Term, TB, PosList) :-
	specified_argspec(PosList, Spec, 1, Term, TB).


specified_arglist([], _, _, _, _).
specified_arglist(_, _, _, _, []) :- !.		% Excess specification args
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


%	specified_list(+Spec, +List, +TB, +PosList, TailPos)

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


		 /*******************************
		 *	   DESCRIPTIONS		*
		 *******************************/

syntax_message(Class) -->
	message(Class), !.
syntax_message(qq) -->
	[ 'Quasi quote delimiter' ].
syntax_message(qq_type) -->
	[ 'Quasi quote type term' ].
syntax_message(qq_content(Type)) -->
	[ 'Quasi quote content (~w syntax)'-[Type] ].
syntax_message(goal(Class, Goal)) --> !,
	goal_message(Class, Goal).
syntax_message(class(Type, Class)) --> !,
	xpce_class_message(Type, Class).

goal_message(meta, _) -->
	[ 'Meta call' ].
goal_message(recursion, _) -->
	[ 'Recursive call' ].
goal_message(not_callable, _) -->
	[ 'Goal is not callable (type error)' ].
goal_message(undefined, _) -->
	[ 'Call to undefined predicate' ].
goal_message(expanded, _) -->
	[ 'Expanded goal' ].
goal_message(global, _) -->
	[ 'Auto-imported from module user' ].
goal_message(Class, Goal) -->
	{ predicate_name(Goal, PI) },
	[ 'Call to ~w predicate ~q'-[Class,PI] ].

xpce_class_message(Type, Class) -->
	[ 'XPCE ~w class ~q'-[Type, Class] ].
