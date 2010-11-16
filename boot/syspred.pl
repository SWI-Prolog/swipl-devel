/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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

:- module('$syspreds',
	  [ leash/1,
	    visible/1,
	    style_check/1,
	    (spy)/1,
	    (nospy)/1,
	    trace/1,
	    trace/2,
	    nospyall/0,
	    debugging/0,
	    rational/3,
	    atom_prefix/2,
	    dwim_match/2,
	    source_file/1,
	    prolog_load_context/2,
	    stream_position_data/3,
	    current_predicate/2,
	    '$defined_predicate'/1,
	    predicate_property/2,
	    '$predicate_property'/2,
	    clause_property/2,
	    recorda/2,
	    recordz/2,
	    current_module/1,
	    module_property/2,
	    module/1,
	    statistics/0,
	    shell/1,
	    shell/0,
	    on_signal/3,
	    current_signal/3,
	    open_shared_object/2,
	    open_shared_object/3,
	    format/1,
	    garbage_collect/0,
	    set_prolog_stack/2,
	    prolog_stack_property/2,
	    arithmetic_function/1,
	    absolute_file_name/2,
	    require/1,
	    call_with_depth_limit/3,
	    length/2,			% ?List, ?Length
	    numbervars/3,		% +Term, +Start, -End
	    nb_setval/2			% +Var, +Value
	  ]).

		/********************************
		*           DEBUGGER            *
		*********************************/

:- meta_predicate
	'$map_bits'(2, +, +, -).

'$map_bits'(_, [], Bits, Bits) :- !.
'$map_bits'(Pred, [H|T], Old, New) :-
	'$map_bits'(Pred, H, Old, New0),
	'$map_bits'(Pred, T, New0, New).
'$map_bits'(Pred, +Name, Old, New) :- !,	% set a bit
	call(Pred, Name, Bits), !,
	New is Old \/ Bits.
'$map_bits'(Pred, -Name, Old, New) :- !, 	% clear a bit
	call(Pred, Name, Bits), !,
	New is Old /\ (\Bits).
'$map_bits'(Pred, ?(Name), Old, Old) :-		% ask a bit
	call(Pred, Name, Bits),
	Old /\ Bits > 0.

'$port_bit'(      call, 2'000000001).
'$port_bit'(      exit, 2'000000010).
'$port_bit'(      fail, 2'000000100).
'$port_bit'(      redo, 2'000001000).
'$port_bit'(     unify, 2'000010000).
'$port_bit'(     break, 2'000100000).
'$port_bit'(  cut_call, 2'001000000).
'$port_bit'(  cut_exit, 2'010000000).
'$port_bit'( exception, 2'100000000).
'$port_bit'(       cut, 2'011000000).
'$port_bit'(       all, 2'000111111).
'$port_bit'(      full, 2'000101111).
'$port_bit'(      half, 2'000101101).	% '

leash(Ports) :-
	'$leash'(Old, Old),
	'$map_bits'('$port_bit', Ports, Old, New),
	'$leash'(_, New).

visible(Ports) :-
	'$visible'(Old, Old),
	'$map_bits'('$port_bit', Ports, Old, New),
	'$visible'(_, New).

'$map_style_check'(atom,     	    2'0000001).
'$map_style_check'(singleton, 	    2'0000010).
'$map_style_check'(dollar,   	    2'0000100).
'$map_style_check'((discontiguous),   2'0001000).
'$map_style_check'(dynamic,	    2'0010000).
'$map_style_check'(charset,	    2'0100000).

style_check(+string) :- !,
	set_prolog_flag(double_quotes, string).
style_check(-string) :- !,
	set_prolog_flag(double_quotes, codes).
style_check(?(string)) :- !,
	current_prolog_flag(double_quotes, string).
style_check(Spec) :-
	'$style_check'(Old, Old),
	'$map_bits'('$map_style_check', Spec, Old, New),
	'$style_check'(_, New).

%	prolog:debug_control_hook(+Action)
%
%	Allow user-hooks in the Prolog debugger interaction.  See the calls
%	below for the provided hooks.  We use a single predicate with action
%	argument to avoid an uncontrolled poliferation of hooks.
%
%	TBD: What hooks to provide for trace/[1,2]

:- multifile
	prolog:debug_control_hook/1.	% +Action

%%	trace(:Preds) is det.
%%	trace(:Preds, +PortSpec) is det.
%
%	Start printing messages if control passes specified ports of
%	the given predicates.

:- meta_predicate
	trace(:),
	trace(:, +).

trace(Preds) :-
	trace(Preds, +all).

trace(_:X, _) :-
	var(X), !,
	throw(error(instantiation_error, _)).
trace(_:[], _) :- !.
trace(M:[H|T], Ps) :- !,
	trace(M:H, Ps),
	trace(M:T, Ps).
trace(Pred, Ports) :-
	'$find_predicate'(Pred, Preds),
	Preds \== [],
	set_prolog_flag(debug, true),
	(   '$member'(PI, Preds),
	        pi_to_head(PI, Head),
	        (   Head = _:_
		->  QHead0 = Head
		;   QHead0 = user:Head
		),
		'$define_predicate'(QHead0),
	        (   predicate_property(QHead0, imported_from(M))
		->  QHead0 = _:Plain,
		    QHead = M:Plain
		;   QHead = QHead0
		),
	        '$trace'(Ports, QHead),
	        trace_ports(QHead, Tracing),
	        print_message(informational, trace(QHead, Tracing)),
	    fail
	;   true
	).

trace_alias(all,  [trace_call, trace_redo, trace_exit, trace_fail]).
trace_alias(call, [trace_call]).
trace_alias(redo, [trace_redo]).
trace_alias(exit, [trace_exit]).
trace_alias(fail, [trace_fail]).

'$trace'([], _) :- !.
'$trace'([H|T], Head) :- !,
	'$trace'(H, Head),
	'$trace'(T, Head).
'$trace'(+H, Head) :-
	trace_alias(H, A0), !,
	tag_list(A0, +, A1),
	'$trace'(A1, Head).
'$trace'(+H, Head) :- !,
	trace_alias(_, [H]),
	'$set_predicate_attribute'(Head, H, 1).
'$trace'(-H, Head) :-
	trace_alias(H, A0), !,
	tag_list(A0, -, A1),
	'$trace'(A1, Head).
'$trace'(-H, Head) :- !,
	trace_alias(_, [H]),
	'$set_predicate_attribute'(Head, H, 0).
'$trace'(H, Head) :-
	atom(H),
	'$trace'(+H, Head).

tag_list([], _, []).
tag_list([H0|T0], F, [H1|T1]) :-
	H1 =.. [F, H0],
	tag_list(T0, F, T1).

:- meta_predicate
	spy(:),
	nospy(:).

%%	spy(:Spec) is det.
%%	nospy(:Spec) is det.
%%	nospyall is det.
%
%	Set/clear spy-points.

spy(_:X) :-
	var(X),
	throw(error(instantiation_error, _)).
spy(_:[]) :- !.
spy(M:[H|T]) :- !,
	spy(M:H),
	spy(M:T).
spy(Spec) :-
	prolog:debug_control_hook(spy(Spec)), !.
spy(Spec) :-
	'$find_predicate'(Spec, Preds),
	'$member'(PI, Preds),
	    pi_to_head(PI, Head),
	    '$define_predicate'(Head),
	    '$spy'(Head),
	fail.
spy(_).

nospy(_:X) :-
	var(X),
	throw(error(instantiation_error, _)).
nospy(_:[]) :- !.
nospy(M:[H|T]) :- !,
	nospy(M:H),
	nospy(M:T).
nospy(Spec) :-
	prolog:debug_control_hook(nospy(Spec)), !.
nospy(Spec) :-
	'$find_predicate'(Spec, Preds),
	'$member'(PI, Preds),
	     pi_to_head(PI, Head),
	    '$nospy'(Head),
	fail.
nospy(_).

nospyall :-
	prolog:debug_control_hook(nospyall),
	fail.
nospyall :-
	spy_point(Head),
	    '$nospy'(Head),
	fail.
nospyall.

pi_to_head(M:PI, M:Head) :- !,
	pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
	functor(Head, Name, Arity).

%%	debugging is det.
%
%	Report current status of the debugger.

debugging :-
	prolog:debug_control_hook(debugging), !.
debugging :-
	current_prolog_flag(debug, true), !,
	print_message(informational, debugging(on)),
	findall(H, spy_point(H), SpyPoints),
	print_message(informational, spying(SpyPoints)),
	findall(trace(H,P), trace_point(H,P), TracePoints),
	print_message(informational, tracing(TracePoints)).
debugging :-
	print_message(informational, debugging(off)).

spy_point(Module:Head) :-
	current_predicate(_, Module:Head),
	'$get_predicate_attribute'(Module:Head, spy, 1),
	\+ predicate_property(Module:Head, imported_from(_)).

trace_point(Module:Head, Ports) :-
	current_predicate(_, Module:Head),
	    '$get_predicate_attribute'(Module:Head, trace_any, 1),
	    \+ predicate_property(Module:Head, imported_from(_)),
	    trace_ports(Module:Head, Ports).

trace_ports(Head, Ports) :-
	findall(Port,
		(trace_alias(Port, [AttName]),
		 '$get_predicate_attribute'(Head, AttName, 1)),
		Ports).


		 /*******************************
		 *	      RATIONAL		*
		 *******************************/

%	rational(+Rat, -M, -N)
%
%	Get parts of a rational number.

rational(Rat, M, N) :-
	rational(Rat),
	(   Rat = rdiv(M, N)
	->  true
	;   integer(Rat)
	->  M = Rat,
	    N = 1
	).


		/********************************
		*             ATOMS             *
		*********************************/

dwim_match(A1, A2) :-
	dwim_match(A1, A2, _).

atom_prefix(Atom, Prefix) :-
	sub_atom(Atom, 0, _, _, Prefix).


		/********************************
		*             SOURCE            *
		*********************************/

%%	source_file(-File) is nondet.
%%	source_file(+File) is semidet.
%
%	True if File is loaded into  Prolog.   If  File is unbound it is
%	bound to the canonical name for it. If File is bound it succeeds
%	if the canonical name  as   defined  by  absolute_file_name/2 is
%	known as a loaded filename.
%
%	Note that Time = 0.0 is used by  PlDoc and other code that needs
%	to create a file record without being interested in the time.

source_file(File) :-
	(   ground(File)
	->  ( 	'$time_source_file'(File, Time, user)
	    ;	absolute_file_name(File, Abs),
		'$time_source_file'(Abs, Time, user)
	    ), !
	;   '$time_source_file'(File, Time, user)
	),
	Time > 0.0.

%%	prolog_load_context(+Key, -Value)
%
%	Provides context information for  term_expansion and directives.
%	Note  that  only  the  line-number  info    is   valid  for  the
%	'$stream_position'. Largely Quintus compatible.

prolog_load_context(module, Module) :-
	'$set_source_module'(Module, Module).
prolog_load_context(file, F) :-
	source_location(F, _).
prolog_load_context(source, F) :-	% SICStus compatibility
	source_location(F, _).
prolog_load_context(stream, S) :-
	source_location(F, _),
	(   system:'$load_input'(F, S0)
	->  S = S0
	).
prolog_load_context(directory, D) :-
	source_location(F, _),
	file_directory_name(F, D).
prolog_load_context(dialect, D) :-
	current_prolog_flag(emulated_dialect, D).
prolog_load_context(term_position, '$stream_position'(0,L,0,0,0)) :-
	source_location(_, L).
prolog_load_context(script, Bool) :-
	(   '$toplevel':loaded_init_file(script, Path),
	    source_location(Path, _)
	->  Bool = true
	;   Bool = false
	).


		 /*******************************
		 *	      STREAMS		*
		 *******************************/

%%	stream_position_data(?Field, +Pos, ?Date)
%
%	Extract values from stream position objects. '$stream_position' is
%	of the format '$stream_position'(Byte, Char, Line, LinePos)

stream_position_data(Prop, Term, Value) :-
	nonvar(Prop), !,
	(   stream_position_field(Prop, Pos)
	->  arg(Pos, Term, Value)
	;   throw(error(domain_error(stream_position_data, Prop)))
	).
stream_position_data(Prop, Term, Value) :-
	stream_position_field(Prop, Pos),
	arg(Pos, Term, Value).

stream_position_field(char_count,    1).
stream_position_field(line_count,    2).
stream_position_field(line_position, 3).
stream_position_field(byte_count,    4).


		 /*******************************
		 *	      CONTROL		*
		 *******************************/

%%	call_with_depth_limit(:Goal, +DepthLimit, -Result)
%
%	Try to proof Goal, but fail on any branch exceeding the indicated
%	depth-limit.  Unify Result with the maximum-reached limit on success,
%	depth_limit_exceeded if the limit was exceeded and fails otherwise.

:- meta_predicate
	call_with_depth_limit(0, +, -).

call_with_depth_limit(G, Limit, Result) :-
	'$depth_limit'(Limit, OLimit, OReached),
	(   catch(G, E, '$depth_limit_except'(OLimit, OReached, E)),
	    '$depth_limit_true'(Limit, OLimit, OReached, Result, Det),
	    ( Det == ! -> ! ; true )
	;   '$depth_limit_false'(OLimit, OReached, Result)
	).


		/********************************
		*           DATA BASE           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The predicate current_predicate/2 is   a  difficult subject since  the
introduction  of defaulting     modules   and   dynamic     libraries.
current_predicate/2 is normally  called with instantiated arguments to
verify some  predicate can   be called without trapping   an undefined
predicate.  In this case we must  perform the search algorithm used by
the prolog system itself.

If the pattern is not fully specified, we only generate the predicates
actually available in this  module.   This seems the best for listing,
etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- meta_predicate
	current_predicate(?, :),
	'$defined_predicate'(:).

current_predicate(Name, Module:Head) :-
	(var(Module) ; var(Head)), !,
	generate_current_predicate(Name, Module, Head).
current_predicate(Name, Term) :-
	'$c_current_predicate'(Name, Term),
	'$defined_predicate'(Term), !.
current_predicate(Name, Module:Head) :-
	default_module(Module, DefModule),
	'$c_current_predicate'(Name, DefModule:Head),
	'$defined_predicate'(DefModule:Head), !.
current_predicate(Name, Module:Head) :-
	current_prolog_flag(autoload, true),
	\+ current_prolog_flag(Module:unknown, fail),
	functor(Head, Name, Arity),
	'$find_library'(Module, Name, Arity, _LoadModule, _Library), !.

generate_current_predicate(Name, Module, Head) :-
	current_module(Module),
	QHead = Module:Head,
	'$c_current_predicate'(Name, QHead),
	'$get_predicate_attribute'(QHead, defined, 1).

'$defined_predicate'(Head) :-
	'$get_predicate_attribute'(Head, defined, 1), !.

:- meta_predicate
	predicate_property(:, ?).

:- '$iso'(predicate_property/2).

predicate_property(Pred, Property) :-
	Property == undefined, !,
	Pred = Module:Head,
	current_module(Module),
	'$c_current_predicate'(_, Pred),
	\+ '$defined_predicate'(Pred),		% Speed up a bit
	\+ current_predicate(_, Pred),
	functor(Head, Name, Arity),
	\+ system_undefined(Module:Name/Arity).
predicate_property(_:Head, Property) :-
	nonvar(Property),
	Property = autoload(File), !,
	current_prolog_flag(autoload, true),
	(   callable(Head)
	->  functor(Head, Name, Arity),
	    (	'$find_library'(_, Name, Arity, _, File)
	    ->	true
	    )
	;   '$find_library'(_, Name, Arity, _, File),
	    functor(Head, Name, Arity)
	).
predicate_property(Pred, Property) :-
	Pred = M:_,
	M == system, !,				% do not autoload into system
	'$c_current_predicate'(_, Pred),
	'$defined_predicate'(Pred),
	'$predicate_property'(Property, Pred).
predicate_property(Pred, Property) :-
	current_predicate(_, Pred),
	'$define_predicate'(Pred),		% autoload if needed
	'$predicate_property'(Property, Pred).

'$predicate_property'(interpreted, Pred) :-
	'$get_predicate_attribute'(Pred, foreign, 0).
'$predicate_property'(built_in, Pred) :-
	'$get_predicate_attribute'(Pred, system, 1).
'$predicate_property'(exported, Pred) :-
	'$get_predicate_attribute'(Pred, exported, 1).
'$predicate_property'(public, Pred) :-
	'$get_predicate_attribute'(Pred, public, 1).
'$predicate_property'(foreign, Pred) :-
	'$get_predicate_attribute'(Pred, foreign, 1).
'$predicate_property'((dynamic), Pred) :-
	'$get_predicate_attribute'(Pred, (dynamic), 1).
'$predicate_property'((volatile), Pred) :-
	'$get_predicate_attribute'(Pred, (volatile), 1).
'$predicate_property'((thread_local), Pred) :-
	'$get_predicate_attribute'(Pred, (thread_local), 1).
'$predicate_property'((multifile), Pred) :-
	'$get_predicate_attribute'(Pred, (multifile), 1).
'$predicate_property'(imported_from(Module), Pred) :-
	'$get_predicate_attribute'(Pred, imported, Module).
'$predicate_property'(transparent, Pred) :-
	'$get_predicate_attribute'(Pred, transparent, 1).
'$predicate_property'(indexed(Pattern), Pred) :-
	'$get_predicate_attribute'(Pred, indexed, Pattern).
'$predicate_property'(meta_predicate(Pattern), Pred) :-
	'$get_predicate_attribute'(Pred, meta_predicate, Pattern).
'$predicate_property'(file(File), Pred) :-
	source_file(Pred, File).
'$predicate_property'(line_count(LineNumber), Pred) :-
	'$get_predicate_attribute'(Pred, line_count, LineNumber).
'$predicate_property'(notrace, Pred) :-
	'$get_predicate_attribute'(Pred, trace, 0).
'$predicate_property'(nodebug, Pred) :-
	'$get_predicate_attribute'(Pred, hide_childs, 1).
'$predicate_property'(spying, Pred) :-
	'$get_predicate_attribute'(Pred, spy, 1).
'$predicate_property'(hashed(N), Pred) :-
	'$get_predicate_attribute'(Pred, hashed, N),
	N > 0.
'$predicate_property'(references(N), Pred) :-
	'$get_predicate_attribute'(Pred, references, N),
	N \== 0.			% show negative for debugging!
'$predicate_property'(number_of_clauses(N), Pred) :-
	'$get_predicate_attribute'(Pred, number_of_clauses, N).
'$predicate_property'(noprofile, Pred) :-
	'$get_predicate_attribute'(Pred, noprofile, 1).
'$predicate_property'(iso, Pred) :-
	'$get_predicate_attribute'(Pred, iso, 1).

system_undefined(user:prolog_trace_interception/4).
system_undefined(user:prolog_exception_hook/4).
system_undefined(system:'$c_call_prolog'/0).
system_undefined(system:window_title/2).

%%	clause_property(+ClauseRef, ?Property) is nondet.
%
%	Provide information on individual clauses.  Defined properties
%	are:
%
%	    * line_count(-Line)
%	    Line from which the clause is loaded.
%	    * file(-File)
%	    File from which the clause is loaded.
%	    * fact
%	    Clause has body =true=.
%	    * erased
%	    Clause was erased.

clause_property(Clause, Property) :-
	'$clause_property'(Property, Clause).

'$clause_property'(line_count(LineNumber), Clause) :-
	'$get_clause_attribute'(Clause, line_count, LineNumber).
'$clause_property'(file(File), Clause) :-
	'$get_clause_attribute'(Clause, file, File).
'$clause_property'(fact, Clause) :-
	'$get_clause_attribute'(Clause, fact, true).
'$clause_property'(erased, Clause) :-
	'$get_clause_attribute'(Clause, erased, true).


		 /*******************************
		 *	       REQUIRE		*
		 *******************************/

:- meta_predicate
	require(:).

%%	require(:ListOfPredIndicators) is det.
%
%	Tag given predicates as undefined, so they will be included
%	into a saved state through the autoloader.
%
%	@see autoload/0.

require(M:List) :-
	(   is_list(List)
	->  require(List, M)
	;   throw(error(type_error(list, List), _))
	).

require([], _).
require([N/A|T], M) :- !,
	functor(Head, N, A),
	'$require'(M:Head),
	require(T, M).
require([H|_T], _) :-
	throw(error(type_error(predicate_indicator, H), _)).


		/********************************
		*            MODULES            *
		*********************************/

%%	current_module(?Module) is nondet.
%
%	True if Module is a currently defined module.

current_module(Module) :-
	'$current_module'(Module, _).

%%	module_property(?Module, ?Property) is nondet.
%
%	True if Property is a property of Module.  Defined properties
%	are:
%
%	    * file(File)
%	    Module is loaded from File.
%	    * line_count(Count)
%	    The module declaration is on line Count of File.
%	    * exports(ListOfPredicateIndicators)
%	    The module exports ListOfPredicateIndicators

module_property(Module, Property) :-
	nonvar(Module), nonvar(Property), !,
	'$module_property'(Module, Property).
module_property(Module, Property) :-
	nonvar(Property), Property = file(File), !,
	'$current_module'(Module, File),
	File \== [].
module_property(Module, Property) :-
	current_module(Module),
	module_property(Property),
	'$module_property'(Module, Property).

module_property(file(_)).
module_property(line_count(_)).
module_property(exports(_)).

%%	module(+Module) is det.
%
%	Set the module that is associated to the toplevel to Module.

module(Module) :-
	atom(Module),
	current_module(Module), !,
	'$module'(_, Module).
module(Module) :-
	'$module'(_, Module),
	print_message(warning, no_current_module(Module)).

		/********************************
		*          STATISTICS           *
		*********************************/

statistics :-
	statistics(user_error).

statistics(Out) :-
	statistics(trail, Trail),
	statistics(trailused, TrailUsed),
	statistics(local, Local),
	statistics(localused, LocalUsed),
	statistics(global, Global),
	statistics(globalused, GlobalUsed),
	statistics(cputime, Cputime),
	statistics(inferences, Inferences),
	statistics(heapused, Heapused),
	statistics(atoms, Atoms),
	statistics(functors, Functors),
	statistics(predicates, Predicates),
	statistics(modules, Modules),
	statistics(codes, Codes),
	statistics(locallimit, LocalLimit),
	statistics(globallimit, GlobalLimit),
	statistics(traillimit, TrailLimit),

	format(Out, '~2f seconds cpu time for ~D inferences~n',
				    [Cputime, Inferences]),
	format(Out, '~D atoms, ~D functors, ~D predicates, ~D modules, ~D VM-codes~n~n',
				    [Atoms, Functors, Predicates, Modules, Codes]),
	format(Out, '                       Limit    Allocated       In use~n', []),
	(   statistics(heap, Heap),
	    statistics(heaplimit, HeapLimit)
	->  format(Out, 'Heap         :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n',
		   [HeapLimit, Heap, Heapused])
	;   format(Out, 'Heap         :                  ~t~D~54| Bytes~n',
		   [Heapused])
	),
	format(Out, 'Local  stack :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n',
	       [LocalLimit, Local, LocalUsed]),
	format(Out, 'Global stack :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n',
	       [GlobalLimit, Global, GlobalUsed]),
	format(Out, 'Trail  stack :~t~D~28| ~t~D~41| ~t~D~54| Bytes~n~n',
	       [TrailLimit, Trail, TrailUsed]),

	gc_statistics(Out),
	agc_statistics(Out),
	shift_statistics(Out),
	thread_statistics(Out).

gc_statistics(Out) :-
	statistics(collections, Collections),
	Collections > 0, !,
	statistics(collected, Collected),
	statistics(gctime, GcTime),

	format(Out, '~D garbage collections gained ~D bytes in ~2f seconds.~n',
	       [Collections, Collected, GcTime]).
gc_statistics(_).

agc_statistics(Out) :-
	catch(statistics(agc, Agc), _, fail),
	Agc > 0, !,
	statistics(agc_gained, Gained),
	statistics(agc_time, Time),
	format(Out, '~D atom garbage collections gained ~D atoms in ~2f seconds.~n',
	       [Agc, Gained, Time]).
agc_statistics(_).

shift_statistics(Out) :-
	statistics(local_shifts, LS),
	statistics(global_shifts, GS),
	statistics(trail_shifts, TS),
	(   LS > 0
	;   GS > 0
	;   TS > 0
	), !,
	format(Out, 'Stack shifts: ~D local, ~D global, ~D trail.~n',
	       [LS, GS, TS]).
shift_statistics(_).

thread_statistics(Out) :-
	current_prolog_flag(threads, true), !,
	statistics(threads, Active),
	statistics(threads_created, Created),
	statistics(thread_cputime, CpuTime),
	Finished is Created - Active,
	format(Out, '~D threads, ~D finished threads used ~2f seconds.~n',
	       [Active, Finished, CpuTime]).
thread_statistics(_).


		/********************************
		*      SYSTEM INTERACTION       *
		*********************************/

shell(Command) :-
	shell(Command, 0).

shell :-
	getenv('SHELL', Shell), !,	% Unix, also Cygwin
	shell(Shell).
shell :-
	getenv(comspec, ComSpec), !,	% Windows
	shell(ComSpec).
shell :-
	shell('/bin/sh').

		 /*******************************
		 *	      SIGNALS		*
		 *******************************/

:- meta_predicate
	on_signal(+, :, :),
	current_signal(?, ?, :).

on_signal(Signal, Old, New) :-
	atom(Signal), !,
	'$on_signal'(_Num, Signal, Old, New).
on_signal(Signal, Old, New) :-
	integer(Signal), !,
	'$on_signal'(Signal, _Name, Old, New).
on_signal(Signal, _Old, _New) :-
	(   var(Signal)
	->  Err = instantiation_error
	;   Err = type_error(signal, Signal)
	),
	throw(error(Err, context(on_signal/3, _))).

current_signal(Name, Id, Handler) :-
	between(1, 32, Id),
	'$on_signal'(Id, Name, Handler, Handler).


		 /*******************************
		 *	      DLOPEN		*
		 *******************************/

%%	open_shared_object(+File, -Handle) is det.
%%	open_shared_object(+File, -Handle, +Flags) is det.
%
%	Open a shared object or DLL file. Flags  is a list of flags. The
%	following flags are recognised. Note   however  that these flags
%	may have no affect on the target platform.
%
%	    * =now=
%	    Resolve all symbols in the file now instead of lazily.
%	    * =global=
%	    Make new symbols globally known.

open_shared_object(File, Handle) :-
	open_shared_object(File, Handle, []). % use pl-load.c defaults

open_shared_object(File, Handle, Flags) :-
	(   is_list(Flags)
	->  true
	;   throw(error(type_error(list, Flags), _))
	),
	map_dlflags(Flags, Mask),
	'$open_shared_object'(File, Handle, Mask).

dlopen_flag(now,	2'01).		% see pl-load.c for these constants
dlopen_flag(global,	2'10).		% Solaris only

map_dlflags([], 0).
map_dlflags([F|T], M) :-
	map_dlflags(T, M0),
	(   dlopen_flag(F, I)
	->  true
	;   throw(error(domain_error(dlopen_flag, F), _))
	),
	M is M0 \/ I.


		 /*******************************
		 *	       I/O		*
		 *******************************/

format(Fmt) :-
	format(Fmt, []).

		 /*******************************
		 *	      FILES		*
		 *******************************/

%	absolute_file_name(+Term, -AbsoluteFile)

absolute_file_name(Name, Abs) :-
	atomic(Name), !,
	'$absolute_file_name'(Name, Abs).
absolute_file_name(Term, Abs) :-
	'$chk_file'(Term, [''], [access(read)], true, File), !,
	'$absolute_file_name'(File, Abs).
absolute_file_name(Term, Abs) :-
	'$chk_file'(Term, [''], [], true, File), !,
	'$absolute_file_name'(File, Abs).


		/********************************
		*	 MEMORY MANAGEMENT      *
		*********************************/

%%	garbage_collect is det.
%
%	Invoke the garbage collector.  The   argument  of the underlying
%	'$garbage_collect'/1  is  the  debugging  level  to  use  during
%	garbage collection. This only works if   the  system is compiled
%	with the -DODEBUG cpp flag. Only to simplify maintenance.

garbage_collect :-
	'$garbage_collect'(0).

%%	set_prolog_stack(+Name, +Option) is det.
%
%	Set a parameter for one of the Prolog stacks.

set_prolog_stack(Stack, Option) :-
	Option =.. [Name,Value0],
	Value is Value0,
	'$set_prolog_stack'(Stack, Name, _Old, Value).

%%	prolog_stack_property(?Stack, ?Property) is nondet.
%
%	Examine stack properties.

prolog_stack_property(Stack, Property) :-
	stack_property(P),
	stack_name(Stack),
	Property =.. [P,Value],
	'$set_prolog_stack'(Stack, P, Value, Value).

stack_name(local).
stack_name(global).
stack_name(trail).

stack_property(limit).
stack_property(spare).
stack_property(min_free).


		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

%%	arithmetic_function(:Spec)
%
%	Register a predicate as an arithmetic function.  Takes Name/Arity
%	and a term as argument.

:- meta_predicate
	arithmetic_function(1).

arithmetic_function(Module:Term) :-
	(   Term = Name/Arity
	;   functor(Term, Name, Arity)
	), !,
	PredArity is Arity + 1,
	functor(Head, Name, PredArity),
	'$arithmetic_function'(Module:Head, 0).

		 /*******************************
		 *	 LIST MANIPULATION	*
		 *******************************/

%%	length(?List, ?N)
%
%	Is true when N is the length of List.

:- '$iso'((length/2)).

length(List, Length) :-
	(   nonvar(Length)
	->  '$length'(List, Length)
	;   '$skip_list'(Length0, List, Tail),
	    (	Tail == []
	    ->	Length = Length0
	    ;	var(Tail)
	    ->  length3(Tail, Length, Length0)
	    ;	throw(error(type_error(list,Tail),
			    context(length/2, _)))
	    )
	).

length3([], N, N).
length3([_|List], N, N0) :-
        succ(N0, N1),
        length3(List, N, N1).


		 /*******************************
		 *	       TERM		*
		 *******************************/

:- '$iso'((numbervars/3)).

%	numbervars(+Term, +StartIndex, -EndIndex)
%
%	Number all unbound variables in Term   using  '$VAR'(N), where the
%	first N is StartIndex and EndIndex is  unified to the index that
%	will be given to the next variable.

numbervars(Term, From, To) :-
	numbervars(Term, From, To, []).


		 /*******************************
		 *	       GVAR		*
		 *******************************/

%	nb_setval(+Name, +Value)
%
%	Bind the non-backtrackable variable Name with a copy of Value

nb_setval(Name, Value) :-
	duplicate_term(Value, Copy),
	nb_linkval(Name, Copy).
