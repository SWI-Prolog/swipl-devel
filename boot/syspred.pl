/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

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
	    source_file_property/2,
	    source_file/1,
	    unload_file/1,
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
	    absolute_file_name/2,
	    require/1,
	    call_with_depth_limit/3,
	    numbervars/3,		% +Term, +Start, -End
	    nb_setval/2			% +Var, +Value
	  ]).

		/********************************
		*           DEBUGGER            *
		*********************************/

%%	map_bits(:Pred, +Modify, +OldBits, -NewBits)

:- meta_predicate
	map_bits(2, +, +, -).

map_bits(_, [], Bits, Bits) :- !.
map_bits(Pred, [H|T], Old, New) :-
	map_bits(Pred, H, Old, New0),
	map_bits(Pred, T, New0, New).
map_bits(Pred, +Name, Old, New) :- !,	% set a bit
	bit(Pred, Name, Bits), !,
	New is Old \/ Bits.
map_bits(Pred, -Name, Old, New) :- !,	% clear a bit
	bit(Pred, Name, Bits), !,
	New is Old /\ (\Bits).
map_bits(Pred, ?(Name), Old, Old) :-		% ask a bit
	bit(Pred, Name, Bits),
	Old /\ Bits > 0.

bit(Pred, Name, Bits) :-
	call(Pred, Name, Bits), !.
bit(_:Pred, Name, _) :-
	throw(error(domain_error(Pred, Name), _)).

:- public port_name/2.			% used by library(test_cover)

port_name(      call, 2'000000001).
port_name(      exit, 2'000000010).
port_name(      fail, 2'000000100).
port_name(      redo, 2'000001000).
port_name(     unify, 2'000010000).
port_name(     break, 2'000100000).
port_name(  cut_call, 2'001000000).
port_name(  cut_exit, 2'010000000).
port_name( exception, 2'100000000).
port_name(       cut, 2'011000000).
port_name(       all, 2'000111111).
port_name(      full, 2'000101111).
port_name(      half, 2'000101101).	% '

leash(Ports) :-
	'$leash'(Old, Old),
	map_bits(port_name, Ports, Old, New),
	'$leash'(_, New).

visible(Ports) :-
	'$visible'(Old, Old),
	map_bits(port_name, Ports, Old, New),
	'$visible'(_, New).

style_name(atom,	    2'0000001).
style_name(singleton,	    2'0000010).
style_name(discontiguous,   2'0001000).
style_name(dynamic,	    2'0010000).
style_name(charset,	    2'0100000).

style_check(+string) :- !,
	set_prolog_flag(double_quotes, string).
style_check(-string) :- !,
	set_prolog_flag(double_quotes, codes).
style_check(?(string)) :- !,
	current_prolog_flag(double_quotes, string).
style_check(Spec) :-
	'$style_check'(Old, Old),
	map_bits(style_name, Spec, Old, New),
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
	notrace(prolog:debug_control_hook(spy(Spec))), !.
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
	notrace(prolog:debug_control_hook(nospy(Spec))), !.
nospy(Spec) :-
	'$find_predicate'(Spec, Preds),
	'$member'(PI, Preds),
	     pi_to_head(PI, Head),
	    '$nospy'(Head),
	fail.
nospy(_).

nospyall :-
	notrace(prolog:debug_control_hook(nospyall)),
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
	notrace(prolog:debug_control_hook(debugging)), !.
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

%%	rational(+Rat, -Numerator, -Denominator) is semidet.
%
%	True when Rat is a  rational   number  with  given Numerator and
%	Denominator.

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
	->  (	'$time_source_file'(File, Time, user)
	    ;	absolute_file_name(File, Abs),
		'$time_source_file'(Abs, Time, user)
	    ), !
	;   '$time_source_file'(File, Time, user)
	),
	Time > 0.0.

%%	source_file_property(?File, ?Property) is nondet.
%
%	True if Property is a property of the loaded source-file File.

source_file_property(File, P) :-
	nonvar(File), !,
	canonical_source_file(File, Path),
	property_source_file(P, Path).
source_file_property(File, P) :-
	property_source_file(P, File).

property_source_file(modified(Time), File) :-
	'$time_source_file'(File, Time, user).
property_source_file(module(M), File) :-
	(   nonvar(M)
	->  '$current_module'(M, File)
	;   nonvar(File)
	->  '$current_module'(ML, File),
	    (	atom(ML)
	    ->	M = ML
	    ;	'$member'(M, ML)
	    )
	;   '$current_module'(M, File)
	).
property_source_file(load_context(Module, Location, Options), File) :-
	'$time_source_file'(File, _, user),
	clause(system:'$load_context_module'(File, Module, Options), true, Ref),
	(   clause_property(Ref, file(FromFile)),
	    clause_property(Ref, line_count(FromLine))
	->  Location = FromFile:FromLine
	;   Location = user
	).
property_source_file(includes(Master, Stamp), File) :-
	system:'$included'(File, _Line, Master, Stamp).
property_source_file(included_in(Master, Line), File) :-
	system:'$included'(Master, Line, File, _).
property_source_file(derived_from(DerivedFrom, Stamp), File) :-
	system:'$derived_source'(File, DerivedFrom, Stamp).


%%	canonical_source_file(+Spec, -File) is semidet.
%
%	File is the canonical representation of the source-file Spec.

canonical_source_file(Spec, File) :-
	atom(Spec),
	'$time_source_file'(Spec, _, _), !,
	File = Spec.
canonical_source_file(Spec, File) :-
	system:'$included'(_Master, _Line, Spec, _), !,
	File = Spec.
canonical_source_file(Spec, File) :-
	absolute_file_name(Spec,
			       [ file_type(prolog),
				 access(read),
				 file_errors(fail)
			       ],
			       File),
	source_file(File).


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
	source_location(F0, _),
	'$input_context'(Context),
	'$top_file'(Context, F0, F).
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

%%	unload_file(+File) is det.
%
%	Remove all traces of loading file.

unload_file(File) :-
	(   canonical_source_file(File, Path)
	->  '$unload_file'(Path),
	    retractall(system:'$load_context_module'(Path, _))
	;   true
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

%%	predicate_property(?Predicate, ?Property) is nondet.
%
%	True when Property is a property of Predicate.

:- meta_predicate
	predicate_property(:, ?).

:- '$iso'(predicate_property/2).

predicate_property(Pred, Property) :-		% Mode ?,+
	nonvar(Property), !,
	property_predicate(Property, Pred).
predicate_property(Pred, Property) :-		% Mode +,-
	define_or_generate(Pred),
	'$predicate_property'(Property, Pred).

%%	property_predicate(+Property, ?Pred)
%
%	First handle the special  cases  that   are  not  about querying
%	normally  defined  predicates:   =undefined=,    =visible=   and
%	=autoload=, followed by the generic case.

property_predicate(undefined, Pred) :- !,
	Pred = Module:Head,
	current_module(Module),
	'$c_current_predicate'(_, Pred),
	\+ '$defined_predicate'(Pred),		% Speed up a bit
	\+ current_predicate(_, Pred),
	functor(Head, Name, Arity),
	\+ system_undefined(Module:Name/Arity).
property_predicate(visible, Pred) :- !,
	visible_predicate(Pred).
property_predicate(autoload(File), _:Head) :- !,
	current_prolog_flag(autoload, true),
	(   callable(Head)
	->  functor(Head, Name, Arity),
	    (	'$find_library'(_, Name, Arity, _, File)
	    ->	true
	    )
	;   '$find_library'(_, Name, Arity, _, File),
	    functor(Head, Name, Arity)
	).
property_predicate(Property, Pred) :-
	define_or_generate(Pred),
	'$predicate_property'(Property, Pred).

%%	define_or_generate(+Head) is semidet.
%%	define_or_generate(-Head) is nondet.
%
%	If the predicate is known, try to resolve it. Otherwise generate
%	the known predicate, but do not try to (auto)load the predicate.

define_or_generate(M:Head) :-
	callable(Head),
	atom(M),
	'$get_predicate_attribute'(M:Head, defined, 1), !.
define_or_generate(M:Head) :-
	callable(Head),
	nonvar(M), M \== system, !,
	'$define_predicate'(M:Head).
define_or_generate(Pred) :-
	current_predicate(_, Pred),
	'$define_predicate'(Pred).


'$predicate_property'(interpreted, Pred) :-
	'$get_predicate_attribute'(Pred, foreign, 0).
'$predicate_property'(visible, Pred) :-
	'$get_predicate_attribute'(Pred, defined, 1).
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
'$predicate_property'(meta_predicate(Pattern), Pred) :-
	'$get_predicate_attribute'(Pred, meta_predicate, Pattern).
'$predicate_property'(file(File), Pred) :-
	'$get_predicate_attribute'(Pred, file, File).
'$predicate_property'(line_count(LineNumber), Pred) :-
	'$get_predicate_attribute'(Pred, line_count, LineNumber).
'$predicate_property'(notrace, Pred) :-
	'$get_predicate_attribute'(Pred, trace, 0).
'$predicate_property'(nodebug, Pred) :-
	'$get_predicate_attribute'(Pred, hide_childs, 1).
'$predicate_property'(spying, Pred) :-
	'$get_predicate_attribute'(Pred, spy, 1).
'$predicate_property'(references(N), Pred) :-
	'$get_predicate_attribute'(Pred, references, N),
	N \== 0.			% show negative for debugging!
'$predicate_property'(number_of_clauses(N), Pred) :-
	'$get_predicate_attribute'(Pred, number_of_clauses, N).
'$predicate_property'(number_of_rules(N), Pred) :-
	'$get_predicate_attribute'(Pred, number_of_rules, N).
'$predicate_property'(indexed(Indices), Pred) :-
	'$get_predicate_attribute'(Pred, indexed, Indices).
'$predicate_property'(noprofile, Pred) :-
	'$get_predicate_attribute'(Pred, noprofile, 1).
'$predicate_property'(iso, Pred) :-
	'$get_predicate_attribute'(Pred, iso, 1).

system_undefined(user:prolog_trace_interception/4).
system_undefined(user:prolog_exception_hook/4).
system_undefined(system:'$c_call_prolog'/0).
system_undefined(system:window_title/2).

%%	visible_predicate(:Head) is nondet.
%
%	True when Head can be called without raising an existence error.
%	This implies it is defined,  can   be  inherited  from a default
%	module or can be autoloaded.

visible_predicate(Pred) :-
	Pred = M:Head,
	current_module(M),
	(   callable(Head)
	->  (   '$get_predicate_attribute'(Pred, defined, 1)
	    ->	\+ hidden_system_predicate(Pred)
	    ;	\+ current_prolog_flag(M:unknown, fail),
		functor(Head, Name, Arity),
		'$find_library'(M, Name, Arity, _LoadModule, _Library)
	    )
	;   (   default_module(M, DefM),
	        '$c_current_predicate'(_, DefM:Head),
		\+ '$get_predicate_attribute'(DefM:Head, imported, _),
		\+ hidden_system_predicate(Pred)
	    ;	'$in_library'(Name, Arity, _),
		functor(Head, Name, Arity),
		\+ '$get_predicate_attribute'(Pred, defined, 1)
	    )
	).

hidden_system_predicate(_:Head) :-
	functor(Head, Name, _),
	sub_atom(Name, 0, _, _, $),
	\+ current_prolog_flag(access_level, system).


%%	clause_property(+ClauseRef, ?Property) is nondet.
%
%	Provide information on individual clauses.  Defined properties
%	are:
%
%	    * line_count(-Line)
%	    Line from which the clause is loaded.
%	    * file(-File)
%	    File from which the clause is loaded.
%	    * source(-File)
%	    File that `owns' the clause: reloading this file wipes
%	    the clause.
%	    * fact
%	    Clause has body =true=.
%	    * erased
%	    Clause was erased.
%	    * predicate(:PI)
%	    Predicate indicator of the predicate this clause belongs
%	    to.  Can be used to find the predicate of erased clauses.

clause_property(Clause, Property) :-
	'$clause_property'(Property, Clause).

'$clause_property'(line_count(LineNumber), Clause) :-
	'$get_clause_attribute'(Clause, line_count, LineNumber).
'$clause_property'(file(File), Clause) :-
	'$get_clause_attribute'(Clause, file, File).
'$clause_property'(source(File), Clause) :-
	'$get_clause_attribute'(Clause, owner, File).
'$clause_property'(fact, Clause) :-
	'$get_clause_attribute'(Clause, fact, true).
'$clause_property'(erased, Clause) :-
	'$get_clause_attribute'(Clause, erased, true).
'$clause_property'(predicate(PI), Clause) :-
	'$get_clause_attribute'(Clause, predicate_indicator, PI).


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
%	    * exported_operators(ListOfOp3)
%	    The module exports the operators ListOfOp3.

module_property(Module, Property) :-
	nonvar(Module), nonvar(Property), !,
	property_module(Property, Module).
module_property(Module, Property) :-	% -, file(File)
	nonvar(Property), Property = file(File), !,
	(   nonvar(File)
	->  '$current_module'(Modules, File),
	    (	atom(Modules)
	    ->	Module = Modules
	    ;	'$member'(Module, Modules)
	    )
	;   '$current_module'(Module, File),
	    File \== []
	).
module_property(Module, Property) :-
	current_module(Module),
	property_module(Property, Module).

property_module(Property, Module) :-
	module_property(Property),
	(   Property = exported_operators(List)
	->  '$exported_ops'(Module, List, []),
	    List \== []
	;   '$module_property'(Module, Property)
	).

module_property(class(_)).
module_property(file(_)).
module_property(line_count(_)).
module_property(exports(_)).
module_property(exported_operators(_)).

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

%%	win_add_dll_directory(+AbsDir) is det.
%
%	Add AbsDir to the directories where  dependent DLLs are searched
%	on Windows systems.

:- if(current_prolog_flag(windows, true)).
:- export(win_add_dll_directory/1).
win_add_dll_directory(Dir) :-
	win_add_dll_directory(Dir, _), !.
win_add_dll_directory(Dir) :-
	prolog_to_os_filename(Dir, OSDir),
	getenv('PATH', Path0),
	atomic_list_concat([Path0, OSDir], ';', Path),
	setenv('PATH', Path).
:- endif.

		 /*******************************
		 *	      SIGNALS		*
		 *******************************/

:- meta_predicate
	on_signal(+, :, :),
	current_signal(?, ?, :).

%%	on_signal(+Signal, -OldHandler, :NewHandler) is det.

on_signal(Signal, Old, New) :-
	atom(Signal), !,
	'$on_signal'(_Num, Signal, Old, New).
on_signal(Signal, Old, New) :-
	integer(Signal), !,
	'$on_signal'(Signal, _Name, Old, New).
on_signal(Signal, _Old, _New) :-
	(   var(Signal)
	->  Err = instantiation_error
	;   Err = type_error(signal_name, Signal)
	),
	throw(error(Err, context(on_signal/3, _))).

%%	current_signal(?Name, ?SignalNumber, :Handler) is nondet.

current_signal(Name, Id, Handler) :-
	between(1, 32, Id),
	'$on_signal'(Id, Name, Handler, Handler).

:- multifile
	prolog:called_by/2.

prolog:called_by(on_signal(_,_,New), [New+1]) :-
	(   new == throw
	;   new == default
	), !, fail.


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
		 *	       TERM		*
		 *******************************/

:- '$iso'((numbervars/3)).

%%	numbervars(+Term, +StartIndex, -EndIndex) is det.
%
%	Number all unbound variables in Term   using  '$VAR'(N), where the
%	first N is StartIndex and EndIndex is  unified to the index that
%	will be given to the next variable.

numbervars(Term, From, To) :-
	numbervars(Term, From, To, []).


		 /*******************************
		 *	       GVAR		*
		 *******************************/

%%	nb_setval(+Name, +Value) is det.
%
%	Bind the non-backtrackable variable Name with a copy of Value

nb_setval(Name, Value) :-
	duplicate_term(Value, Copy),
	nb_linkval(Name, Copy).
