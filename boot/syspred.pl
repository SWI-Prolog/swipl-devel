/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Prolog system predicate definitions
*/

:- module($syspreds,
	[ leash/1
	, visible/1
	, style_check/1
	, please/3
	, (spy)/1
	, (nospy)/1
	, trace/1
	, trace/2
	, nospyall/0
	, debugging/0
	, concat_atom/2
	, term_to_atom/2
	, atom_to_term/3
	, int_to_atom/2
	, gensym/2
	, dwim_match/2
	, source_file/1
	, source_file/2
	, current_predicate/2
	, predicate_property/2
	, $predicate_property/2
	, clause/2
	, clause/3
	, recorda/2
	, recordz/2
	, recorded/2
	, retractall/1
	, current_module/1
	, current_module/2
	, module/1
	, statistics/0
	, shell/2
	, shell/1
	, shell/0
	, format/1
	, sformat/2
	, sformat/3
	, garbage_collect/0
	, arithmetic_function/1
        , default_module/2
	, save_program/1
	, save_program/2
	, save/1
	]).	

		/********************************
		*           DEBUGGER            *
		*********************************/

$map_bits(_, [], Bits, Bits) :- !.
$map_bits(Pred, [H|T], Old, New) :-
	$map_bits(Pred, H, Old, New0),
	$map_bits(Pred, T, New0, New).
$map_bits(Pred, +Name, Old, New) :- !, 		% set a bit
	$apply(Pred, [Name, Bits]), !,
	New is Old \/ Bits.
$map_bits(Pred, -Name, Old, New) :- !, 		% clear a bit
	$apply(Pred, [Name, Bits]), !,
	New is Old /\ (\Bits).
$map_bits(Pred, ?Name, Old, Old) :-		% ask a bit
	$apply(Pred, [Name, Bits]), !,
	Old /\ Bits > 0.

$port_bit(  call, 2'00001).
$port_bit(  exit, 2'00010).
$port_bit(  fail, 2'00100).
$port_bit(  redo, 2'01000).
$port_bit( unify, 2'10000).
$port_bit(   all, 2'11111).
$port_bit(  full, 2'01111).
$port_bit(  half, 2'01101).

leash(Ports) :-
	$leash(Old, Old),
	$map_bits($port_bit, Ports, Old, New),
	$leash(_, New).

visible(Ports) :-
	$visible(Old, Old),
	$map_bits($port_bit, Ports, Old, New),
	$visible(_, New).

$map_style_check(atom,     	   2'00001).
$map_style_check(singleton, 	   2'00010).
$map_style_check(dollar,   	   2'00100).
$map_style_check((discontiguous),  2'01000).
$map_style_check(string,	   2'10000).

style_check(Spec) :-
	$style_check(Old, Old),
	$map_bits($map_style_check, Spec, Old, New),
	$style_check(_, New).

please(autoload, Old, New) :- !,
	flag($enable_autoload, Old, New).
please(verbose_autoload, Old, New) :- !,
	flag($verbose_autoload, Old, New).
please(Key, Old, New) :-
	$please(Key, Old, New).

:- module_transparent
	trace/1,
	trace/2,
	$trace/2,
	spy/1,
	nospy/1.

trace(Preds) :-
	trace(Preds, +all).

trace([], _) :- !.
trace([H|T], Ps) :- !,
	trace(H, Ps),
	trace(T, Ps).
trace(Pred, Ports) :-
	debug,
	$find_predicate(Pred, Preds),
	Preds \== [],
	(   member(Head, Preds),
	        $trace(Ports, Head),
	        show_trace_point(Head),
	    fail
	;   true
	).


trace_alias(all,  [trace_call, trace_redo, trace_exit, trace_fail]).
trace_alias(call, [trace_call]).
trace_alias(redo, [trace_redo]).
trace_alias(exit, [trace_exit]).
trace_alias(fail, [trace_fail]).

$trace([], _) :- !.
$trace([H|T], Head) :- !,
	$trace(H, Head),
	$trace(T, Head).
$trace(+H, Head) :-
	trace_alias(H, A0), !,
	tag_list(A0, +, A1),
	$trace(A1, Head).
$trace(+H, Head) :- !,
	$set_predicate_attribute(Head, H, 1).
$trace(-H, Head) :-
	trace_alias(H, A0), !,
	tag_list(A0, -, A1),
	$trace(A1, Head).
$trace(-H, Head) :- !,
	$set_predicate_attribute(Head, H, 0).
$trace(H, Head) :-
	$set_predicate_attribute(Head, H, 1).

tag_list([], _, []).
tag_list([H0|T0], F, [H1|T1]) :-
	H1 =.. [F, H0],
	tag_list(T0, F, T1).


spy([]) :- !.
spy([H|T]) :- !,
	spy(H),
	spy(T).
spy(Spec) :-
	$find_predicate(Spec, Preds),
	member(Head, Preds),
	    $spy(Head),
	    $predicate_name(Head, Name),
	    $ttyformat('Spy point on ~w~n', [Name]),
	fail.
spy(_).

nospy([]) :- !.
nospy([H|T]) :- !,
	nospy(H),
	nospy(T).
nospy(Spec) :-
	$find_predicate(Spec, Preds),
	member(Head, Preds),
	    $nospy(Head),
	    $predicate_name(Head, Name),
	    $ttyformat('Spy point removed from ~w~n', [Name]),
	fail.
nospy(_).

nospyall :-
	current_predicate(_, Module:Head),
	    $nospy(Module:Head),
	fail.
nospyall.

debugging :-
	$debugging, !,
	format('Debug mode is on; spy points (see spy/1) on:~n'),
	$show_spy_points,
	format('Trace points (see trace/1) on:~n'),
	show_trace_points.

debugging :-
	format('Debug mode is off~n').

$show_spy_points :-
	current_predicate(_, Module:Head),
	$get_predicate_attribute(Module:Head, spy, 1),
	\+ predicate_property(Module:Head, imported_from(_)),
	$predicate_name(Module:Head, Name),
	format('~t~8|~w~n', [Name]),
	fail.
$show_spy_points.

show_trace_points :-
	current_predicate(_, Module:Head),
	$get_predicate_attribute(Module:Head, trace_any, 1),
	\+ predicate_property(Module:Head, imported_from(_)),
	show_trace_point(Module:Head),
	fail.
show_trace_points.

:- module_transparent
	show_trace_point/1,
	show_trace_ports/1.

show_trace_point(Head) :-
	$predicate_name(Head, Name),
	format('~t~8|~w:', [Name]),
	show_trace_ports(Head),
	nl.

show_trace_ports(Head) :-
	trace_alias(Port, [AttName]),
	$get_predicate_attribute(Head, AttName, 1),
	format(' ~w', [Port]),
	fail.
show_trace_ports(_).


		/********************************
		*             ATOMS             *
		*********************************/

concat_atom([A, B], C) :- !,
	concat(A, B, C).
concat_atom(L, Atom) :-
	$concat_atom(L, Atom).

term_to_atom(Term, Atom) :-
	$term_to_atom(Term, Atom, 0).

atom_to_term(Atom, Term, Bindings) :-
	var(Bindings),
	$term_to_atom(Term, Atom, Bindings).

int_to_atom(Int, Atom) :-
	int_to_atom(Int, 10, Atom).

gensym(Base, Atom) :-
	concat($gs_, Base, Key),
	flag(Key, Old, Old),
	succ(Old, New),
	flag(Key, _, New),
	concat(Base, New, Atom).

dwim_match(A1, A2) :-
	dwim_match(A1, A2, _).

		/********************************
		*             SOURCE            *
		*********************************/

:- module_transparent
	source_file/2.

source_file(Pred, File) :-
	current_predicate(_, Pred),
	$source_file(Pred, File).

source_file(File) :-
	$time_source_file(File, _).
source_file(File) :-
	atom(File),
	absolute_file_name(File, Abs),	% canonise
	$time_source_file(Abs, _).


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


:- module_transparent
	current_predicate/2,
	$defined_predicate/1.

current_predicate(Name, Head) :-
	var(Head), !,
	context_module(Module),
	generate_current_predicate(Name, Module, Head).
current_predicate(Name, Module:Head) :-
	(var(Module) ; var(Head)), !,
	generate_current_predicate(Name, Module, Head).
current_predicate(Name, Term) :-
	$c_current_predicate(Name, Term),
	$defined_predicate(Term), !.
current_predicate(Name, Term) :-
	$strip_module(Term, Module, Head),
	default_module(Module, DefModule),
	$c_current_predicate(Name, DefModule:Head),
	$defined_predicate(DefModule:Head), !.
current_predicate(Name, Term) :-
	flag($enable_autoload, on, on),
	$strip_module(Term, Module, Head),
	functor(Head, Name, Arity),
	$find_library(Module, Name, Arity, _LoadModule, _Library), !.

generate_current_predicate(Name, Module, Head) :-
	current_module(Module),
	$c_current_predicate(Name, Module:Head),
	$defined_predicate(Module:Head).

$defined_predicate(Head) :-
	$get_predicate_attribute(Head, defined, 1), !.
$defined_predicate(Head) :-
	$get_predicate_attribute(Head, (dynamic), 1).

:- module_transparent
	predicate_property/2,
	$predicate_property/2.

predicate_property(Pred, Property) :-
	Property == undefined, !,
	(   Pred = Module:Head,
	    var(Module)
	;   $strip_module(Pred, Module, Head)
	), !,
	current_module(Module),
	Term = Module:Head,
	$c_current_predicate(_, Term),
	\+ $defined_predicate(Term),		% Speed up a bit
	\+ current_predicate(_, Term).
predicate_property(Pred, Property) :-
	current_predicate(_, Pred),
	$predicate_property(Pred, Property).

:- index($predicate_property(0, 1)).

$predicate_property(Pred, interpreted) :-
	$get_predicate_attribute(Pred, foreign, 0).
$predicate_property(Pred, built_in) :-
	$get_predicate_attribute(Pred, system, 1).
$predicate_property(Pred, exported) :-
	$get_predicate_attribute(Pred, exported, 1).
$predicate_property(Pred, foreign) :-
	$get_predicate_attribute(Pred, foreign, 1).
$predicate_property(Pred, (dynamic)) :-
	$get_predicate_attribute(Pred, (dynamic), 1).
$predicate_property(Pred, (multifile)) :-
	$get_predicate_attribute(Pred, (multifile), 1).
$predicate_property(Pred, imported_from(Module)) :-
	$get_predicate_attribute(Pred, imported, Module).
$predicate_property(Pred, transparent) :-
	$get_predicate_attribute(Pred, transparent, 1).
$predicate_property(Pred, indexed(Pattern)) :-
	$get_predicate_attribute(Pred, indexed, Pattern).
$predicate_property(Pred, file(File)) :-
	source_file(Pred, File).
$predicate_property(Pred, line_count(LineNumber)) :-
	$get_predicate_attribute(Pred, line_count, LineNumber).

:- module_transparent
	clause/2,
	clause/3,
	retractall/1.

clause(Head, Body, Ref) :-
	nonvar(Ref), !,
	$clause(Head, Clause, Ref),
	$strip_module(Head, _, H),
	$clause2(H, Body, Clause).
clause(Head, Body, Ref) :-
	current_predicate(_, Head),
	$clause(Head, Clause, Ref),
	$strip_module(Head, _, H),
	$clause2(H, Body, Clause).

clause(Head, Body) :-
	current_predicate(_, Head),
	$clause(Head, Clause, _),
	$strip_module(Head, _, H),
	$clause2(H, Body, Clause).

$clause2(Head, Body, (Head :- Body)) :- !.
$clause2(Head, true, Head).

recorda(Key, Value) :-
	recorda(Key, Value, _).
recordz(Key, Value) :-
	recordz(Key, Value, _).
recorded(Key, Value) :-
	recorded(Key, Value, _).

retractall(Term) :-
	retract(Term),
	fail.
retractall(_).


		/********************************
		*            MODULES            *
		*********************************/

current_module(Module) :-
	$current_module(Module, _).

current_module(Module, File) :-
	$current_module(Module, File),
	File \== [].

module(Module) :-
	atom(Module),
	current_module(Module), !,
	$module(_, Module).
module(Module) :-
	$break($warning('~w is not a current module', [Module])),
	$module(_, Module).

		/********************************
		*          STATISTICS           *
		*********************************/

statistics :-
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
	statistics(externals, Externals),
	statistics(locallimit, LocalLimit),
	statistics(globallimit, GlobalLimit),
	statistics(traillimit, TrailLimit),

	format('~2f seconds cpu time for ~D inferences~n',
				    [Cputime, Inferences]),
	format('~D atoms, ~D functors, ~D predicates, ~D modules~n',
				    [Atoms, Functors, Predicates, Modules]),
	format('~D byte codes; ~D external references~n~n',
				    [Codes, Externals]),
	format('                      Limit    Allocated       In use~n'),
	format('Heap         :                  ~t~D~53| Bytes~n',
	       [Heapused]),
	format('Local  stack :~t~D~27| ~t~D~40| ~t~D~53| Bytes~n',
	       [LocalLimit, Local, LocalUsed]),
	format('Global stack :~t~D~27| ~t~D~40| ~t~D~53| Bytes~n',
	       [GlobalLimit, Global, GlobalUsed]),
	format('Trail  stack :~t~D~27| ~t~D~40| ~t~D~53| Bytes~n',
	       [TrailLimit, Trail, TrailUsed]),

	gc_statistics,
	shift_statistics.

gc_statistics :-
	statistics(collections, Collections),
	Collections > 0, !,
	statistics(collected, Collected),
	statistics(gctime, GcTime),

	format('~n~D garbage collections gained ~D bytes in ~2f seconds.~n',
	       [Collections, Collected, GcTime]).
gc_statistics.

shift_statistics :-
	statistics(local_shifts, LS),
	statistics(global_shifts, GS),
	statistics(trail_shifts, TS),
	(   LS > 0
	;   GS > 0
	;   TS > 0
	), !,
	format('~nStack shifts: ~D local, ~D global, ~D trail.~n',
	       [LS, GS, TS]).
shift_statistics.


		/********************************
		*      SYSTEM INTERACTION       *
		*********************************/

shell(Command, Status) :-
	$shell(Command, Status).

shell(Command) :-
	shell(Command, 0).

shell :-
	getenv('SHELL', Shell), !,
	shell(Shell).
shell :-
	shell('/bin/sh').


		/********************************
		*              I/O              *
		*********************************/

format(Fmt) :-
	format(Fmt, []).

sformat(String, Format, Arguments) :-
	$write_on_string(format(Format, Arguments), String).
sformat(String, Format) :-
	$write_on_string(format(Format), String).

		/********************************
		*         MISCELLENEOUS         *
		*********************************/

%	Invoke the garbage collector.  The argument is the debugging level
%	to use during garbage collection.  This only works if the system
%	is compiled with the -DODEBUG cpp flag.  Only to simplify maintenance.

garbage_collect :-
	$garbage_collect(0).

%	save_program(+Name[, +Options])
%	Save the currently running image in file

save_program(Name) :-
	save_program(Name, []).

save_program(Name, Options) :-
	$autoload:clear_library_index,
	$save_program(Name, Options).

%	save(+File)
%	Create saved-state in file

save(File) :-
	save(File, _).

%	arithmetic_function(Spec)
%	Register a predicate as an arithmetic function.  Takes Name/Arity
%	and a term as argument.

:- module_transparent
	arithmetic_function/1.

arithmetic_function(Spec) :-
	$strip_module(Spec, Module, Term),
	(   Term = Name/Arity
	;   functor(Term, Name, Arity)
	), !,
	PredArity is Arity + 1,
	functor(Head, Name, PredArity),
	$arithmetic_function(Module:Head).

%	default_module(+Me, -Super)
%	Is true if `Super' is `Me' or a super (auto import) module of `Me'.

default_module(Me, Me).
default_module(Me, Super) :-
	$default_module(Me, S, S),
	S \== [],
	default_module(S, Super).
