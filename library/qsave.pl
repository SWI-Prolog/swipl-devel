/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

:- module(qsave,
	  [ qsave_program/1,			% +File
	    qsave_program/2			% +File, +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(error)).

/** <module> Save current program as a state or executable

This library provides qsave_program/1  and   qsave_program/2,  which are
also used by the commandline sequence below.

  ==
  swipl -o exe -c file.pl ...
  ==
*/

:- meta_predicate
	qsave_program(+, :).

:- predicate_options(qsave_program/2, 2,
		     [ local(integer),
		       global(integer),
		       trail(integer),
		       goal(callable),
		       toplevel(callable),
		       init_file(atom),
		       class(oneof([runtime,kernel,development])),
		       autoload(boolean),
		       map(atom),
		       op(oneof([save,standard])),
		       stand_alone(boolean),
		       foreign(oneof([save,no_save])),
		       emulator(atom)
		     ]).

:- set_prolog_flag(generate_debug_info, false).

:- dynamic verbose/1.
:- volatile verbose/1.			% contains a stream-handle

%%	qsave_program(+File) is det.
%%	qsave_program(+File, :Options) is det.
%
%	Make a saved state in file `File'.

qsave_program(File) :-
	qsave_program(File, []).

qsave_program(FileBase, Options0) :-
	meta_options(is_meta, Options0, Options),
	check_options(Options),
	exe_file(FileBase, File),
	option(class(SaveClass),    Options, runtime),
	option(init_file(InitFile), Options, DefInit),
	default_init_file(SaveClass, DefInit),
	save_autoload(Options),
	open_map(Options),
	create_prolog_flag(saved_program, true, []),
	create_prolog_flag(saved_program_class, SaveClass, []),
	(   exists_file(File)
	->  delete_file(File)
	;   true
	),
	'$rc_open_archive'(File, RC),
	make_header(RC, SaveClass, Options),
	save_options(RC, SaveClass,
		     [ init_file(InitFile)
		     | Options
		     ]),
	save_resources(RC, SaveClass),
	'$rc_open'(RC, '$state', '$prolog', write, StateFd),
	'$open_wic'(StateFd),
	setup_call_cleanup(
	    ( current_prolog_flag(access_level, OldLevel),
	      set_prolog_flag(access_level, system) % generate system modules
	    ),
	    ( save_modules(SaveClass),
	      save_records,
	      save_flags,
	      save_imports,
	      save_prolog_flags,
	      save_operators(Options),
	      save_format_predicates
	    ),
	    set_prolog_flag(access_level, OldLevel)),
	'$close_wic',
	close(StateFd),
	save_foreign_libraries(RC, Options),
	'$rc_close_archive'(RC),
	'$mark_executable'(File),
	close_map.

is_meta(goal).
is_meta(toplevel).

exe_file(Base, Exe) :-
	current_prolog_flag(windows, true),
	file_name_extension(_, '', Base), !,
	file_name_extension(Base, exe, Exe).
exe_file(Exe, Exe).

default_init_file(runtime, none) :- !.
default_init_file(_,       InitFile) :-
	'$option'(init_file, InitFile).


		 /*******************************
		 *	     HEADER		*
		 *******************************/

make_header(RC, _, Options) :-
	option(emulator(OptVal), Options), !,
	absolute_file_name(OptVal, [access(read)], Emulator),
	'$rc_append_file'(RC, '$header', '$rc', none, Emulator).
make_header(RC, _, Options) :-
	(   current_prolog_flag(windows, true)
	->  DefStandAlone = true
	;   DefStandAlone = false
	),
	option(stand_alone(true), Options, DefStandAlone), !,
	current_prolog_flag(executable, Executable),
	'$rc_append_file'(RC, '$header', '$rc', none, Executable).
make_header(RC, SaveClass, _Options) :-
	current_prolog_flag(unix, true), !,
	current_prolog_flag(executable, Executable),
	'$rc_open'(RC, '$header', '$rc', write, Fd),
	format(Fd, '#!/bin/sh~n', []),
	format(Fd, '# SWI-Prolog saved state~n', []),
	(   SaveClass == runtime
	->  ArgSep = ' -- '
	;   ArgSep = ' '
	),
	format(Fd, 'exec ${SWIPL-~w} -x "$0"~w"$@"~n~n', [Executable, ArgSep]),
	close(Fd).
make_header(_, _, _).


		 /*******************************
		 *	     OPTIONS		*
		 *******************************/

min_stack(local,    32).
min_stack(global,   16).
min_stack(trail,    16).

convert_option(Stack, Val, NewVal) :-	% stack-sizes are in K-bytes
	min_stack(Stack, Min), !,
	(   Val == 0
	->  NewVal = Val
	;   NewVal is max(Min, Val*1024)
	).
convert_option(goal, Callable, Atom) :- !,
	term_to_atom(Callable, Atom).
convert_option(toplevel, Callable, Atom) :- !,
	term_to_atom(Callable, Atom).
convert_option(_, Value, Value).

doption(Name) :- min_stack(Name, _).
doption(goal).
doption(toplevel).
doption(init_file).
doption(system_init_file).
doption(class).
doption(home).

%%	save_options(+ArchiveHandle, +SaveClass, +Options)
%
%	Save the options in the '$options'  resource. The home directory
%	is saved for development saves,  so   it  keeps  refering to the
%	development home.
%
%	The script-file (-s script) is not saved at all. I think this is
%	fine to avoid a save-script loading itself.

save_options(RC, SaveClass, Options) :-
	'$rc_open'(RC, '$options', '$prolog', write, Fd),
	(   doption(OptionName),
	    '$option'(OptionName, OptionVal0),
	        save_option_value(SaveClass, OptionName, OptionVal0, OptionVal1),
	        OptTerm =.. [OptionName,OptionVal2],
	        (   option(OptTerm, Options)
		->  convert_option(OptionName, OptionVal2, OptionVal)
		;   OptionVal = OptionVal1
		),
	        format(Fd, '~w=~w~n', [OptionName, OptionVal]),
	    fail
	;   true
	),
	close(Fd).

%%	save_option_value(+SaveClass, +OptionName, +OptionValue, -FinalValue)

save_option_value(Class,   class, _,     Class) :- !.
save_option_value(runtime, home,  _,     _) :- !, fail.
save_option_value(_,       _,     Value, Value).


		 /*******************************
		 *	     RESOURCES		*
		 *******************************/

save_resources(_RC, development) :- !.
save_resources(RC, _SaveClass) :-
	feedback('~nRESOURCES~n~n', []),
	copy_resources(RC),
	(   current_predicate(_, M:resource(_,_,_)),
	    forall(M:resource(Name, Class, FileSpec),
		   (   mkrcname(M, Name, RcName),
		       save_resource(RC, RcName, Class, FileSpec)
		   )),
	    fail
	;   true
	).

mkrcname(user, Name, Name) :- !.
mkrcname(M, Name, RcName) :-
	atomic_list_concat([M, :, Name], RcName).

save_resource(RC, Name, Class, FileSpec) :-
	absolute_file_name(FileSpec,
			   [ access(read),
			     file_errors(fail)
			   ], File), !,
	feedback('~t~8|~w~t~32|~w~t~48|~w~n',
		 [Name, Class, File]),
	'$rc_append_file'(RC, Name, Class, none, File).
save_resource(RC, Name, Class, _) :-
	'$rc_handle'(SystemRC),
	copy_resource(SystemRC, RC, Name, Class), !.
save_resource(_, Name, Class, FileSpec) :-
	print_message(warning,
		      error(existence_error(resource,
					    resource(Name, Class, FileSpec)),
			    _)).

copy_resources(ToRC) :-
	'$rc_handle'(FromRC),
	'$rc_members'(FromRC, List),
	(   member(rc(Name, Class), List),
	    \+ user:resource(Name, Class, _),
	    \+ reserved_resource(Name, Class),
	    copy_resource(FromRC, ToRC, Name, Class),
	    fail
	;   true
	).

reserved_resource('$header',	'$rc').
reserved_resource('$state',	'$prolog').
reserved_resource('$options',	'$prolog').

copy_resource(FromRC, ToRC, Name, Class) :-
	setup_call_cleanup(
	    '$rc_open'(FromRC, Name, Class, read,  FdIn),
	    setup_call_cleanup(
		'$rc_open'(ToRC,   Name, Class, write, FdOut),
		( feedback('~t~8|~w~t~24|~w~t~40|~w~n',
			   [Name, Class, '<Copied from running state>']),
		  copy_stream_data(FdIn, FdOut)
		),
		close(FdOut)),
	    close(FdIn)).


		 /*******************************
		 *	      MODULES		*
		 *******************************/

save_modules(SaveClass) :-
	forall(special_module(X),
	       save_module(X, SaveClass)),
	forall((current_module(X), \+ special_module(X)),
	       save_module(X, SaveClass)).

special_module(system).
special_module(user).

define_predicate(Head) :-
	'$define_predicate'(Head), !.	% autoloader
define_predicate(Head) :-
	strip_module(Head, _, Term),
	functor(Term, Name, Arity),
	throw(error(existence_error(procedure, Name/Arity), _)).


		 /*******************************
		 *	      AUTOLOAD		*
		 *******************************/

define_init_goal(Options) :-
	option(goal(Goal), Options), !,
	define_predicate(Goal).
define_init_goal(_) :-
	flag('$banner_goal', BannerGoal, BannerGoal),
	define_predicate(user:BannerGoal).

define_toplevel_goal(Options) :-
	option(toplevel(Goal), Options), !,
	define_predicate(Goal).
define_toplevel_goal(_).

save_autoload(Options) :-
	define_init_goal(Options),
	define_toplevel_goal(Options),
	option(autoload(true),  Options, true), !,
	autoload(Options).


		 /*******************************
		 *	       MODULES		*
		 *******************************/

%%	save_module(+Module, +SaveClass)
%
%	Saves a module

save_module(M, SaveClass) :-
	'$qlf_start_module'(M),
	feedback('~n~nMODULE ~w~n', [M]),
	save_unknown(M),
	(   P = (M:_H),
	    current_predicate(_, P),
	    \+ predicate_property(P, imported_from(_)),
	    save_predicate(P, SaveClass),
	    fail
	;   '$qlf_end_part',
	    feedback('~n', [])
	).

save_predicate(P, _SaveClass) :-
	predicate_property(P, foreign), !,
	P = (M:H),
	functor(H, Name, Arity),
	feedback('~npre-defining foreign ~w/~d ', [Name, Arity]),
	'$add_directive_wic'('$predefine_foreign'(M:Name/Arity)).
save_predicate(P, SaveClass) :-
	P = (M:H),
	functor(H, F, A),
	feedback('~nsaving ~w/~d ', [F, A]),
	(   H = resource(_,_,_),
	    SaveClass \== development
	->  save_attribute(P, (dynamic)),
	    (   M == user
	    ->  save_attribute(P, (multifile))
	    ),
	    feedback('(Skipped clauses)', []),
	    fail
	;   true
	),
	save_attributes(P),
	\+ predicate_property(P, (volatile)),
	(   nth_clause(P, _, Ref),
	    feedback('.', []),
	    '$qlf_assert_clause'(Ref, SaveClass),
	    fail
	;   true
	).


pred_attrib(meta_predicate(Term), Head, meta_predicate(M:Term)) :- !,
	    strip_module(Head, M, _).
pred_attrib(Attrib, Head,
	    '$set_predicate_attribute'(M:Name/Arity, AttName, Val)) :-
	attrib_name(Attrib, AttName, Val),
	strip_module(Head, M, Term),
	functor(Term, Name, Arity).

attrib_name(dynamic,	   dynamic,	  1).
attrib_name(volatile,	   volatile,	  1).
attrib_name(thread_local,  thread_local,  1).
attrib_name(multifile,	   multifile,	  1).
attrib_name(public,	   public,	  1).
attrib_name(transparent,   transparent,	  1).
attrib_name(discontiguous, discontiguous, 1).
attrib_name(notrace,	   trace,	  0).
attrib_name(show_childs,   hide_childs,	  0).
attrib_name(built_in,      system,	  1).
attrib_name(nodebug,       hide_childs,	  1).


save_attribute(P, Attribute) :-
	pred_attrib(Attribute, P, D),
	(   Attribute == built_in	% no need if there are clauses
	->  (   predicate_property(P, number_of_clauses(0))
	    ->	true
	    ;	predicate_property(P, volatile)
	    )
	;   true
	),
	'$add_directive_wic'(D),
	feedback('(~w) ', [Attribute]).

save_attributes(P) :-
	(   predicate_property(P, Attribute),
	    save_attribute(P, Attribute),
	    fail
	;   true
	).

%	Save status of the unknown flag

save_unknown(M) :-
	current_prolog_flag(M:unknown, Unknown),
	(   Unknown == error
	->  true
	;   '$add_directive_wic'(set_prolog_flag(M:unknown, Unknown))
	).

		 /*******************************
		 *	      RECORDS		*
		 *******************************/

save_records :-
	feedback('~nRECORDS~n', []),
	(   current_key(X),
	    feedback('~n~t~8|~w ', [X, V]),
	    recorded(X, V, _),
	    feedback('.', []),
	    '$add_directive_wic'(recordz(X, V, _)),
	    fail
	;   true
	).


		 /*******************************
		 *	      FLAGS		*
		 *******************************/

save_flags :-
	feedback('~nFLAGS~n~n', []),
	(   current_flag(X),
	    flag(X, V, V),
	    feedback('~t~8|~w = ~w~n', [X, V]),
	    '$add_directive_wic'(flag(X, _, V)),
	    fail
	;   true
	).

		 /*******************************
		 *	     IMPORTS		*
		 *******************************/

%%	save_imports
%
%	Save  import  relations.  An  import  relation  is  saved  if  a
%	predicate is imported from a module that is not a default module
%	for the destination module. If  the   predicate  is  dynamic, we
%	always define the explicit import relation to make clear that an
%	assert must assert on the imported predicate.

save_imports :-
	feedback('~nIMPORTS~n~n', []),
	(   predicate_property(M:H, imported_from(I)),
	    \+ default_import(M, H, I),
	    functor(H, F, A),
	    feedback('~t~8|~w:~w/~d <-- ~w~n', [M, F, A, I]),
	    '$add_directive_wic'(qsave:restore_import(M, I, F/A)),
	    fail
	;   true
	).

default_import(To, Head, From) :-
	'$get_predicate_attribute'(To:Head, (dynamic), 1),
	predicate_property(From:Head, exported), !,
	fail.
default_import(Into, _, From) :-
	default_module(Into, From).

%%	restore_import(+TargetModule, +SourceModule, +PI) is det.
%
%	Restore import relation. This notably   deals  with imports from
%	the module =user=, avoiding a message  that the predicate is not
%	exported.

restore_import(To, user, PI) :- !,
	export(user:PI),
	To:import(user:PI).
restore_import(To, From, PI) :-
	To:import(From:PI).

		 /*******************************
		 *	   PROLOG FLAGS		*
		 *******************************/

save_prolog_flags :-
	feedback('~nPROLOG FLAGS~n~n', []),
	'$current_prolog_flag'(Flag, Value, _Scope, write, Type),
	\+ no_save_flag(Flag),
	feedback('~t~8|~w: ~w (type ~q)~n', [Flag, Value, Type]),
	'$add_directive_wic'(qsave:restore_prolog_flag(Flag, Value, Type)),
	fail.
save_prolog_flags.

no_save_flag(argv).
no_save_flag(access_level).
no_save_flag(tty_control).
no_save_flag(readline).
no_save_flag(associated_file).
no_save_flag(hwnd).			% should be read-only, but comes
					% from user-code

%%	restore_prolog_flag(+Name, +Value, +Type)
%
%	Deal  with  possibly   protected    flags   (debug_on_error  and
%	report_error are protected flags for the runtime kernel).

restore_prolog_flag(Flag, Value, _Type) :-
	current_prolog_flag(Flag, Value), !.
restore_prolog_flag(Flag, Value, _Type) :-
	current_prolog_flag(Flag, _), !,
	catch(set_prolog_flag(Flag, Value), _, true).
restore_prolog_flag(Flag, Value, Type) :-
	create_prolog_flag(Flag, Value, [type(Type)]).


		 /*******************************
		 *	     OPERATORS		*
		 *******************************/

%%	save_operators(+Options) is det.
%
%	Save operators for all modules.   Operators for =system= are
%	not saved because these are read-only anyway.

save_operators(Options) :- !,
	option(op(save), Options, save),
	feedback('~nOPERATORS~n', []),
	forall(current_module(M), save_module_operators(M)),
	feedback('~n', []).
save_operators(_).

save_module_operators(system) :- !.
save_module_operators(M) :-
	forall('$local_op'(P,T,M:N),
	       (   feedback('~n~t~8|~w ', [op(P,T,M:N)]),
		   '$add_directive_wic'(op(P,T,M:N))
	       )).


		 /*******************************
		 *       FORMAT PREDICATES	*
		 *******************************/

save_format_predicates :-
	feedback('~nFORMAT PREDICATES~n', []),
	current_format_predicate(Code, Head),
	qualify_head(Head, QHead),
	D = format_predicate(Code, QHead),
	feedback('~n~t~8|~w ', [D]),
	'$add_directive_wic'(D),
	fail.
save_format_predicates.

qualify_head(T, T) :-
	functor(T, :, 2), !.
qualify_head(T, user:T).


		 /*******************************
		 *	 FOREIGN LIBRARIES	*
		 *******************************/

%%	save_foreign_libraries(+Archive, +Options) is det.
%
%	Save current foreign libraries into the archive.

save_foreign_libraries(RC, Options) :-
	option(foreign(save), Options), !,
	feedback('~nFOREIGN LIBRARIES~n', []),
	forall(current_foreign_library(FileSpec, _Predicates),
	       ( find_foreign_library(FileSpec, File),
		 term_to_atom(FileSpec, Name),
		 '$rc_append_file'(RC, Name, shared, none, File)
	       )).
save_foreign_libraries(_, _).

%%	find_foreign_library(+FileSpec, -File) is det.
%
%	Find the shared object specified by   FileSpec.  If posible, the
%	shared object is stripped to reduce   its size. This is achieved
%	by calling strip -o <tmp> <shared-object>. Note that the file is
%	a Prolog tmp file and will be deleted on halt.
%
%	@bug	Should perform OS search on failure

find_foreign_library(FileSpec, SharedObject) :-
	absolute_file_name(FileSpec,
			   [ file_type(executable),
			     file_errors(fail)
			   ], File), !,
	(   absolute_file_name(path(strip), Strip,
			       [ access(execute),
				 file_errors(fail)
			       ]),
	    tmp_file(shared, Stripped),
	    format(atom(Cmd), '"~w" -o "~w" "~w"',
		   [ Strip, Stripped, File ]),
	    shell(Cmd)
	->  SharedObject = Stripped,
	    Delete = true
	;   SharedObject = File,
	    Delete = false
	).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

open_map(Options) :-
	option(map(Map), Options), !,
	open(Map, write, Fd),
	asserta(verbose(Fd)).
open_map(_) :-
	retractall(verbose(_)).

close_map :-
	retract(verbose(Fd)),
	close(Fd), !.
close_map.

feedback(Fmt, Args) :-
	verbose(Fd), !,
	format(Fd, Fmt, Args).
feedback(_, _).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Option checking and exception generation.  This should be in a library!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

option_type(Name,	 integer) :- min_stack(Name, _MinValue).
option_type(class,	 oneof([runtime,kernel,development])).
option_type(autoload,	 boolean).
option_type(map,	 atom).
option_type(op,		 oneof([save, standard])).
option_type(stand_alone, boolean).
option_type(foreign,	 oneof([save, no_save])).
option_type(goal,	 callable).
option_type(toplevel,	 callable).
option_type(init_file,	 atom).
option_type(emulator,	 ground).

check_options([]) :- !.
check_options([Var|_]) :-
	var(Var), !,
	throw(error(domain_error(save_options, Var), _)).
check_options([Name=Value|T]) :- !,
	(   option_type(Name, Type)
	->  (   must_be(Type, Value)
	    ->  check_options(T)
	    ;	throw(error(domain_error(Type, Value), _))
	    )
	;   throw(error(domain_error(save_option, Name), _))
	).
check_options([Term|T]) :-
	Term =.. [Name,Arg], !,
	check_options([Name=Arg|T]).
check_options([Var|_]) :-
	throw(error(domain_error(save_options, Var), _)).
check_options(Opt) :-
	throw(error(domain_error(list, Opt), _)).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:message/3.

prolog:message(no_resource(Name, Class, File)) -->
	[ 'Could not find resource ~w/~w on ~w or system resources'-
	  [Name, Class, File] ].
