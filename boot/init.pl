/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Get the Ball Rolling ...
*/

/*
Consult, derivates and basic things.   This  module  is  loaded  by  the
C-written  bootstrap  compiler.

The $:- directive  is  executed  by  the  bootstrap  compiler,  but  not
inserted  in  the  intermediate  code  file.   Used  to print diagnostic
messages and start the Prolog defined compiler for  the  remaining  boot
modules.

If you want  to  debug  this  module,  put  a  '$:-'  trace.   directive
somewhere.   The  tracer will work properly under boot compilation as it
will use the C defined write predicate  to  print  goals  and  does  not
attempt to call the Prolog defined trace interceptor.
*/

'$:-' format('Loading boot file ...~n', []).

		/********************************
		*    LOAD INTO MODULE SYSTEM	*
		********************************/

:- $set_source_module(_, system).

		/********************************
		*          DIRECTIVES           *
		*********************************/

op(_, _, []) :- !.
op(Priority, Type, [Name|Rest]) :- !,
	$op(Priority, Type, Name),
	op(Priority, Type, Rest).
op(Priority, Type, Name) :-
	$op(Priority, Type, Name).

dynamic((Pred/Arity, More)) :- !,
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, (dynamic), 1),
	dynamic(More).
dynamic(Pred/Arity) :-
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, (dynamic), 1).

multifile((Pred/Arity, More)) :- !,
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, (multifile), 1),
	multifile(More).
multifile(Pred/Arity) :-
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, (multifile), 1).

module_transparent((Pred/Arity, More)) :- !,
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, transparent, 1),
	module_transparent(More).
module_transparent(Pred/Arity) :-
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, transparent, 1).

discontiguous((Pred/Arity, More)) :- !,
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, (discontiguous), 1),
	discontiguous(More).
discontiguous(Pred/Arity) :-
	functor(Term, Pred, Arity),
	$set_predicate_attribute(Term, (discontiguous), 1).

:- module_transparent
	(dynamic)/1,
	(multifile)/1,
	(module_transparent)/1,
	(discontiguous)/1,
	$hide/2,
	$show_childs/2.


		/********************************
		*        TRACE BEHAVIOUR        *
		*********************************/

%	$hide(+Name, +Arity)
%	Predicates protected this way are never visible in the tracer.

$hide(Name, Arity) :-
	functor(Head, Name, Arity),
	$set_predicate_attribute(Head, trace, 0).

%	$show_childs(+Name, +Arity)
%	Normally system predicates hide their childs frames if these are
%	system predicates as well.  $show_childs suppresses this.

$show_childs(Name, Arity) :-  
	functor(Head, Name, Arity),
        $set_predicate_attribute(Head, hide_childs, 0).

		/********************************
		*       CALLING, CONTROL        *
		*********************************/

:- module_transparent
	';'/2,
	'|'/2,
	','/2,
	call/1,
	(^)/2,
	(not)/1,
	(\+)/1,
	(->)/2,
	once/1,
	ignore/1,
	block/3,
	apply/2.

%   ->/2, ;/2, |/2 and \+/1 are normally compiled. These predicate catch them
%   in case they are called via the meta-call predicates.

(If -> Then) :- If, !, Then.

(If -> Then; _Else) :- If, !, Then.
(_If -> _Then; Else) :- !, Else.
';'(Goal, _) :- Goal.
';'(_, Goal) :- Goal.

(If -> Then | _Else) :- If, !, Then.
(_If -> _Then | Else) :- !, Else.
'|'(Goal, _) :- Goal.
'|'(_, Goal) :- Goal.

','(Goal1, Goal2) :-			% Puzzle for beginners!
	Goal1,
	Goal2.

call(Goal) :-
	Goal.

not(Goal) :-
	\+ Goal.

%	This version of not is compiled as well. For meta-calls only

\+ Goal :-
	\+ Goal.

%	once/1 can normally be replaced by ->/2. For historical reasons
%	only.

once(Goal) :-
	Goal, !.

ignore(Goal) :-
	Goal, !.
ignore(_Goal).

apply(Pred, Arguments) :-
	$apply(Pred, Arguments).		% handled by the compiler

_Var^Goal :-					% setof/3, bagof/3
	Goal.

%	block/3, !/1, exit/2, fail/1
%	`longjmp' like control-structures.  See manual.  The predicate
%	system:block/3 is used by the VMI's I_CUT_BLOCK and B_EXIT.
%	$exit and $cut are interpreted by the compiler/decompiler,
%	just like $apply/2.

block(_Label, Goal, _RVal) :-
	Goal.

!(Label) :-
	$cut(Label).				% handled by compiler

exit(Label, RVal) :-
	$exit(Label, RVal).			% handled by compiler

fail(Label) :-
	$cut(Label),				% handled by compiler
	fail.

:-
	$hide((';'), 2),
	$hide(('|'), 2),
	$hide((','), 2),
	$hide((->), 2),
	$show_childs(^, 2),
	$show_childs(call, 1),
	$show_childs(not, 1),
	$show_childs(\+, 1),
	$show_childs(once, 1),
	$show_childs(ignore, 1), 	
	$show_childs((','), 2), 	
	$show_childs((';'), 2), 	
	$show_childs(('|'), 2),
	$show_childs(block, 3),
	$show_childs((->), 2).


		/********************************
		*            MODULES            *
		*********************************/

%	$prefix_module(+Module, +Context, +Term, -Prefixed)
%	Tags `Term' with `Module:' if `Module' is not the context module.

$prefix_module(Module, Module, Head, Head) :- !.
$prefix_module(Module, _, Head, Module:Head).


		/********************************
		*      TRACE AND EXCEPTIONS     *
		*********************************/

:- user:dynamic((prolog_trace_interception/3, exception/3)).
:- user:multifile((prolog_trace_interception/3,	exception/3)).

:- user:$hide($prolog_trace_interception, 2),
   user:$hide(prolog_trace_interception, 3).	

$map_trace_action(continue, 	0).
$map_trace_action(retry, 	1).
$map_trace_action(fail, 	2).

%	This function is called from C by the tracer. Allows the user
%	to intercept the tracer. If this predicate fails, the C tracer
%	takes over.

$prolog_trace_interception(Port, Frame) :-
	user:prolog_trace_interception(Port, Frame, Action),
	$map_trace_action(Action, Int), !,
	$trace_continuation(Int).

%	This function is called from C on undefined predicates.  First
%	allows the user to take care of it using exception/3. Else try
%	to give a DWIM warning. Otherwise fail. C will print an error
%	message.

:- flag($verbose_autoload, _, off).
:- flag($enable_autoload, _, on).
:- flag($autoloading, _, 0).

$undefined_procedure(Module, Name, Arity) :-
	$prefix_module(Module, user, Name/Arity, Pred),
	user:exception(undefined_predicate, Pred, Action),
	$map_trace_action(Action, Int), !,
	$trace_continuation(Int).
$undefined_procedure(Module, Name, Arity) :-
	flag($enable_autoload, on, on),
	$find_library(Module, Name, Arity, LoadModule, Library),
	functor(Head, Name, Arity),
	flag($autoloading, Old, Old+1),
	(   Module == LoadModule
	->  ignore(ensure_loaded(Library))
	;   (   $c_current_predicate(_, LoadModule:Head)
	    ->	Module:import(LoadModule:Head)
	    ;	ignore(Module:use_module(Library, [Name/Arity]))
	    )
	),
	flag($autoloading, _, Old),
	$c_current_predicate(_, Module:Head), !,
	$map_trace_action(retry, Int),
	$trace_continuation(Int).
$undefined_procedure(Module, Name, Arity) :-
	$prefix_module(Module, user, Name, MName),
	findall(Dwim, dwim_predicate(MName, Dwim), Dwims),
	Dwims \== [],
	functor(Goal, Name, Arity),
	$prefix_module(Module, user, Goal, Pred),
	$warn_undefined(Pred, Dwims),
	trace,
	$map_trace_action(fail, Int),
	$trace_continuation(Int).


		/********************************
		*        SYSTEM MESSAGES        *
		*********************************/

%	$ttyformat(+Format, [+ArgList])
%	Format on the user stream.  Used to print system messages.

$ttyformat(Format) :-
	$ttyformat(Format, []).

$ttyformat(Format, Args) :-
	format(user_output, Format, Args).

%	$confirm(Format, Args)
%
%	Ask the user to confirm a question.

$confirm(Format, Args) :-
	$ttyformat(Format, Args),
	$ttyformat('? '),
	between(0, 5, _),
	    (   get_single_char(Answer),
		memberchk(Answer, [0'y, 0'Y, 0'j, 0'J, 0'n, 0'N, 0' ,10])
	    ->  !, $confirm_(Answer)
	    ;   $ttyformat('Please answer ''y'' or ''n''~n'),
		fail
	    ).

$confirm_(Answer) :-
	memberchk(Answer, [0'y, 0'Y, 0'j, 0'J, 0' ,10]), !,
	(   $tty
	->  $ttyformat('yes~n')
	;   true
	).
$confirm_(_) :-
	$tty,
	$ttyformat('no~n'),
	fail.

%	$warning(+Format, [+ArgList])
%	Format a standard warning to the user and start the tracer.

$warning(Format) :-
	$warning(Format, []).
$warning(Format, Args) :-
	source_location(File, Line), !,
	sformat(Msg, Format, Args),
	(   user:exception(warning, warning(File, Line, Msg), _)
	->  true
	;   format(user_error, '[WARNING: (~w:~d)~n~t~8|~w]~n',
		   [File, Line, Msg])
	).
$warning(Format, Args) :-
	format(user_error, '[WARNING: ', []), 
	format(user_error, Format, Args), 
	format(user_error, ']~n', []),
	trace.


%	$warn_undefined(+Goal, +Dwims)
%	Tell the user that the predicate implied by `Goal' does not exists,
%	If there are alternatives (DWIM) tell the user about them.

:- module_transparent
	$warn_undefined/2,
	$write_alternatives/1,
	$predicate_name/2.

$warn_undefined(Goal, Dwims) :-
	$predicate_name(Goal, Name),
	$ttyformat('[WARNING: Undefined predicate: `~w''', [Name]),
	(   Dwims == []
	;   $ttyformat('~nHowever there are definitions for:'), 
	    $write_alternatives(Dwims)
	), !,
	$ttyformat(']~n').

$write_alternatives([]) :- !.
$write_alternatives([Dwim|Rest]) :-
	$predicate_name(Dwim, Name), 
	$ttyformat('~n~t~8|~w', [Name]), 
	$write_alternatives(Rest).

%	$predicate_name(+Head, -String)
%	Convert `Head' into a predicate name.

$predicate_name(Goal, String) :-
	$strip_module(Goal, Module, Head), 
	functor(Head, Name, Arity), 
	(   memberchk(Module, [user, system])
	->  sformat(String, '~w/~w',	[Name, Arity])
	;   sformat(String, '~w:~w/~w',	[Module, Name, Arity])
	).


		 /*******************************
		 *	 FILE_SEARCH_PATH	*
		 *******************************/

:- user:dynamic file_search_path/2.
:- user:multifile file_search_path/2.

:- user:assert((file_search_path(library, Dir) :-
	library_directory(Dir))).

expand_file_search_path(Spec, Expanded) :-
	functor(Spec, Alias, 1),
	user:file_search_path(Alias, Exp0),
	expand_file_search_path(Exp0, Exp1),
	arg(1, Spec, Base),
	$make_path(Exp1, Base, Expanded).
expand_file_search_path(Spec, Spec) :-
	atomic(Spec).

$make_path(Dir, File, Path) :-
	concat(_, /, Dir), !,
	concat(Dir, File, Path).
$make_path(Dir, File, Path) :-
	$concat_atom([Dir, '/', File], Path).


		/********************************
		*         FILE CHECKING         *
		*********************************/

%	File is a specification of a Prolog source file. Return the full
%	path of the file.

$check_file(0, _) :- !, fail.			% deal with variables
$check_file(user, user) :- !.
$check_file(File, Absolute) :-
	flag($compiling, database, database), !,
	$chk_file(File, ['.qlf', '.pl', ''], Absolute).
$check_file(File, Absolute) :-
	$chk_file(File, ['.pl', ''], Absolute).

$chk_file(Spec, Extensions, FullName) :-
	functor(Spec, Alias, 1),
	user:file_search_path(Alias, _), !,
	$chk_alias_file(Spec, Extensions, FullName).
$chk_file(Term, Ext, FullName) :-	% allow a/b, a-b, etc.
	\+ atomic(Term), !,
	term_to_atom(Term, Raw),
	name(Raw, S0),
	delete(S0, 0' , S1),
	name(Atom, S1),
	$chk_file(Atom, Ext, FullName).
$chk_file(File, Exts, FullName) :-
	is_absolute_file_name(File), !,
	member(Ext, Exts),
	ensure_extension(File, Ext, Extended),
	exists_file(Extended),
	$absolute_file_name(Extended, FullName).
$chk_file(File, Exts, FullName) :-
	source_location(ContextFile, _Line),
	$file_dir_name(ContextFile, ContextDir), !,
	$concat_atom([ContextDir, /, File], AbsFile),
	member(Ext, Exts),
	ensure_extension(AbsFile, Ext, Extended),
	exists_file(Extended),
	$absolute_file_name(Extended, FullName).
$chk_file(File, Exts, FullName) :-
	member(Ext, Exts),
	ensure_extension(File, Ext, Extended),
	exists_file(Extended),
	$absolute_file_name(Extended, FullName).

ensure_extension(File, Ext, Extended) :-
	(   concat(_, Ext, File)
	->  Extended = File
	;   concat(File, Ext, Extended)
	).

:- dynamic
	$search_path_file_cache/3.

$chk_alias_file(Spec, Exts, FullFile) :-
	$search_path_file_cache(Spec, FullFile, Ext),
	memberchk(Ext, Exts).
$chk_alias_file(Spec, Exts, FullFile) :-
	expand_file_search_path(Spec, Expanded),
	member(Ext, Exts),
	ensure_extension(Expanded, Ext, LibFile),
	exists_file(LibFile),
	\+ exists_directory(LibFile),
	$absolute_file_name(LibFile, FullFile),
	asserta($search_path_file_cache(Spec, FullFile, Ext)).
	

		/********************************
		*            CONSULT            *
		*********************************/

:- user:(dynamic
	 	library_directory/1,
		$start_compilation/2,
		$end_compilation/2).
:- user:(multifile
	 	library_directory/1,
		$start_compilation/2,
		$end_compilation/2).


:-	flag($break_level,	_, 0),
	flag($compiling,	_, database),
	flag($preprocessor,	_, none),
	prompt(_, '|: ').

%	compiling
%	Is true if SWI-Prolog is generating an intermediate code file

compiling :-
	\+ flag($compiling, database, database).

:- module_transparent
	$ifcompiling/1.

$ifcompiling(_) :-
	flag($compiling, database, database), !.
$ifcompiling(G) :-
	G.

		/********************************
		*         PREPROCESSOR          *
		*********************************/

preprocessor(Old, New) :-
	flag($preprocessor, Old, New).

$open_source(File, Goal) :-
	preprocessor(none, none), !,
	seeing(Old), see(File),
	$open_source_call(File, Goal, True),
	seen, see(Old),
	True == yes.
$open_source(File, Goal) :-
	preprocessor(Pre, Pre),
	(   $substitute_atom('%f', File, Pre, Command)
	->  seeing(Old), see(pipe(Command)),
	    $open_source_call(File, Goal, True),
	    seen, see(Old), !,
	    True == yes
	;   $warning('Illegal preprocessor specification: `~w''', [Pre]),
	    fail
	).


$open_source_call(File, Goal, Status) :-
	flag($compilation_level, Level, Level+1),
	ignore(user:$start_compilation(File, Level)),
	(   Goal
	->  Status = yes
	;   Status = no
	),
	ignore(user:$end_compilation(File, Level)),
	flag($compilation_level, _, Level).


$substitute_atom(Old, New, Org, Result) :-
	name(Old, OS),
	name(New, NS),
	name(Org, OrgS),
	append(Before, Rest, OrgS),
	append(OS, After, Rest), !,
	append(Before, NS, R1),
	append(R1, After, R2), !,
	name(Result, R2).


		/********************************
		*       LOAD PREDICATES         *
		*********************************/

:- module_transparent
	ensure_loaded/1,
	'.'/2,
	consult/1,
	use_module/1,
	use_module/2,
	$use_module/3,
	$ensure_loaded/2,
	$consult_file/2.

%	ensure_loaded(+File|+ListOfFiles)
%	
%	Load specified files, provided they where not loaded before. If the
%	file is a module file import the public predicates into the context
%	module.

ensure_loaded([]) :- !.
ensure_loaded([Spec|Rest]) :- !,
	ensure_loaded(Spec),
	ensure_loaded(Rest).
ensure_loaded(Spec) :-
	$strip_module(Spec, _, File),
	(   $check_file(File, FullFile)
	->  $ensure_loaded(Spec, FullFile)
	;   $warning('ensure_loaded/1: No such file: ~w', Spec),
	    fail
	).

$ensure_loaded(_Spec, FullFile) :-
	source_file(FullFile), !.
$ensure_loaded(Spec, _) :-
	$consult_file(Spec, [verbose]).

%	use_module(+File|+ListOfFiles)
%	
%	Very similar to ensure_loaded/1, but insists on the loaded file to
%	be a module file. If the file is already imported, but the public
%	predicates are not yet imported into the context module, then do
%	so.

use_module([]) :- !.
use_module([Spec|Rest]) :- !,
	use_module(Spec),
	use_module(Rest).
use_module(Spec) :-
	use_module(Spec, all).

%	use_module(+File, +ImportList)
%	
%	As use_module/1, but takes only one file argument and imports only
%	the specified predicates rather than all public predicates.

use_module(File, ImportList) :-
	$use_module(File, ImportList, [verbose]).

$use_module(Spec, Import, _) :-
	$strip_module(Spec, _, File),
	$check_file(File, FullFile),
	$current_module(Module, FullFile), !,
	context_module(Context),
	$import_list(Context, Module, Import).	
$use_module(Spec, Import, Options) :-
	$consult_file(Spec, [import = Import, is_module | Options]).
	

[F|R] :-
	consult([F|R]).
[].

consult([]) :- !.
consult([File|Rest]) :- !,
	consult(File),
	consult(Rest).
consult(Spec) :-
	$consult_file(Spec, [verbose]).


%	Compilation extensions

$compiler_extension('.qlf', $qload_file).
$compiler_extension('',  $consult_file).

$consult_goal(Path, Goal) :-
	$compiler_extension(Ext, Goal),
	concat(_, Ext, Path), !.


%	$consult_file(+File, +Options)
%	
%	Common entry for all the consult derivates.  File is the raw user
%	specified file specification, possibly tagged with the module.
%	
%	`Options' is a list of additional options.  Defined values are
%
%	    verbose		Print statistics on user channel
%	    is_module		File MUST be a module file
%	    import = List	List of predicates to import

$consult_file(Spec, Options) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),
 
	(memberchk(import = Import, Options) -> true ; Import = all),
	(memberchk(is_module, Options) -> IsModule = true ; IsModule = false),

	$strip_module(Spec, Module, File),
	(   $check_file(File, Absolute),
	    $consult_goal(Absolute, Goal),
	    $apply(Goal, [Absolute, Module, Import, IsModule, Action, LM])
	->  true
	;   $warning('No such file: ~w', Spec),
	    fail
	),

	(   memberchk(verbose, Options),
	    (flag($autoloading, 0, 0) ; flag($verbose_autoload, on, on))
	->  statistics(heapused, Heap),
	    statistics(cputime, Time),
	    HeapUsed is Heap - OldHeap,
	    TimeUsed is Time - OldTime,
	    $confirm_file(File, Absolute, ConfirmFile),
	    $confirm_module(LM, ConfirmModule),

	    $ttyformat('~N~w ~w~w, ~2f sec, ~D bytes.~n',
		       [ConfirmFile, Action, ConfirmModule,
			TimeUsed, HeapUsed])
	;   true
	).

$confirm_file(library(_), Absolute, Absolute) :- !.
$confirm_file(File, _, File).

$confirm_module(user, '') :- !.
$confirm_module(Module, Message) :-
	atom(Module), !,
	concat(' into ', Module, Message).
$confirm_module(_, '').

$read_clause(Clause) :-				% get the first non-syntax
	repeat,					% error
	    read_clause(Clause), !.

$consult_file(Absolute, Module, Import, IsModule, What, LM) :-
	$set_source_module(Module, Module), !, % same module
	$consult_file_2(Absolute, Module, Import, IsModule, What, LM).
$consult_file(Absolute, Module, Import, IsModule, What, LM) :-
	$set_source_module(OldModule, Module),
	$ifcompiling($qlf_start_sub_module(Module)),
        $consult_file_2(Absolute, Module, Import, IsModule, What, LM),
	$ifcompiling($qlf_end_part),
	$set_source_module(_, OldModule).

$consult_file_2(Absolute, Module, Import, IsModule, What, LM) :-
	$set_source_module(OldModule, Module),	% Inform C we start loading
	$start_consult(Absolute),
	$compile_type(What),
	(   flag($compiling, wic, wic)	% TBD
	->  $add_directive_wic($assert_load_context_module(Absolute,OldModule))
	;   true
	),
	$assert_load_context_module(Absolute, OldModule),

	$style_check(OldStyle, OldStyle),	% Save style parameters
	$open_source(Absolute, (		% Load the file
	    $read_clause(First),
	    $load_file(First, Absolute, Import, IsModule, LM))),
	$style_check(_, OldStyle),		% Restore old style
	$set_source_module(_, OldModule).	% Restore old module

$compile_type(What) :-
	flag($compiling, How, How),
	(   How == database
	->  What = compiled
	;   How == qlf
	->  What = '*qcompiled*'
	;   What = 'boot compiled'
	).

%	$load_context_module(+File, -Module)
%	Record the module a file was loaded from (see make/0)

$load_context_module(File, Module) :-
	recorded($load_context_module, File/Module, _).

$assert_load_context_module(File, Module) :-
	recorded($load_context_module, File/Module, _), !.
$assert_load_context_module(File, Module) :-
	recordz($load_context_module, File/Module, _).

%   $load_file(+FirstTerm, +Path, +Import, +IsModule, -Module)
%
%   $load_file5 does the actual loading. The first term has already been
%   read as this may be the module declaraction.

$load_file((?- module(Module, Public)), File, all, _, Module) :- !,
	$load_module(Module, Public, Public, File).
$load_file((:- module(Module, Public)), File, all, _, Module) :- !,
	$load_module(Module, Public, Public, File).
$load_file((?- module(Module, Public)), File, Import, _, Module) :- !,
	$load_module(Module, Public, Import, File).
$load_file((:- module(Module, Public)), File, Import, _, Module) :- !,
	$load_module(Module, Public, Import, File).
$load_file(_, File, _, true, _) :- !,
	$warning('use_module: ~w is not a module file', [File]),
	fail.
$load_file(end_of_file, _, _, _, Module) :- !,		% empty file
	$set_source_module(Module, Module).
$load_file(FirstClause, File, _, false, Module) :- !,
	$set_source_module(Module, Module),
	$ifcompiling($qlf_start_file(File)),
	ignore($consult_clause(FirstClause, File)),
	repeat,
	    read_clause(Clause),
	    $consult_clause(Clause, File), !,
	$ifcompiling($qlf_end_part).


$reserved_module(system).
$reserved_module(user).

$load_module(Reserved, _, _, _) :-
	$reserved_module(Reserved), !,
	$warning('Cannot load into module "~w": reserved module name',
		 [Reserved]),
	fail.
$load_module(Module, Public, Import, File) :-
	$set_source_module(OldModule, OldModule),
	$declare_module(Module, File),
	$export_list(Module, Public),
	$ifcompiling($qlf_start_module(Module)),

	repeat,
	    read_clause(Clause),
	    $consult_clause(Clause, File), !,

	Module:$check_export,
	$ifcompiling($qlf_end_part),
	$import_list(OldModule, Module, Import).


$import_list(_, _, []) :- !.
$import_list(Module, Source, [Name/Arity|Rest]) :- !,
	functor(Term, Name, Arity),
	$import_wic(Source, Term),
	ignore(Module:import(Source:Term)),
	$import_list(Module, Source, Rest).
$import_list(Context, Module, all) :- !,
	export_list(Module, Exports),
	$import_all(Exports, Context, Module).


$import_all([], _, _).
$import_all([Head|Rest], Context, Source) :-
	ignore(Context:import(Source:Head)),
	$import_wic(Source, Head),
	$import_all(Rest, Context, Source).


$export_list(_, []) :- !.
$export_list(Module, [Name/Arity|Rest]) :- !,
	functor(Term, Name, Arity),
	export(Module:Term),
	$export_list(Module, Rest).
$export_list(Module, [Term|Rest]) :-
	$warning('Illegal predicate specification in public list: `~w''',
		 [Term]),
	$export_list(Module, Rest).

$consult_clause(end_of_file, _) :- !.
$consult_clause(Clause, File) :-
	expand_term(Clause, Expanded),
	$store_clause(Expanded, File), !,
	fail.

$execute_directive(Goal) :-
	compiling, !,
	$add_directive_wic2(Goal, Type),
	(   Type == call		% suspend compiling into .qlf file
	->  flag($compiling, Old, database),
	    (	$execute_directive2(Goal)
	    ->	flag($compiling, _, Old)
	    ;	flag($compiling, _, Old),
		fail
	    )
	;   $execute_directive2(Goal)
	).
$execute_directive(Goal) :-
	$execute_directive2(Goal).

$execute_directive2(Goal) :-
	$set_source_module(Module, Module),
	Module:Goal, !.
$execute_directive2(Goal) :-
	$set_source_module(Module, Module),
	(   Module == user
	->  $warning('Directive failed: ~w', [Goal])
	;   $warning('Directive failed: ~w:~w', [Module, Goal])
        ),
	fail.

%	Note that the list, consult and ensure_loaded directives are already
%	handled at compile time and therefore should not go into the
%	intermediate code file.

$add_directive_wic2(Goal, Type) :-
	$common_goal_type(Goal, Type), !,
	(   Type == load
	->  true
	;   $set_source_module(Module, Module),
	    $add_directive_wic(Module:Goal)
	).
$add_directive_wic2(Goal, _) :-
	(   flag($compiling, qlf, qlf)	% no problem for qlf files
	->  true
	;   $warning('Cannot compile mixed loading/calling directives: ~w',
		     [Goal])
	).
	
$common_goal_type((A,B), Type) :- !,
	$common_goal_type(A, Type),
	$common_goal_type(B, Type).
$common_goal_type((A;B), Type) :- !,
	$common_goal_type(A, Type),
	$common_goal_type(B, Type).
$common_goal_type((A->B), Type) :- !,
	$common_goal_type(A, Type),
	$common_goal_type(B, Type).
$common_goal_type(Goal, Type) :-
	$goal_type(Goal, Type).

$goal_type(Goal, Type) :-
	(   $load_goal(Goal)
	->  Type = load
	;   Type = call
	).

$load_goal([_|_]).
$load_goal(consult(_)).
$load_goal(ensure_loaded(_)) :- flag($compiling, wic, wic).
$load_goal(use_module(_))    :- flag($compiling, wic, wic).
$load_goal(use_module(_, _)) :- flag($compiling, wic, wic).

		/********************************
		*        TERM EXPANSION         *
		*********************************/

:- user:dynamic(term_expansion/2).
:- user:multifile(term_expansion/2).

expand_term(Term, Expanded) :-
	user:term_expansion(Term, Expanded), !.
expand_term(Term, Expanded) :-
	$translate_rule(Term, Expanded), !.
expand_term(Term, Term).

$store_clause([], _) :- !.
$store_clause([C|T], F) :- !,
	$store_clause(C, F),
	$store_clause(T, F).
$store_clause((:- Goal), _) :- !,
	$execute_directive(Goal).
$store_clause((?- Goal), _) :- !,
	$execute_directive(Goal).
$store_clause((_, _), _) :- !,
	$warning('Full stop in clause body? (attempt to define ,/2)').
$store_clause((_:-B), _) :-
	nonvar(B), B = (_:-_), !,
	$warning('Clause not closed by `.''? (attempt to call :-/2)').
$store_clause(Term, File) :-
	$record_clause(Term, File, Ref),
        $ifcompiling($qlf_assert_clause(Ref)).

		 /*******************************
		 *	 FOREIGN INTERFACE	*
		 *******************************/

%	call-back from PL_register_foreign().  First argument is the module
%	into which the foreign predicate is loaded and second is a term
%	describing the arguments.

:- dynamic
	$foreign_registered/2.


		/********************************
		*        GRAMMAR RULES          *
		*********************************/

/*  Original version by Fernando Pereira, Edinburgh, 1984

 ** Thu Sep  1 15:57:59 1988  jan@swivax.UUCP (Jan Wielemaker)  */

$translate_rule((LP-->List), H) :-
	nonvar(List),
	(  List = []
	-> $t_head(LP, S, S, H)
	;  List = [X]
	-> $t_head(LP, [X|S], S, H)
	;  List = [_|_]
	-> append(List, SR, S),
	   $extend([S, SR], LP, H)
	).
$translate_rule((LP-->RP), (H:-B)):-
	$t_head(LP, S, SR, H),
	$t_body(RP, S, SR, B1),
	$t_tidy(B1, B).


$t_head((LP, List), S, SR, H):- !,
	append(List, SR, List2),
	$extend([S, List2], LP, H).
$t_head(LP, S, SR, H) :-
	$extend([S, SR], LP, H).


$t_body(Var, S, SR, $apply(Var, [S, SR])) :-
	var(Var), !.
$t_body(!, S, S, !) :- !.
$t_body([], S, S1, S=S1) :- !.
$t_body([X], S, SR, $char(S, X, SR)) :- !.
$t_body([X|R], S, SR, ($char(S, X, SR1), RB)) :- !,
	$t_body(R, SR1, SR, RB).
$t_body({T}, S, S, T) :- !.
$t_body((T, R), S, SR, (Tt, Rt)) :- !,
	$t_body(T, S, SR1, Tt),
	$t_body(R, SR1, SR, Rt).
$t_body((T;R), S, SR, (Tt;Rt)) :- !,
	$t_body(T, S, S1, T1), $t_fill(S, SR, S1, T1, Tt),
	$t_body(R, S, S2, R1), $t_fill(S, SR, S2, R1, Rt).
$t_body((T|R), S, SR, (Tt;Rt)) :- !,
	$t_body(T, S, S1, T1), $t_fill(S, SR, S1, T1, Tt),
	$t_body(R, S, S2, R1), $t_fill(S, SR, S2, R1, Rt).
$t_body((C->T;E), S, SR, (Ct->Tt;Et)) :- !,
	$t_body(C, S, S1, Ct),
	$t_body(T, S1, S2, T1), $t_fill(S, SR, S2, T1, Tt),
	$t_body(E, S1, S3, E1), $t_fill(S, SR, S3, E1, Et).
$t_body((C->T|E), S, SR, (Ct->Tt;Et)) :- !,
	$t_body(C, S, S1, Ct),
	$t_body(T, S1, S2, T1), $t_fill(S, SR, S2, T1, Tt),
	$t_body(E, S1, S3, E1), $t_fill(S, SR, S3, E1, Et).
$t_body((C->T), S, SR, (Ct->Tt)) :- !,
	$t_body(C, S, SR1, Ct),
	$t_body(T, SR1, SR, Tt).
$t_body(T, S, SR, Tt) :-
	$extend([S, SR], T, Tt).


$t_fill(S, SR, S1, T, (T, SR=S)) :-
	S1 == S, !.
$t_fill(_S, SR, SR, T, T).


$extend(More, OldT, NewT) :-
	OldT =.. OldL,
	append(OldL, More, NewL),
	NewT =.. NewL.

$t_tidy(Var, Var) :-
 	var(Var), !.
$t_tidy((P1;P2), (Q1;Q2)) :- !,
	$t_tidy(P1, Q1),
	$t_tidy(P2, Q2).
$t_tidy(((Conj, P2), P3), Q) :-		% Fix my Michael Huebner: test nonvar!
        nonvar(Conj),
	Conj = (P1, P2),
	$t_tidy((P1, (P2, P3)), Q).
$t_tidy((P1, P2), (Q1, Q2)) :- !,
	$t_tidy(P1, Q1),
	$t_tidy(P2, Q2).
$t_tidy(A, A).

$char([X|S], X, S).

:- module_transparent
	phrase/2,
	phrase/3.

phrase(RuleSet, Input) :-
	phrase(RuleSet, Input, []).
phrase(RuleSet, Input, Rest) :-
	$apply(RuleSet, [Input, Rest]).


		/********************************
		*     WIC CODE COMPILER         *
		*********************************/

/*  This  entry  point  is  called  from  pl-main.c  if  the  -c  option
    (intermediate  code  compilation) is given.  It's job is simple: get
    the output file  and  input  files,  open  the  output  file,  setup
    intermediate  code  compilation  flag  and  finally just compile the
    input files.
*/

$compile_wic :-
	$argv(Argv),			% gets main() argv as a list of atoms
	$get_files_argv(Argv, Files),
	$get_wic_argv(Argv, Wic),
	$compile_wic(Files, Wic).

$compile_wic(FileList, Wic) :-
	$open_wic(Wic),
	flag($compiling, Old, wic),
	    $style_check(Style, Style),
	    $execute_directive($style_check(_, Style)),
	    user:consult(FileList),
	flag($compiling, _, Old),
	$close_wic.

$get_files_argv([], []) :- !.
$get_files_argv(['-c'|Files], Files) :- !.
$get_files_argv([_|Rest], Files) :-
	$get_files_argv(Rest, Files).

$get_wic_argv([], 'a.out').
$get_wic_argv(['-o', Wic|_], Wic) :- !.
$get_wic_argv([_|Rest], Wic) :-
	$get_wic_argv(Rest, Wic).


		/********************************
		*       LIST PROCESSING         *
		*********************************/

member(X, [X|_]).
member(X, [_|T]) :-
	member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).


		 /*******************************
		 *	       HALT		*
		 *******************************/

halt :-
	halt(0).


:- module_transparent
	at_halt/1.
:- dynamic
	$at_halt/1.

at_halt(Spec) :-
	$strip_module(Spec, Module, Goal),
	assert(system:$at_halt(Module:Goal)).

$run_at_halt :-
	$at_halt(Goal),
	Goal,
	fail ; true.


		/********************************
		*      LOAD OTHER MODULES       *
		*********************************/

:- module_transparent
	$load_wic_files/2,
	$load_additional_boot_files/0.

$load_wic_files(Module, Files) :-
	$execute_directive($set_source_module(OldM, Module)),
	$style_check(OldS, 2'1111),
	flag($compiling, OldC, wic),
	consult(Files),
	$execute_directive($set_source_module(_, OldM)),
	$execute_directive($style_check(_, OldS)),
	flag($compiling, _, OldC).


$load_additional_boot_files :-
	$argv(Argv),
	$get_files_argv(Argv, Files),
	(   Files \== []
	->  format('Loading additional boot files~n'),
	    $load_wic_files(user, Files),
	    format('additional boot files loaded~n')
	;   true
        ).


'$:-'	format('Loading Prolog startup files~n', []),
	source_location(File, _Line),
	$file_dir_name(File, Dir),
	concat(Dir, '/load.pl', LoadFile),
	$load_wic_files(system, [LoadFile]),
	format('SWI-Prolog boot files loaded~n', []),
	$execute_directive($style_check(_, 2'1011)),
	$execute_directive($set_source_module(_, user)).
