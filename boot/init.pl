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

dynamic((Spec, More)) :- !,
	dynamic(Spec),
	dynamic(More).
dynamic(Spec) :-
	$set_predicate_attribute(Spec, (dynamic), 1).

multifile((Spec, More)) :- !,
	multifile(Spec),
	multifile(More).
multifile(Spec) :-
	$set_predicate_attribute(Spec, (multifile), 1).

module_transparent((Spec, More)) :- !,
	module_transparent(Spec),
	module_transparent(More).
module_transparent(Spec) :-
	$set_predicate_attribute(Spec, transparent, 1).

discontiguous((Spec, More)) :- !,
	discontiguous(Spec),
	discontiguous(More).
discontiguous(Spec) :-
	$set_predicate_attribute(Spec, (discontiguous), 1).

volatile((Spec, More)) :- !,
	volatile(Spec),
	volatile(More).
volatile(Spec) :-
	$set_predicate_attribute(Spec, (volatile), 1).

:- module_transparent
	(dynamic)/1,
	(multifile)/1,
	(module_transparent)/1,
	(discontiguous)/1,
	(volatile)/1,
	$hide/2,
	$show_childs/2.


		/********************************
		*        TRACE BEHAVIOUR        *
		*********************************/

%	$hide(+Name, +Arity)
%	Predicates protected this way are never visible in the tracer.

$hide(Name, Arity) :-
	$set_predicate_attribute(Name/Arity, trace, 0).

%	$show_childs(+Name, +Arity)
%	Normally system predicates hide their childs frames if these are
%	system predicates as well.  $show_childs suppresses this.

$show_childs(Name, Arity) :-  
        $set_predicate_attribute(Name/Arity, hide_childs, 0).

		/********************************
		*       CALLING, CONTROL        *
		*********************************/

:- module_transparent
	';'/2,
	'|'/2,
	','/2,
	call/1,
	call/2,
	call/3,
	call/4,
	call/5,
	call/6,
	(^)/2,
	(not)/1,
	(\+)/1,
	(->)/2,
	(*->)/2,
	once/1,
	ignore/1,
	block/3,
	catch/3,
	apply/2.

%   ->/2, ;/2, |/2 and \+/1 are normally compiled. These predicate catch them
%   in case they are called via the meta-call predicates.

(If ->  Then) :- If, !, Then.
(If *-> Then) :- (If *-> Then ; fail).

(If ->  Then; Else) :- !, (If  -> Then ; Else).
(If *-> Then; Else) :- !, (If *-> Then ; Else).
(A ; B) :- (A ; B).

(If ->  Then| Else) :- !, (If  -> Then ; Else).
(If *-> Then| Else) :- !, (If *-> Then ; Else).
(A | B) :- (A ; B).

','(Goal1, Goal2) :-			% Puzzle for beginners!
	Goal1,
	Goal2.

call(Goal) :-				% make these available as predicates
	Goal.
call(G, A) :-
	call(G, A).
call(G, A, B) :-
	call(G, A, B).
call(G, A, B, C) :-
	call(G, A, B, C).
call(G, A, B, C, D) :-
	call(G, A, B, C, D).
call(G, A, B, C, D, E) :-
	call(G, A, B, C, D, E).

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

%	catch(:Goal, +Catcher, :Recover)
%	throw(+Exception)
%
%	ISO compliant exception handling.  $throw/1 is compiled to
%	the virtual instruction B_THROW.  See pl-wam.c for details.

catch(Goal, _Catcher, _Recover) :-
	Goal.

throw(Exception) :-
	$throw(Exception).

:-
	$hide((';'), 2),
	$hide(('|'), 2),
	$hide((','), 2),
	$hide((->), 2),
	$show_childs(^, 2),
	$show_childs(call, 1),
	$show_childs(call, 2),
	$show_childs(call, 3),
	$show_childs(call, 4),
	$show_childs(call, 5),
	$show_childs(call, 6),
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

:- user:dynamic((exception/3,
		 prolog_event_hook/1)).
:- user:multifile((exception/3,
		   prolog_event_hook/1)).

%	This function is called from C on undefined predicates.  First
%	allows the user to take care of it using exception/3. Else try
%	to give a DWIM warning. Otherwise fail. C will print an error
%	message.

:- set_feature(autoload, true).
:- set_feature(verbose_autoload, false).
:- flag($autoloading, _, 0).

$undefined_procedure(Module, Name, Arity, Action) :-
	$prefix_module(Module, user, Name/Arity, Pred),
	user:exception(undefined_predicate, Pred, Action), !.
$undefined_procedure(Module, Name, Arity, retry) :-
	feature(autoload, true),
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
	$c_current_predicate(_, Module:Head).
$undefined_procedure(_, _, _, fail).

$calleventhook(Term) :-
	(   notrace(user:prolog_event_hook(Term))
	->  true
	;   true
	).

:- $hide($calleventhook, 1).


		/********************************
		*        SYSTEM MESSAGES        *
		*********************************/

%	$ttyformat(+Format, [+ArgList])
%	Format on the user stream.  Used to print system messages.

$ttyformat(Format) :-
	$ttyformat(Format, []).
$ttyformat(Format, Args) :-
	format(user_error, Format, Args).

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
	(   feature(report_error, true)
	->  sformat(Msg, Format, Args),
	    (   user:exception(warning, warning(File, Line, Msg), _)
	    ->  true
	    ;   format(user_error, '[WARNING: (~w:~d)~n~t~8|~w]~n',
		       [File, Line, Msg])
	    )
	;   true
	).
$warning(Format, Args) :-
	(   feature(report_error, true)
	->  format(user_error, '[WARNING: ', []), 
	    format(user_error, Format, Args), 
	    format(user_error, ']~n', [])
	;   true
	),
	(   feature(debug_on_error, true)
	->  trace
	;   true
	).


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


:- dynamic
	user:portray/1.
:- multifile
	user:portray/1.
	

		 /*******************************
		 *	 FILE_SEARCH_PATH	*
		 *******************************/

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

user:(file_search_path(library, Dir) :-
	library_directory(Dir)).
user:file_search_path(swi, Home) :-
	feature(home, Home).
user:file_search_path(foreign, swi(ArchLib)) :-
	feature(arch, Arch),
	concat('lib/', Arch, ArchLib).
user:file_search_path(foreign, swi(lib)).

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

%	absolute_file_name(+Term, +Args, -AbsoluteFile)

absolute_file_name(Spec, Args, Path) :-
	(   select(Args, extensions(Exts), Conditions)
	->  true
	;   select(Args, file_type(Type), Conditions)
	->  $file_type_extensions(Type, Exts)
	;   Conditions = Args,
	    Exts = ['']
	),
	(   select(Conditions, solutions(Sols), C1)
	->  true
	;   Sols = first,
	    C1 = Conditions
	),
	(   select(C1, file_errors(FileErrors), C2)
	->  true
	;   FileErrors = error,
	    C2 = C1
	),
	(   $chk_file(Spec, Exts, C2, Path)
	*-> (   Sols == first
	    ->  !
	    ;   true
	    )
	;   (   FileErrors == fail
	    ->  fail
	    ;   throw(error(existence_error(source_sink, Spec), _))
	    )
	).

$file_type_extensions(Type, Exts) :-
	'$current_module'('$bags', _File), !,
	findall(Ext, user:prolog_file_type(Ext, Type), Exts0),
	append(Exts0, [''], Exts).
$file_type_extensions(prolog, [pl, '']). % findall is not yet defined ...

%	user:prolog_file_type/2
%
%	Define type of file based on the extension.  This is used by
%	absolute_file_name/3 and may be used to extend the list of
%	extensions used for some type.

:- multifile(user:prolog_file_type/2),
   dynamic(user:prolog_file_type/2).

user:prolog_file_type(pl,	prolog).
user:prolog_file_type(Ext,	prolog) :-
	feature(associate, Ext),
	Ext \== pl.
user:prolog_file_type(qlf,	qlf).
user:prolog_file_type(so,	executable) :-
	feature(open_shared_object, true).
user:prolog_file_type(dll,	executable) :-
	feature(dll, true).

%	File is a specification of a Prolog source file. Return the full
%	path of the file.

$chk_file(Spec, Extensions, Cond, FullName) :-
	$canonise_extensions(Extensions, Exts),
	$dochk_file(Spec, Exts, Cond, FullName).

$dochk_file(Spec, Extensions, Cond, FullName) :-
	functor(Spec, Alias, 1),
	user:file_search_path(Alias, _), !,
	$chk_alias_file(Spec, Extensions, Cond, FullName).
$dochk_file(Term, Ext, Cond, FullName) :-	% allow a/b, a-b, etc.
	\+ atomic(Term), !,
	term_to_atom(Term, Raw),
	atom_chars(Raw, S0),
	delete(S0, 0' , S1),
	atom_chars(Atom, S1),
	$dochk_file(Atom, Ext, Cond, FullName).
$dochk_file(File, Exts, Cond, FullName) :-
	is_absolute_file_name(File), !,
	$extend_file(File, Exts, Extended),
	$file_condition(Cond, Extended),
	$absolute_file_name(Extended, FullName).
$dochk_file(File, Exts, Cond, FullName) :-
	source_location(ContextFile, _Line),
	file_directory_name(ContextFile, ContextDir),
	$concat_atom([ContextDir, /, File], AbsFile),
	$extend_file(AbsFile, Exts, Extended),
	$file_condition(Cond, Extended), !,
	$absolute_file_name(Extended, FullName).
$dochk_file(File, Exts, Cond, FullName) :-
	$extend_file(File, Exts, Extended),
	$file_condition(Cond, Extended),
	$absolute_file_name(Extended, FullName).

:- dynamic
	$search_path_file_cache/4.
:- volatile
	$search_path_file_cache/4.

$chk_alias_file(Spec, Exts, Cond, FullFile) :-
	$search_path_file_cache(Spec, Cond, FullFile, Exts).
$chk_alias_file(Spec, Exts, Cond, FullFile) :-
	expand_file_search_path(Spec, Expanded),
	$extend_file(Expanded, Exts, LibFile),
	$file_condition(Cond, LibFile),
	$absolute_file_name(LibFile, FullFile),
	\+ $search_path_file_cache(Spec, Cond, FullFile, Exts),
	asserta($search_path_file_cache(Spec, Cond, FullFile, Exts)).
	
$file_condition([], _) :- !.
$file_condition([H|T], File) :- !,
	$file_condition(H, File),
	$file_condition(T, File).
$file_condition(exists, File) :- !,
	exists_file(File).
$file_condition(file_type(directory), File) :- !,
	exists_directory(File).
$file_condition(file_type(file), File) :- !,
	exists_file(File),
	\+ exists_directory(File).
$file_condition(access([A1|AT]), File) :- !,
	$file_condition(access(A1), File),
	$file_condition(access(AT), File).
$file_condition(access([]), _) :- !.
$file_condition(access(Access), File) :- !,
	access_file(File, Access).

$extend_file(File, Exts, FileEx) :-
	$ensure_extensions(Exts, File, Fs),
	$list_to_set(Fs, FsSet),
	member(FileEx, FsSet).
	
$ensure_extensions([], _, []).
$ensure_extensions([E|E0], F, [FE|E1]) :-
	file_name_extension(F, E, FE),
	$ensure_extensions(E0, F, E1).

$list_to_set([], []).
$list_to_set([H|T], R) :-
	memberchk(H, T), !, 
	$list_to_set(T, R).
$list_to_set([H|T], [H|R]) :-
	$list_to_set(T, R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Canonise the extension list. Old SWI-Prolog   require  `.pl', etc, which
the Quintus compatibility  requests  `pl'.   This  layer  canonises  all
extensions to .ext
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

$canonise_extensions([], []) :- !.
$canonise_extensions([H|T], [CH|CT]) :- !,
	$canonise_extension(H, CH),
	$canonise_extensions(T, CT).
$canonise_extensions(E, [CE]) :-
	$canonise_extension(E, CE).

$canonise_extension('', '') :- !.
$canonise_extension(DotAtom, DotAtom) :-
	concat('.', _, DotAtom), !.
$canonise_extension(Atom, DotAtom) :-
	concat('.', Atom, DotAtom).


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
	$load_file/2,
	load_files/1,
	load_files/2.

%	ensure_loaded(+File|+ListOfFiles)
%	
%	Load specified files, provided they where not loaded before. If the
%	file is a module file import the public predicates into the context
%	module.

ensure_loaded(Files) :-
	load_files(Files, [if(changed)]).

%	use_module(+File|+ListOfFiles)
%	
%	Very similar to ensure_loaded/1, but insists on the loaded file to
%	be a module file. If the file is already imported, but the public
%	predicates are not yet imported into the context module, then do
%	so.

use_module(Files) :-
	load_files(Files, [if(changed), must_be_module(true)]).

%	use_module(+File, +ImportList)
%	
%	As use_module/1, but takes only one file argument and imports only
%	the specified predicates rather than all public predicates.

use_module(Files, Import) :-
	load_files(Files, [ if(changed),
			    must_be_module(true),
			    imports(Import)]).

[F|R] :-
	consult([F|R]).
[].

consult(List) :-
	load_files(List).

%	$consult_goal(+Path, -Goal)
%
%	Determine how to load the indicated file

$consult_goal(Path, Goal) :-
	(   file_name_extension(_, Ext, Path),
	    user:prolog_file_type(Ext, qlf)
	->  Goal = $qload_file
	;   Goal = $consult_file
	).

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

load_files(Files) :-
	load_files(Files, []).
load_files(Files, Options) :-
	$strip_module(Files, Module, TheFiles),
        $load_files(TheFiles, Module, Options).

$load_files([], _, _) :- !.
$load_files([H|T], Module, Options) :- !,
	$load_file(Module:H, Options),
	$load_files(T, Module, Options).
$load_files(File, Module, Options) :-
	$load_file(Module:File, Options).


$get_option(Term, Options, Default) :-
	(   memberchk(Term, Options)
	->  true
	;   arg(1, Term, Default)
	).


$noload(true, _) :- !,
	fail.
$noload(not_loaded, FullFile) :-
	source_file(FullFile), !.
$noload(changed, FullFile) :-
	$time_source_file(FullFile, LoadTime),
        time_file(FullFile, Modified),
        Modified @=< LoadTime, !.

%	$qlf_file(+PlFile, -LoadFile)
%
%	Return the QLF file if it exists.  Might check for modification
%	time, version, etc.

$qlf_file(user, user) :- !.
$qlf_file(FullFile, QlfFile) :-
	flag($compiling, database, database),
	file_name_extension(Base, PlExt, FullFile),
	user:prolog_file_type(PlExt, prolog),
	user:prolog_file_type(QlfExt, qlf),
	file_name_extension(Base, QlfExt, QlfFile),
	access_file(QlfFile, read).
$qlf_file(FullFile, FullFile).

:- flag($load_silent, _, false).

$load_file(Spec, Options) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),
 
        $strip_module(Spec, Module, File),
	(   File == user
	->  true
	;   absolute_file_name(File,
			       [ file_type(prolog),
				 access(read)
			       ],
			       FullFile)
	),
	    
	$get_option(imports(Import), Options, all),
	$get_option(must_be_module(IsModule), Options, false),
	flag($load_silent, DefSilent, DefSilent),
	$get_option(silent(Silent), Options, DefSilent),
	flag($load_silent, _, Silent),
	$get_option(if(If), Options, true),

	(   $noload(If, FullFile)
	->  (   $current_module(LoadModule, FullFile)
	    ->  $import_list(Module, LoadModule, Import)
	    ;   (   Module == user
		->  true
		;   $load_file(Spec, [if(true)|Options])
		)
	    )
	;   $qlf_file(FullFile, Absolute),
	    $calleventhook(load_file(Absolute, start)),
	    (   $consult_goal(Absolute, Goal),
		call(Goal, Absolute, Module, Import, IsModule, Action, LM)
	    ->  true
	    ;   $warning('Failed to load file: ~w', Spec),
		$calleventhook(load_file(Absolute, false)),
		fail
	    ),
	    $calleventhook(load_file(Absolute, true)),

	    (   Silent == false,
		(   flag($autoloading, 0, 0)
		;   feature(verbose_autoload, true)
		)
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
	    )
	),
	flag($load_silent, _, DefSilent).


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
	$load_module(Module, Public, all, File).
$load_file((:- module(Module, Public)), File, all, _, Module) :- !,
	$load_module(Module, Public, all, File).
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

$consult_clause(Clause, File) :-
	expand_term(Clause, Expanded),
	(   $store_clause(Expanded, File)
	->  Clause == end_of_file
	;   fail
	).

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
	catch(Module:Goal, Term, $exception_in_directive(Term)), !.
$execute_directive2(Goal) :-
	$set_source_module(Module, Module),
	(   Module == user
	->  $warning('Directive failed: ~w', [Goal])
	;   $warning('Directive failed: ~w:~w', [Module, Goal])
        ),
	fail.

$exception_in_directive(Term) :-
	print_message(error, Term),
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
:- user:dynamic(goal_expansion/2).
:- user:multifile(goal_expansion/2).

expand_term(Term, Expanded) :-		% local term-expansion
	$term_expansion_module(Module),
	Module:term_expansion(Term, Expanded0), !,
	$expand_clauses(Expanded0, Expanded).
expand_term(Term, Expanded) :-
	$translate_rule(Term, Expanded0), !,
	$expand_clauses(Expanded0, Expanded).
expand_term(Term0, Term) :-
	$expand_clauses(Term0, Term).

$store_clause([], _) :- !.
$store_clause([C|T], F) :- !,
	$store_clause(C, F),
	$store_clause(T, F).
$store_clause(end_of_file, _) :- !.
$store_clause((:- Goal), _) :- !,
	$execute_directive(Goal).
$store_clause((?- Goal), _) :- !,
	$execute_directive(Goal).
$store_clause((_, _), _) :- !,
	$warning('Full stop in clause body? (attempt to define ,/2)').
$store_clause($source_location(File, Line):Term, _) :- !,
	$record_clause(Term, File:Line, Ref),
        $ifcompiling($qlf_assert_clause(Ref, development)).
$store_clause(Term, File) :-
	$record_clause(Term, File, Ref),
        $ifcompiling($qlf_assert_clause(Ref, development)).

		 /*******************************
		 *	 FOREIGN INTERFACE	*
		 *******************************/

%	call-back from PL_register_foreign().  First argument is the module
%	into which the foreign predicate is loaded and second is a term
%	describing the arguments.

:- dynamic
	$foreign_registered/2.


		 /*******************************
		 *   GOAL_EXPANSION/2 SUPPORT	*
		 *******************************/

$expand_clauses(X, X) :-
	\+ $goal_expansion_module(_), !.
$expand_clauses(X, X) :-
	var(X), !.
$expand_clauses([H0|T0], [H|T]) :- !,
	$expand_clauses(H0, H),
	$expand_clauses(T0, T).
$expand_clauses((Head :- Body), (Head :- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body,  ExpandedBody).
$expand_clauses(Head, Head).

expand_goal(A, B) :-
        $do_expand_body(A, B0),
	$tidy_body(B0, B).

$do_expand_body(G, G) :-
        var(G), !.
$do_expand_body((A,B), (EA,EB)) :- !,
        $do_expand_body(A, EA),
        $do_expand_body(B, EB).
$do_expand_body((A;B), (EA;EB)) :- !,
        $do_expand_body(A, EA),
        $do_expand_body(B, EB).
$do_expand_body((A->B), (EA->EB)) :- !,
        $do_expand_body(A, EA),
        $do_expand_body(B, EB).
$do_expand_body((A*->B), (EA*->EB)) :- !,
        $do_expand_body(A, EA),
        $do_expand_body(B, EB).
$do_expand_body((\+A), (\+EA)) :- !,
        $do_expand_body(A, EA).
$do_expand_body(not(A), not(EA)) :- !,
        $do_expand_body(A, EA).
$do_expand_body(call(A), call(EA)) :- !,
        $do_expand_body(A, EA).
$do_expand_body(once(A), once(EA)) :- !,
        $do_expand_body(A, EA).
$do_expand_body(ignore(A), ignore(EA)) :- !,
        $do_expand_body(A, EA).
$do_expand_body(forall(A, B), forall(EA, EB)) :- !,
        $do_expand_body(A, EA),
        $do_expand_body(B, EB).
$do_expand_body(A, B) :-
        $goal_expansion_module(M),
        M:goal_expansion(A, B0), !,
	$do_expand_body(B0, B).
$do_expand_body(A, A).

%	Delete extraneous true's that result from goal_expansion(..., true)
%
%	Is the really necessary?  Should we only do it if -O is effective?

$tidy_body(A, A) :-
	feature(optimise, false), !.
$tidy_body(A, A) :-
        var(A), !.
$tidy_body((A,B), (A, TB)) :-
        var(A), !,
        $tidy_body(B, TB).
$tidy_body((A,B), (TA, B)) :-
        var(B), !,
        $tidy_body(A, TA).
$tidy_body(((A,B),C), R) :- !,
	$tidy_body((A,B,C), R).
$tidy_body((true,A), R) :- !,
        $tidy_body(A, R).
$tidy_body((A,true), R) :- !,
        $tidy_body(A, R).
$tidy_body((A,B), (TA, TB)) :- !,
        $tidy_body(A, TA),
        $tidy_body(B, TB).
$tidy_body((A;B), (TA; TB)) :- !,
        $tidy_body(A, TA),
        $tidy_body(B, TB).
$tidy_body((A->B), (TA->TB)) :- !,
        $tidy_body(A, TA),
        $tidy_body(B, TB).
$tidy_body(A, A).


		/********************************
		*        GRAMMAR RULES          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The DCG compiler. The original code was copied from C-Prolog and written
by Fernando Pereira, EDCAAD, Edinburgh,  1984.   Since  then many people
have modified and extended this code. It's a nice mess now and it should
be redone from scratch. I won't be doing   this  before I get a complete
spec explaining all an implementor needs to   know  about DCG. I'm a too
basic user of this facility myself (though   I  learned some tricks from
people reporting bugs :-)

The original version contained  $t_tidy/2  to   convert  ((a,b),  c)  to
(a,(b,c)), but as the  SWI-Prolog  compiler   doesn't  really  care (the
resulting code is simply the same), I've removed that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

$translate_rule((LP-->List), H) :-
	proper_list(List), !,
	(   List = []
	->  $t_head(LP, S, S, H)
	;   List = [X]
	->  $t_head(LP, [X|S], S, H)
	;   append(List, SR, S),
	    $extend([S, SR], LP, H)
	), !.
$translate_rule((LP-->RP), (H:-B)):-
	$t_head(LP, S, SR, H),
	$t_body(RP, S, SR, B).

$tailvar(X, X) :-
	var(X), !.
$tailvar([_|T], V) :-
	$tailvar(T, V).

$t_head((LP, List), S, SR, H) :-
	append(List, SR, List2), !,
	$extend([S, List2], LP, H).
$t_head(LP, S, SR, H) :-
	$extend([S, SR], LP, H).


$t_body(Var, S, SR, phrase(Var, S, SR)) :-
	var(Var), !.
$t_body(List, S, SR, C) :-
	proper_list(List), !,
	(   List = []
	->  C = (S=SR)
	;   List = [X]
	->  C = 'C'(S, X, SR)
	;   C = append(List, SR, S)
	).
$t_body(List, S, SR, C) :-
	List = [_|_], !,
	C = append(List, SR, S).
$t_body(!, S, S, !) :- !.
$t_body({T}, S, SR, (T, SR = S)) :- !.
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
$t_body((\+ C), S, SR, (\+ Ct)) :- !,
	$t_body(C, S, SR, Ct).
$t_body(T, S, SR, Tt) :-
	$extend([S, SR], T, Tt).


$t_fill(S, SR, S1, T, (T, SR=S)) :-
	S1 == S, !.
$t_fill(_S, SR, SR, T, T).


$extend(More, M:OldT, M:NewT) :- !,
	$extend(More, OldT, NewT).
$extend(More, OldT, NewT) :-
	OldT =.. OldL,
	append(OldL, More, NewL),
	NewT =.. NewL.

'C'([X|S], X, S).

:- module_transparent
	phrase/2,
	phrase/3.

phrase(RuleSet, Input) :-
	phrase(RuleSet, Input, []).
phrase(RuleSet, Input, Rest) :-
	$strip_module(RuleSet, _, Head),
	(   is_list(Head)
	->  append(Head, Rest, Input)
	;   call(RuleSet, Input, Rest)
	).


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
	$translate_options(Argv, Options),
	$option(compileout, Out, Out),
        user:consult(Files),
	user:qsave_program(Out, Options).

$get_files_argv([], []) :- !.
$get_files_argv(['-c'|Files], Files) :- !.
$get_files_argv([_|Rest], Files) :-
	$get_files_argv(Rest, Files).

$translate_options([], []).
$translate_options([O|T0], [Opt|T]) :-
	atom_chars(O, [0'-,0'-|Rest]),
	$split(Rest, "=", Head, Tail), !,
	atom_chars(Name, Head),
	name(Value, Tail),
	Opt =.. [Name, Value],
	$translate_options(T0, T).
$translate_options([_|T0], T) :-
	$translate_options(T0, T).

$split(List, Split, [], Tail) :-
	append(Split, Tail, List), !.
$split([H|T0], Split, [H|T], Tail) :-
	$split(T0, Split, T, Tail).


		/********************************
		*       LIST PROCESSING         *
		*********************************/

member(X, [X|_]).
member(X, [_|T]) :-
	member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).

%	select(?List1, ?Elem, ?List2)
%	Is true when List1, with Elem removed results in List2.

select([X|Tail], X, Tail).
select([Head|Tail], Elem, [Head|Rest]) :-
	select(Tail, Elem, Rest).


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


'$:-'	
	format('Loading Prolog startup files~n', []),
	source_location(File, _Line),
	file_directory_name(File, Dir),
	concat(Dir, '/load.pl', LoadFile),
	$load_wic_files(system, [LoadFile]),
	format('SWI-Prolog boot files loaded~n', []),
	flag($compiling, OldC, wic),
	$execute_directive($set_source_module(_, user)),
	flag($compiling, _, OldC).
