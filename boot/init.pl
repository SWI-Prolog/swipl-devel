/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

/*
Consult, derivates and basic things.   This  module  is  loaded  by  the
C-written  bootstrap  compiler.

The $:- directive  is  executed  by  the  bootstrap  compiler,  but  not
inserted  in  the  intermediate  code  file.   Used  to print diagnostic
messages and start the Prolog defined compiler for  the  remaining  boot
modules.

If you want  to  debug  this  module,  put  a  '$:-'(trace).   directive
somewhere.   The  tracer will work properly under boot compilation as it
will use the C defined write predicate  to  print  goals  and  does  not
attempt to call the Prolog defined trace interceptor.

Please note that of version  3.3,  $:-   is  no  longer  an operator and
therefore you have to use braces.
*/

'$:-'(format('Loading boot file ...~n', [])).

		/********************************
		*    LOAD INTO MODULE SYSTEM	*
		********************************/

:- '$set_source_module'(_, system).

		/********************************
		*          DIRECTIVES           *
		*********************************/

'$set_pattr'([], _) :- !.
'$set_pattr'([H|T], Attr) :- !,		% ISO
	'$set_pattr'(H, Attr),
	'$set_pattr'(T, Attr).
'$set_pattr'((A,B), Attr) :- !,		% ISO and traditional
	'$set_pattr'(A, Attr),
	'$set_pattr'(B, Attr).
'$set_pattr'(A, Attr) :-
	'$set_predicate_attribute'(A, Attr, 1).

dynamic(Spec)		 :- '$set_pattr'(Spec, (dynamic)).
multifile(Spec)		 :- '$set_pattr'(Spec, (multifile)).
module_transparent(Spec) :- '$set_pattr'(Spec, transparent).
discontiguous(Spec)	 :- '$set_pattr'(Spec, (discontiguous)).
volatile(Spec)		 :- '$set_pattr'(Spec, (volatile)).
thread_local(Spec)	 :- '$set_pattr'(Spec, (thread_local)).
noprofile(Spec)		 :- '$set_pattr'(Spec, (noprofile)).

:- module_transparent
	'$set_pattr'/2,
	(dynamic)/1,
	(multifile)/1,
	(module_transparent)/1,
	(discontiguous)/1,
	(volatile)/1,
	(thread_local)/1,
	(noprofile)/1,
	'$hide'/2.


		/********************************
		*        TRACE BEHAVIOUR        *
		*********************************/

%	'$hide'(+Name, +Arity)
%	Predicates protected this way are never visible in the tracer.

'$hide'(Name, Arity) :-
	'$set_predicate_attribute'(Name/Arity, trace, 0).


		/********************************
		*       CALLING, CONTROL        *
		*********************************/

:- noprofile((call/1,
	      catch/3,
	      once/1,
	      ignore/1,
	      call_cleanup/2,
	      call_cleanup/3,
	      setup_and_call_cleanup/3,
	      setup_and_call_cleanup/4,
	      (^)/2)).

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
	call/7,
	call/8,
	call/9,
	call/10,
	call/11,
	(^)/2,
	(not)/1,
	(\+)/1,
	(->)/2,
	(*->)/2,
	once/1,
	ignore/1,
	block/3,
	catch/3,
	setup_and_call_cleanup/3,
	setup_and_call_cleanup/4,
	call_cleanup/2,
	call_cleanup/3,
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
call(G, A, B, C, D, E, F) :-
	call(G, A, B, C, D, E, F).
call(G, A, B, C, D, E, F, G) :-
	call(G, A, B, C, D, E, F, G).
call(G, A, B, C, D, E, F, G, H) :-
	call(G, A, B, C, D, E, F, G, H).
call(G, A, B, C, D, E, F, G, H, I) :-
	call(G, A, B, C, D, E, F, G, H, I).
call(G, A, B, C, D, E, F, G, H, I, J) :-
	call(G, A, B, C, D, E, F, G, H, I, J).

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
	'$apply'(Pred, Arguments).		% handled by the compiler

_Var^Goal :-					% setof/3, bagof/3
	Goal.

%	block/3, !/1, exit/2, fail/1
%	`longjmp' like control-structures.  See manual.  The predicate
%	system:block/3 is used by the VMI's I_CUT_BLOCK and B_EXIT.
%	'$exit' and '$cut' are interpreted by the compiler/decompiler,
%	just like '$apply'/2.

block(_Label, Goal, _RVal) :-
	Goal.

!(Label) :-
	'$cut'(Label).				% handled by compiler

exit(Label, RVal) :-
	'$exit'(Label, RVal).			% handled by compiler

fail(Label) :-
	'$cut'(Label),				% handled by compiler
	fail.

%%	catch(:Goal, +Catcher, :Recover)
%
%	ISO compliant exception handling.

catch(_Goal, _Catcher, _Recover) :-
	'$catch'.

%%	setup_and_call_cleanup(:Setup, :Goal, :Cleanup).
%%	setup_and_call_cleanup(:Setup, :Goal, +Catcher, :Cleanup).
%%	call_cleanup(:Goal, :Cleanup)
%%	call_cleanup(:Goal, +Catcher, :Cleanup)
%
%	Call Cleanup as Goal finished (deterministic success, failure,
%	exception or cut).  '$call_cleanup' translated to I_CALLCLEANUP.

setup_and_call_cleanup(Setup, _Goal, _Catcher, _Cleanup) :-
	Setup,
	'$call_cleanup'.

setup_and_call_cleanup(Setup, Goal, Cleanup) :-
	setup_and_call_cleanup(Setup, Goal, _Catcher, Cleanup).

call_cleanup(Goal, Cleanup) :-
	setup_and_call_cleanup(true, Goal, _Catcher, Cleanup).

call_cleanup(Goal, Catcher, Cleanup) :-
	setup_and_call_cleanup(true, Goal, Catcher, Cleanup).


		/********************************
		*            MODULES            *
		*********************************/

%	'$prefix_module'(+Module, +Context, +Term, -Prefixed)
%	Tags `Term' with `Module:' if `Module' is not the context module.

'$prefix_module'(Module, Module, Head, Head) :- !.
'$prefix_module'(Module, _, Head, Module:Head).


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

:- set_prolog_flag(autoload, true).
:- set_prolog_flag(verbose_autoload, false).
:- flag('$autoloading', _, 0).

'$undefined_procedure'(Module, Name, Arity, Action) :-
	'$prefix_module'(Module, user, Name/Arity, Pred),
	user:exception(undefined_predicate, Pred, Action), !.
'$undefined_procedure'(Module, Name, Arity, retry) :-
	current_prolog_flag(autoload, true),
	with_mutex('$load', '$autoload'(Module, Name, Arity)).
'$undefined_procedure'(_, _, _, error).

'$autoload'(Module, Name, Arity) :-
	'$find_library'(Module, Name, Arity, LoadModule, Library),
	functor(Head, Name, Arity),
	flag('$autoloading', Old, Old+1),
	(   current_prolog_flag(verbose_autoload, true)
	->  Level = informational
	;   Level = silent
	),
	print_message(Level, autoload(Module:Name/Arity, Library)),
	(   Module == LoadModule
	->  ensure_loaded(Module:Library)
	;   (   '$c_current_predicate'(_, LoadModule:Head)
	    ->	Module:import(LoadModule:Head)
	    ;	use_module(Module:Library, [Name/Arity])
	    )
	),
	flag('$autoloading', _, Old),
	'$c_current_predicate'(_, Module:Head).

'$calleventhook'(Term) :-
	(   notrace(user:prolog_event_hook(Term))
	->  true
	;   true
	).

:- '$hide'('$calleventhook', 1).

%	 handle debugger 'w', 'p' and <N> depth options.

'$set_debugger_print_options'(write) :- !,
	set_prolog_flag(debugger_print_options,
			[ quoted(true),
			  attributes(write)
			]).
'$set_debugger_print_options'(print) :- !,
	set_prolog_flag(debugger_print_options,
			[ quoted(true),
			  portray(true),
			  max_depth(10),
			  attributes(portray)
			]).
'$set_debugger_print_options'(Depth) :-
	current_prolog_flag(debugger_print_options, Options0),
	(   '$select'(max_depth(_), Options0, Options)
	->  true
	;   Options = Options0
	),
	set_prolog_flag(debugger_print_options,
			[max_depth(Depth)|Options]).


		/********************************
		*        SYSTEM MESSAGES        *
		*********************************/

%	'$confirm'(Spec)
%
%	Ask the user to confirm a question.  Spec is a term as used for
%	print_message/2.

'$confirm'(Spec) :-
	print_message(query, Spec),
	between(0, 5, _),
	    get_single_char(Answer),
	    (	memberchk(Answer, "yYjJ \n")
	    ->	!,
	        print_message(query, if_tty(yes))
	    ;	memberchk(Answer, "nN")
	    ->	!,
	        print_message(query, if_tty(no)),
		fail
	    ;	print_message(help, query(confirm)),
		fail
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
	current_prolog_flag(home, Home).
user:file_search_path(foreign, swi(ArchLib)) :-
	current_prolog_flag(arch, Arch),
	atom_concat('lib/', Arch, ArchLib).
user:file_search_path(foreign, swi(lib)).
user:file_search_path(user_profile, '.').
user:file_search_path(user_profile, app_preferences('.')).
user:file_search_path(app_preferences, PrologAppData) :-
	current_prolog_flag(windows, true),
	catch(win_folder(appdata, AppData), _, fail),
	atom_concat(AppData, '/SWI-Prolog', PrologAppData),
	(   exists_directory(PrologAppData)
	->  true
	;   catch(make_directory(PrologAppData), _, fail)
	).
user:file_search_path(app_preferences, UserHome) :-
	catch(expand_file_name(~, [UserHome]), _, fail).


%	expand_file_search_path(+Spec, -Expanded)
%
%	Expand a search path.  The system uses depth-first search upto a
%	specified depth.  If this depth is exceeded an exception is raised.
%	TBD: bread-first search?

expand_file_search_path(Spec, Expanded) :-
	catch('$expand_file_search_path'(Spec, Expanded, 0, []),
	      loop(Used),
	      throw(error(loop_error(Spec), file_search(Used)))).

'$expand_file_search_path'(Spec, Expanded, N, Used) :-
	functor(Spec, Alias, 1),
	user:file_search_path(Alias, Exp0),
	NN is N + 1,
	(   NN > 16
	->  throw(loop(Used))
	;   true
	),
	'$expand_file_search_path'(Exp0, Exp1, NN, [Alias=Exp0|Used]),
	arg(1, Spec, Base),
	'$make_path'(Exp1, Base, Expanded).
'$expand_file_search_path'(A/B, Expanded, _, _) :- !,
	'$make_path_from_slash'(A/B, Parts, []),
	concat_atom(Parts, Expanded).
'$expand_file_search_path'(Spec, Spec, _, _) :-
	atomic(Spec).

'$make_path'(Dir, File, Path) :-
	atom_concat(_, /, Dir), !,
	atom_concat(Dir, File, Path).
'$make_path'(Dir, File, Path) :-
	'$make_path_from_slash'(File, Parts, []),
	'$append'([Dir, /], Parts, AllParts),
	'$concat_atom'(AllParts, Path).


%%	'$make_path_from_slash'(+SlashPath, -Parts, ?Tail) is det.
%
%	Translate a/b/c into [a,/,b,/,c]. We cannot   use DCG as the DCG
%	compiler is not yet defined.

'$make_path_from_slash'(A/B, P0, P) :- !,
	'$make_path_from_slash'(A, P0, [/|P1]),
	'$make_path_from_slash'(B, P1, P).
'$make_path_from_slash'(A, [A|T], T).


		/********************************
		*         FILE CHECKING         *
		*********************************/

%%	absolute_file_name(+Term, +Args, -AbsoluteFile) is nondet.
%%	absolute_file_name(+Term, -AbsoluteFile, +Args) is nondet.
%	
%	Translate path-specifier into a full   path-name. This predicate
%	originates from Quintus was introduced  in SWI-Prolog very early
%	and  has  re-appeared  in  SICStus  3.9.0,  where  they  changed
%	argument order and  added  some   options.  As  arguments aren't
%	really ambiguous we swap the arguments if we find the new order.
%	The  SICStus  options  file_type(source)   and  relative_to  are
%	supported as well.

absolute_file_name(Spec, Args, Path) :-
	is_list(Path),
	\+ is_list(Args), !,
	absolute_file_name(Spec, Path, Args).
absolute_file_name(Spec, Args, Path) :-
	(   is_list(Args)
	->  true
	;   throw(error(type_error(list, Args), _))
	),
	(   '$select'(extensions(Exts), Args, Conditions)
	->  true
	;   memberchk(file_type(Type), Args)
	->  '$file_type_extensions'(Type, Exts),
	    Conditions = Args
	;   Conditions = Args,
	    Exts = ['']
	),
	(   '$select'(solutions(Sols), Conditions, C1)
	->  true
	;   Sols = first,
	    C1 = Conditions
	),
	(   '$select'(file_errors(FileErrors), C1, C2)
	->  true
	;   FileErrors = error,
	    C2 = C1
	),
	(   atomic(Spec),
	    '$select'(expand(true), C2, C3)
	->  expand_file_name(Spec, List),
	    '$member'(Spec1, List)
	;   Spec1 = Spec,
	    C3 = C2
	),
	(   '$chk_file'(Spec1, Exts, C3, Path)
	*-> (   Sols == first
	    ->  !
	    ;   true
	    )
	;   (   FileErrors == fail
	    ->  fail
	    ;   throw(error(existence_error(source_sink, Spec), _))
	    )
	).

'$file_type_extensions'(source, Exts) :- !, 	% SICStus 3.9 compatibility
	'$file_type_extensions'(prolog, Exts).
'$file_type_extensions'(Type, Exts) :-
	'$current_module'('$bags', _File), !,
	findall(Ext, user:prolog_file_type(Ext, Type), Exts0),
	'$append'(Exts0, [''], Exts).
'$file_type_extensions'(prolog, [pl, '']). % findall is not yet defined ...

%	user:prolog_file_type/2
%
%	Define type of file based on the extension.  This is used by
%	absolute_file_name/3 and may be used to extend the list of
%	extensions used for some type.

:- multifile(user:prolog_file_type/2),
   dynamic(user:prolog_file_type/2).

user:prolog_file_type(pl,	prolog).
user:prolog_file_type(Ext,	prolog) :-
	current_prolog_flag(associate, Ext),
	Ext \== pl.
user:prolog_file_type(qlf,	prolog).
user:prolog_file_type(qlf,	qlf).
user:prolog_file_type(Ext,	executable) :-
	current_prolog_flag(shared_object_extension, Ext).

%	File is a specification of a Prolog source file. Return the full
%	path of the file.

'$chk_file'(Spec, Extensions, Cond, FullName) :-
	'$canonise_extensions'(Extensions, Exts),
	'$dochk_file'(Spec, Exts, Cond, FullName).

'$dochk_file'(Spec, Extensions, Cond, FullName) :-
	compound(Spec),
	functor(Spec, Alias, 1),
	user:file_search_path(Alias, _), !,
	'$relative_to'(Cond, cwd, CWD),
	'$chk_alias_file'(Spec, Extensions, Cond, CWD, FullName).
'$dochk_file'(Term, Ext, Cond, FullName) :-	% allow a/b, a-b, etc.
	\+ atomic(Term), !,
	term_to_atom(Term, Raw),
	atom_chars(Raw, S0),
	'$delete'(S0, ' ', S1),
	atom_chars(Atom, S1),
	'$dochk_file'(Atom, Ext, Cond, FullName).
'$dochk_file'(File, Exts, Cond, FullName) :-
	is_absolute_file_name(File), !,
	'$extend_file'(File, Exts, Extended),
	'$file_condition'(Cond, Extended),
	'$absolute_file_name'(Extended, FullName).
'$dochk_file'(File, Exts, Cond, FullName) :-
	'$relative_to'(Cond, source, Dir),
	'$concat_atom'([Dir, /, File], AbsFile),
	'$extend_file'(AbsFile, Exts, Extended),
	'$file_condition'(Cond, Extended), !,
	'$absolute_file_name'(Extended, FullName).
'$dochk_file'(File, Exts, Cond, FullName) :-
	'$extend_file'(File, Exts, Extended),
	'$file_condition'(Cond, Extended),
	'$absolute_file_name'(Extended, FullName).

%	'$relative_to'(+Condition, +Default, -Dir)
%	
%	Determine the directory to work from.  This can be specified
%	explicitely using one or more relative_to(FileOrDir) options
%	or implicitely relative to the working directory or current
%	source-file.

'$relative_to'(Conditions, Default, Dir) :-
	(   '$member'(relative_to(FileOrDir), Conditions)
	*-> (   exists_directory(FileOrDir)
	    ->	Dir = FileOrDir
	    ;	atom_concat(Dir, /, FileOrDir)
	    ->	true
	    ;	file_directory_name(FileOrDir, Dir)
	    )
	;   Default == cwd
	->  working_directory(Dir, Dir)
	;   Default == source
	->  source_location(ContextFile, _Line),
	    file_directory_name(ContextFile, Dir)
	).

:- dynamic
	'$search_path_file_cache'/5.
:- volatile
	'$search_path_file_cache'/5.

:- set_prolog_flag(verbose_file_search, false).

'$chk_alias_file'(Spec, Exts, Cond, CWD, FullFile) :-
	'$search_path_file_cache'(Spec, Cond, CWD, FullFile, Exts),
	'$file_condition'(Cond, FullFile),
	'$search_message'(file_search(cache(Spec, Cond), FullFile)).
'$chk_alias_file'(Spec, Exts, Cond, CWD, FullFile) :-
	expand_file_search_path(Spec, Expanded),
	'$extend_file'(Expanded, Exts, LibFile),
	(   '$file_condition'(Cond, LibFile),
	    '$absolute_file_name'(LibFile, FullFile),
	    \+ '$search_path_file_cache'(Spec, Cond, CWD, FullFile, Exts),
	    assert('$search_path_file_cache'(Spec, Cond, CWD, FullFile, Exts))
	->  '$search_message'(file_search(found(Spec, Cond), FullFile))
	;   '$search_message'(file_search(tried(Spec, Cond), LibFile)),
	    fail
	).

'$search_message'(Term) :-
	current_prolog_flag(verbose_file_search, true), !,
	print_message(informational, Term).
'$search_message'(_).


	
%	'$file_condition'(+Condition, +Path)
%
%	Verify Path satisfies Condition. 

'$file_condition'([], _) :- !.
'$file_condition'([H|T], File) :- !,
	'$file_condition'(H, File),
	'$file_condition'(T, File).
'$file_condition'(exists, File) :- !,
	exists_file(File).
'$file_condition'(file_type(directory), File) :- !,
	exists_directory(File).
'$file_condition'(file_type(_), File) :- !,
	\+ exists_directory(File).
'$file_condition'(access([A1|AT]), File) :- !,
	'$file_condition'(access(A1), File),
	'$file_condition'(access(AT), File).
'$file_condition'(access([]), _) :- !.
'$file_condition'(access(Access), File) :- !,
	access_file(File, Access).
'$file_condition'(relative_to(_), _File).		% This isn't a condition

'$extend_file'(File, Exts, FileEx) :-
	'$ensure_extensions'(Exts, File, Fs),
	'$list_to_set'(Fs, FsSet),
	'$member'(FileEx, FsSet).
	
'$ensure_extensions'([], _, []).
'$ensure_extensions'([E|E0], F, [FE|E1]) :-
	file_name_extension(F, E, FE),
	'$ensure_extensions'(E0, F, E1).

'$list_to_set'([], []).
'$list_to_set'([H|T], R) :-
	memberchk(H, T), !, 
	'$list_to_set'(T, R).
'$list_to_set'([H|T], [H|R]) :-
	'$list_to_set'(T, R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Canonise the extension list. Old SWI-Prolog   require  `.pl', etc, which
the Quintus compatibility  requests  `pl'.   This  layer  canonises  all
extensions to .ext
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$canonise_extensions'([], []) :- !.
'$canonise_extensions'([H|T], [CH|CT]) :- !,
	'$canonise_extension'(H, CH),
	'$canonise_extensions'(T, CT).
'$canonise_extensions'(E, [CE]) :-
	'$canonise_extension'(E, CE).

'$canonise_extension'('', '') :- !.
'$canonise_extension'(DotAtom, DotAtom) :-
	atom_concat('.', _, DotAtom), !.
'$canonise_extension'(Atom, DotAtom) :-
	atom_concat('.', Atom, DotAtom).


		/********************************
		*            CONSULT            *
		*********************************/

:- user:(dynamic
	 	library_directory/1,
		'$start_compilation'/2,
		'$end_compilation'/2,
	        prolog_load_file/2).
:- user:(multifile
	 	library_directory/1,
		'$start_compilation'/2,
		'$end_compilation'/2,
	        prolog_load_file/2).


:-	flag('$break_level',	_, 0),
	flag('$compiling',	_, database),
	flag('$directive',	_, database),
	flag('$preprocessor',	_, none),
	prompt(_, '|: ').

%	compiling
%
%	Is true if SWI-Prolog is generating a state or qlf file or
%	executes a `call' directive while doing this.

compiling :-
	\+ (   flag('$compiling', database, database),
	       flag('$directive', database, database)
	   ).

:- module_transparent
	'$ifcompiling'/1.

'$ifcompiling'(_) :-
	flag('$compiling', database, database), !.
'$ifcompiling'(G) :-
	G.

		/********************************
		*         PREPROCESSOR          *
		*********************************/

preprocessor(Old, New) :-
	flag('$preprocessor', Old, New).

'$set_encoding'(default, _) :- !.
'$set_encoding'(Encoding, Stream) :-
	set_stream(Stream, encoding(Encoding)).

'$open_source'(stream(Id, In), Enc, In, Goal) :- !,
	'$push_input_context',
	'$set_encoding'(Enc, In),
	set_stream(In, file_name(Id)),
	set_stream(In, record_position(true)),
	'$open_source_call'(Id, In, Goal, True),
	'$pop_input_context',
	True == yes.
'$open_source'(File, Enc, In, Goal) :-
	preprocessor(none, none), !,
	'$push_input_context',
	open(File, read, In),
	'$set_encoding'(Enc, In),
	'$open_source_call'(File, In, Goal, True),
	close(In),
	'$pop_input_context',
	True == yes.
'$open_source'(File, Enc, In, Goal) :-
	preprocessor(Pre, Pre),
	(   '$substitute_atom'('%f', File, Pre, Command)
	->  '$push_input_context',
	    open(pipe(Command), read, In),
	    '$set_encoding'(Enc, In),
	    '$open_source_call'(File, In, Goal, True),
	    close(In),
	    '$pop_input_context',
	    True == yes
	;   throw(error(domain_error(preprocessor, Pre), _))
	).


:- dynamic
	'$load_input'/2.
:- volatile
	'$load_input'/2.

'$open_source_call'(File, In, Goal, Status) :-
	flag('$compilation_level', Level, Level+1),
	asserta('$load_input'(File, In), Ref),
	ignore(user:'$start_compilation'(File, Level)),
	(   catch(Goal, E,
		  (print_message(error, E),
		   fail))
	->  Status = yes
	;   Status = no
	),
	ignore(user:'$end_compilation'(File, Level)),
	erase(Ref),
	flag('$compilation_level', _, Level).


%	'$substitute_atom'(+From, +To, +In, -Out)

'$substitute_atom'(From, To, In, Out) :-
	sub_atom(In, B, _, A, From),
	sub_atom(In, 0, B, _, Before),
	sub_atom(In, _, A, 0, After),
	concat_atom([Before, To, After], Out).


		 /*******************************
		 *	    DERIVED FILES	*
		 *******************************/

:- dynamic
	'$derived_source_db'/3.		% Loaded, DerivedFrom, Time

'$register_derived_source'(Loaded, DerivedFrom) :-
	retractall('$derived_source_db'(Loaded, _, _)),
	time_file(DerivedFrom, Time),
	assert('$derived_source_db'(Loaded, DerivedFrom, Time)).

%	Auto-importing dynamic predicates is not very elegant and
%	leads to problems with qsave_program/[1,2]

'$derived_source'(Loaded, DerivedFrom, Time) :-
	'$derived_source_db'(Loaded, DerivedFrom, Time).


		/********************************
		*       LOAD PREDICATES         *
		*********************************/

:- module_transparent
	ensure_loaded/1,
	'.'/2,
	consult/1,
	use_module/1,
	use_module/2,
	reexport/1,
	reexport/2,
	'$load_file'/3,
	load_files/1,
	load_files/2.

%	ensure_loaded(+File|+ListOfFiles)
%	
%	Load specified files, provided they where not loaded before. If the
%	file is a module file import the public predicates into the context
%	module.

ensure_loaded(Files) :-
	load_files(Files, [if(not_loaded)]).

%	use_module(+File|+ListOfFiles)
%	
%	Very similar to ensure_loaded/1, but insists on the loaded file to
%	be a module file. If the file is already imported, but the public
%	predicates are not yet imported into the context module, then do
%	so.

use_module(Files) :-
	load_files(Files, [ if(not_loaded),
			    must_be_module(true)
			  ]).

%	use_module(+File, +ImportList)
%	
%	As use_module/1, but takes only one file argument and imports only
%	the specified predicates rather than all public predicates.

use_module(File, Import) :-
	load_files(File, [ if(not_loaded),
			   must_be_module(true),
			   imports(Import)
			 ]).

%%	reexport(+Files)
%
%	As use_module/1, exporting all imported predicates.

reexport(Files) :-
	load_files(Files, [ if(not_loaded),
			    must_be_module(true),
			    reexport(true)
			  ]).

%%	reexport(+File, +ImportList)
%
%	As use_module/1, re-exporting all imported predicates.

reexport(File, Import) :-
	load_files(File, [ if(not_loaded),
			   must_be_module(true),
			   imports(Import),
			   reexport(true)
			 ]).


[X] :- !,
	consult(X).
[F|R] :-
	consult([F|R]).
[].

consult(X) :-
	X == user, !,
	flag('$user_consult', N, N+1),
	NN is N + 1,
	atom_concat('user://', NN, Id),
	load_files(Id, [stream(user_input)]).
consult(List) :-
	load_files(List).

%	'$consult_goal'(+Path, -Goal)
%
%	Determine how to load the indicated file

'$consult_goal'(Path, Goal) :-
	(   file_name_extension(_, Ext, Path),
	    user:prolog_file_type(Ext, qlf)
	->  Goal = '$qload_file'
	;   Goal = '$consult_file'
	).

%	load_files(+File, +Options)
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
	strip_module(Files, Module, TheFiles),
        with_mutex('$load', '$load_files'(TheFiles, Module, Options)).

'$load_files'(Id, Module, Options) :-	% load_files(foo, [stream(In)])
	memberchk(stream(_), Options), !,
	(   atom(Id)
	->  '$load_file'(Id, Module, Options)
	;   throw(error(type_error(atom, Id), _))
	).
'$load_files'(X, _, _) :-
	var(X), !,
	throw(error(instantiation_error, context(load_files/2,_))).
'$load_files'([], _, _) :- !.
'$load_files'([H|T], Module, Options) :- !,
	'$load_files'(H, Module, Options),
	'$load_files'(T, Module, Options).
'$load_files'(Spec, Module, Options) :-
	atom(Spec),
	'$get_option'(expand(Expand), Options, true),
	Expand == true, !,
	expand_file_name(Spec, Files),
	'$load_files'(Files, Module, [expand(false)|Options]).
'$load_files'(File, Module, Options) :-
	strip_module(Module:File, Into, PlainFile),
	'$load_file'(PlainFile, Into, Options).


'$get_option'(Term, Options, Default) :-
	(   memberchk(Term, Options)
	->  true
	;   arg(1, Term, Default)
	).


'$noload'(true, _) :- !,
	fail.
'$noload'(not_loaded, FullFile) :-
	source_file(FullFile), !.
'$noload'(changed, Derived) :-
	'$derived_source'(_FullFile, Derived, LoadTime),
	time_file(Derived, Modified),
        Modified @=< LoadTime, !.
'$noload'(changed, FullFile) :-
	'$time_source_file'(FullFile, LoadTime, user),
        time_file(FullFile, Modified),
        Modified @=< LoadTime, !.

%	'$qlf_file'(+Spec, +PlFile, -LoadFile)
%
%	Return the QLF file if it exists.  Might check for modification
%	time, version, etc.
%	
%	If the user-specification specified a prolog file, do not
%	replace this with a .qlf file.

'$qlf_file'(Spec, FullFile, FullFile) :-
	'$spec_extension'(Spec, Ext),
	user:prolog_file_type(Ext, prolog), !.
'$qlf_file'(_, FullFile, QlfFile) :-
	flag('$compiling', database, database),
	file_name_extension(Base, PlExt, FullFile),
	user:prolog_file_type(PlExt, prolog),
	user:prolog_file_type(QlfExt, qlf),
	file_name_extension(Base, QlfExt, QlfFile),
	access_file(QlfFile, read).
'$qlf_file'(_, FullFile, FullFile).


'$spec_extension'(File, Ext) :-
	atom(File),
	file_name_extension(_, Ext, File).
'$spec_extension'(Spec, Ext) :-
	compound(Spec),
	arg(1, Spec, Arg),
	'$spec_extension'(Arg, Ext).


'$load_file'(File, Module, Options) :-
	\+ memberchk(stream(_), Options),
	user:prolog_load_file(Module:File, Options), !.
'$load_file'(File, Module, Options) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),
 
	(   memberchk(stream(FromStream), Options)
	->  true
	;   absolute_file_name(File,
			       [ file_type(prolog),
				 access(read)
			       ],
			       FullFile)
	),
	    
	'$get_option'(imports(Import), Options, all),
	'$get_option'(reexport(Reexport), Options, false),
	'$get_option'(must_be_module(IsModule), Options, false),
	current_prolog_flag(verbose_load, DefVerbose),
	'$negate'(DefVerbose, DefSilent),
	'$get_option'(silent(Silent), Options, DefSilent),
	'$negate'(Silent, Verbose),
	set_prolog_flag(verbose_load, Verbose),
	'$get_option'(if(If), Options, true),
	'$get_option'(autoload(Autoload), Options, false),
	'$get_option'(derived_from(DerivedFrom), Options, -),
	'$get_option'(encoding(Encoding), Options, default),
	current_prolog_flag(generate_debug_info, DebugInfo),

	(   Autoload == false
	->  flag('$autoloading', AutoLevel, AutoLevel)
	;   flag('$autoloading', AutoLevel, AutoLevel+1)
	),

	(   var(FromStream),
	    '$noload'(If, FullFile)
	->  (   '$current_module'(LoadModule, FullFile)
	    ->  '$import_list'(Module, LoadModule, Import, Reexport)
	    ;   (   Module == user
		->  true
		;   '$load_file'(File, Module, [if(true)|Options])
		)
	    )
	;   (   nonvar(FromStream)
	    ->	Absolute = File
	    ;   '$qlf_file'(File, FullFile, Absolute)
	    ),

	    flag('$compilation_level', Level, Level),
	    (   Silent == false,
		(   flag('$autoloading', 0, 0)
		;   current_prolog_flag(verbose_autoload, true)
		)
	    ->	MessageLevel = informational
	    ;	MessageLevel = silent
	    ),

	    '$print_message'(silent /*MessageLevel*/,
			     load_file(start(Level,
					     file(File, Absolute)))),
	    (   nonvar(FromStream),
	        '$consult_file'(stream(Absolute, FromStream), Encoding,
				Module, IsModule, Action, LM)
	    ->	true
	    ;   var(FromStream),
		'$consult_goal'(Absolute, Goal),
		call(Goal,
		     Absolute, Encoding,
		     Module, IsModule, Action, LM)
	    ->  true
	    ;   print_message(error, load_file(failed(File))),
		fail
	    ),

	    (	atom(LM)
	    ->  '$import_list'(Module, LM, Import, Reexport)
	    ;	true
	    ),

	    (	Level == 0
	    ->	garbage_collect_clauses
	    ;	true
	    ),

	    (	DerivedFrom \== -
	    ->	'$register_derived_source'(Absolute, DerivedFrom)
	    ;	true
	    ),

	    statistics(heapused, Heap),
	    statistics(cputime, Time),
	    HeapUsed is Heap - OldHeap,
	    TimeUsed is Time - OldTime,

	    '$print_message'(MessageLevel,
			     load_file(done(Level,
					    file(File, Absolute),
					    Action,
					    LM,
					    TimeUsed,
					    HeapUsed)))
	),
	flag('$autoloading', _, AutoLevel),
	set_prolog_flag(verbose_load, DefVerbose),
	set_prolog_flag(generate_debug_info, DebugInfo).


'$negate'(false, true).
'$negate'(true,  false).

'$print_message'(Level, Term) :-
	'$current_module'('$messages', _), !,
	print_message(Level, Term).
'$print_message'(_Level, _Term).

'$print_message_fail'(E) :-
	print_message(error, E),
	fail.

%%	'$consult_file'(+Path, +Encoding, +Module, +IsModule, -Action, -LoadedIn)

'$consult_file'(Absolute, Enc, Module, IsModule, What, LM) :-
	'$set_source_module'(Module, Module), !, % same module
	'$consult_file_2'(Absolute, Enc, Module, IsModule, What, LM).
'$consult_file'(Absolute, Enc, Module, IsModule, What, LM) :-
	'$set_source_module'(OldModule, Module),
	'$ifcompiling'('$qlf_start_sub_module'(Module)),
        '$consult_file_2'(Absolute, Enc, Module, IsModule, What, LM),
	'$ifcompiling'('$qlf_end_part'),
	'$set_source_module'(_, OldModule).

'$consult_file_2'(Absolute, Enc, Module, IsModule, What, LM) :-
	'$set_source_module'(OldModule, Module),	% Inform C we start loading
	'$load_id'(Absolute, Id),
	'$start_consult'(Id),
	'$compile_type'(What),
	(   flag('$compiling', wic, wic)	% TBD
	->  '$add_directive_wic'('$assert_load_context_module'(Id, OldModule))
	;   true
	),
	'$assert_load_context_module'(Id, OldModule),

	
	'$save_lex_state'(LexState),
	'$open_source'(Absolute, Enc, In,
		     '$load_file'(In, Id, IsModule, LM)),
	'$restore_lex_state'(LexState),
	'$set_source_module'(_, OldModule).	% Restore old module

:- thread_local
	'$current_load_dialect'/1.

%%	'$set_dialect'(-Old, +New) is det.
%
%	Switch the currently expected dialect.

'$set_dialect'(Old, New) :-
	Old == New, !,
	(   '$current_load_dialect'(Dialect)
	->  true
	;   Dialect = swi
	).
'$set_dialect'(Old, Dialect) :-
	(   retract('$current_load_dialect'(Old))
	->  true
	;   Old = swi
	),
	assert('$current_load_dialect'(Dialect)).

'$save_lex_state'(lexstate(Style, Dialect)) :-
	'$style_check'(Style, Style),
	'$set_dialect'(Dialect, Dialect).
'$restore_lex_state'(lexstate(Style, Dialect)) :-
	'$style_check'(_, Style),
	'$set_dialect'(_, Dialect).


'$load_id'(stream(Id, _), Id) :- !.
'$load_id'(Id, Id).

'$compile_type'(What) :-
	flag('$compiling', How, How),
	(   How == database
	->  What = compiled
	;   How == qlf
	->  What = '*qcompiled*'
	;   What = 'boot compiled'
	).

%%	'$load_context_module'(+File, -Module)
%	
%	Record the module a file was loaded from (see make/0)
%	
%	@tbd	Should also know which predicates are imported!

:- dynamic
	'$load_context_module'/2.

'$assert_load_context_module'(File, Module) :-
	(   '$load_context_module'(File, Module)
	->  true
	;   assert('$load_context_module'(File, Module))
	).


%   '$load_file'(+In, +Path, +IsModule, -Module)
%
%   '$load_file'/4 does the actual loading.

'$load_file'(In, File, IsModule, Module) :-
	(   peek_char(In, #)
	->  skip(In, 10)
	;   true
	),
	'$read_clause'(In, First),
	'$load_file'(First, In, File, IsModule, Module).


'$load_file'((?- module(Module, Public)), In, File, _, Module) :- !,
	'$load_module'(Module, Public, In, File).
'$load_file'((:- module(Module, Public)), In, File, _, Module) :- !,
	'$load_module'(Module, Public, In, File).
'$load_file'((?- module(Module, Public)),
	     In, File, _, Module) :- !,
	'$load_module'(Module, Public, In, File).
'$load_file'((:- module(Module, Public)),
	     In, File, _, Module) :- !,
	'$load_module'(Module, Public, In, File).
'$load_file'(_, _, File, true, _) :- !,
	throw(error(domain_error(module_file, File), _)).
'$load_file'(end_of_file, _, _, _, Module) :- !, 	% empty file
	'$set_source_module'(Module, Module).
'$load_file'(FirstClause, In, File, false, Module) :- !,
	'$set_source_module'(Module, Module),
	'$ifcompiling'('$qlf_start_file'(File)),
	ignore('$consult_clause'(FirstClause, File)),
	'$consult_stream'(In, File),
	'$ifcompiling'('$qlf_end_part').

'$reserved_module'(system).
'$reserved_module'(user).

'$load_module'(Reserved, _, _, _) :-
	'$reserved_module'(Reserved), !,
	throw(error(permission_error(load, module, Reserved), _)).
'$load_module'(Module, Public, In, File) :-
	'$set_source_module'(OldModule, OldModule),
	source_location(_File, Line),
	'$declare_module'(Module, File, Line),
	'$export_list'(Public, Module, Ops),
	'$ifcompiling'('$qlf_start_module'(Module)),
	'$export_ops'(Ops, Module, File),
	'$consult_stream'(In, File),
	Module:'$check_export',
	'$ifcompiling'('$qlf_end_part').


%%	'$import_list'(+TargetModule, +FromModule, +Import, +Reexport) is det.
%
%	Import from FromModule to TargetModule. Import  is one of =all=,
%	a list of optionally  mapped  predicate   indicators  or  a term
%	except(Import).

'$import_list'(_, _, Var, _) :-
	var(Var), !,
	throw(error(instantitation_error, _)).
'$import_list'(Target, Source, all, Reexport) :- !,
	export_list(Source, Import),
	'$import_ops'(Target, Source),
	'$import_list'(Target, Source, Import, Reexport).
'$import_list'(Target, Source, except(Spec), Reexport) :- !,
	export_list(Source, Export),
	(   is_list(Spec)
	->  true
	;   throw(error(type_error(list, Spec), _))
	),
	'$import_except'(Spec, Export, Import),
	'$import_list'(Target, Source, Import, Reexport).
'$import_list'(Target, Source, Import, Reexport) :- !,
	is_list(Import), !,
	'$import_all'(Import, Target, Source, Reexport).
'$import_list'(_, _, Import, _) :-
	throw(error(type_error(import_specifier, Import))).


:- op(700, xfx, user:as).		% TBD: kernel table

'$import_except'([], List, List).
'$import_except'([H as N|T], List0, List) :- !,
	'$import_as'(H, N, List0, List1),
	'$import_except'(T, List1, List).
'$import_except'([H|T], List0, List) :-
	'$select'(P, List0, List1),
	'$same_pi'(H, P), !,
	'$import_except'(T, List1, List).

'$import_as'(PI, N, [PI2|T], [PI as N|T]) :-
	'$same_pi'(PI, PI2), !.
'$import_as'(PI, N, [H|T0], [H|T]) :- !,
	'$import_as'(PI, N, T0, T).
'$import_as'(PI, _, _, _) :-
	throw(error(existence_error(export, PI), _)).

'$same_pi'(PI1, PI2) :-
	'$canonical_pi'(PI1, PI),
	'$canonical_pi'(PI2, PI).

'$canonical_pi'(N//A0, N/A) :- !,
	A is A0 + 2.
'$canonical_pi'(PI, PI).


'$import_all'(Import, Context, Source, Reexport) :-
	'$import_all2'(Import, Context, Source, Imported),
	(   Reexport == true,
	    '$list_to_conj'(Imported, Conj)
	->  export(Context:Conj),
	    (	flag('$compiling', wic, wic)
	    ->	'$add_directive_wic'(export(Context:Conj))
	    ;	true
	    )
	;   true
	).

'$import_all2'([], _, _, []).
'$import_all2'([PI as NewName|Rest], Context, Source,
	       [NewName/Arity|Imported]) :- !,
	'$canonical_pi'(PI, Name/Arity),
	length(Args, Arity),
	Head =.. [Name|Args],
	NewHead =.. [NewName|Args],
	source_location(File, _Line),
	(   '$get_predicate_attribute'(Source:Head, transparent, 1)
	->  '$set_predicate_attribute'(Context:NewHead, transparent, 1)
	;   true
	),
	'$store_clause'((NewHead :- Source:Head), File),
	'$import_all2'(Rest, Context, Source, Imported).
'$import_all2'([Pred|Rest], Context, Source, [Pred|Imported]) :-
	Context:import(Source:Pred),
	'$import_wic'(Source, Pred),
	'$import_all2'(Rest, Context, Source, Imported).


'$list_to_conj'([One], One) :- !.
'$list_to_conj'([H|T], (H,Rest)) :-
	'$list_to_conj'(T, Rest).

%%	'$import_ops'(+Target, +Source)
%	
%	Import the operators export from Source into the module table of
%	Target.

'$import_ops'(To, From) :-
	(   '$c_current_predicate'(_, From:'$exported_op'(_, _, _)),
	    From:'$exported_op'(Pri, Assoc, Name),
	    op(Pri, Assoc, To:Name),
	    fail
	;   true
	).


%%	'$export_list'(+Declarations, +Module, -Ops)
%	
%	Handle the export list of the module declaration for Module
%	associated to File.

'$export_list'(Decls, Module, Ops) :-
	is_list(Decls), !,
	'$do_export_list'(Decls, Module, Ops).
'$export_list'(Decls, _, _) :-
	var(Decls),
	throw(error(instantiation_error, _)).
'$export_list'(Decls, _, _) :-
	throw(error(type_error(list, Decls), _)).

'$do_export_list'([], _, []) :- !.
'$do_export_list'([H|T], Module, Ops) :- !,
	'$export1'(H, Module, Ops, Ops1),
	'$do_export_list'(T, Module, Ops1).

'$export1'(Var, _, _, _) :-
	var(Var), !,
	throw(error(instantiation_error, _)).
'$export1'(Op, _, [Op|T], T) :-
	Op = op(_,_,_), !.
'$export1'(PI, Module, Ops, Ops) :-
	export(Module:PI).

'$export_ops'([op(Pri, Assoc, Name)|T], Module, File) :-
	op(Pri, Assoc, Module:Name),
	'$store_clause'('$exported_op'(Pri, Assoc, Name), File),
	'$export_ops'(T, Module, File).
'$export_ops'([], _, _).


%%	'$consult_stream'(+Stream, +File)
%
%	Read and record all clauses until the rest of the file.

'$consult_stream'(In, File) :-
	repeat,
	catch('$consult_stream2'(In, File),
	      E,
	      (	  print_message(error, E),
		  fail
	      )), !.

	      
'$consult_stream2'(In, File) :-
	repeat,
	    '$read_clause'(In, Clause),
	    expand_term(Clause, Expanded),
	    '$store_clause'(Expanded, File),
	    Clause == end_of_file, !.

'$consult_clause'(Clause, File) :-
	catch((expand_term(Clause, Expanded),
	       '$store_clause'(Expanded, File)),
	       E,
	       '$print_message_fail'(E)).

'$execute_directive'(include(File), F) :- !,
	'$expand_include'(File, F).
'$execute_directive'(encoding(Encoding), F) :- !,
	source_location(F, _),
	'$load_input'(F, S),
	set_stream(S, encoding(Encoding)).
'$execute_directive'(ISO, F) :-
	'$expand_directive'(ISO, Normal), !,
	'$execute_directive'(Normal, F).
'$execute_directive'(Goal, _) :-
	\+ flag('$compiling', database, database), !,
	'$add_directive_wic2'(Goal, Type),
	(   Type == call		% suspend compiling into .qlf file
	->  flag('$compiling', Old, database),
	    flag('$directive', OldDir, Old),
	    call_cleanup('$execute_directive2'(Goal),
			 (   flag('$compiling', _, Old),
			     flag('$directive', _, OldDir)
			 ))
	;   '$execute_directive2'(Goal)
	).
'$execute_directive'(Goal, _) :-
	'$execute_directive2'(Goal).

'$execute_directive2'(Goal) :-
	'$set_source_module'(Module, Module),
	catch(Module:Goal, Term, '$exception_in_directive'(Term)), !.
'$execute_directive2'(Goal) :-
	'$set_source_module'(Module, Module),
	print_message(warning, goal_failed(directive, Module:Goal)),
	fail.

'$exception_in_directive'(Term) :-
	print_message(error, Term),
	fail.

%	This predicate deals with the very odd ISO requirement to allow
%	for :- dynamic(a/2, b/3, c/4) instead of the normally used
%	:- dynamic a/2, b/3, c/4 or, if operators are not desirable,
%	:- dynamic((a/2, b/3, c/4)).

'$expand_directive'(Directive, Expanded) :-
	functor(Directive, Name, Arity),
	Arity > 1,
	'$iso_property_directive'(Name),
	Directive =.. [Name|Args],
	'$mk_normal_args'(Args, Normal),
	Expanded =.. [Name, Normal].

'$iso_property_directive'(dynamic).
'$iso_property_directive'(multifile).
'$iso_property_directive'(discontiguous).

'$mk_normal_args'([One], One).
'$mk_normal_args'([H|T0], (H,T)) :-
	'$mk_normal_args'(T0, T).


%	Note that the list, consult and ensure_loaded directives are already
%	handled at compile time and therefore should not go into the
%	intermediate code file.

'$add_directive_wic2'(Goal, Type) :-
	'$common_goal_type'(Goal, Type), !,
	(   Type == load
	->  true
	;   '$set_source_module'(Module, Module),
	    '$add_directive_wic'(Module:Goal)
	).
'$add_directive_wic2'(Goal, _) :-
	(   flag('$compiling', qlf, qlf)	% no problem for qlf files
	->  true
	;   print_message(error, mixed_directive(Goal))
	).
	
'$common_goal_type'((A,B), Type) :- !,
	'$common_goal_type'(A, Type),
	'$common_goal_type'(B, Type).
'$common_goal_type'((A;B), Type) :- !,
	'$common_goal_type'(A, Type),
	'$common_goal_type'(B, Type).
'$common_goal_type'((A->B), Type) :- !,
	'$common_goal_type'(A, Type),
	'$common_goal_type'(B, Type).
'$common_goal_type'(Goal, Type) :-
	'$goal_type'(Goal, Type).

'$goal_type'(Goal, Type) :-
	(   '$load_goal'(Goal)
	->  Type = load
	;   Type = call
	).

'$load_goal'([_|_]).
'$load_goal'(consult(_)).
'$load_goal'(load_files(_)).
'$load_goal'(load_files(_,Options)) :-
	memberchk(qcompile(true), Options).
'$load_goal'(ensure_loaded(_)) :- flag('$compiling', wic, wic).
'$load_goal'(use_module(_))    :- flag('$compiling', wic, wic).
'$load_goal'(use_module(_, _)) :- flag('$compiling', wic, wic).

		/********************************
		*        TERM EXPANSION         *
		*********************************/

:- user:dynamic(term_expansion/2).
:- user:multifile(term_expansion/2).
:- user:dynamic(goal_expansion/2).
:- user:multifile(goal_expansion/2).

expand_term(Var, Expanded) :-
	var(Var), !,
	Expanded = Var.
expand_term(Term, Expanded) :-		% local term-expansion
	'$term_expansion_module'(Module),
	Module:term_expansion(Term, Expanded0), !,
	'$expand_clauses'(Expanded0, Expanded).
expand_term(Head --> Body, Expanded) :-
	'$translate_rule'(Head --> Body, Expanded0), !,
	'$expand_clauses'(Expanded0, Expanded).
expand_term(Term, []) :-
	'$if_expansion'(Term, X),
	X == [], !.
expand_term(Term0, Term) :-
	'$goal_expansion_module'(_), !,
	'$expand_clauses'(Term0, Term).
expand_term(Term, Term).


'$store_clause'([], _) :- !.
'$store_clause'([C|T], F) :- !,
	'$store_clause'(C, F),
	'$store_clause'(T, F).
'$store_clause'(end_of_file, _) :- !.
'$store_clause'((:- Goal), F) :- !,
	'$execute_directive'(Goal, F).
'$store_clause'((?- Goal), F) :- !,
	'$execute_directive'(Goal, F).
'$store_clause'((_, _), _) :- !,
	print_message(error, cannot_redefine_comma),
	fail.
'$store_clause'('$source_location'(File, Line):Term, _) :- !,
	'$record_clause'(Term, File:Line, Ref),
	'$qlf_assert_clause'(Ref).
'$store_clause'(Term, File) :-
	'$record_clause'(Term, File, Ref),
        '$qlf_assert_clause'(Ref).

'$qlf_assert_clause'(_) :-
	flag('$compiling', database, database), !.
'$qlf_assert_clause'(Ref) :-
	'$qlf_assert_clause'(Ref, development).


%%	compile_aux_clauses(+Clauses) is det.
%
%	Compile clauses given the current  source   location  but do not
%	change  the  notion  of   the    current   procedure  such  that
%	discontiguous  warnings  are  not  issued.    The   clauses  are
%	associated with the current file and  therefore wiped out if the
%	file is reloaded.

compile_aux_clauses(Clauses) :-
	source_location(File, _Line),
	'$start_aux'(File, Context),
	call_cleanup('$store_clause'(Clauses, File),
		     '$end_aux'(File, Context)).


		 /*******************************
		 *	     INCLUDE		*
		 *******************************/

:- multifile
	'$included'/2.
:- dynamic
	'$included'/2.

'$expand_include'(File, FileInto) :-
	absolute_file_name(File,
			   [ file_type(prolog),
			     access(read)
			   ], Path),
	'$push_input_context',
	open(Path, read, In),
	'$read_clause'(In, Term0),
	'$read_include_file'(Term0, In, Terms),
	close(In),
	'$pop_input_context',
	'$store_clause'(system:'$included'(FileInto, Path), FileInto),
	'$consult_clauses'(Terms, FileInto).

'$read_include_file'(end_of_file, _, []) :- !.
'$read_include_file'(T0, In, [T0|T]) :-
	'$read_clause'(In, T1),
	'$read_include_file'(T1, In, T).

'$consult_clauses'([], _).
'$consult_clauses'([H|T], File) :-
	'$consult_clause'(H, File),
	'$consult_clauses'(T, File).


		 /*******************************
		 *	:- IF ... :- ENDIF	*
		 *******************************/

:- thread_local
	'$include_code'/1.

'$including' :-
	'$include_code'(X), !,
	X == true.
'$including'.

'$if_expansion'((:- if(G)), []) :-
	(   '$including'
	->  (   catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true))
	    ;   asserta('$include_code'(false))
	    )
	;   asserta('$include_code'(else_false))
	).
'$if_expansion'((:- elif(G)), []) :-
	(   retract('$include_code'(Old))
	->  (   Old == true
	    ->  asserta('$include_code'(else_false))
	    ;   Old == false,
		catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true))
	    ;	asserta('$include_code'(Old))
	    )
	;    throw(error(context_error(no_if), _))
	).
'$if_expansion'((:- else), []) :-
	(   retract('$include_code'(X))
	->  (   X == true
	    ->  X2 = false 
	    ;   X == false
	    ->	X2 = true
	    ;	X2 = X
	    ),
	    asserta('$include_code'(X2))
	;   throw(error(context_error(no_if), _))
	).
'$if_expansion'(end_of_file, end_of_file) :- !. % TBD: Check completeness
'$if_expansion'((:- endif), []) :-
	retract('$include_code'(_)), !.

'$if_expansion'(_, []) :-
	\+ '$including'.

'$eval_if'(G) :-
	expand_goal(G, G2),
	G2.


		 /*******************************
		 *	       READING		*
		 *******************************/

:- multifile
	prolog:comment_hook/3.

'$read_clause'(In, Term) :-
	'$get_predicate_attribute'(prolog:comment_hook(_,_,_),
				   number_of_clauses, N),
	N > 0, !,
	'$set_source_module'(SM, SM),
	Options0 = [ errors(dec10),
		     comments(Comments),
		     term_position(Pos),
		     module(SM)
		   ],
	'$singleton_option'(SM, Options, Options0),
	read_term(In, Term, Options),
	(   Comments \== [],
	    catch(prolog:comment_hook(Comments, Pos, Term), E,
		  print_message(error, E))
	->  true
	;   true
	).
'$read_clause'(In, Term) :-
	read_clause(In, Term).

'$singleton_option'(M, [singletons(warning)|T],T) :-
	M:'$style_check'(Old, Old),
	Old /\ 2'0000010 =\= 0, !.	% See style_check/1
'$singleton_option'(_, T, T).


		 /*******************************
		 *	 FOREIGN INTERFACE	*
		 *******************************/

%	call-back from PL_register_foreign().  First argument is the module
%	into which the foreign predicate is loaded and second is a term
%	describing the arguments.

:- dynamic
	'$foreign_registered'/2.


		 /*******************************
		 *   GOAL_EXPANSION/2 SUPPORT	*
		 *******************************/

'$expand_clauses'(X, X) :-
	var(X), !.
'$expand_clauses'([H0|T0], [H|T]) :- !,
	'$expand_clauses'(H0, H),
	'$expand_clauses'(T0, T).
'$expand_clauses'('$source_location'(File, Line):Clause0,
		  '$source_location'(File, Line):Clause) :- !,
	'$expand_clauses'(Clause0, Clause).
'$expand_clauses'((Head :- Body), (Head :- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body,  ExpandedBody).
'$expand_clauses'((:- Body), (:- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body,  ExpandedBody).
'$expand_clauses'(Head, Head).

expand_goal(A, B) :-
        '$do_expand_body'(A, B0),
	'$tidy_body'(B0, B).

'$do_expand_body'(G, G) :-
        var(G), !.
'$do_expand_body'((A,B), (EA,EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((A;B), (EA;EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((A->B), (EA->EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((A*->B), (EA*->EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((\+A), (\+EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(A, B) :-
        '$goal_expansion_module'(M),
        M:goal_expansion(A, B0),
	B0 \== A, !,			% avoid a loop
	'$do_expand_body'(B0, B).
'$do_expand_body'(not(A), not(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(call(A), call(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(once(A), once(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(ignore(A), ignore(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(initialization(A), initialization(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(catch(A, E, B), catch(EA, E, EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'(call_cleanup(A, B), call_cleanup(EA, EB)) :- !,
        '$do_expand_body'(A, EA),
	'$do_expand_body'(B, EB).
'$do_expand_body'(call_cleanup(A, R, B), call_cleanup(EA, R, EB)) :- !,
        '$do_expand_body'(A, EA),
	'$do_expand_body'(B, EB).
'$do_expand_body'(forall(A, B), forall(EA, EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'(findall(V, G, B), findall(V, EG, B)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(findall(V, G, B, T), findall(V, EG, B, T)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(bagof(V, G, B), bagof(V, EG, B)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(setof(V, G, B), setof(V, EG, B)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(V^G, V^EG) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(M:G, M:EG) :-
	atom(M),
	'$set_source_module'(Old, M),
	call_cleanup('$do_expand_body'(G, EG),
		     '$set_source_module'(_, Old)), !.
'$do_expand_body'(A, A).

%	Delete extraneous true's that result from goal_expansion(..., true)
%
%	Is the really necessary?  Should we only do it if -O is effective?

'$tidy_body'(A, A) :-
	current_prolog_flag(optimise, false), !.
'$tidy_body'(A, A) :-
        var(A), !.
'$tidy_body'((A,B), (A, TB)) :-
        var(A), !,
        '$tidy_body'(B, TB).
'$tidy_body'((A,B), (TA, B)) :-
        var(B), !,
        '$tidy_body'(A, TA).
'$tidy_body'(((A,B),C), R) :- !,
	'$tidy_body'((A,B,C), R).
'$tidy_body'((true,A), R) :- !,
        '$tidy_body'(A, R).
'$tidy_body'((A,true), R) :- !,
        '$tidy_body'(A, R).
'$tidy_body'((A,B), (TA, TB)) :- !,
        '$tidy_body'(A, TA),
        '$tidy_body'(B, TB).
'$tidy_body'((A;B), (TA; TB)) :- !,
        '$tidy_body'(A, TA),
        '$tidy_body'(B, TB).
'$tidy_body'((A->B), (TA->TB)) :- !,
        '$tidy_body'(A, TA),
        '$tidy_body'(B, TB).
'$tidy_body'(A, A).


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

The original version contained  '$t_tidy'/2  to   convert  ((a,b),  c)  to
(a,(b,c)), but as the  SWI-Prolog  compiler   doesn't  really  care (the
resulting code is simply the same), I've removed that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$translate_rule'(((LP,MNT)-->RP),(H:-B)) :- !,
	( var(LP) -> throw(error(instantiation_error,_)) ; true ),
	'$extend'(LP, S0, SR, H),
	'$t_body'(RP, S0, S1, B0),
	'$t_body'(MNT, SR, S1,M),
	'$body_optimized'((B0,M),B1,S0),
	'$body_optimized'(B1,B,SR).
'$translate_rule'((LP-->RP), (H:-B)):-
	'$extend'(LP, S0, S, H),
	'$t_body'(RP, S0, S, B0),
	'$body_optimized'(B0,B,S0).

'$body_optimized'(B0,B,S0) :-
	(   B0 = (S00=X, B),		% map a(H,T) :- H = [a,b|T], b(T)
	    S00 == S0
	->  S0 = X			% into a([a,b|T0]) :- b(T0, T).
	;   B0 = (S00=X),		% map a(H,T) :- H = [a,b|T]
	    S00 == S0
	->  S0 = X,			% into a([a,b|T], T)
	    B = true
	;   B0 = B
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On the DCG Translation of {}

	a --> x, {y}.

There are two options.  In traditional systems we see:

	a(A, B) :- x(A, B), y.

And in modern system we see:

	a(A, B) :- x(A, C), y, B=C.


Martin Sondergaard's grammar was breaking down on
=================================================

s --> v, star0, {write('You can not do that')}.
star0 --> [].
star0 --> [_], star0.

meaning to write a  message  for  any   sentence  starting  with  a `v',
skipping the remainder. With delayed binding  this causes a large number
of messages as star0 only eats one token on backtracing.

You can fix this using remaining as below rather then star0.

remaining --> [_], !, remaining.
remaining --> [].


Without delayed unification of the tail we get the following trouble
====================================================================
(comment from Richard O'Keefe)

Suppose I have

    p --> [a], !, {fail}. p --> [].

That is, p//0 is suppose to match the empty  string as long as it is not
followed by a. Now consider

    p([a], [a])

If the first clause is translated as

    p(S0, S) :- S0 = [a|S1], !, fail, S = S1.

then it will work *correctly*, and the call  p([a], [a]) will fail as it
is supposed to. If the first clause is translated as

    p(S0, S) :- S0 = [a|S], !, fail.

then the call p([a], [a]) will succeed, which is quite definitely wrong.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$t_body'(Var, S, SR, phrase(Var, S, SR)) :-
	var(Var), !.
'$t_body'([], S, SR, S=SR) :- !.		% inline lists
'$t_body'(List, S, SR, C) :-
	List = [_|_], !,
	(   is_list(List)
	->  '$append'(List, SR, OL),
	    C = (S = OL)
	;   C = '$append'(List, SR, S)	% Deals with [H|T] in body
	).
'$t_body'(!, S, S, !) :- !.
'$t_body'({}, S, S, true) :- !.
'$t_body'({T}, S, SR, (T, SR = S)) :- !.		% (*)
%'$t_body'({T}, S, S, T) :- !.			% (*)
'$t_body'((T, R), S, SR, (Tt, Rt)) :- !,
	'$t_body'(T, S, SR1, Tt),
	'$t_body'(R, SR1, SR, Rt).
'$t_body'((T;R), S, SR, (Tt;Rt)) :- !,
	'$t_body'(T, S, S1, T1), '$t_fill'(S, SR, S1, T1, Tt),
	'$t_body'(R, S, S2, R1), '$t_fill'(S, SR, S2, R1, Rt).
'$t_body'((T|R), S, SR, (Tt;Rt)) :- !,
	'$t_body'(T, S, S1, T1), '$t_fill'(S, SR, S1, T1, Tt),
	'$t_body'(R, S, S2, R1), '$t_fill'(S, SR, S2, R1, Rt).
'$t_body'((C->T), S, SR, (Ct->Tt)) :- !,
	'$t_body'(C, S, SR1, Ct),
	'$t_body'(T, SR1, SR, Tt).
'$t_body'((C*->T), S, SR, (Ct*->Tt)) :- !,
	'$t_body'(C, S, SR1, Ct),
	'$t_body'(T, SR1, SR, Tt).
'$t_body'((\+ C), S, S, (\+ Ct)) :- !,
	'$t_body'(C, S, _, Ct).
'$t_body'(T, S, SR, Tt) :-
	'$extend'(T, S, SR, Tt).


'$t_fill'(S, SR, S1, T, (T, SR=S)) :-
	S1 == S, !.
'$t_fill'(_S, SR, SR, T, T).


%	'$extend'(+Head, +Extra1, +Extra2, -NewHead)
%	
%	Extend Head with two more arguments (on behalf DCG compilation).
%	The solution below is one option. Using   =..  and append is the
%	alternative. In the current version (5.3.2), the =.. is actually
%	slightly faster, but it creates less garbage.

:- dynamic  '$extend_cache'/4.
:- volatile '$extend_cache'/4.

'$dcg_reserved'([]).
'$dcg_reserved'([_|_]).
'$dcg_reserved'({_}).
'$dcg_reserved'({}).
'$dcg_reserved'(!).
'$dcg_reserved'((\+_)).
'$dcg_reserved'((_,_)).
'$dcg_reserved'((_;_)).
'$dcg_reserved'((_|_)).
'$dcg_reserved'((_->_)).
'$dcg_reserved'((_*->_)).
'$dcg_reserved'((_-->_)).

'$extend'(V, _, _, _) :-
	var(V), !,
	throw(error(instantiation_error,_)).
'$extend'(M:OldT, A1, A2, M:NewT) :- !,
	'$extend'(OldT, A1, A2, NewT).
'$extend'(OldT, A1, A2, NewT) :-
	'$extend_cache'(OldT, A1, A2, NewT), !.
'$extend'(OldT, A1, A2, NewT) :-
	( callable(OldT) -> true ; throw(error(type_error(callable,OldT),_)) ),
	( '$dcg_reserved'(OldT) -> throw(error(permission_error(define,dcg_nonterminal,OldT),_)) ; true ),
	functor(OldT, Name, Arity),
	functor(CopT, Name, Arity),
	NewArity is Arity+2,
	functor(NewT, Name, NewArity),
	'$copy_args'(1, Arity, CopT, NewT),
	A1Pos is Arity+1,
	A2Pos is Arity+2,
	arg(A1Pos, NewT, A1C),
	arg(A2Pos, NewT, A2C),
	assert('$extend_cache'(CopT, A1C, A2C, NewT)),
	OldT = CopT,
	A1C = A1,
	A2C = A2.

'$copy_args'(I, Arity, Old, New) :-
	I =< Arity, !,
	arg(I, Old, A),
	arg(I, New, A),
	I2 is I + 1,
	'$copy_args'(I2, Arity, Old, New).
'$copy_args'(_, _, _, _).

:- module_transparent
	phrase/2,
	phrase/3.

phrase(RuleSet, Input) :-
	phrase(RuleSet, Input, []).
phrase(RuleSet, Input, Rest) :-
	strip_module(RuleSet, M, Plain),
	( var(Plain) -> throw(error(instantiation_error,_)) ; true ),
	'$t_body'(Plain, S0, S, Body),
	Input = S0, Rest = S,
	M:Body.


		/********************************
		*     WIC CODE COMPILER         *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This entry point is called from pl-main.c  if the -c option (compile) is
given. It compiles all files and finally calls qsave_program to create a
saved state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'$compile_wic' :-
	current_prolog_flag(argv, Argv),
	'$get_files_argv'(Argv, Files),
	'$translate_options'(Argv, Options),
	'$option'(compileout, Out, Out),
        user:consult(Files),
	user:qsave_program(Out, Options).

'$get_files_argv'([], []) :- !.
'$get_files_argv'(['-c'|Files], Files) :- !.
'$get_files_argv'([_|Rest], Files) :-
	'$get_files_argv'(Rest, Files).

'$translate_options'([], []).
'$translate_options'([O|T0], [Opt|T]) :-
	atom_chars(O, [-,-|Rest]),
	'$split'(Rest, [=], Head, Tail), !,
	atom_chars(Name, Head),
	name(Atom, Tail),
	term_to_atom(Value, Atom),
	Opt =.. [Name, Value],
	'$translate_options'(T0, T).
'$translate_options'([_|T0], T) :-
	'$translate_options'(T0, T).

'$split'(List, Split, [], Tail) :-
	'$append'(Split, Tail, List), !.
'$split'([H|T0], Split, [H|T], Tail) :-
	'$split'(T0, Split, T, Tail).


		/********************************
		*       LIST PROCESSING         *
		*********************************/

'$member'(X, [X|T]) :-
	(   T == []
	->  !
	;   true
	).
'$member'(X, [_|T]) :-
	'$member'(X, T).

'$append'([], L, L).
'$append'([H|T], L, [H|R]) :-
	'$append'(T, L, R).

'$select'(X, [X|Tail], Tail).
'$select'(Elem, [Head|Tail], [Head|Rest]) :-
	'$select'(Elem, Tail, Rest).

'$reverse'(L1, L2) :-
	'$reverse'(L1, [], L2).

'$reverse'([], List, List).
'$reverse'([Head|List1], List2, List3) :-
	'$reverse'(List1, [Head|List2], List3).

'$delete'([], _, []) :- !.
'$delete'([Elem|Tail], Elem, Result) :- !, 
	'$delete'(Tail, Elem, Result).
'$delete'([Head|Tail], Elem, [Head|Rest]) :-
	'$delete'(Tail, Elem, Rest).


		 /*******************************
		 *   HANDLE TRACER 'L'-COMMAND	*
		 *******************************/

:- multifile
	user:prolog_list_goal/1.

'$prolog_list_goal'(Goal) :-
	user:prolog_list_goal(Goal), !.
'$prolog_list_goal'(Goal) :-
	user:listing(Goal).


		 /*******************************
		 *	       HALT		*
		 *******************************/

halt :-
	halt(0).


:- module_transparent
	at_halt/1.
:- dynamic
	'$at_halt'/1.

at_halt(Spec) :-
	strip_module(Spec, Module, Goal),
	asserta(system:'$at_halt'(Module:Goal)).

'$run_at_halt' :-
	(   '$at_halt'(Goal),
	    catch(Goal, E, print_message(error, E)),
	    fail
	;   true
	).


		/********************************
		*      LOAD OTHER MODULES       *
		*********************************/

:- module_transparent
	'$load_wic_files'/2,
	'$load_additional_boot_files'/0.

'$load_wic_files'(Module, Files) :-
	'$execute_directive'('$set_source_module'(OldM, Module), []),
	'$save_lex_state'(LexState),
	'$style_check'(_, 2'1111),
	flag('$compiling', OldC, wic),
	consult(Files),
	'$execute_directive'('$set_source_module'(_, OldM), []),
	'$execute_directive'('$restore_lex_state'(LexState), []),
	flag('$compiling', _, OldC).


'$load_additional_boot_files' :-
	current_prolog_flag(argv, Argv),
	'$get_files_argv'(Argv, Files),
	(   Files \== []
	->  format('Loading additional boot files~n'),
	    '$load_wic_files'(user, Files),
	    format('additional boot files loaded~n')
	;   true
        ).

'$:-'((format('Loading Prolog startup files~n', []),
       source_location(File, _Line),
       file_directory_name(File, Dir),
       atom_concat(Dir, '/load.pl', LoadFile),
       '$load_wic_files'(system, [LoadFile]),
       (   current_prolog_flag(windows, true)
       ->  atom_concat(Dir, '/menu.pl', MenuFile),
	   '$load_wic_files'(system, [MenuFile])
       ;   true
       ),
       format('SWI-Prolog boot files loaded~n', []),
       flag('$compiling', OldC, wic),
       '$execute_directive'('$set_source_module'(_, user), []),
       flag('$compiling', _, OldC)
      )).
