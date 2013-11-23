/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam, VU University Amsterdam

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

:- module(qp_foreign,
	  [ load_foreign_files/0,		%
	    load_foreign_files/2,		% +Files, +Libs
	    load_foreign_files/3,		% +Object, +Files, +Libs
	    make_shared_object/3,		% +Object, +Files, +Libs
	    make_foreign_wrapper_file/1,	% +OutBase
	    make_foreign_wrapper_file/2,	% +OFiles, +OutBase
						% SICStus stuff
	    make_foreign_resource_wrapper/3,    % +Resource, +ResBase, +FileBase
	    load_foreign_resource/2		% +Resource, +Dir
	  ]).

:- use_module(library(shlib)).
:- use_module(library(gensym)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).

/** <module> Quintus compatible foreign loader

This module defines a  Quintus   compatible  foreign  language interface
based on the foreign_file/2 and foreign/3 declarations.

Predicates:

	* load_foreign_files
	Load all foreign files defined with foreign_file/2 statement
	in the current module.

	* load_foreign_files(+Files, +Libs)
	Load specified foreign files, linking them with the given
	libraries

	* load_foreign_files(+SharedObject, +Files, +Libs)
	As load_foreign_files/2, but first tries to load `SharedObject'.
	If the SharedObject exists it is loaded using load_foreign_library/1.
	Otherwise it is first created with make_shared_object/3.

	* make_shared_object(+SharedObject, +Files, +Libs)
	Generate a wrapper and link it using plld to the given SharedObject.

	* make_foreign_wrapper_file(+Files, +OutBase)
	Generate wrapper for the named object files in OutBase.c.

	* make_foreign_wrapper_file(+Files)
	Generate wrapper for all declared object files in OutBase.c.

Example:

	==
	foreign_file('-lm', [sin/2]).
	foreign(sin, c, sin(+float, [-float])).
	:- load_foreign_files,
	   abolish(foreign_file, 2),
	   abolish(foreign, 3).
	==

Supported types:

	| *Spec*	| *Prolog*	| *C*                           |
	| integer	| integer	| long                          |
	| float		| float,integer	| double                        |
	| single	| float,integer	| single                        |
	| string	| atom,string	| char *			|
	| atom		| atom		| atom identifier (type atomic) |

*NOTE*	This modules requires a correctly functioning plld and
	load_foreign_library/1 on your system.  If this isn't the
	case use make_foreign_wrapper_file/[1,2] to generate a
	wrapper and use static embedding.

@bug	Only supports C-interface
@bug	Insufficient checking for misusage.
@bug	Documentation is too short and very outdated
*/

:- module_transparent
	load_foreign_files/0.

:- meta_predicate
	load_foreign_files(:, +),
	load_foreign_files(+, :, +),
	make_shared_object(+, :, +),
	make_foreign_wrapper_file(:),
	make_foreign_wrapper_file(:, +),
					% SICStus
	make_foreign_resource_wrapper(:, +),
	load_foreign_resource(:, +).

setting(linker, 'swipl-ld').

hook(M:Goal) :-
	M:Goal.

%%	make_wrappers(+PrologHeads, +Module, +OutStream)

make_wrappers([], _, _).
make_wrappers([H|T], M, Out) :-
	make_wrapper(Out, M:H),
	make_wrappers(T, M, Out).

%%	make_wrapper(+Stream, :PrologHead)
%
%	Generates a C-wrapper function for the given foreign defined
%	Prolog predicate.  The wrapper is called _plw_<predname><arity>.

make_wrapper(Out, Spec) :-
	get_foreign_head(Spec, Func, Head), !,
	(   check_head(Head)
	->  wrapper_name(Head, WrapName, ArgN),
	    make_C_header(Out, WrapName, ArgN),
	    make_C_decls(Out, Head),
	    make_C_prototype(Out, Head),
	    make_C_input_conversions(Out, Head),
	    make_C_wrapper_setup(Out),
	    make_C_call(Out, Head, Func),
	    make_C_wrapper_check(Out),
	    make_C_output_conversions(Out, Head),
	    make_C_footer(Out)
	;   fail
	).
make_wrapper(_, Spec) :-
	existence_error(foreign_declaration, Spec).

%%	get_foreign_head(:Spec, -Func, -Head)
%
%	Get 3rd argument of relevant foreign/3   clause. Seems there are
%	two versions. In Quintus Spec was  a predicate specification and
%	in SICStus it seems to be a (C) function name.

get_foreign_head(M:Function, Function, M:Head) :-
	prolog_load_context(dialect, sicstus), !,
	hook(M:foreign(Function, c, Head)).
get_foreign_head(M:Spec, Func, M:Head) :-
	(   atom(Spec),
	    hook(M:foreign(Spec, c, Head)),
	    functor(Head, Spec, _)
	->  true
	;   Spec = Name/Arity
	->  functor(Head, Name, Arity),
	    hook(M:foreign(Func, c, Head))
	;   Head = Spec,
	    hook(M:foreign(Func, c, Head))
	).


check_head(_:Head) :-
	functor(Head, _, Arity),
	(   Arity == 0
	->  true
	;   arg(_, Head, [-T]),
	    \+ valid_type(T)
	->  warning('Bad return type ~w in ~w', [T, Head]),
	    fail
	;   arg(N, Head, [-_T]),
	    N \== Arity
	->  warning('Return type must be last in ~w', Head),
	    fail
	;   (arg(_, Head, -T) ; arg(_, Head, +T)),
	    \+ valid_type(T)
	->  warning('Bad type ~w in ~w', [T, Head]),
	    fail
	;   true
	).

valid_type(int).
valid_type(integer).
valid_type(size_t).
valid_type(float).
valid_type(single).
valid_type(string).
valid_type(chars).			% actually, `codes'!
valid_type(atom).
valid_type(term).
valid_type(address).
valid_type(address(_)).

%%	cvt_name(+Type, +IO, -Suffix) is det.

cvt_name(chars,	     _,	codes) :- !.
cvt_name(address(_), _,	address) :- !.
cvt_name(int,	     o,	int64) :- !.
cvt_name(integer,    o,	int64) :- !.
cvt_name(size_t,     o,	int64) :- !.
cvt_name(integer,    i,	long) :- !.
cvt_name(Type,	     _,	Type).


%%	make_C_header(+Stream, +WrapperName, +Arity)
%
%	Write function-header for the wrapper.  This is easy as the
%	the return-type is always foreign_t and the arguments are
%	always of type `term_t'.  The arguments are simply named `a',
%	`b', ...

make_C_header(Out, WrapName, ArgN) :-
	format(Out, '~n~nstatic foreign_t~n~w(', [WrapName]),
	forall(between(1, ArgN, A),
	       (   (A \== 1 -> format(Out, ', ', []) ; true)
	       ,   arg_name(A, AName),
		   format(Out, 'term_t ~w', [AName])
	       )),
	format(Out, ')~n{ ', []).

%%	make_C_decls(+Stream, :PrologHead)
%
%	Writes the C variable declarations. If  the return value is used
%	a variable named `rval' is created. For each input parameter a C
%	variable named i<argname> is created;   for each output variable
%	o<argname>.

make_C_decls(Out, _:Head) :-
	compound(Head),
	arg(_, Head, [-PlType]),
	map_C_type(PlType, CType),
	format(Out, '~wrval;~n  ', [CType]),
	fail.
make_C_decls(Out, _:Head) :-
	compound(Head),
	arg(N, Head, -PlType),
	arg_name(N, AName),
	(   PlType == term
	->  format(Out, 'term_t o_~w = PL_new_term_ref();~n  ', [AName])
	;   map_C_type(PlType, CType),
	    format(Out, '~wo_~w;~n  ', [CType, AName])
	),
	fail.
make_C_decls(Out, _:Head) :-
	compound(Head),
	arg(N, Head, +PlType),
	PlType \== term,
	map_C_type(PlType, CType),
	CType \== term,
	arg_name(N, AName),
	format(Out, '~wi_~w;~n  ', [CType, AName]),
	fail.
make_C_decls(Out, _) :-
	format(Out, '~n', []).

%%	make_C_prototype(+Stream, :PrologHead)
%
%	If the function handles floats or doubles, make	a prototype
%	declaration for it to avoid unwanted conversions.

make_C_prototype(Out, M:Head) :-
	(   compound(Head),
	    arg(_, Head, [-Type])
	->  map_C_type(Type, CType)
	;   CType = 'void '
	),
	copy_term(Head, H2),		% don't bind Head
	hook(M:foreign(CFunc, c, H2)), !,
	format(Out, '  extern ~w~w(', [CType, CFunc]),
	(   compound(Head),
	    arg(N, Head, AType),
	    AType \= [_],		% return-type
	    (N > 1 -> format(Out, ', ', []) ; true),
	    (   AType = +T2
	    ->  map_C_type(T2, CT2),
		format(Out, '~w', [CT2])
	    ;	AType == -term
	    ->	format(Out, term_t, [])
	    ;   AType = -T2
	    ->  map_C_type(T2, CT2),
		format(Out, '~w *', [CT2])
	    ),
	    fail
        ;   format(Out, ');~n~n', [])
	).
make_C_prototype(_, _).


%%	make_C_input_conversions(+Stream, :PrologHead)
%
%	Generate the input checking and conversion code.  Assumes
%	boolean functions that take a Prolog term_t as first argument
%	and a pointer to the requested C-type as a second argument.
%	Function returns 0 if the conversion fails.

make_C_input_conversions(Out, _:Head) :-
	findall(N-T, (compound(Head),arg(N, Head, +T)), IArgs0),
	exclude(term_arg, IArgs0, IArgs),
	(   IArgs == []
	->  true
	;   format(Out, '  if ( ', []),
	    (	member(N-T, IArgs),
		T \== term,
		(IArgs \= [N-T|_] -> format(Out, ' ||~n       ', []) ; true),
		arg_name(N, AName),
		atom_concat(i_, AName, IName),
		cvt_name(T, i, CVT),
		format(Out, '!PL_cvt_i_~w(~w, &~w)', [CVT, AName, IName]),
		fail
	    ;	true
	    ),
	    format(Out, ' )~n    return FALSE;~n~n', [])
	).

term_arg(_-term).


%%	make_C_call(+Stream, :PrologHead, +CFunction)
%
%	Generate  the  actual  call  to   the  foreign  function.  Input
%	variables may be handed directly; output  variables as a pointer
%	to the o<var>, except for output-variables of type term.

make_C_call(Out, _:Head, CFunc) :-
	(   compound(Head),
	    arg(_, Head, [-_])
	->  format(Out, '  rval = ~w(', [CFunc])
	;   format(Out, '  (void) ~w(', [CFunc])
	),
	compound(Head),
	arg(N, Head, Arg),
	Arg \= [_],
	(N \== 1 -> format(Out, ', ', []) ; true),
	arg_name(N, AName),
	(   Arg = -term
	->  format(Out, 'o_~w', [AName])
	;   Arg = -_
	->  format(Out, '&o_~w', [AName])
	;   Arg = +term
	->  format(Out, '~w', [AName])
	;   format(Out, 'i_~w', [AName])
	),
	fail.
make_C_call(Out, _, _) :-
	format(Out, ');~n', []).

%%	make_C_wrapper_setup(+Stream)
%
%	Call SP_WRAP_INIT() when  running  on   SICStus.  This  supports
%	SP_fail() and SP_raise_exception().

make_C_wrapper_setup(Stream) :-
	prolog_load_context(dialect, sicstus), !,
	format(Stream, '  SP_WRAP_INIT();~n', []).
make_C_wrapper_setup(_).


%%	make_C_wrapper_check(+Stream)
%
%	Call  SP_WRAP_CHECK_STATE()  when  running    on  SICStus.  This
%	supports SP_fail() and SP_raise_exception().

make_C_wrapper_check(Stream) :-
	prolog_load_context(dialect, sicstus), !,
	format(Stream, '  SP_WRAP_CHECK_STATE();~n', []).
make_C_wrapper_check(_).


%%	make_C_output_conversions(+Stream, :PrologHead)
%
%	Generate conversions for the output arguments and unify them
%	with the Prolog term_t arguments.

make_C_output_conversions(Out, _:Head) :-
	findall(N-T, (compound(Head),arg(N, Head, -T)), OArgs0),
	(   compound(Head),
	    arg(_, Head, [-RT])
	->  OArgs = [rval-RT|OArgs0]
	;   OArgs = OArgs0
	),
	(   OArgs == []
	->  true
	;   format(Out, '~n  if ( ', []),
	    (	member(N-T, OArgs),
		(   N == rval
		->  OName = rval,
		    arg(RN, Head, [-_]),
		    arg_name(RN, AName)
		;   arg_name(N, AName),
		    atom_concat(o_, AName, OName)
		),
		(OArgs = [N-T|_] -> true ; format(Out, ' ||~n       ', [])),
		(   T == term
		->  format(Out, '!PL_unify(~w, ~w)', [OName, AName])
		;   cvt_name(T, o, CVT),
		    format(Out, '!PL_cvt_o_~w(~w, ~w)', [CVT, OName, AName])
		),
		fail
	    ;	true
	    ),
	    format(Out, ' )~n    return FALSE;~n', [])
	).


make_C_footer(Out) :-
	format(Out, '~n  return TRUE;~n}~n', []).

		 /*******************************
		 *	  INIT STATEMENT	*
		 *******************************/

%%	make_C_init(+Stream, +InstallFunc, +InitFunc, +Module, +PredList)
%
%	Generate an array of PL_extension structures,   that may be used
%	to create a statically  linked  image   as  well  as through the
%	PL_load_extensions() call.
%
%	Of the supported PL_FA_<FLAGS>, TRANSPARENT   may be declared by
%	looking at the transparent  (meta_predivate)   attribute  of the
%	predicate.

make_C_init(Out, InstallFunc, Init, M, Preds) :-
	format(Out, '~n~nstatic PL_extension predicates [] =~n{~n', []),
	format(Out, '/*{ "name", arity, function, PL_FA_<flags> },*/~n', []),
	(   member(Pred, Preds),
	    get_foreign_head(M:Pred, _Func, Head),
	    Head = M:H,
	    functor(H, Name, Arity),
	    wrapper_name(Head, Wrapper, Arity),
	    foreign_attributes(M:H, Atts),
	    format(Out, '  { "~w", ~d, ~w, ~w },~n',
		   [Name, Arity, Wrapper, Atts]),
	    fail
	;   true
	),
	format(Out, '  { NULL, 0, NULL, 0 } /* terminator */~n};~n~n', []),
	format(Out, 'install_t~n~w()~n{ PL_load_extensions(predicates);~n',
	       [InstallFunc]),
	sicstus_init_function(Out, Init),
	format(Out, '}~n', []).

sicstus_init_function(_, -) :- !.
sicstus_init_function(Out, Init) :-
	format(Out, '  extern void ~w(int);~n', [Init]),
	format(Out, '  ~w(0);~n', [Init]).

foreign_attributes(Head, Atts) :-
	findall(A, foreign_attribute(Head, A), A0),
	(   A0 == []
	->  Atts = 0
	;   atomic_list_concat(A0, '|', Atts)
	).

foreign_attribute(Head, 'PL_FA_TRANSPARENT') :-
	predicate_property(Head, transparent).

%%	make_C_deinit(+Stream, +UninstallFunc, +DeInitFunc) is det.
%
%	Write the uninstall function

make_C_deinit(_, _, -) :- !.
make_C_deinit(Out, Func, DeInit) :-
	format(Out, '~ninstall_t~n', []),
	format(Out, '~w()~n', [Func]),
	format(Out, '{ extern void ~w(int);~n', [DeInit]),
	format(Out, '  ~w(0);~n', [DeInit]),
	format(Out, '}~n', []).


%%	make_C_file_header(+Stream)
%
%	Output the generic header declarations needed and some comments

make_C_file_header(Out) :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
	get_time(Time),
	format_time(string(When), '%F %H:%M', Time),
	format(Out, '/*  SWI-Prolog link wrapper~n', []),
	format(Out, '    Generated by SWI-Prolog version ~w.~w.~w~n',
	       [Major, Minor, Patch]),
	format(Out, '    At ~s~n', [When]),
	(   source_location(File, Line)
	->  format(Out, '    Source context ~w:~d~n', [File, Line])
	;   true
	),
	format(Out, '*/~n~n', []),
	format(Out, '#include <SWI-Prolog.h>~n', []),
	make_C_compat_file_header(Out),
	format(Out, '#ifndef NULL~n', []),
	format(Out, '#define NULL ((void *)0)~n', []),
	format(Out, '#endif~n~n', []).


make_C_compat_file_header(Out) :-
	prolog_load_context(dialect, sicstus), !,
	format(Out, '#define SP_WRAPPER 1~n', []),
	format(Out, '#include <sicstus/sicstus.h>~n', []).
make_C_compat_file_header(_).


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

%%	load_foreign_files is det.
%%	load_foreign_files(:Files, +Libs) is det.
%%	load_foreign_files(+SharedObject, :Files, +Libs) is det.
%
%	Calls make_foreign_wrapper_file(+File), compiles the wrapper
%	and loads the predicates.

load_foreign_files :-
	context_module(M),
	findall(File, hook(M:foreign_file(File, _)), OFiles),
	load_foreign_files(M:OFiles, []).
load_foreign_files(OFiles, Libs) :-
	gensym(link, LinkBase),
	load_foreign_files(LinkBase, OFiles, Libs).

load_foreign_files(LinkBase, M:_, _) :-
	catch(load_foreign_library(M:LinkBase), _, fail), !.
load_foreign_files(LinkBase, OFiles, Libs) :-
	make_shared_object(LinkBase, OFiles, Libs),
	OFiles = M:_List,
	load_foreign_library(M:LinkBase).

%%	make_shared_object(+Object, :Files, +Libs) is det.
%
%	Generate  a  wrapper  and  link  it  using  plld  to  the  given
%	SharedObject.

make_shared_object(LinkBase, M:OFiles, Libs) :-
	make_foreign_wrapper_file(M:OFiles, LinkBase),
	file_name_extension(LinkBase, c, CFile),
	build_shared_object(LinkBase, [CFile|OFiles], Libs).

%%	make_foreign_wrapper_file(:OutFile) is det.
%%	make_foreign_wrapper_file(:Files, +OutFile) is det.
%
%	Just output the wrapper file to the named .c file.  May be used
%	to prepare for static linking or the preparation of the native
%	SWI-Prolog foreign-file.

make_foreign_wrapper_file(M:CFile) :-
	findall(File, hook(M:foreign_file(File, _)), OFiles),
	make_foreign_wrapper_file(M:OFiles, CFile).
make_foreign_wrapper_file(M:OFiles, Base) :-
	file_name_extension(Base, c, CFile),
	file_base_name(Base, FuncBase),
	atom_concat(install_, FuncBase, InstallFunc),
	collect_foreign_predicates(OFiles, M, Preds),
	open(CFile, write, Out),
	make_C_file_header(Out),
	make_wrappers(Preds, M, Out),
	make_C_init(Out, InstallFunc, -, M, Preds),
	close(Out).


collect_foreign_predicates([], _, []).
collect_foreign_predicates([File|Files], M, Preds) :-
	hook(M:foreign_file(File, P0)),
	collect_foreign_predicates(Files, M, P1),
	append(P0, P1, Preds).

build_shared_object(Object, Files, Libs) :-
	current_prolog_flag(shared_object_extension, Ext),
	file_name_extension(Object, Ext, SharedObject),
	append(Files, Libs, Input),
	atomic_list_concat(Input, ' ', InputAtom),
	setting(linker, Linker),
	format(string(Command),
	       '~w -shared -o ~w ~w', [Linker, SharedObject, InputAtom]),
	shell(Command).


		 /*******************************
		 *	      SICSTUS		*
		 *******************************/

%%	make_foreign_resource_wrapper(:Resource, +ResBase, +FileBase)
%
%	Create a wrapper-file for the given foreign resource

make_foreign_resource_wrapper(M:Resource, ResBase, FileBase) :-
	hook(M:foreign_resource(Resource, Functions)),
	take(init(Init), Functions, Functions1, -),
	take(deinit(DeInit), Functions1, Preds, -),
	file_name_extension(FileBase, c, CFile),
	file_base_name(ResBase, FuncBase),
	atom_concat(install_, FuncBase, InstallFunc),
	atom_concat(uninstall_, FuncBase, UninstallFunc),
	open(CFile, write, Out),
	make_C_file_header(Out),
	make_wrappers(Preds, M, Out),
	make_C_init(Out, InstallFunc, Init, M, Preds),
	make_C_deinit(Out, UninstallFunc, DeInit),
	close(Out).

take(Term, List, Rest, Default) :-
	(   select(Term, List, Rest)
	->  true
	;   arg(1, Term, Default),
	    Rest = List
	).


%%	load_foreign_resource(:Resource, +Dir)
%
%	Load a foreign module. First try to  load from the same direcory
%	as the Prolog file. Otherwise   load  using SWI-Prolog's default
%	search path.

load_foreign_resource(M:Resource, Source) :-
	absolute_file_name(Resource, Object,
			   [ file_type(executable),
			     relative_to(Source),
			     file_errors(fail)
			   ]), !,
	load_foreign_library(M:Object).
load_foreign_resource(M:Resource, _) :-
	load_foreign_library(M:foreign(Resource)).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

arg_name(N, Name) :-
	C is N + 0'a - 1,
	atom_codes(Name, [C]).

wrapper_name(_:Head, Wrapper, Arity) :-
	functor(Head, Name, Arity),
	atomic_list_concat(['_plw_', Name, Arity], Wrapper).

%%	map_C_type(+Prolog, -C)
%
%	Map Prolog interface type declarations into C types.

map_C_type(X, Y) :-
	map_C_type_(X, Y), !.
map_C_type(X, X).

map_C_type_(int, 'int ').
map_C_type_(integer, 'long ').
map_C_type_(size_t, 'size_t ').
map_C_type_(float,   'double ').
map_C_type_(string,  'char *').
map_C_type_(chars,   'char *').
map_C_type_(address, 'void *').
map_C_type_(address(Of), Type) :-
	atom_concat(Of, ' *', Type).
map_C_type_(term,    'term_t ').

warning(Fmt, Args) :-
	print_message(warning, format(Fmt, Args)).


		 /*******************************
		 *	      XREF		*
		 *******************************/

:- multifile
	prolog:hook/1.

prolog:hook(foreign(_,_,_)).
prolog:hook(foreign_resource(_,_)).
