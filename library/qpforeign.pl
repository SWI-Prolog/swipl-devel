/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(qp_foreign,
	  [ load_foreign_files/0,		% 
	    load_foreign_files/2,		% +Files, +Libs
	    load_foreign_files/3,		% +Object, +Files, +Libs
	    make_shared_object/3,		% +Object, +Files, +Libs
	    make_foreign_wrapper_file/1,	% +OutBase
	    make_foreign_wrapper_file/2		% +OFiles, +OutBase
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a  Quintus   compatible  foreign  language interface
based on the foreign_file/2 and foreign/3 declarations.

Predicates:

	# load_foreign_files/0
	Load all foreign files defined with foreign_file/2 statement
	in the current module.

	# load_foreign_files(+Files, +Libs)
	Load specified foreign files, linking them with the given
	libraries

	# load_foreign_files(+SharedObject, +Files, +Libs)
	As load_foreign_files/2, but first tries to load `SharedObject'.
	If the SharedObject exists it is loaded using load_foreign_library/1.
	Otherwise it is first created with make_shared_object/3.

	# make_shared_object(+SharedObject, +Files, +Libs)
	Generate a wrapper and link it using plld to the given SharedObject.

	# make_foreign_wrapper_file(+Files, +OutBase)
	Generate wrapper for the named object files in OutBase.c.

	# make_foreign_wrapper_file(+Files)
	Generate wrapper for all declared object files in OutBase.c.
	
Example:

	foreign_file('-lm', [sin/2]).
	foreign(sin, c, sin(+float, [-float])).
	:- load_foreign_files,
	   abolish(foreign_file, 2),
	   abolish(foreign, 3).

Supported types:

	=============================================================
	Spec		Prolog		C
	=============================================================
	integer		integer		long
	float		float|integer	double
	single		float|integer	single
	string		atom|string	char *
	atom		atom		atom identifier (type atomic)
	=============================================================

NOTE:	This modules requires a correctly functioning plld and
	load_foreign_library/1 on your system.  If this isn't the
	case use make_foreign_wrapper_file/[1,2] to generate a
	wrapper and use static embedding.

NOTE:	The generated linkN.c file may also be used for creating a
	statically linked executable as described in pl-extend.c

BUGS:	* Only supports C-interface
	* Insufficient checking for misusage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module_transparent
	load_foreign_files/0,
	load_foreign_files/2,
	load_foreign_files/3,
	make_shared_object/3,
	make_C_prototype/2,
	make_C_init/3,
	make_wrappers/2,
	make_wrapper/2,
	get_foreign_head/3,
	make_foreign_wrapper_file/1,
	make_foreign_wrapper_file/2,
	collect_foreign_predicates/2.

%	make_wrapper(+Stream, +PrologHead)
%
%	Generates a C-wrapper function for the given foreign defined
%	Prolog predicate.  The wrapper is called _plw_<predname><arity>.

make_wrappers([], _).
make_wrappers([H|T], Out) :-
	make_wrapper(Out, H),
	make_wrappers(T, Out).

make_wrapper(Out, Spec) :-
	get_foreign_head(Spec, Func, Head),
	(   check_head(Head)
	->  functor(Head, _Name, ArgN),
	    wrapper_name(Head, WrapName),
	    make_C_header(Out, WrapName, ArgN),
	    make_C_decls(Out, Head),
	    make_C_prototype(Out, Head),
	    make_C_input_conversions(Out, Head),
	    make_C_call(Out, Head, Func),
	    make_C_output_conversions(Out, Head),
	    make_C_footer(Out)
	;   fail
	).

%	get_foreign_head(+Spec, -Func, -Head)
%
%	Get 3rd argument of relevant foreign/3 clause.

get_foreign_head(Spec, Func, Head) :-
	(   atom(Spec),
	    call(foreign(Func, c, Head)),
	    functor(Head, Spec, _)
	->  true
	;   Spec = Name/Arity
	->  functor(Head, Name, Arity),
	    call(foreign(Func, c, Head))
	;   Head = Spec,
	    call(foreign(Func, c, Head))
	).
	

check_head(Head) :-
	functor(Head, _, Arity),
	(   arg(N, Head, [-T]),
	    \+ valid_type(T)
	->  warning('Bad return type ~w in ~w', [T, Head]),
	    fail
	;   arg(N, Head, [-T]),
	    N \== Arity
	->  warning('Return type must be last in ~w', Head),
	    fail
	;   (arg(_, Head, -T) ; arg(_, Head, +T)),
	    \+ valid_type(T)
	->  warning('Bad type ~w in ~w', [T, Head]),
	    fail
	;   true
	).
	
valid_type(integer).
valid_type(float).
valid_type(single).
valid_type(string).
valid_type(atom).


%	make_C_header(+Stream, +WrapperName, +Arity)
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

%	make_C_decls(+Stream, +PrologHead)
%      
%	Writes the C variable declarations.  If the return value is
%	used a variable named `rval' is created.  For each input parameter
%	a C variable named i<argname> is created; for each output variable
%	o<argname>.

make_C_decls(Out, Head) :-
	arg(_, Head, [-PlType]),
	map_C_type(PlType, CType),
	format(Out, '~wrval;~n  ', [CType]),
	fail.
make_C_decls(Out, Head) :-
	arg(N, Head, -PlType),
	map_C_type(PlType, CType),
	arg_name(N, AName),
	format(Out, '~wo~w;~n  ', [CType, AName]),
	fail.
make_C_decls(Out, Head) :-
	arg(N, Head, +PlType),
	map_C_type(PlType, CType),
	arg_name(N, AName),
	format(Out, '~wi~w;~n  ', [CType, AName]),
	fail.
make_C_decls(Out, _) :-
	format(Out, '~n', []).

%	make_C_prototype(+Stream, +PrologHead)
%
%	If the function handles floats or doubles, make	a prototype
%	declaration for it to avoid unwanted conversions.

make_C_prototype(Out, Head) :-
%	arg(_, Head, A),
%	(   term_member(float, A)
%	;   term_member(single, A)
%	), !,
	(   arg(_, Head, [-Type])
	->  map_C_type(Type, CType)
	;   CType = ''
	),
	copy_term(Head, H2),		% don't bind Head
	call(foreign(CFunc, c, H2)),
	format(Out, '  extern ~w~w(', [CType, CFunc]),
	(   arg(N, Head, AType),
	    (   AType = +T2
	    ->  (N > 1 -> format(Out, ', ', []) ; true),
		map_C_type(T2, CT2),
		format(Out, '~w', [CT2])
	    ;   AType = -T2
	    ->  (N > 1 -> format(Out, ', ', []) ; true),
		map_C_type(T2, CT2),
		format(Out, '~w *', [CT2])
	    ),
	    fail
        ;   format(Out, ');~n~n', [])
	).
make_C_prototype(_, _).


term_member(A, A).
term_member(A, B) :-
	arg(_, B, C),
	term_member(A, C).

%	make_C_input_conversions(+Stream, +PrologHead)
%
%	Generate the input checking and conversion code.  Assumes
%	boolean functions that take a Prolog term_t as first argument
%	and a pointer to the requested C-type as a second argument.
%	Function returns 0 if the conversion fails.

make_C_input_conversions(Out, Head) :-
	findall(N-T, arg(N, Head, +T), IArgs),
	(   IArgs == []
	->  true
	;   format(Out, '  if ( ', []),
	    (	member(N-T, IArgs),
		(IArgs \= [N-T|_] -> format(Out, ' ||~n       ', []) ; true),
		arg_name(N, AName),
		concat(i, AName, IName),
		format(Out, '!PL_cvt_i_~w(~w, &~w)', [T, AName, IName]),
		fail
	    ;	true
	    ),
	    format(Out, ' )~n    return FALSE;~n~n', [])
	).


%	make_C_call(+Stream, +Prolog, +CFunction)
%
%	Generate the actual call to the foreign function.  Input variables
%	may be handed directly; output variables as a pointer to the o<var>.

make_C_call(Out, Head, CFunc) :-
	(   arg(_, Head, [-_])
	->  format(Out, '  rval = ~w(', [CFunc])
	;   format(Out, '  (void) ~w(', [CFunc])
	),
	arg(N, Head, Arg),
	Arg \= [_],
	(N \== 1 -> format(Out, ', ', []) ; true),
	arg_name(N, AName),
	(   Arg = -_
	->  format(Out, '&o~w', [AName])
	;   format(Out, 'i~w', [AName])
	),
	fail.
make_C_call(Out, _, _) :-
	format(Out, ');~n', []).

%	make_C_output_conversions(+Stream, +PrologHead)
%
%	Generate conversions for the output arguments and unify them
%	with the Prolog term_t arguments.

make_C_output_conversions(Out, Head) :-
	findall(N-T, arg(N, Head, -T), OArgs0),
	(   arg(_, Head, [-T])
	->  OArgs = [rval-T|OArgs0]
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
		    concat(o, AName, OName)
		),
		(OArgs = [N-T|_] -> true ; format(Out, ' ||~n       ', [])),
		format(Out, '!PL_cvt_o_~w(~w, ~w)', [T, OName, AName]),
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

%	make_C_init(+Stream, +PredList)
%
%	Generate an array of PL_extension structures, that may be used to
%	create a statically linked image as well as through the
%	PL_load_extensions() call.
%
%	Of the supported PL_FA_<FLAGS>, TRANSPARENT may be declared by looking
%	at the transparent (meta_predivate) attribute of the predicate.

make_C_init(Out, InstallFunc, Preds) :-
	format(Out, '~n~nstatic PL_extension predicates [] =~n{~n', []),
	format(Out, '/*{ "name", arity, function, PL_FA_<flags> },*/~n', []),
	(   member(Pred, Preds),
	    get_foreign_head(Pred, _Func, Head),
	    functor(Head, Name, Arity),
	    wrapper_name(Head, Wrapper),
	    strip_module(Head, M, H),
	    foreign_attributes(M:H, Atts),
	    format(Out, '  { "~w", ~d, ~w, ~w },~n',
		   [Name, Arity, Wrapper, Atts]),
	    fail
	;   true
	),
	format(Out, '  { NULL, 0, NULL, 0 } /* terminator */~n};~n~n', []),
	format(Out, 'install_t~n~w()~n{ PL_load_extensions(predicates);~n',
	       [InstallFunc]),
	format(Out, '}~n', []).


foreign_attributes(Head, Atts) :-
	findall(A, foreign_attribute(Head, A), A0),
	(   A0 == []
	->  Atts = '0'
	;   insert_separator(A0, '|', A1),
	    concat_atom(A1, Atts)
	).

insert_separator([], _, []).
insert_separator([H], _, [H]).
insert_separator([A|T], S, [A, S|NT]) :-
	insert_separator(T, S, NT).

foreign_attribute(Head, 'PL_FA_TRANSPARENT') :-
	predicate_property(Head, transparent).

%	make_C_file_header(+Stream)
%	
%	Output the generic header declarations needed and some comments

make_C_file_header(Out) :-
	current_prolog_flag(version, Version),
	get_time(Time),
	convert_time(Time, Y, M, D, H, Min, _, _),
	format(Out, '/*  SWI-Prolog link wrapper~n', []),
	format(Out, '    Generated by SWI-Prolog version ~w~n', [Version]),
	format(Out, '    At ~w/~w/~w ~w:~w~n', [D, M, Y, H, Min]),
	(   source_location(File, Line)
	->  format(Out, '    Source context ~w:~d~n', [File, Line])
	;   true
	),
	format(Out, '*/~n~n', []),
	format(Out, '#include <SWI-Prolog.h>~n', []),
	format(Out, '#ifndef NULL~n', []),
	format(Out, '#define NULL ((void *)0)~n', []),
	format(Out, '#endif~n~n', []).


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

%	Calls make_foreign_wrapper_file(+File), compiles the wrapper
%	and loads the predicates.

load_foreign_files :-
	findall(File, foreign_file(File, _), OFiles),
	load_foreign_files(OFiles, []).
load_foreign_files(OFiles, Libs) :-
	gensym(link, LinkBase),
	load_foreign_files(LinkBase, OFiles, Libs).

load_foreign_files(LinkBase, _, _) :-
	catch(load_foreign_library(LinkBase), _, fail), !.
load_foreign_files(LinkBase, OFiles, Libs) :-
	make_shared_object(LinkBase, OFiles, Libs),
	load_foreign_library(LinkBase).

make_shared_object(LinkBase, OFiles, Libs) :-
	make_foreign_wrapper_file(OFiles, LinkBase),
	file_name_extension(LinkBase, c, CFile),
	build_shared_object(LinkBase, [CFile|OFiles], Libs).
	
%	Just output the wrapper file to the named .c file.  May be used
%	to prepare for static linking or the preparation of the native
%	SWI-Prolog foreign-file.

make_foreign_wrapper_file(CFile) :-
	findall(File, foreign_file(File, _), OFiles),
	make_foreign_wrapper_file(OFiles, CFile).
make_foreign_wrapper_file(OFiles, Base) :-
	file_name_extension(Base, c, CFile),
	atom_concat(install_, Base, InstallFunc),
	collect_foreign_predicates(OFiles, Preds),
	open(CFile, write, Out),
	make_C_file_header(Out),
	make_wrappers(Preds, Out),
	make_C_init(Out, InstallFunc, Preds),
	close(Out).


collect_foreign_predicates([], []).
collect_foreign_predicates([File|Files], Preds) :-
	call(foreign_file(File, P0)),
	collect_foreign_predicates(Files, P1),
	append(P0, P1, Preds).

build_shared_object(Object, Files, Libs) :-
	current_prolog_flag(shared_object_extension, Ext),
	file_name_extension(Object, Ext, SharedObject),
	append(Files, Libs, Input),
	concat_atom(Input, ' ', InputAtom),
	sformat(Command, 'plld -shared -o ~w ~w', [SharedObject, InputAtom]),
	shell(Command).
	

		 /*******************************
		 *	       UTIL		*
		 *******************************/

arg_name(N, Name) :-
	C is N + 0'a - 1,
	name(Name, [C]).

wrapper_name(Head, Wrapper) :-
	functor(Head, Name, Arity),
	concat_atom(['_plw_', Name, Arity], Wrapper).

%	map_C_type(+Prolog, -C)
%
%	Map Prolog interface type declarations into C types.

map_C_type(X, Y) :-
	map_C_type_(X, Y), !.
map_C_type(X, X).

map_C_type_(integer, 'long ').
map_C_type_(float,   'double ').
map_C_type_(string,  'char *').

warning(Fmt, Args) :-
	print_message(warning, format(Fmt, Args)).
