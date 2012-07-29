/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(settings,
	  [ setting/4,			% :Name, +Type, +Default, +Comment
	    setting/2,			% :Name, ?Value
	    set_setting/2,		% :Name, +Value
	    set_setting_default/2,	% :Name, +Value
	    restore_setting/1,		% :Name
	    load_settings/1,		% +File
	    load_settings/2,		% +File, +Options
	    save_settings/0,
	    save_settings/1,		% +File
	    current_setting/1,		% Module:Name
	    setting_property/2,		% ?Setting, ?Property
	    list_settings/0,
	    list_settings/1,		% +Module

	    convert_setting_text/3	% +Type, +Text, -Value
	  ]).
:- use_module(library(error)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(arithmetic)).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Setting management

This library allows management  of   configuration  settings  for Prolog
applications. Applications define settings  in   one  or  multiple files
using the directive setting/4 as illustrated below:

==
:- use_module(library(settings)).

:- setting(version, atom,   '1.0', 'Current version').
:- setting(timeout, number,    20, 'Timeout in seconds').
==

The directive is subject to   term_expansion/2,  which guarantees proper
synchronisation of the  database  if   source-files  are  reloaded. This
implies it is *not* possible to call setting/4 as a predicate.

Settings are local to a  module.  This   implies  they  are defined in a
two-level namespace. Managing settings  per   module  greatly simplifies
assembling large applications from multiple   modules that configuration
through  settings.  This  settings  management  library  ensures  proper
access, loading and saving of settings.

@see	library(config) distributed with XPCE provides an alternative
	aimed at graphical applications.
@author	Jan Wielemaker
*/

:- dynamic
	st_value/3,			% Name, Module, Value
	st_default/3,			% Name, Module, Value
	local_file/1.			% Path

:- multifile
	current_setting/6.		% Name, Module, Type, Default, Comment, Source

:- meta_predicate
	setting(:, +, +, +),
	setting(:, ?),
	set_setting(:, +),
	set_setting_default(:, +),
	current_setting(:),
	restore_setting(:).

:- predicate_options(load_settings/2, 2, [undefined(oneof([load,error]))]).

curr_setting(Name, Module, Type, Default, Comment, Src) :-
	current_setting(Name, Module, Type, Default0, Comment, Src),
	(   st_default(Name, Module, Default1)
	->  Default = Default1
	;   Default = Default0
	).

%%	setting(:Name, +Type, +Default, +Comment) is det.
%
%	Define a setting. Name denotes the name of the setting, Type its
%	type. Default is the value before  it is modified. Default refer
%	to environment variables  and  use   arithmetic  expressions  as
%	defined by eval_default/4.
%
%	@param Name	Name of the setting (an atom)
%	@param Type	Type for setting.  One of =any= or a type defined
%			by must_be/2.
%	@param Default  Default value for the setting.
%	@param Comment	Atom containing a (short) descriptive note.


setting(Name, Type, Default, Comment) :-
	throw(error(context_error(nodirective,
				  setting(Name, Type, Default, Comment)),
		    _)).

:- multifile
	system:term_expansion/2.

system:term_expansion((:- setting(QName, Type, Default, Comment)),
		    Expanded) :-
	\+ current_prolog_flag(xref, true),
	prolog_load_context(module, M0),
	strip_module(M0:QName, Module, Name),
	must_be(atom, Name),
	to_atom(Comment, CommentAtom),
	eval_default(Default, Module, Type, Value),
	check_type(Type, Value),
	(   current_setting(Name, Module, _, _, _, OldLoc)
	->  format(string(Message),
		   'Already defined at: ~w', [OldLoc]),
	    throw(error(permission_error(redefine, setting, Module:Name),
			context(Message, _)))
	;   source_location(File, Line)
	->  Expanded = settings:current_setting(Name, Module, Type, Default,
						CommentAtom, File:Line)
	).

to_atom(Atom, Atom) :-
	atom(Atom), !.
to_atom(String, Atom) :-
	format(atom(Atom), '~s', String).

%%	setting(:Name, ?Value) is nondet.
%
%	True if Name is a currently defined setting with Value.
%
%	@error	existence_error(setting, Name)

setting(QName, Value) :-
	strip_module(QName, Module, Name),
	(   ground(Name)
	->  (   st_value(Name, Module, Value0)
	    ->  Value = Value0
	    ;   curr_setting(Name, Module, Type, Default, _, _)
	    ->	eval_default(Default, Module, Type, Value)
	    ;	existence_error(setting, Module:Name)
	    )
	;   current_setting(Name, Module, _, _, _, _),
	    setting(Module:Name, Value)
	).


:- dynamic
	setting_cache/3.
:- volatile
	setting_cache/3.

%%	clear_setting_cache is det.
%
%	Clear the cache for evaluation of default values.

clear_setting_cache :-
	retractall(setting_cache(_,_,_)).

%%	eval_default(+Default, +Module, +Type, -Value) is det.
%
%	Convert the settings default value. The notation allows for some
%	`function-style' notations to make the library more generic:
%
%		* env(Name)
%		Get value from the given environment variable. The value
%		is handed to convert_setting_text/3 to convert the
%		textual representation into a Prolog term.  Raises an
%		existence_error of the variable is not defined.
%
%		* env(Name, Default)
%		As env(Name), but uses the value Default if the variable
%		is not defined.
%
%		* setting(Name)
%		Ask the value of another setting.
%
%		* Expression
%		If Type is numeric, evaluate the expression.  env(Var)
%		evaluates to the value of an environment variable.
%		If Type is =atom=, concatenate A+B+....  Elements of the
%		expression can be env(Name).

:- multifile
	eval_default/3.			% +Default, +Type, -Value

eval_default(Default, _, Type, Value) :-
	eval_default(Default, Type, Val), !,
	Value = Val.
eval_default(Default, _, _, Value) :-
	atomic(Default), !,
	Value = Default.
eval_default(Default, _, Type, Value) :-
	setting_cache(Default, Type, Val), !,
	Value = Val.
eval_default(env(Name), _, Type, Value) :- !,
	(   getenv(Name, TextValue)
	->  convert_setting_text(Type, TextValue, Val),
	    assert(setting_cache(env(Name), Type, Val)),
	    Value = Val
	;   existence_error(environment_variable, Name)
	).
eval_default(env(Name, Default), _, Type, Value) :- !,
	(   getenv(Name, TextValue)
	->  convert_setting_text(Type, TextValue, Val)
	;   Value = Default
	),
	assert(setting_cache(env(Name), Type, Val)),
	Value = Val.
eval_default(setting(Name), Module, Type, Value) :- !,
	strip_module(Module:Name, M, N),
	setting(M:N, Value),
	must_be(Type, Value).
eval_default(Expr, _, Type, Value) :-
	numeric_type(Type, Basic), !,
	arithmetic_expression_value(Expr, Val0),
	(   Basic == float
	->  Val is float(Val0)
	;   Basic = integer
	->  Val is round(Val0)
	;   Val = Val0
	),
	assert(setting_cache(Expr, Type, Val)),
	Value = Val.
eval_default(A+B, Module, atom, Value) :- !,
	phrase(expr_to_list(A+B, Module), L),
	atomic_list_concat(L, Val),
	assert(setting_cache(A+B, atom, Val)),
	Value = Val.
eval_default(List, Module, list(Type), Value) :- !,
	eval_list_default(List, Module, Type, Val),
	assert(setting_cache(List, list(Type), Val)),
	Value = Val.
eval_default(Default, _, _, Default).


%%	eval_list_default(+List, +Module, +ElementType, -DefaultList)
%
%	Evaluate the default for a list of values.

eval_list_default([], _, _, []).
eval_list_default([H0|T0], Module, Type, [H|T]) :-
	eval_default(H0, Module, Type, H),
	eval_list_default(T0, Module, Type, T).

%%	expr_to_list(+Expression, +Module)// is det.
%
%	Process the components to create an  atom. Atom concatenation is
%	expressed as A+B. Components may refer to envrionment variables.

expr_to_list(A+B, Module) --> !,
	expr_to_list(A, Module),
	expr_to_list(B, Module).
expr_to_list(env(Name), _) --> !,
	(   { getenv(Name, Text) }
	->  [Text]
	;   { existence_error(environment_variable, Name) }
	).
expr_to_list(env(Name, Default), _) --> !,
	(   { getenv(Name, Text) }
	->  [Text]
	;   [Default]
	).
expr_to_list(setting(Name), Module) --> !,
	{ strip_module(Module:Name, M, N),
	  setting(M:N, Value)
	},
	[ Value ].
expr_to_list(A, _) -->
	[A].

%%	env(+Name:atom, -Value:number) is det.
%%	env(+Name:atom, +Default:number, -Value:number) is det
%
%	Evaluate  environment  variables   on    behalf   of  arithmetic
%	expressions.

:- arithmetic_function(env/1).
:- arithmetic_function(env/2).

env(Name, Value) :-
	(   getenv(Name, Text)
	->  convert_setting_text(number, Text, Value)
	;   existence_error(environment_variable, Name)
	).
env(Name, Default, Value) :-
	(   getenv(Name, Text)
	->  convert_setting_text(number, Text, Value)
	;   Value = Default
	).


%%	numeric_type(+Type, -BaseType)
%
%	True if Type is a numeric type   and  BaseType is the associated
%	basic Prolog type. BaseType is  one   of  =integer=,  =float= or
%	=number=.

numeric_type(integer, integer).
numeric_type(nonneg, integer).
numeric_type(float, float).
numeric_type(between(L,_), Type) :-
	( integer(L) -> Type = integer ; Type = float ).


%%	set_setting(:Name, +Value) is det.
%
%	Change a setting. Performs existence   and type-checking for the
%	setting. If the effective value  of   the  setting is changed it
%	broadcasts the event below.
%
%		settings(changed(Module:Name, Old, New))
%
%	@error	existence_error(setting, Name)
%	@error  type_error(Type, Value)

set_setting(QName, Value) :-
	strip_module(QName, Module, Name),
	must_be(atom, Name),
	(   curr_setting(Name, Module, Type, Default0, _Comment, _Src),
	    eval_default(Default0, Module, Type, Default)
	->  setting(Module:Name, Old),
	    (   Value == Default
	    ->	retract_setting(Module:Name)
	    ;	st_value(Name, Module, Value)
	    ->	true
	    ;	check_type(Type, Value)
	    ->	retract_setting(Module:Name),
	        assert_setting(Module:Name, Value)
	    ),
	    (	Old == Value
	    ->	true
	    ;	broadcast(settings(changed(Module:Name, Old, Value))),
		clear_setting_cache	% might influence dependent settings
	    )
	;   existence_error(setting, Name)
	).

retract_setting(Module:Name) :-
	retractall(st_value(Name, Module, _)).

assert_setting(Module:Name, Value) :-
	assert(st_value(Name, Module, Value)).

%%	restore_setting(:Name) is det.
%
%	Restore the value of setting Name   to  its default. Broadcast a
%	change like set_setting/2 if  the  current   value  is  not  the
%	default.

restore_setting(QName) :-
	strip_module(QName, Module, Name),
	must_be(atom, Name),
	(   st_value(Name, Module, Old)
	->  retract_setting(Module:Name),
	    setting(Module:Name, Value),
	    (	Old \== Value
	    ->	broadcast(settings(changed(Module:Name, Old, Value)))
	    ;	true
	    )
	;   true
	).

%%	set_setting_default(:Name, +Default) is det.
%
%	Change the default for a setting.  The   effect  is  the same as
%	set_setting/2, but the new value is  considered the default when
%	saving and restoring  a  setting.  It   is  intended  to  change
%	application defaults in a particular context.

set_setting_default(QName, Default) :-
	strip_module(QName, Module, Name),
	must_be(atom, Name),
	(   current_setting(Name, Module, Type, Default0, _Comment, _Src)
	->  retractall(settings:st_default(Name, Module, _)),
	    retract_setting(Module:Name),
	    (   Default == Default0
	    ->	true
	    ;	assert(settings:st_default(Name, Module, Default))
	    ),
	    eval_default(Default, Module, Type, Value),
	    set_setting(Module:Name, Value)
	;   existence_error(setting, Module:Name)
	).


		 /*******************************
		 *	       TYPES		*
		 *******************************/

%%	check_type(+Type, +Term)
%
%	Type  checking  for  settings.  Currently  simply  forwarded  to
%	must_be/2.

check_type(Type, Term) :-
	must_be(Type, Term).


		 /*******************************
		 *	       FILE		*
		 *******************************/

%%	load_settings(File) is det.
%%	load_settings(File, +Options) is det.
%
%	Load local settings from File. Succeeds  if File does not exist,
%	setting the default save-file to File.  Options are:
%
%	  * undefined(+Action)
%	  Define how to handle settings that are not defined.  When
%	  =error=, an error is printed and the setting is ignored.
%	  when =load=, the setting is loaded anyway, waiting for a
%	  definition.

load_settings(File) :-
	load_settings(File, []).

load_settings(File, Options) :-
	absolute_file_name(File, Path,
			   [ access(read),
			     file_errors(fail)
			   ]), !,
	assert(local_file(Path)),
	open(Path, read, In, [encoding(utf8)]),
	read_setting(In, T0),
	call_cleanup(load_settings(T0, In, Options), close(In)),
	clear_setting_cache.
load_settings(File, _) :-
	absolute_file_name(File, Path,
			   [ access(write),
			     file_errors(fail)
			   ]), !,
	assert(local_file(Path)).
load_settings(_, _).

load_settings(end_of_file, _, _) :- !.
load_settings(Setting, In, Options) :-
	catch(store_setting(Setting, Options), E,
	      print_message(warning, E)),
	read_setting(In, Next),
	load_settings(Next, In, Options).

read_setting(In, Term) :-
	read_term(In, Term,
		  [ syntax_errors(dec10)
		  ]).

%%	store_setting(Term, +Options)
%
%	Store setting loaded from file in the Prolog database.

store_setting(setting(Module:Name, Value), _) :-
	curr_setting(Name, Module, Type, Default0, _Commentm, _Src), !,
	eval_default(Default0, Module, Type, Default),
	(   Value == Default
	->  true
	;   check_type(Type, Value)
	->  retractall(st_value(Name, Module, _)),
	    assert(st_value(Name, Module, Value)),
	    broadcast(settings(changed(Module:Name, Default, Value)))
	).
store_setting(setting(Module:Name, Value), Options) :- !,
	(   option(undefined(load), Options, load)
	->  retractall(st_value(Name, Module, _)),
	    assert(st_value(Name, Module, Value))
	;   existence_error(setting, Module:Name)
	).
store_setting(Term, _) :-
	type_error(setting, Term).

%%	save_settings is det.
%%	save_settings(+File) is det.
%
%	Save modified settings to File.

save_settings :-
	local_file(File), !,
	save_settings(File).

save_settings(File) :-
	absolute_file_name(File, Path,
			   [ access(write)
			   ]), !,
	open(Path, write, Out,
	     [ encoding(utf8),
	       bom(true)
	     ]),
	write_setting_header(Out),
	forall(current_setting(Name, Module, _, _, _, _),
	       save_setting(Out, Module:Name)),
	close(Out).


write_setting_header(Out) :-
	get_time(Now),
	format_time(string(Date), '%+', Now),
	format(Out, '/*  Saved settings~n', []),
	format(Out, '    Date: ~w~n', [Date]),
	format(Out, '*/~n~n', []).

save_setting(Out, Module:Name) :-
	curr_setting(Name, Module, Type, Default, Comment, _Src),
	(   st_value(Name, Module, Value),
	    \+ ( eval_default(Default, Module, Type, DefValue),
		 debug(setting, '~w <-> ~w~n', [DefValue, Value]),
	         DefValue =@= Value
	       )
	->  format(Out, '~n%\t~w~n', [Comment]),
	    format(Out, 'setting(~q:~q, ~q).~n', [Module, Name, Value])
	;   true
	).

%%	current_setting(?Setting) is nondet.
%
%	True if Setting is a currently defined setting

current_setting(Setting) :-
	ground(Setting), !,
	strip_module(Setting, Module, Name),
	current_setting(Name, Module, _, _, _, _).
current_setting(Module:Name) :-
	current_setting(Name, Module, _, _, _, _).

%%	setting_property(+Setting, +Property) is det.
%%	setting_property(?Setting, ?Property) is nondet.
%
%	Query currently defined settings.  Property is one of
%
%		* comment(-Atom)
%		* type(-Type)
%		Type of the setting.
%		* default(-Default)
%		Default value.  If this is an expression, it is
%		evaluated.
%		* source(-File:-Line)
%		Location where the setting is defined.

setting_property(Setting, Property) :-
	ground(Setting), !,
	Setting = Module:Name,
	curr_setting(Name, Module, Type, Default, Comment, Src), !,
	setting_property(Property, Module, Type, Default, Comment, Src).
setting_property(Setting, Property) :-
	Setting = Module:Name,
	curr_setting(Name, Module, Type, Default, Comment, Src),
	setting_property(Property, Module, Type, Default, Comment, Src).

setting_property(type(Type), _, Type, _, _, _).
setting_property(default(Default), M, Type, Default0, _, _) :-
	eval_default(Default0, M, Type, Default).
setting_property(comment(Comment), _, _, _, Comment, _).
setting_property(source(Src), _, _, _, _, Src).

%%	list_settings is det.
%%	list_settings(+Module) is det.
%
%	List settings to =current_output=. The   second  form only lists
%	settings on the matching module.
%
%	@tbd	Compute the required column widths

list_settings :-
	list_settings(_).

list_settings(Spec) :-
	spec_term(Spec, Term),
	TS1 = 25,
	TS2 = 40,
	format('~`=t~72|~n'),
	format('~w~t~*| ~w~w~t~*| ~w~n',
	       ['Name', TS1, 'Value (*=modified)', '', TS2, 'Comment']),
	format('~`=t~72|~n'),
	forall(current_setting(Term),
	       list_setting(Term, TS1, TS2)).

spec_term(M:S, M:S) :- !.
spec_term(M, M:_).


list_setting(Module:Name, TS1, TS2) :-
	curr_setting(Name, Module, Type, Default0, Comment, _Src),
	eval_default(Default0, Module, Type, Default),
	setting(Module:Name, Value),
	(   Value \== Default
	->  Modified = (*)
	;   Modified = ''
	),
        format('~w~t~*| ~q~w~t~*| ~w~n', [Module:Name, TS1, Value, Modified, TS2, Comment]).


		 /*******************************
		 *	      TYPES		*
		 *******************************/

%%	convert_setting_text(+Type, +Text, -Value)
%
%	Converts from textual form to  Prolog   Value.  Used  to convert
%	values obtained from the environment.  Public to provide support
%	in user-interfaces to this library.
%
%	@error	type_error(Type, Value)

:- multifile
	convert_text/3.			% +Type, +Text, -Value

convert_setting_text(Type, Text, Value) :-
	convert_text(Type, Text, Value), !.
convert_setting_text(atom, Value, Value) :- !,
	must_be(atom, Value).
convert_setting_text(boolean, Value, Value) :- !,
	must_be(boolean, Value).
convert_setting_text(integer, Atom, Number) :- !,
	term_to_atom(Term, Atom),
	Number is round(Term).
convert_setting_text(float, Atom, Number) :- !,
	term_to_atom(Term, Atom),
	Number is float(Term).
convert_setting_text(between(L,U), Atom, Number) :- !,
	(   integer(L)
	->  convert_setting_text(integer, Atom, Number)
	;   convert_setting_text(float, Atom, Number)
	),
	must_be(between(L,U), Number).
convert_setting_text(Type, Atom, Term) :-
	term_to_atom(Term, Atom),
	must_be(Type, Term).


