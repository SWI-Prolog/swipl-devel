/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2009, University of Amsterdam

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

:- module(yap,
	  [ gc/0,
	    depth_bound_call/2,		% :Goal, +Limit
	    system/1,			% +Command
	    exists/1,			% +File
	    assert_static/1,		% :Term
	    source/0,
	    yap_flag/2,			% +Flag, +Value
	    yap_style_check/1		% +Style
	  ]).

/** <module> YAP Compatibility module

This  module  provides  compatibility  to   YAP  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(yap)
	==

The task of this module is:

	* Implement system predicates available in YAP we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide yap_<name>(...) predicates for predicates that exist
	both in YAP and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the yap_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/yap *before*
	the system libraries.

	* Allow for =|.yap|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.yap|= is present, the =|.yap|= file
	is loaded if the current environment expects YAP.

Current            set            is              taken             from
http://www.david-reitter.com/compling/prolog/compat_swi.pl,  written  by
David Reitter and Steve Moyle

@tbd	Fill it in!
@author Jan Wielemaker
*/

:- meta_predicate
	yap_initialization(0).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	yap_expansion/2.
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

user:goal_expansion(In, Out) :-
	prolog_load_context(dialect, yap),
	yap_expansion(In, Out).

%%	yap_expansion(+In, +Out)
%
%	goal_expansion rules to emulate YAP behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

yap_expansion(eval_arith(Expr, Result),
	      Result is Expr).
yap_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
yap_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
yap_expansion(style_check(Style),
	      yap_style_check(Style)).


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_yap_library
%
%	Pushes searching for  dialect/yap  in   front  of  every library
%	directory that contains such as sub-directory.

push_yap_library :-
	(   absolute_file_name(library(dialect/yap), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, yap))),
	    fail
	;   true
	).


%%	push_yap_file_extension
%
%	Looks for .yap files before looking for .pl files if the current
%	dialect is =yap=.

push_yap_file_extension :-
	asserta((user:prolog_file_type(yap, prolog) :-
		    prolog_load_context(dialect, yap))).

:- push_yap_library,
   push_yap_file_extension.


		 /*******************************
		 *	 SYSTEM PREDICATES	*
		 *******************************/

%%	gc
%
%	Garbage collect.
%
%	@compat yap

gc :-
	garbage_collect.

%%	depth_bound_call(:Goal, :Limit)
%
%	Equivalent to call_with_depth_limit(Goal, Limit, _Reached)
%
%	@compat yap

:- module_transparent
	depth_bound_call/2.

depth_bound_call(G, L) :-
	call_with_depth_limit(G, L, _).

%%	system(+Command)
%
%	Equivalent to shell(Command).
%
%	@compat yap

system(Command) :-
	shell(Command).

%%	exists(+File)
%
%	Equivalent to exists_file(File).
%
%	@compat yap

exists(File) :-
	exists_file(File).

%%	assert_static(:Term)
%
%	Assert    as    static    predicate.      SWI-Prolog    provides
%	compile_predicates/1 to achieve this. The   emulation  is a mere
%	alias for assert/1, as  immediate   compilation  would  prohibit
%	further calls to this predicate.
%
%	@compat yap
%	@deprecated Use assert/1 and compile_predicates/1 after
%	completing the predicate definition.

:- module_transparent
	assert_static/1.

assert_static(Term) :-
	assert(Term).


%%	source is det.
%
%	YAP directive to  maintain  source-information.   We  have  that
%	always.

source.


%%	yap_flag(+Key, +Value) is det.
%
%	Map some YAP flags to SWI-Prolog.  Supported flags:
%
%	    * write_strings: Bool
%	    If =on=, writes strings as "..." instead of a list of
%	    integers.  In SWI-Prolog this only affects write routines
%	    that use portray.

yap_flag(write_strings, OnOff) :- !,
	map_bool(OnOff, Bool),
	set_prolog_flag(write_strings, Bool).
yap_flag(Flag, Value) :-
	fixme_true(yap_flag(Flag, Value)).

map_bool(on, true) :- !.
map_bool(off, false) :- !.
map_bool(Bool, Bool).

:- multifile
	user:portray/1.

user:portray(String) :-
	current_prolog_flag(write_strings, true),
	is_list(String),
	length(String, L),
	L > 2,
	maplist(printable, String),
	format('"~s"', [String]).

printable(C) :-	code_type(C, graph), !.
printable(C) :-	code_type(C, space), !.


%%	yap_style_check(+Style) is det.
%
%	Map YAP style-check options onto the SWI-Prolog ones.

yap_style_check(all) :- !,
	system:style_check([ +singleton,
			     +discontiguous
			   ]).
yap_style_check(Style) :-
	fixme_true(yap_style_check(Style)).


		 /*******************************
		 *	   UNIMPLEMENTED		*
		 *******************************/

:- dynamic
	fixme_reported/1.

fixme_true(Goal) :-
	fixme_reported(Goal), !.
fixme_true(Goal) :-
	print_message(warning, yap_unsupported(Goal)),
	assert(fixme_reported(Goal)).


:- multifile
	prolog:message//1.

prolog:message(yap_unsupported(Goal)) -->
	[ 'YAP emulation (yap.pl): unsupported: ~p'-[Goal] ].
