/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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

:- module(ifprolog,
	  [ calling_context/1,			% -Module
	    debug_mode/3,			% +PI, -Old, +New
	    debug_config/3,			% +Key, +Current, +Value
	    float_format/2,			% -Old, +New
	    program_parameters/1,		% -Argv
	    user_parameters/1,			% -Argv
	    match/2,				% +Mask, +Atom
	    match/3,				% +Mask, +Atom, ?Replacements
	    lower_upper/2,			% ?Lower, ?Upper
	    current_error/1,			% -Stream
	    write_atom/2,			% +Term, -Atom
	    write_formatted_atom/3,		% -Atom, +Format, +ArgList
	    write_formatted/2,			% +Format, +ArgList
	    write_formatted/3,			% +Stream, +Format, +ArgList
	    parse_atom/6,			% +Atom, +StartPos, ?EndPos,
						% ?Term, ?VarList, ?Error
	    load/1,				% :FileName
	    file_test/2,			% +File, +Mode
	    get_until/3,			% +SearchChar, ?Text, ?EndChar
	    get_until/4,			% @In, +SearchChar, ?Text, ?EndChar
	    for/3,				% +Start, ?Counter, +End

	    op(1150, fx, (meta)),
	    op(1150, fx, (export)),
	    op(100, xfx, @),
	    op(900, xfx, =>),
	    op(900,  fy, not)
	  ]).
:- use_module(library(debug)).
:- use_module(library(arithmetic)).
:- use_module(library(memfile)).


:- module_transparent
	calling_context/1.

:- meta_predicate
	system:modify_mode(:, -, +),
	debug_mode(:, -, +),
	load(:).

		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:term_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	ifprolog_goal_expansion/2,
	ifprolog_term_expansion/2.
:- dynamic
	user:goal_expansion/2,
	user:term_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

user:goal_expansion(In, Out) :-
	prolog_load_context(dialect, ifprolog),
	ifprolog_goal_expansion(In, Out).

user:term_expansion(In, Out) :-
	prolog_load_context(dialect, ifprolog),
	ifprolog_term_expansion(In, Out).

%%	ifprolog_goal_expansion(+In, +Out)
%
%	goal_expansion  rules  to   emulate    IF/Prolog   behaviour  in
%	SWI-Prolog. The expansions  below   maintain  optimization  from
%	compilation.   Defining   them   as   predicates   would   loose
%	compilation.


ifprolog_goal_expansion(Goal@Module, Module:Goal).
ifprolog_goal_expansion(context(Goal, [Error => Recover]),
			catch(Goal, Error, Recover)) :-
	assertion(Error = error(_,_)).
ifprolog_goal_expansion(assertz(Head,Body),
			assertz((Head:-Body))).
ifprolog_goal_expansion(asserta(Head,Body),
			asserta((Head:-Body))).
ifprolog_goal_expansion(retract(Head,Body),
			retract((Head:-Body))).


%%	ifprolog_term_expansion(+In, +Out)
%
%	term_expansion  rules  to   emulate    IF/Prolog   behaviour  in
%	SWI-Prolog.

ifprolog_term_expansion((:- meta([])), []).
ifprolog_term_expansion((:- meta(List)),
			(:- module_transparent(Spec))) :-
	pi_list_to_pi_term(List, Spec).

ifprolog_term_expansion((:- export([])), []).
ifprolog_term_expansion((:- export(List)),
			(:- export(Spec))) :-
	is_list(List),
	pi_list_to_pi_term(List, Spec).

ifprolog_term_expansion((:- discontiguous([])), []).
ifprolog_term_expansion((:- discontiguous(List)),
			(:- discontiguous(Spec))) :-
	is_list(List),
	pi_list_to_pi_term(List, Spec).

ifprolog_term_expansion((:- module(Name)),
			(:- module(Name, []))).
ifprolog_term_expansion((:- begin_module(Name)), []) :-
	prolog_load_context(module, Loading),
	assertion(Name == Loading).
ifprolog_term_expansion((:- end_module(_)), []).
ifprolog_term_expansion((:- end_module), []).
ifprolog_term_expansion((:- nonotify), []).	% TBD: set verbosity


%%	pi_list_to_pi_term(+List, -CommaList) is det.

pi_list_to_pi_term([PI], PI) :- !.
pi_list_to_pi_term([H|T], (H,CommaList)) :-
	pi_list_to_pi_term(T, CommaList).

                 /*******************************
                 *          LIBRARY SETUP       *
                 *******************************/

%%      push_ifprolog_library
%
%       Pushes searching for dialect/ifprolog in   front of every library
%       directory that contains such as sub-directory.

push_ifprolog_library :-
        (   absolute_file_name(library(dialect/ifprolog), Dir,
                               [ file_type(directory),
                                 access(read),
                                 solutions(all),
                                 file_errors(fail)
                               ]),
            asserta((user:file_search_path(library, Dir) :-
                    prolog_load_context(dialect, ifprolog))),
            fail
        ;   true
        ).

%%	push_ifprolog_file_extension
%
%	Looks for .pro files before looking for .pl files if the current
%	dialect is =pro=.

push_ifprolog_file_extension :-
	asserta((user:prolog_file_type(pro, prolog) :-
		prolog_load_context(dialect, ifprolog))).


:- push_ifprolog_library,
   push_ifprolog_file_extension.


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

calling_context(Context) :-
	context_module(Context).


%%	system:modify_mode(+PI, -OldMode, +NewMode) is det.
%
%

system:modify_mode(PI, OldMode, NewMode) :-
	pi_head(PI, Head),
	old_mode(Head, OldMode),
	set_mode(PI, NewMode).

old_mode(Head, Mode) :-
	(   predicate_property(Head, dynamic)
	->  Mode = on
	;   Mode = off
	).

set_mode(PI, on) :- !,
	dynamic(PI).
set_mode(PI, off) :-
	compile_predicates([PI]).

pi_head(M:PI, M:Head) :- !,
	pi_head(PI, Head).
pi_head(Name/Arity, Term) :-
	functor(Term, Name, Arity).

debug_mode(PI, _, off) :-
	'$hide'(PI).

debug_config(Key,Current,Value) :-
	print_message(informational, ignored(debug_config(Key,Current,Value))).

float_format(Old, New) :-
	print_message(informational, ignored(float_format(Old, New))).

program_parameters(Argv) :-
	current_prolog_flag(argv, Argv).

user_parameters(Argv) :-
	current_prolog_flag(argv, AllArgv),
	(   append(_, [--|Argv], AllArgv)
	->  true
	;   assertion(fail)
	).

match(Mask, Atom) :-
	assertion(\+sub_atom(Mask, _, _, _, '[')),
	assertion(\+sub_atom(Mask, _, _, _, '{')),
	wildcard_match(Mask, Atom).

%%	match(+Mask, +Atom, ?Replacements) is nondet.

match(Mask, Atom, Replacements) :-
	atom_codes(Mask, MaskCodes),
	atom_codes(Atom, Codes),
	phrase(match_pattern(Pattern), MaskCodes), !,
	pattern_goal(Pattern, Codes, Replacements, Goal),
	Goal.

pattern_goal([], [], [], true).
pattern_goal([string(String)|T], Codes, Replacements, Goal) :- !,
	append(String, Rest, Codes),
	pattern_goal(T, Rest, Replacements, Goal).
pattern_goal([star|T], Codes, [Atom|Replacements], Goal) :-
	append(Replacement, Rest, Codes),
	Goal = (atom_codes(Atom, Replacement),Goal2),
	pattern_goal(T, Rest, Replacements, Goal2).
pattern_goal([any|T], [C|Rest], [Atom|Replacements], Goal) :-
	Goal = (char_code(Atom, C),Goal2),
	pattern_goal(T, Rest, Replacements, Goal2).

match_pattern([string(List)|T]) -->
	non_special(List),
	{ List \== [] }, !,
	match_pattern(T).
match_pattern([star|T]) -->
	"*", !,
	match_pattern(T).
match_pattern([any|T]) -->
	"?", !,
	match_pattern(T).
match_pattern([]) --> [].

non_special([H|T]) -->
	[H],
	{ \+ special(H) }, !,
	non_special(T).
non_special([]) --> [].

special(0'*).
special(0'?).




lower_upper(Lower, Upper) :-
	nonvar(Lower), !,
	upcase_atom(Lower, Upper).
lower_upper(Lower, Upper) :-
	downcase_atom(Upper, Lower).

load(File) :-
	consult(File).

file_test(File, Mode) :-
	access_file(File, Mode).

write_atom(Term, Atom) :-
	with_output_to(atom(Atom), write(Term)).

current_error(user_error).


		 /*******************************
		 *	  FORMATTED WRITE	*
		 *******************************/

write_formatted_atom(Atom, Format, ArgList) :-
	with_output_to(atom(Atom), write_formatted(Format, ArgList)).

write_formatted(Format, ArgList) :-
	write_formatted(current_output, Format, ArgList).

write_formatted(Out, Format, ArgList) :-
	atom_codes(Format, Codes),
	phrase(format_string(FormatCodes), Codes), !,
	format(Out, FormatCodes, ArgList).

format_string([]) --> [].
format_string(Fmt) -->
	"%", [IFC], !,
	{   map_format([IFC], Repl)
	->  append(Repl, T, Fmt)
	;   print_message(warning, ifprolog_format(IFC)),
	    T = Fmt
	},
	format_string(T).
format_string([H|T]) -->
	[H],
	format_string(T).

map_format("t", "~w").
map_format("q", "~q").
map_format("s", "~a").
map_format("d", "~d").
map_format("c", "~c").
map_format("%", "%").


get_until(SearchChar, Text, EndChar) :-
	get_until(current_input, SearchChar, Text, EndChar).

get_until(In, Search, Text, EndChar) :-
	get_char(In, C0),
	get_until(C0, In, Search, Codes, EndChar),
	atom_chars(Text, Codes).

get_until(C0, _, C0, [], C0) :- !.
get_until(-1, _, _,  [], end_of_file).
get_until(C0, In, Search, [C0|T], End) :-
	get_char(In, C1),
	get_until(C1, In, Search, T, End).


		 /*******************************
		 *	      PARSE		*
		 *******************************/

%%	parse_atom(+Atom, +StartPos, ?EndPos, ?Term, ?VarList, ?Error)

parse_atom(Atom, StartPos, EndPos, Term, VarList, Error) :-
	setup_call_cleanup(
	    ( atom_to_memory_file(Atom, MemF),
	      open(MemF, read, In)
	    ),
	    ( seek(In, StartPos, bof, _),
	      catch(read_term(In, Term, [variable_names(VarList)]), E,
		    Error = E),
	      character_count(In, EndPos)
	    ),
	    ( close(In),
	      free_memory_file(MemF)
	    )).


		 /*******************************
		 *	      MISC		*
		 *******************************/

%%	for(+Start, ?Count, +End) is nondet.
%
%

for(Start, Count, End) :-
	Start =< End, !,
	between(Start, End, Count).
for(Start, Count, End) :-
	nonvar(Count), !,
	between(End, Start, Count).
for(Start, Count, End) :-
	Range is Start-End,
	between(0, Range, X),
	Count is Start-X.


		 /*******************************
		 *	    ARITHMETIC		*
		 *******************************/

:- arithmetic_function(system:time/0).

system:time(Time) :-
	get_time(Time).
