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

/** <module> IF/Prolog compatibility package

This library realises emulation of IF/Prolog.  As with all the emulation
layers in the dialect directory, the   emulation has been established on
`as needed' basis from porting programs. This implies that the emulation
is incomplete. Emumated directives, predicates   and libraries are often
not 100% compatible with the IF/Prolog version.

Please   help   extending   this   library   and   submit   patches   to
bugs@swi-prolog.org.
*/

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


%%	Goal@Module
%
%	This is emulated as Module:Goal, which   is incorrect. Where @/2
%	sets only the calling context, :/2 both defines where the Prolog
%	is called and sets the calling context.

%%	context(:Goal, Handler)
%
%	Is  mapped  to  catch(Goal,  Error,    Recover)  is  Handler  is
%	=|error(_,_) => Recover|=. Other cases are   not  covered by the
%	emulation.

%%	asserta(Head,Body) is det.
%%	assertz(Head,Body) is det.
%%	retract(Head,Body) is det.
%
%	Mapped to asserta((Head:-Body)),  etc.  Note   that  this  masks
%	SWI-Prolog's asserta/2, etc.

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

%%	meta(+ListOfPI)
%
%	Mapped  to  module_transparent/1.  Not  sure   whether  this  is
%	correct. It surely is not very elegant   to  map to a deprecated
%	feature.  Luckily,  although  the  module_transparent/1  API  is
%	deprecated, the underlying functionality is   still  core of the
%	module system.

%%	export(+ListOfPI) is det.
%%	discontiguous(+ListOfPI) is det.
%
%	Mapped to comma-lists

%%	module(+Name).
%%	begin_module(+Name).
%%	end_module(+Name).
%
%	These are emulated correctly,  provided   module/1  is the first
%	term of the file and the  implementation   is  part  of the same
%	file. Begin/end are ignored.

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

%%	calling_context(-Context)
%
%	Mapped to context_module/1.

calling_context(Context) :-
	context_module(Context).


%%	system:modify_mode(+PI, -OldMode, +NewMode) is det.
%
%	Switch between static and  dynamic   code.  Fully supported, but
%	notably changing static to dynamic code   is  not allowed if the
%	predicate has clauses.

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

%%	debug_mode(:PI, -Old, +New)
%
%	Old is not unified.  Only  New  ==   off  is  mapped  to disable
%	debugging of a predicate.

debug_mode(PI, _, off) :-
	'$hide'(PI).

%%	debug_config(+Key, -Current, +Value)
%
%	Ignored.  Prints a message.

debug_config(Key,Current,Value) :-
	print_message(informational, ignored(debug_config(Key,Current,Value))).

%%	float_format(-Old, +New)
%
%	Ignored. Prints a message. Cannot   be emulated. Printing floats
%	with a specified precision can only be done using format/2.

float_format(Old, New) :-
	print_message(informational, ignored(float_format(Old, New))).

%%	program_parameters(-List:atom)
%
%	All command-line argument, including the executable,

program_parameters(Argv) :-
	current_prolog_flag(argv, Argv).

%%	user_parameters(-List:atom)
%
%	Parameters after =|--|=.

user_parameters(Argv) :-
	current_prolog_flag(argv, AllArgv),
	(   append(_, [--|Argv], AllArgv)
	->  true
	;   assertion(fail)
	).

%%	match(+Mask, +Atom) is semidet.
%
%	Matched  to  wildcard_match/2,  but  the  latter  also  supports
%	=|[a-z]|= and =|{p1,p2,...}|=. Checks the the existence of [ and
%	{ in the pattern. Could be mapped   to  match/3, which is slower
%	but correct.

match(Mask, Atom) :-
	assertion(\+sub_atom(Mask, _, _, _, '[')),
	assertion(\+sub_atom(Mask, _, _, _, '{')),
	wildcard_match(Mask, Atom).

%%	match(+Mask, +Atom, ?Replacements) is nondet.
%
%	Pattern matching. This emulation  should   be  complete.  Can be
%	optimized using caching of  the   pattern-analysis  or doing the
%	analysis at compile-time.

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

%%	lower_upper(+Lower, -Upper) is det.
%%	lower_upper(-Lower, +Upper) is det.
%
%	Multi-moded combination of upcase_atom/2 and downcase_atom/2.


lower_upper(Lower, Upper) :-
	nonvar(Lower), !,
	upcase_atom(Lower, Upper).
lower_upper(Lower, Upper) :-
	downcase_atom(Upper, Lower).

%%	load(File)
%
%	Mapped to consult.  I think that the compatible version should
%	only load .qlf (compiled) code.

load(File) :-
	consult(File).

%%	file_test(+File, +Mode)
%
%	Mapped to access_file/2 (which understand more modes).

file_test(File, Mode) :-
	access_file(File, Mode).

%%	write_atom(+Term, -Atom)
%
%	Use write/1 to write Term to Atom.

write_atom(Term, Atom) :-
	with_output_to(atom(Atom), write(Term)).

%%	current_error(-Stream)
%
%	Doesn't exist in SWI-Prolog, but =user_error= is always an alias
%	to the current error stream.

current_error(user_error).


		 /*******************************
		 *	  FORMATTED WRITE	*
		 *******************************/

%%	write_formatted_atom(-Atom, +Format, +ArgList) is det.
%%	write_formatted(+Format, +ArgList) is det.
%%	write_formatted(@Stream, +Format, +ArgList) is det.
%
%	Emulation of IF/Prolog formatted write.   The  emulation is very
%	incomplete. Notable asks for dealing with aligned fields, etc.

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


%%	get_until(+SearchChar, -Text, -EndChar) is det.
%%	get_until(@Stream, +SearchChar, -Text, -EndChar) is det.
%
%	Read input from Stream  until   SearchChar.  Unify  EndChar with
%	either SearchChar or the atom =end_of_file=.

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
%
%	Read from an atom.

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
%	Similar to between/3, but can count down if Start > End.

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
