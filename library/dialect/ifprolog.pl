/*  Part of SWI-Prolog


    Author:        Jan Wielemaker, Johan Romme
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
	    context/2,				% :Goal, Mapping
	    modify_mode/3,			% +PI, -Old, +New
	    debug_mode/3,			% +PI, -Old, +New
	    ifprolog_debug/1,			% :Goal,
	    debug_config/3,			% +Key, +Current, +Value
	    float_format/2,			% -Old, +New
	    program_parameters/1,		% -Argv
	    user_parameters/1,			% -Argv
	    match/2,				% +Mask, +Atom
	    match/3,				% +Mask, +Atom, ?Replacements
	    lower_upper/2,			% ?Lower, ?Upper
	    current_error/1,			% -Stream
	    writeq_atom/2,			% +Term, -Atom
	    write_atom/2,			% +Term, -Atom
	    write_formatted_atom/3,		% -Atom, +Format, +ArgList
	    write_formatted/2,			% +Format, +ArgList
	    write_formatted/3,			% +Stream, +Format, +ArgList
	    atom_part/4,			% +Atom, +Pos, +Len, -Sub
	    atom_prefix/3,			% +Atom, +Len, -Sub
	    atom_suffix/3,			% +Atom, +Len, -Sub
	    atom_split/3,			% +Atom, +Delimiter, ?Subatoms
	    getchar/3,				% +Atom, +Pos, -Char
	    parse_atom/6,			% +Atom, +StartPos, ?EndPos,
						% ?Term, ?VarList, ?Error
	    index/3,				% +Atom, +String, -Position
	    list_length/2,			% +List, ?Length
	    load/1,				% :FileName
%	    unload/1,				% +Module
	    file_test/2,			% +File, +Mode
	    filepos/2,				% @Stream, -Line
	    filepos/3,				% @Stream, -Line, -Column
	    getcwd/1,				% -Dir
	    assign_alias/2,			% +Alias, @Stream
	    get_until/3,			% +SearchChar, ?Text, ?EndChar
	    get_until/4,			% @In, +SearchChar, ?Text, ?EndChar
	    for/3,				% +Start, ?Counter, +End
	    (@)/2,				% Goal, Module
	    prolog_version/1,                   % -Atom
	    proroot/1,				% -Atom
	    system_name/1,			% -Atom
	    localtime/9,			% +Time, ?Year, ?Month, ?Day, ?DoW, ?DoY, ?Hour, ?Min, ?Sec

	    asserta_with_names/2,		% @Term, +VarNames
	    assertz_with_names/2,		% @Term, +VarNames
	    clause_with_names/3,		% ?Head, ?Body, ?VarNames
	    retract_with_names/2,		% ?Clause, ?VarNames
	    predicate_type/2,			% @Predicate, ?Type
	    current_visible/2,			% @Module, @Predicate

	    current_global/1,			% +Name
	    get_global/2,			% +Name, ?Value
	    set_global/2,			% +Name, ?Value
	    unset_global/1,			% +Name

	    current_default_module/1,		% -Module
	    set_default_module/1,		% +Module

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

Note that this emulation layer targets primarily IF/Prolog version 5.

Please   help   extending   this   library   and   submit   patches   to
bugs@swi-prolog.org.
*/

:- module_transparent
	calling_context/1.

:- meta_predicate
	context(0, +),
	modify_mode(:, -, +),
	debug_mode(:, -, +),
	ifprolog_debug(0),
	load(:),
	asserta_with_names(:, +),
	assertz_with_names(:, +),
	clause_with_names(:, -, -),
	retract_with_names(:, -),
	predicate_type(:, -),
	current_global(:),
	get_global(:, -),
	set_global(:, +),
	unset_global(:).


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

:- dynamic
	in_module_interface/1.

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

ifprolog_goal_expansion(Module:Goal, Expanded) :-
	Module == system, nonvar(Goal), !,
	expand_goal(Goal, ExpandedGoal),
	head_pi(ExpandedGoal, PI),
	(   current_predicate(ifprolog:PI),
	    \+ predicate_property(ExpandedGoal, imported_from(_))
	->  Expanded = ifprolog:ExpandedGoal
	;   Expanded = ExpandedGoal
	).
ifprolog_goal_expansion(Goal, Expanded) :-
	if_goal_expansion(Goal, Expanded).

if_goal_expansion(call(Goal)@Module, MetaGoal) :-
	qcallable(Goal), !,
	if_goal_expansion(Goal@Module, MetaGoal).
if_goal_expansion(Goal@Module, MetaGoal) :-
	qcallable(Goal),
	(   if_goal_expansion(Goal, Goal1)
	->  true
	;   Goal1 = Goal
	),
	(   predicate_property(Goal1, meta_predicate(Decl))
	->  qualify(Goal1, Decl, Module, MetaGoal)
	;   MetaGoal = Goal1
	).
if_goal_expansion(Goal@Module, Module:Goal).
if_goal_expansion(context(Goal, [Error => Recover]),
		  catch(Goal, Error, Recover)) :-
	assertion(Error = error(_,_)).
if_goal_expansion(assertz(Head,Body),
		  assertz((Head:-Body))).
if_goal_expansion(asserta(Head,Body),
		  asserta((Head:-Body))).
if_goal_expansion(retract(Head,Body),
		  retract((Head:-Body))).

qcallable(M:Goal) :- !,
	atom(M), callable(Goal).
qcallable(Goal) :-
	callable(Goal).

qualify(M:Goal, Decl, Module, M:QGoal) :- !,
	qualify(Goal, Decl, Module, QGoal).
qualify(Goal, Decl, Module, QGoal) :-
	Goal =.. [Name|Args0],
	Decl =.. [_|DeclArgs],
	qualify_list(Args0, DeclArgs, Module, QArgs),
	QGoal =.. [Name|QArgs].

qualify_list([], [], _, []).
qualify_list([A|AT], [D|DT], M, [Q|QT]) :-
	(   meta_arg(D)
	->  Q = (M:A)
	;   Q = A
	),
	qualify_list(AT, DT, M, QT).

meta_arg(:).
meta_arg(^).
meta_arg(I) :- integer(I).

head_pi(M:Head, M:PI) :- !,
	head_pi(Head, PI).
head_pi(Head, Name/Arity) :-
	functor(Head, Name, Arity).


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
%
%	Note that if :- meta  appears   inside  a  module interface, the
%	predicate is also exported.

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
			[ (:- module_transparent(Spec))
			| Export
			]) :-
	pi_list_to_pi_term(List, Spec),
	(   in_module_interface(_)
	->  Export = [(:- export(Spec))]
	;   Export = []
	).

ifprolog_term_expansion((:- export([])), []).
ifprolog_term_expansion((:- export(List)),
			(:- export(Spec))) :-
	is_list(List),
	pi_list_to_pi_term(List, Spec).

ifprolog_term_expansion((:- private(_)), []).

ifprolog_term_expansion((:- discontiguous([])), []).
ifprolog_term_expansion((:- discontiguous(List)),
			(:- discontiguous(Spec))) :-
	is_list(List),
	pi_list_to_pi_term(List, Spec).

ifprolog_term_expansion((:- multifile([])), []).
ifprolog_term_expansion((:- multifile(List)),
			(:- multifile(Spec))) :-
	is_list(List),
	pi_list_to_pi_term(List, Spec).

ifprolog_term_expansion((:- module(Name)),
			(:- module(Name, []))) :-
	asserta(in_module_interface(Name)).
ifprolog_term_expansion((:- begin_module(Name)), []) :-
	prolog_load_context(module, Loading),
	assertion(Name == Loading),
	retract(in_module_interface(Name)).
ifprolog_term_expansion((:- end_module(_)), []).
ifprolog_term_expansion((:- end_module), []).
ifprolog_term_expansion((:- nonotify), []).	% TBD: set verbosity


ifprolog_term_expansion((:- import(Module)),
			(:- use_module(File))) :-
	(   module_property(Module, file(File))
	->  true
	;   existence_error(module, Module)
	).
ifprolog_term_expansion((:- import(Module, ImportList)),
			(:- use_module(File, ImportList))) :-
	(   module_property(Module, file(File))
	->  true
	;   existence_error(module, Module)
	).

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
%	dialect is =pro=. If the dialect is   not active, the .pro files
%	are found as last resort.

push_ifprolog_file_extension :-
	asserta((user:prolog_file_type(pro, prolog) :-
		prolog_load_context(dialect, ifprolog))).

user:prolog_file_type(pro, prolog) :-
	\+ prolog_load_context(dialect, ifprolog).

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

%%	context(:Goal, +Mapping)
%
%	IF/Prolog context/2 construct. This is  the true predicate. This
%	is normally mapped by goal-expansion.
%
%	@bug	Does not deal with IF/Prolog signal mapping

context(M:Goal, Mapping) :-
	member(Error => Action, Mapping),
	nonvar(Error),
	Error = error(_,_), !,
	catch(M:Goal, Error, Action).
context(M:Goal, _Mapping) :-
	M:Goal.

%%	modify_mode(+PI, -OldMode, +NewMode) is det.
%
%	Switch between static and  dynamic   code.  Fully supported, but
%	notably changing static to dynamic code   is  not allowed if the
%	predicate has clauses.

modify_mode(PI, OldMode, NewMode) :-
	pi_head(PI, Head),
	old_mode(Head, OldMode),
	set_mode(PI, OldMode, NewMode).

old_mode(Head, Mode) :-
	(   predicate_property(Head, dynamic)
	->  Mode = on
	;   Mode = off
	).

set_mode(_, Old, Old) :- !.
set_mode(PI, _, on) :- !,
	dynamic(PI).
set_mode(PI, _, off) :-
	compile_predicates([PI]).

pi_head(M:PI, M:Head) :- !,
	pi_head(PI, Head).
pi_head(Name/Arity, Term) :-
	functor(Term, Name, Arity).

%%	debug_mode(:PI, -Old, +New)
%
%	Old is not unified.  Only  New  ==   off  is  mapped  to disable
%	debugging of a predicate.

debug_mode(PI, _, off) :- !,
	'$hide'(PI).
debug_mode(_, _, on).

%%	ifprolog_debug(:Goal)
%
%	Map IF/Prolog debug(Goal)@Module. This should  run Goal in debug
%	mode. We rarely needs this type of measures in SWI-Prolog.

ifprolog_debug(Goal) :-
	Goal.

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
%	Same as once(match(Mask, Atom, _Replacements)).

match(Mask, Atom) :-
	match(Mask, Atom, _), !.

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

%%	unload(+Module) is det.
%
%	Unload the named module.
%
%	@bug: What to do with modules that are not associated to a
%	file?

unload(Module) :-
	module_property(Module, file(File)), !,
	unload_file(File).
unload(_Module) :-
	assertion(fail).

%%	file_test(+File, +Mode)
%
%	Mapped to access_file/2 (which understand more modes). Note that
%	this predicate is defined in the   module  =system= to allow for
%	direct calling.

file_test(File, Mode) :-
	access_file(File, Mode).

%%	filepos(@Stream, -Line)
%
%	from  the  IF/Prolog  documentation    The  predicate  filepos/2
%	determines the current line  position   of  the  specified input
%	stream and unifies the  result  with   Line.  The  current  line
%	position is the number of line processed + 1

filepos(Stream, Line) :-
	line_count(Stream, L),
	Line is L + 1.


%%	getcwd(-Dir)
%
%	The predicate getcwd/1 unifies Dir with the full pathname of the
%	current working directory.

getcwd(Dir) :-
	working_directory(Dir, Dir).

%%	filepos(@Stream, -Line, -Column)
%
%	from  the  IF/Prolog  documentation    The  predicate  filepos/2
%	determines the current line  position   of  the  specified input
%	stream and unifies the  result  with   Line.  The  current  line
%	position is the number of line processed + 1

filepos(Stream, Line, Column) :-
	line_count(Stream, L),
	line_position(Stream, C),
	Line is L + 1,
	Column is C + 1.

%%	assign_alias(+Alias, @Stream) is det.
%

assign_alias(Alias, Stream) :-
	set_stream(Stream, alias(Alias)).

%%	writeq_atom(+Term, -Atom)
%
%	Use writeq/1 to write Term to Atom.

writeq_atom(Term, Atom) :-
	with_output_to(atom(Atom), writeq(Term)).

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
	"%", option_number(_Arg), [IFC], !,	% FIXME: Use Arg
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

option_number(Arg) -->
	"-",
	digits(DL),
	{ DL \== [], !, number_codes(Arg, [0'-|DL]) }.
option_number(Arg) -->
	digits(DL),
	{ (DL == [] -> Arg = none ; number_codes(Arg, DL)) }.

digits([D0|T]) -->
	digit(D0), !,
	digits(T).
digits([]) --> [].

digit(D) --> [D], {between(0'0, 0'9, D)}.


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

%%	atom_part(+Atom, +Pos, +Len, -Sub) is det.
%
%	True when Sub is part  of   the  atom [Pos,Pos+Len). Unifies Sub
%	with '' if Pos or Len is out of range!?

atom_part(_, Pos, _, Sub) :-
	Pos < 1, !,
	Sub = ''.
atom_part(_, _, Len, Sub) :-
	Len < 1, !,
	Sub = ''.
atom_part(Atom, Pos, _, Sub) :-
	atom_length(Atom, Len),
	Pos > Len, !,
	Sub = ''.
atom_part(Atom, Pos, Len, Sub) :-
	Pos >= 1,
	Pos0 is Pos - 1,
	atom_length(Atom, ALen),
	Len0 is min(Len, ALen-Pos0),
	sub_atom(Atom, Pos0, Len0, _, Sub).

%%	atom_prefix(+Atom, +Len, -Sub) is det.
%
%	Unifies Sub with the atom formed by  the first Len characters in
%	atom.
%
%	 - If Len < 1, Sub is unified with the null atom ''.
%	 - If Len > length of Atom, Sub is unified with Atom.

atom_prefix(_, Len, Sub) :-
	Len < 1, !,
	Sub = ''.
atom_prefix(Atom, Len, Sub) :-
	atom_length(Atom, AtomLen),
	Len > AtomLen, !,
	Sub = Atom.
atom_prefix(Atom, Len, Sub) :-
	sub_atom(Atom, 0, Len, _, Sub).

%%	atom_suffix(+Atom, +Len, -Sub) is det.
%
%	Unifies Sub with the atom formed by   the last Len characters in
%	atom.
%
%	  - If Len < 1, Sub is unified with the null atom ''.
%	  - If Len > length of Atom, Sub is unified with Atom.

atom_suffix(_, Len, Sub) :-
	Len < 1, !,
	Sub = ''.
atom_suffix(Atom, Len, Sub) :-
	atom_length(Atom, AtomLen),
	Len > AtomLen, !,
	Sub = Atom.
atom_suffix(Atom, Len, Sub) :-
	atom_length(Atom, AtomLen),
	Pos is AtomLen - Len,
	sub_atom(Atom, Pos, Len, _, Sub).

%%	atom_split( +Atom, +Delimiter, ?Subatoms )
%
%	Split Atom over Delimiter and unify the parts with Subatoms.

atom_split(Atom, Delimiter, Subatoms)  :-
	atomic_list_concat(Subatoms, Delimiter, Atom).

%%	getchar(+Atom, +Pos, -Char)
%
%	Unifies Char with the Position-th character in Atom
%	If Pos < 1 or Pos > length of Atom, then fail.

getchar(_, Pos, _) :-
	Pos < 1, !,
	fail.
getchar(Atom, Pos, _) :-
	atom_length(Atom, Len),
	Pos > Len, !,
	fail.
getchar(Atom, Pos, Char) :-
	P is Pos - 1,
	sub_atom(Atom, P, 1, _, Char).


%%	parse_atom(+Atom, +StartPos, ?EndPos, ?Term, ?VarList, ?Error)
%
%	Read from an atom.

parse_atom(Atom, StartPos, EndPos, Term, VarList, Error) :-
	setup_call_cleanup(
	    ( atom_to_memory_file(Atom, MemF),
	      open_memory_file(MemF, read, In)
	    ),
	    ( seek(In, StartPos, bof, _),
	      catch(read_term(In, Term, [variable_names(VarList)]), E,
		    Error = E),
	      ignore(Error = 0),
	      character_count(In, EndPos)
	    ),
	    ( close(In),
	      free_memory_file(MemF)
	    )).

%%	index(+Atom, +String, -Position) is semidet.
%
%	True when Position is the first   occurrence  of String in Atom.
%	Position is 1-based.

index(Atom, String, Position) :-
	sub_string(Atom, Pos0, _, _, String), !,
        Position is Pos0 + 1.

%%	list_length(+List, ?Length) is det.
%
%	Deterministic version of length/2. Current implementation simply
%	calls length/2.

list_length(List, Length) :-
	length(List, Length).


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

%%	Goal@Module
%
%	FIXME: not really correct

(Goal@Module) :-
	Module:Goal.

%%	prolog_version(-Version)
%
%	Return IF/Prolog simulated version string

prolog_version(Version) :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
	atomic_list_concat([Major, Minor, Patch], '.', Version).

%%	proroot(-Path)
%
%	True when Path is  the  installation   location  of  the  Prolog
%	system.

proroot(Path) :-
	current_prolog_flag(home, Path).

%%	system_name(-SystemName)
%
%	True when SystemName identifies the  operating system. Note that
%	this returns the SWI-Prolog =arch= flag,   and not the IF/Prolog
%	identifiers.

system_name(SystemName) :-
	current_prolog_flag(arch, SystemName).

%%	localtime(+Time, ?Year, ?Month, ?Day, ?DoW, ?DoY, ?Hour, ?Min, ?Sec)
%
%	Break system time into its components.  Deefines components:
%
%	  | Year    | Year number    | 4 digits        |
%	  | Month   | Month number   | 1..12           |
%	  | Day	    | Day of month   | 1..31           |
%	  | DoW	    | Day of week    | 1..7 (Mon-Sun)  |
%	  | DoY	    | Day in year    | 1..366          |
%	  | Hour    | Hours	     | 0..23           |
%	  | Min	    | Minutes	     | 0..59           |
%	  | Sec	    | Seconds	     | 0..59           |
%
%	@bug DoY is incorrect

localtime(Time, Year, Month, Day, DoW, DoY, Hour, Min, Sec) :-
        stamp_date_time(Time, date(Year4, Month, Hour, Day, Min, SecFloat, _Off, _TZ, _DST), local),
        Year is Year4 rem 100,
        Sec is floor(SecFloat),
        day_of_the_week(date(Year, Month, Day), DoW),
        DoY is (Month - 1) * 30 + Day. % INCORRECT!!!


%%	current_global(+Name) is semidet.
%%	get_global(+Name, ?Value) is det.
%%	set_global(+Name, ?Value) is det.
%%	unset_global(+Name) is det.
%
%	IF/Prolog  global  variables,  mapped    to   SWI-Prolog's  nb_*
%	predicates.

current_global(Name) :-
	gvar_name(Name, GName),
	nb_current(GName, _).

get_global(Name, Value) :-
	gvar_name(Name, GName),
	nb_getval(GName, Value).

set_global(Name, Value) :-
	gvar_name(Name, GName),
	nb_setval(GName, Value).

unset_global(Name) :-
	gvar_name(Name, GName),
	nb_delete(GName).

gvar_name(Module:Name, GName) :-
	atomic_list_concat([Module, :, Name], GName).


%%	current_default_module(-Module) is det.
%
%	Name of the toplevel typein module.

current_default_module(Module) :-
	'$module'(Module, Module).

%%	set_default_module(+Module) is det.
%
%	Set the default toplevel module.

set_default_module(Module) :-
	module(Module).


		 /*******************************
		 *	      DATABASE		*
		 *******************************/

:- dynamic
	names/2.

%%	asserta_with_names(@Clause, +VarNames) is det.
%%	assertz_with_names(@Clause, +VarNames) is det.
%%	clause_with_names(?Head, ?Body, -VarNames) is det.
%%	retract_with_names(?Clause, -VarNames) is det.
%
%	Predicates that manage  the  database   while  keeping  track of
%	variable names.

asserta_with_names(M:Clause, VarNames) :-
	term_varnames(Clause, VarNames, VarTerm),
	system:asserta(M:Clause, Ref),
	asserta(names(Ref, VarTerm)).
assertz_with_names(M:Clause, VarNames) :-
	term_varnames(Clause, VarNames, VarTerm),
	system:assertz(M:Clause, Ref),
	asserta(names(Ref, VarTerm)).

term_varnames(Term, VarNames, VarTerm) :-
	findall(Vars,
		( term_variables(Term, Vars),
		  bind_names(VarNames)
		),
		[ VarList ]),
	VarTerm =.. [ v | VarList ].

bind_names([]).
bind_names([Name=Var|T]) :-
	Name=Var,
	bind_names(T).


clause_with_names(M:Head, Body, VarNames) :-
	clause(M:Head, Body, Ref),
	(   names(Ref, VarTerm)
	->  term_variables((Head:-Body), Vars),
	    VarTerm =.. [v|NameList],
	    make_bindings(NameList, Vars, VarNames)
	;   VarNames = []
	).

retract_with_names(M:Term, VarNames) :-
	clause(M:Term, Ref),
	erase(Ref),
	(   retract(names(Ref, VarTerm))
	->  term_variables((Term), Vars),
	    VarTerm =.. [v|NameList],
	    make_bindings(NameList, Vars, VarNames)
	;   VarNames = []
	).

make_bindings([], [], []).
make_bindings([Name|NT], [Var|VT], [Name=Var|BT]) :-
	make_bindings(NT, VT, BT).


%%	predicate_type(:PI, -Type) is det.
%
%	True when Type describes the type  of   PI.  Note that the value
%	=linear= seems to mean you can use clause/2 on it, which is true
%	for any SWI-Prolog predicate that is  defined. Therefore, we use
%	it for any predicate that is defined.

predicate_type(M:Name/Arity, Type) :-
	functor(Head, Name, Arity),
	Pred = M:Head,
	(   (   predicate_property(Pred, built_in)
	    ;	predicate_property(Pred, foreign)
	    )
	->  Type = builtin
	;   predicate_property(Pred, imported_from(_))
	->  Type = imported
	;   predicate_property(Pred, dynamic)
	->  Type = linear
	;   control(Head)
	->  Type = control
	;   Name == call
	->  Type = control
	;   current_predicate(M:Name/Arity)
	->  Type = linear
	;   Type = undefined
	).

control((_,_)).
control((_;_)).
control((_->_)).
control((_*->_)).
control((!)).

%%	current_visible(@Module, @Predicate).
%
%	FIXME check with documentation

current_visible(Module, Predicate) :-
	predicate_property(Module:Predicate, visible).

		 /*******************************
		 *	    ARITHMETIC		*
		 *******************************/

:- arithmetic_function(system:time/0).
:- arithmetic_function(system:trunc/1).
:- arithmetic_function(system:ln/1).
:- arithmetic_function(system:maxint/0).
:- arithmetic_function(system:dbsize/0).
:- arithmetic_function(system:dbused/0).
:- arithmetic_function(system:ssize/0).
:- arithmetic_function(system:gused/0).
:- arithmetic_function(system:lused/0).
:- arithmetic_function(system:tused/0).

system:time(Time) :-
	get_time(Time).
system:trunc(Val, Trunc) :-
	Trunc is truncate(Val).
system:ln(Val, Log) :-
	Log is log(Val).
system:maxint(MaxInt) :-
	MaxInt is 1<<63.
system:dbsize(0).
system:dbused(0).
system:ssize(Size) :-
	statistics(globallimit, Size).
system:gused(Size) :-
	statistics(globalused, Size).
system:lused(Size) :-
	statistics(localused, Size).
system:tused(Size) :-
	statistics(trailused, Size).
