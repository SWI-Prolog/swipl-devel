/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Simple program consistency check predicates
*/

:- module(progman,
	  [ progman_groups/1,		% -ListOfExistingGroups
	    progman_group_info/3,	% +Group, -File, -Items

	    progman_make_group/1,	% +Group
	    progman_make_group/2,	% +Group, +GroupFile
	    progman_make_item/4,	% +Group, +Title, +CmdLine, +Cwd
	    progman_make_item/5,	% +Group, +Title, +CmdLine, +Cwd, +Icon

	    progman_setup/0		% Installs icons
	  ]).

:- (   feature(dde, true)
   ->  true
   ;   '$warning'('Module "library(progman)" requires DDE support')
   ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MS-Windows  PROGMAN  interface  and  installation   for  SWI-Prolog  and
XPCE/SWI-Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	progman_groups(-Groups)
%
%	Return list of atoms containing the titles of the currently
%	available program groups.

progman_groups(Groups) :-
	open_dde_conversation(progman, progman, DDE),
	dde_request(DDE, groups, Lines),
	lines_to_atoms(Lines, Groups0),
	close_dde_conversation(DDE),
	Groups = Groups0.

%	progman_group_info(+Group, -File, -Items)
%
%	Extracts info on the given group: Filename used to store the
%	group and a list of item(Title, CmdLine, Dir) terms.

progman_group_info(Group, File, Items) :-
	open_dde_conversation(progman, progman, DDE),
	dde_request(DDE, Group, Info),
	close_dde_conversation(DDE),
	lines_to_atoms(Info, [GrounInfo|ItemLines]),
	line_to_args(GrounInfo, [_,File|_]),
	maplist(map_item_info, ItemLines, Items).

%	progman_make_group(+Name, [+File])
%
%	Create a group in the program manager.  If file is given, this
%	is the file used by Windows to store the group info.

progman_make_group(Name) :-
	open_dde_conversation(progman, progman, DDE),
	dde_fmt_execute(DDE, '[CreateGroup("~w")]', [Name]),
	close_dde_conversation(DDE).
progman_make_group(Name, File) :-
	open_dde_conversation(progman, progman, DDE),
	dde_fmt_execute(DDE, '[CreateGroup("~w", "~w")]', [Name, File]),
	close_dde_conversation(DDE).

%	progman_make_item(+Group, +Title, +CmdLine, +Dir)
%	
%	Make a new program item in the named group.  If the item already
%	exists, delete it.

progman_make_item(Group, Title, CmdLine, Dir) :-
	progman_make_item(Group, Title, CmdLine, Dir, -).
progman_make_item(Group, Title, CmdLine, Dir, Icon) :-
	(   nonvar(Icon),
	    Icon = IconFile:IconNum
	->  true
	;   IconFile = '',
	    IconFile = ''
	),
	progman_group_info(Group, _File, Items),
	open_dde_conversation(progman, progman, DDE),
	dde_fmt_execute(DDE, '[ShowGroup("~w", 1)]', Group),
	(   memberchk(item(Title, _, _), Items)
	->  dde_fmt_execute(DDE, '[ReplaceItem("~w")]', [Title])
	;   true
	),
	dde_fmt_execute(DDE, '[addItem(~w, "~w",~w,~w,,, "~w",,)]',
		    [CmdLine, Title, IconFile, IconNum, Dir]),
	dde_fmt_execute(DDE, '[ShowGroup("~w", 0)]', Group),
	close_dde_conversation(DDE).

%	program_group(+Default, -Group)
%
%	Given a default group name, ask for a new name if this group
%	already exists.

program_group(Default, Group) :-
	progman_groups(Existing),
	memberchk(Default, Existing), !,
	(   '$confirm'('Put (replace) items in existing group ~w', [Default])
	->  Group = Default
	;   format('Enter new group name: '),
	    read_line(NewDef),
	    program_group(NewDef, Group)
	).
program_group(Default, Default).

		 /*******************************
		 *	       INSTALL		*
		 *******************************/

progman_setup :-
	explain(start),

	program_group('SWI-Prolog', Group),
	feature(symbol_file, PlExe),
	prolog_to_os_filename(PlExe, OsPlExe),

	progman_make_group(Group),
	progman_make_item(Group, 'SWI-Prolog', OsPlExe, 'c:').
	explain(end).


		 /*******************************
		 *	      EXPLAIN		*
		 *******************************/

explanation(start, '').
explanation(start, '*******************************************************').
explanation(start, 'SWI-Prolog installation run').
explanation(start, '*******************************************************').
explanation(start, '').

explanation(end, '').
explanation(end, 'Program manager setup completed').
explanation(end, '').

explain(Id) :-
	explanation(Id, X),
	format('~w~n', [X]),
	fail ; true.

%	line_to_args(+Line, -Args)
%
%	Translate a line (Atom) as returned by PROGMAN's request for
%	the contents of a group into a list of atomic arguments.  Arguments
%	are separated by `,', may be double-quoted and don't contain
%	blank space.

line_to_args(Line, Args) :-
	name(Line, Str),
	phrase(line(Args), Str).

map_item_info(Line, item(Title, CmdLine, Dir)) :-
	line_to_args(Line, [Title, CmdLine, Dir|_]).

line([Arg|More]) -->
	string(Arg), !,
	line(More).
line(Args) -->
	char(","), !,
	line(Args).
line([Arg|More]) -->
	char([C]),
	string_val(A0),
	(   char(",")
	;   end_of_string
	), !,
	{ name(Arg, [C|A0]) },
	line(More).
line([]) -->
	[].
	
string(Arg) -->
	char(""""), !,
	string_val(A0),
	char(""""), !,
	{ name(Arg, A0) }.

string_val([]) -->
	[].
string_val([C|M]) -->
	char([C]),
	string_val(M).

char([C], [C|T], T).
end_of_string([], []).

%	lines_to_atoms(+Lines, -Atoms)
%       
%	Break a multiline answer from PROGMAN in multiple atoms, each
%	describing a single line of the answer without the \r\n.

lines_to_atoms(Lines, Atoms) :-
	name(Lines, Str),
	string_to_atoms(Str, [], Atoms).

string_to_atoms([], [], []) :- !.
string_to_atoms([], S0,  [A]) :- !,
	reverse(S0, S),
	name(A, S).
string_to_atoms([13,10|Rest], S0, [A|T]) :- !,
	reverse(S0, S),
	name(A, S),
	string_to_atoms(Rest, [], T).
string_to_atoms([10|Rest], S0, [A|T]) :- !,
	reverse(S0, S),
	name(A, S),
	string_to_atoms(Rest, [], T).
string_to_atoms([C|T], M, A) :-
	string_to_atoms(T, [C|M], A).

%	read_line(-Line)
%
%	Flush pending output and read input upto a newline.  Return the
%	entered line as an atom.

read_line(Line) :-
	flush,
	prompt(O, ''),
	read_chars(Chars),
	prompt(_, O),
	name(Line, Chars).

read_chars([C|T]) :-
	get0(C),
	\+ memberchk(C, [10,13,4]),
	read_chars(T).
read_chars([]).

%	dde_fmt_execute(+DdeId, +Format, +Args)
%
%	Utility predicate to create DDE commands from a formatted spec.

dde_fmt_execute(DDE, Fmt, Args) :-
	sformat(Cmd, Fmt, Args),
	dde_execute(DDE, Cmd).
