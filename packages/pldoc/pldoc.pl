/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc,
	  [ mode/2			% ?:Head, -Det
	  ]).
:- use_module(wiki).
:- use_module(library(debug)).

%:- prolog_set_comment_hook([]).
%:- debug(pldoc).

%%	process_comment(+Comments:list,
%			+Start:stream_position,
%			+Term:any) is det.
%
%	Process comments.

process_comment(Comments, _Start, _Term) :-
	process_comments(Comments).

process_comments([]).
process_comments([H|T]) :-
	process_comment(H),
	process_comments(T).

process_comment(Pos-Comment) :-
	stream_position_data(line_position, Pos, 0), !,
	process_comment0(Comment).
process_comment(_).

process_comment0(Comment) :-
	sub_string(Comment, 0, _, _, '%%'),
	(   sub_string(Comment, 2, _, _, '\t')
	;   sub_string(Comment, 2, _, _, ' ')
	), !,
	debug(pldoc, 'Processing ~q', [Comment]),
	indented_lines(Comment, "%", Lines),
	process_structured_comment(Lines).
process_comment0(Comment) :-
	sub_string(Comment, 0, _, _, '/**'),
	(   sub_string(Comment, 3, _, _, '\t')
	;   sub_string(Comment, 3, _, _, ' ')
	;   sub_string(Comment, 3, _, _, '\n')
	), !,
	debug(pldoc, 'Processing ~q', [Comment]),
	indented_lines(Comment, "/*", Lines),
	process_structured_comment(Lines).
process_comment0(_).


		 /*******************************
		 *	     PROCESS		*
		 *******************************/

%%	process_structured_comment(+Lines:list(int-line)) is det.

process_structured_comment(Lines) :-
	process_modes(Lines, RestLines, ModeDecls, VarNames),
	assert_modes(ModeDecls),
	wiki_lines_to_dom(RestLines, VarNames, _HTML).


		 /*******************************
		 *	       MODES		*
		 *******************************/

%	TBD: Extensive tests on style and completeness

:- dynamic
	mode/3.				% ?Mode, ?Module, ?Det

%%	process_modes(+Lines:lines, -RestLines:lines, -Modes:list, -Args:list(atom)) is det
%
%	Process the formal header lines  (upto   the  first blank line),
%	returning the remaining lines and  the   names  of the arguments
%	used in the various header lines.

process_modes(Lines, RestLines, ModeDecls, Vars) :-
	modes(Lines, ModeDecls, RestLines, Vars0, []),
	sort(Vars0, Vars).
	
modes([], [], [], VN, VN).
modes([_Indent-""|Lines], [], Lines, VN, VN) :- !.		% blank line
modes([_Indent-H|T], [ModeDecl|RestModes], Lines, VN0, VN) :-
	catch(atom_to_term(H, ModeDecl, VarNames), _, fail), !,
	bind_varnames(VarNames, VN0, VN1),
	modes(T, RestModes, Lines, VN1, VN).
modes(Lines, [], Lines, VN, VN).
			
bind_varnames([], VN, VN).
bind_varnames([Name=Var|T], [Name|VN0], VN) :-
	Name = Var,
	bind_varnames(T, VN0, VN).

assert_modes(Modes) :-
	maplist(retract_old_mode, Modes),
	maplist(assert_mode, Modes).

retract_old_mode(Spec is _) :- !,
	retract_old_mode(Spec).
retract_old_mode(Spec) :-
	functor(Spec, Name, Arity),
	functor(Gen, Name, Arity),
	prolog_load_context(module, M),
	retractall(mode(Gen, M, _)).

assert_mode(Spec is Det) :- !,
	prolog_load_context(module, M),
	assert(mode(Spec, M, Det)).
assert_mode(Spec) :-
	assert_mode(Spec is unknown).

mode(Module:Head, Det) :- !,
	mode(Head, Module, Det).
mode(Spec, Det) :-
	strip_module(Spec, Module, Head),
	mode(Head, Module, Det).


		 /*******************************
		 *	      REGISTER		*
		 *******************************/

%:- prolog_set_comment_hook(process_comment).

