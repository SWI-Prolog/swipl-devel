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
	  [ 
	  ]).
:- use_module(wiki).
:- use_module(modes).
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
	process_modes(Lines, ModeDecls, VarNames, RestLines),
	assert_modes(ModeDecls),
	wiki_lines_to_dom(RestLines, VarNames, _HTML).


		 /*******************************
		 *	      REGISTER		*
		 *******************************/

%:- prolog_set_comment_hook(process_comment).

