/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(pldoc_register,
	  [ process_stored_comments/0
	  ]).
:- use_module(doc_process).
:- set_prolog_flag(generate_debug_info, false).

pldoc_module(pldoc_modes).              % avoid recursive behaviour
pldoc_module(pldoc_process).
pldoc_module(pldoc_wiki).
pldoc_module(pldoc).

		 /*******************************
		 *	      REGISTER		*
		 *******************************/

:- multifile
	prolog:comment_hook/3.

:- dynamic
	mydoc/3.			%  +Comments, +TermPos, +File

do_comment_hook(_, _, _, _) :-
	current_prolog_flag(pldoc_collecting, false), !.
do_comment_hook(Comments, TermPos, File, _) :-
	pldoc_loading, !,
	assert(mydoc(Comments, TermPos, File)).
do_comment_hook(Comments, TermPos, File, Term) :-
	prolog_load_context(module, Module),
	pldoc_module(Module), !,
	assert(mydoc(Comments, TermPos, File)),
	(   Term == end_of_file
	->  process_stored_comments
	;   true
	).
do_comment_hook(Comments, TermPos, File, _) :-
	process_comments(Comments, TermPos, File).

%%	prolog:comment_hook(+Comments, +TermPos, +Term) is det.
%
%	Hook called by the compiler and cross-referencer. In addition to
%	the comment, it passes the  term  itself   to  see  what term is
%	commented  as  well  as  the  start-position   of  the  term  to
%	distinguish between comments before the term and inside it.
%	
%	@param Comments	List of comments read before the end of Term
%	@param TermPos	Start location of Term
%	@param Term 	Actual term read

prolog:comment_hook(Comments, TermPos, Term) :-
	source_location(File, _TermLine),
	'$push_input_context',		% Preserve input file and line
	call_cleanup(do_comment_hook(Comments, TermPos, File, Term),
		     '$pop_input_context').
	
process_stored_comments :-
	forall(retract(mydoc(Comments, TermPos, File)),
	       delayed_process(Comments, TermPos, File)).

delayed_process(Comments, TermPos, File) :-
	current_module(Module, File),
	'$set_source_module'(Old, Module),
	process_comments(Comments, TermPos, File),
	'$set_source_module'(_, Old).
