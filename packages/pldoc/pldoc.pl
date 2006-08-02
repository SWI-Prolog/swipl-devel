/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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
	  [ read_structured_comments/2,	% +File, -Comments
	    is_structured_comment/2,	% +Comment, -Prefixes
	    doc_file_name/3		% +Source, -Doc, +Options
	  ]).
:- use_module(wiki).
:- use_module(modes).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(operators)).
:- use_module(library(prolog_source)).

/** <module> Process source documentation
The pldoc module processes structured comments in Prolog source files into
well formatted HTML documents.

@author  Jan Wielemaker
@license GPL
*/

%%	read_structured_comments(+File, -Comments)
%
%	Read the comments from file.
%	
%	@tbd	Deal with XPCE, etc. implied operators.

read_structured_comments(Source, Comments) :-
	prolog_canonical_source(Source, Id),
	prolog_open_source(Id, In),
	call_cleanup((read_comments(In, Term0, Comments0),
		      read_comments(Term0, Comments0, In, Comments)),
		     cleanup(In)).

cleanup(In) :-
	prolog_close_source(In).

read_comments(end_of_file, Comments0, _, Comments) :- !,
	structured_comments(Comments0, Comments, []).
read_comments(_, Comments0, In, Comments) :-
	structured_comments(Comments0, Comments, Tail),
	read_comments(In, Term1, Comments1),
	read_comments(Term1, Comments1, In, Tail).
	
structured_comments([], T, T).
structured_comments([H|Comments], [H|T0], T) :-
	is_structured_comment(H, _), !,
	structured_comments(Comments, T0, T).
structured_comments([_|Comments], T0, T) :-
	structured_comments(Comments, T0, T).


%%	read_comments(+In:stream, -Term, -Comments:list) is det.
%
%	Read next term and its comments from   In.  If a syntax error is
%	encountered, it is printed and  reading   continues  to the next
%	term.

read_comments(In, Term, Comments) :-
	repeat,
	catch(prolog_read_source_term(In, Term, _Expanded,
				      [ comments(Comments)
				      ]),
	      E,
	      (	  print_message(error, E),
		  fail
	      )), !.

%%	is_structured_comment(+Comment:string,
%%			      -Prefixes:list(codes)) is semidet.
%
%	True if Comment is a structured comment that should use Prefixes
%	to extract the plain text using indented_lines/3.

is_structured_comment(_Pos-Comment, Prefixes) :- !,
	is_structured_comment(Comment, Prefixes).
is_structured_comment(Comment, Prefixes) :-
	sub_string(Comment, 0, _, _, '%%'),
	(   sub_string(Comment, 2, _, _, '\t')
	;   sub_string(Comment, 2, _, _, ' ')
	), !,
	Prefixes = ["%"].
is_structured_comment(Comment, Prefixes) :-
	sub_string(Comment, 0, _, _, '/**'),
	(   sub_string(Comment, 3, _, _, '\t')
	;   sub_string(Comment, 3, _, _, ' ')
	;   sub_string(Comment, 3, _, _, '\n')
	), !,
	Prefixes = ["/**", " *"].

%%	doc_file_name(+Source:atom, -Doc:atom, +Options:list) is det.
%
%	Doc is the name of the file documenting Source.
%	
%	@error	permission_error(overwrite, Source)

doc_file_name(Source, Doc, Options) :-
	option(format(Format), Options, html),
	file_name_extension(Base, _Ext, Source),
	file_name_extension(Base, Format, Doc),
	(   Source == Doc
	->  throw(error(permission_error(overwrite, Source), _))
	;   true
	).


		 /*******************************
		 *	      REGISTER		*
		 *******************************/

%:- prolog_set_comment_hook(process_comment).

