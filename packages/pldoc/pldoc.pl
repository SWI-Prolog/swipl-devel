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
	  [ pldoc_comment/4,		% ?Object, ?Pos, ?Summary, ?Comment
	    read_structured_comments/2,	% +File, -Comments
	    is_structured_comment/2,	% +Comment, -Prefixes
	    doc_file_name/3		% +Source, -Doc, +Options
	  ]).
:- use_module(modes).
:- use_module(wiki).
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

		 /*******************************
		 *	    READING MODE	*
		 *******************************/

%%	read_structured_comments(+File, -Comments) is det.
%
%	Read the structured comments from file.

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
%	@param Source	Prolog source to be documented
%	@param Doc	the name of the file documenting Source.
%	@param Options	Option list:
%			
%			* format(-Format)
%			Output format.  One of =html= or =latex=
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

%%	pldoc_comment(?Object, -Pos,
%%		      -Summary:string, -Comment:string) is nondet.
%
%	True if Comment is the  comment   describing  object. Comment is
%	returned as a string object  containing   the  original from the
%	source-code.  Object is one of
%	
%		* Name/Arity
%		Predicate indicator
%		
%		* Name//Arity
%		DCG rule indicator.  Same as Name/Arity+2
%		
%		* module(Module)
%		Comment appearing in Module.
%		
%	@param Summary	First sentence.  Normalised spacing.
%	@parsm Comment	Comment string from the source-code (untranslated)
%
%	@tbd	Handle comments covering multiple predicates

pldoc_comment(Object, Pos, Summary, Comment) :-
	var(Object), !,
	current_module(M),
	'$c_current_predicate'(_, M:'$pldoc'(_,_,_,_)),
	M:'$pldoc'(Obj, Pos, Summary, Comment),
	qualify(M, Obj, Object).
pldoc_comment(M:Object, Pos, Summary, Comment) :-
	current_module(M),
	'$c_current_predicate'(_, M:'$pldoc'(_,_,_,_)),
	M:'$pldoc'(Object, Pos, Summary, Comment).

qualify(system, H, H) :- !.
qualify(user,   H, H) :- !.
qualify(M,      H, M:H).


		 /*******************************
		 *	CALL-BACK COLLECT	*
		 *******************************/

%%	process_comments(+Comments:list, +TermPos) is det.
%
%	Processes comments returned by read_term/3 using the =comments=
%	option.  It creates clauses of the form
%	
%		* '$mode'(Head, Det)
%		* '$pldoc'(Id, Type, DOM)
%	
%	where object is one of 
%	
%		* =predicate=
%		* =module=
%		* =section=

process_comments([], _).
process_comments([Pos-Comment|T], TermPos) :-
	(   Pos @> TermPos		% comments inside term
	->  true
	;   process_comment(Pos, Comment),
	    process_comments(T, TermPos)
	).

process_comment(Pos, Comment) :-
	is_structured_comment(Comment, Prefixes), !,
	stream_position_data(line_count, Pos, Line),
	source_location(File, TermLine),
	FilePos = File:Line,
	'$push_input_context',		% Preserve input file and line
	call_cleanup(process_structured_comment(FilePos, Comment, Prefixes),
		     '$pop_input_context'),
	assertion(source_location(File, TermLine)).
process_comment(_, _).

process_structured_comment(FilePos, Comment, Prefixes) :-
	indented_lines(Comment, Prefixes, Lines),
	(   section_comment_header(Lines, Header, RestLines)
	->  Header = \section(Type, Title),
	    Id =.. [Type,Title],
	    compile_clause('$pldoc'(Id, FilePos, Title, Comment), FilePos)
	;   process_modes(Lines, FilePos, Modes, _, RestLines)
	->  store_modes(Modes, FilePos),
	    modes_to_predicate_indicators(Modes, [PI0|PIs]),
	    summary(RestLines, Summary),
	    compile_clause('$pldoc'(PI0, FilePos, Summary, Comment), FilePos),
	    forall(member(PI, PIs),
		   compile_clause('$pldoc_link'(PI, PI0), FilePos))
	), !.
process_structured_comment(File:Line, Comment, _) :-
	print_message(warning,
		      format('~w:~d: Failed to process comment:~n~s~n',
			     [File, Line, Comment])),
	fail.
	

		 /*******************************
		 *	      REGISTER		*
		 *******************************/

:- multifile
	prolog:comment_hook/3.

pldoc_module(pldoc_modes).		% avoid recursive behaviour
pldoc_module(pldoc_wiki).
pldoc_module(pldoc).

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

prolog:comment_hook(Comments, TermPos, _Term) :-
	(   prolog_load_context(module, Module),
	    pldoc_module(Module),
	    \+ current_prolog_flag(xref, true)
	->  true
	;   process_comments(Comments, TermPos)
	).

