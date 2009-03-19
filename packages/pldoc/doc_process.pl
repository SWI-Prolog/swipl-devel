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

:- module(pldoc_process,
	  [ doc_comment/4,		% ?Object, ?Pos, ?Summary, ?Comment
	    read_structured_comments/2,	% +File, -Comments
	    is_structured_comment/2,	% +Comment, -Prefixes
	    process_comments/3,		% +Comments, +StartTermPos, +File
	    doc_file_name/3		% +Source, -Doc, +Options
	  ]).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(pldoc, library(pldoc)).

:- load_files([ pldoc(doc_register),
		pldoc(doc_modes),
		pldoc(doc_wiki),
		library(debug),
		library(option),
		library(lists),
		library(operators),
		library(prolog_source)
	      ],
	      [ silent(true),
		if(not_loaded)
	      ]).

/** <module> Process source documentation
The pldoc module processes structured comments in Prolog source files into
well formatted HTML documents.

@author  Jan Wielemaker
@license GPL
*/

:- multifile
	prolog:predicate_summary/2.	% ?PI, -Summary


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
%
%	@tbd	=|%% SWI begin|= and =|%% SICStus begin|= are used by chr.
%		We need a more general mechanism to block some comments.

is_structured_comment(_Pos-Comment, Prefixes) :- !,
	is_structured_comment(Comment, Prefixes).
is_structured_comment(Comment, Prefixes) :-
	is_list(Comment), !,
	phrase(structured_comment(Prefixes), Comment, _).
is_structured_comment(Comment, Prefixes) :-
	sub_string(Comment, 0, _, _, '%%'), !,
	sub_atom(Comment, 2, 1, _, Space),
	char_type(Space, space),
	\+ blanks_to_nl(Comment),
	\+ sub_string(Comment, 2, _, _, ' SWI '),	% HACK
	\+ sub_string(Comment, 2, _, _, ' SICStus '),	% HACK
	\+ sub_string(Comment, 2, _, _, ' Mats '), 	% HACK
	Prefixes = ["%"].
is_structured_comment(Comment, Prefixes) :-
	sub_string(Comment, 0, _, _, '/**'), !,
	sub_atom(Comment, 3, 1, _, Space),
	char_type(Space, space),
	Prefixes = ["/**", " *"].

blanks_to_nl(Comment) :-
	sub_atom(Comment, At, 1, _, Char),
	At >= 2,
	(   char_type(Char, end_of_line)
	->  !
	;   (   char_type(Char, space)
	    ;	Char == '%'
	    )
	->  fail
	;   !, fail
	).
blanks_to_nl(_).

%%	structured_comment(-Prefixes:list(codes)) is semidet.
%
%	Grammar rule version of the above.  Avoids the need for
%	conversion.

structured_comment(["%"]) -->
	"%%", space,
	\+ blanks_to_nl,
	\+ contains(" SWI "),
	\+ contains(" SICStus "),
	\+ contains(" Mats ").
structured_comment(Prefixes) -->
	"/**", space,
	{ Prefixes = ["/**", " *"]
	}.

space -->
	[H],
	{ code_type(H, space) }.

blanks_to_nl -->
	blank_or_percents,
	"\n".

blank_or_percents -->
	space, !,
	blank_or_percents.
blank_or_percents -->
	"%", !,
	blank_or_percents.
blank_or_percents -->
	"".

contains(String) -->
	...,
	String, !.

... --> "".
... --> [_], ... .


%%	doc_file_name(+Source:atom, -Doc:atom, +Options:list) is det.
%
%	Doc is the name of the file for documenting Source.
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

%%	doc_comment(?Objects, -Pos,
%%		    -Summary:string, -Comment:string) is nondet.
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
%	If Object is  unbound  and  multiple   objects  share  the  same
%	description, Object is unified with a   list  of terms described
%	above.
%
%	@param Summary	First sentence.  Normalised spacing.
%	@param Comment	Comment string from the source-code (untranslated)

doc_comment(Object, Pos, Summary, Comment) :-
	var(Object), !,
	current_module(M),
	'$c_current_predicate'(_, M:'$pldoc'(_,_,_,_)),
	M:'$pldoc'(Obj, Pos, Summary, Comment),
	qualify(M, Obj, Object0),
	(   '$c_current_predicate'(_, M:'$pldoc_link'(_, _)),
	    findall(L, M:'$pldoc_link'(L, Obj), Ls), Ls \== []
	->  maplist(qualify(M),	Ls, QLs),
	    Object = [Object0|QLs]
	;   Object = Object0
	).
doc_comment(M:Object, Pos, Summary, Comment) :- !,
	current_module(M),
	'$c_current_predicate'(_, M:'$pldoc'(_,_,_,_)),
	(   M:'$pldoc'(Object, Pos, Summary, Comment)
	;   '$c_current_predicate'(_, M:'$pldoc_link'(_, _)),
	    M:'$pldoc_link'(Object, Obj2),
	    M:'$pldoc'(Obj2, Pos, Summary, Comment)
	).
doc_comment(Name/Arity, Pos, Summary, Comment) :-
	system_module(M),
	doc_comment(M:Name/Arity, Pos, Summary, Comment).


qualify(M, H, H) :- system_module(M), !.
qualify(M, H, H) :- sub_atom(M, 0, _, _, $), !.
qualify(M, H, M:H).

system_module(user).
system_module(system).


%	Make the summary available to external tools on plugin basis.

prolog:predicate_summary(PI, Summary) :-
	doc_comment(PI, _, Summary, _).


		 /*******************************
		 *	CALL-BACK COLLECT	*
		 *******************************/

%%	process_comments(+Comments:list, +TermPos, +File) is det.
%
%	Processes comments returned by read_term/3 using the =comments=
%	option.  It creates clauses of the form
%
%		* '$mode'(Head, Det)
%		* '$pldoc'(Id, Pos, Summary, Comment)
%		* '$pldoc_link'(Id0, Id)
%
%	where Id is one of
%
%		* module(Title)
%		Generated from /** <module> Title */
%		* Name/Arity
%		Generated from Name(Arg, ...)
%		* Name//Arity
%		Generated from Name(Arg, ...)//

process_comments([], _, _).
process_comments([Pos-Comment|T], TermPos, File) :-
	(   Pos @> TermPos		% comments inside term
	->  true
	;   process_comment(Pos, Comment, File),
	    process_comments(T, TermPos, File)
	).

process_comment(Pos, Comment, File) :-
	is_structured_comment(Comment, Prefixes), !,
	stream_position_data(line_count, Pos, Line),
	FilePos = File:Line,
	process_structured_comment(FilePos, Comment, Prefixes).
process_comment(_, _, _).

%%	process_structured_comment(+FilePos, +Comment:string) is det.

process_structured_comment(FilePos, Comment, _) :- % already processed
	prolog_load_context(module, M),
	'$c_current_predicate'(_, M:'$pldoc'(_,_,_,_)),
	catch(M:'$pldoc'(_, FilePos, _, Comment), _, fail), !.
process_structured_comment(FilePos, Comment, Prefixes) :-
	string_to_list(Comment, CommentCodes),
	indented_lines(CommentCodes, Prefixes, Lines),
	(   section_comment_header(Lines, Header, RestLines)
	->  Header = \section(Type, Title),
	    Id =.. [Type,Title],
	    compile_clause('$pldoc'(Id, FilePos, Title, Comment), FilePos)
	;   prolog_load_context(module, Module),
	    process_modes(Lines, Module, FilePos, Modes, _, RestLines)
	->  store_modes(Modes, FilePos),
	    modes_to_predicate_indicators(Modes, [PI0|PIs]),
	    summary_from_lines(RestLines, Codes),
	    string_to_list(Summary, Codes),
	    compile_clause('$pldoc'(PI0, FilePos, Summary, Comment), FilePos),
	    forall(member(PI, PIs),
		   compile_clause('$pldoc_link'(PI, PI0), FilePos))
	), !.
process_structured_comment(File:Line, Comment, _) :-
	print_message(warning,
		      format('~w:~d: Failed to process comment:~n~s~n',
			     [File, Line, Comment])),
	fail.
