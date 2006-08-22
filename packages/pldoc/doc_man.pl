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

:- module(pldoc_man,
	  [ clean_man_index/0,		% 
	    index_man_directory/1,	% +DirSpec
	    index_man_file/1,		% +FileSpec
	    load_man_object/2,		% +Obj, -DOM
	    man_page/4			% +Obj, +Options, //
	  ]).
:- use_module(library(sgml)).
:- use_module(library(occurs)).
:- use_module(library(lists)).
:- use_module(doc_wiki).
:- use_module(library('http/html_write')).

/** <module> Process SWI-Prolog HTML manuals

*/

:- dynamic
	man_index/4.			% Object, Summary, File, Offset

clean_man_index :-
	retractall(man_index(_,_,_,_)).


		 /*******************************
		 *	    PARSE MANUAL	*
		 *******************************/

%%	load_man_directory(Dir) is det
%
%	Index the HTML directory Dir

index_man_directory(Spec) :-
	absolute_file_name(Spec, Dir,
			   [ file_type(directory)
			   ]),
	atom_concat(Dir, '/*.html', Pattern),
	expand_file_name(Pattern, Files),
	maplist(index_man_file, Files).


%%	index_man_file(+File)
%
%	Collect the documented objects from the SWI-Prolog manual file
%	File.

index_man_file(File) :-
	absolute_file_name(File, Path,
			   [ access(read)
			   ]),
	open(Path, read, In, [type(binary)]),
	dtd(html, DTD),
        new_sgml_parser(Parser, [dtd(DTD)]),
        set_sgml_parser(Parser, file(File)),
        set_sgml_parser(Parser, dialect(sgml)),
	set_sgml_parser(Parser, shorttag(false)),
	nb_setval(pldoc_man_index, []),
	call_cleanup(sgml_parse(Parser,
				[ source(In),
				  call(begin, index_on_begin)
				]),
		     (	 free_sgml_parser(Parser),
			 close(In),
			 nb_delete(pldoc_man_index)
		     )).


%%	index_on_begin(+Element, +Attributes, +Parser) is semidet.
%
%	Called from sgml_parse/2 in  index_man_file/1.   Element  is the
%	name of the element, Attributes the  list of Name=Value pairs of
%	the open attributes. Parser is the parser objects.

index_on_begin(dt, Attributes, Parser) :-
	memberchk(class=pubdef, Attributes),
        get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(DT),
		     parse(content)
		   ]),
	sub_term(element(a, AA, _), DT),
        memberchk(name=Id, AA), !,
	concat_atom([Name, ArityAtom], /, Id),
	catch(atom_number(ArityAtom, Arity), _, fail),
	integer(Arity),
	Arity > 0,
	nb_setval(pldoc_man_index, dd(Name/Arity, File, Offset)).
index_on_begin(dd, _, Parser) :-
	nb_getval(pldoc_man_index, dd(Name/Arity, File, Offset)),
	nb_setval(pldoc_man_index, []),
	sgml_parse(Parser,
		   [ document(DD),
		     parse(content)
		   ]),
	summary(DD, Summary),
        assert(man_index(Name/Arity, Summary, File, Offset)).


%%	summary(+DOM, -Summary:string) is det.
%
%	Summary is the first sentence of DOM.

summary(DOM, Summary) :-
	phrase(summary(DOM, _), SummaryCodes0),
	phrase(normalise_white_space(SummaryCodes), SummaryCodes0),
	string_to_list(Summary, SummaryCodes).

summary([], _) --> !,
	[].
summary(_, Done) -->
	{ Done == true }, !,
	[].
summary([element(_,_,Content)|T], Done) --> !,
	summary(Content, Done),
	summary(T, Done).
summary([CDATA|T], Done) -->
	{ atom_codes(CDATA, Codes)
	},
	(   { Codes = [Period|Rest],
	      code_type(Period, period),
	      space(Rest)
	    }
	->  [ Period ],
	    { Done = true }
	;   { append(Sentence, [C, Period|Rest], Codes),
	      code_type(Period, period),
	      \+ code_type(C, period),
	      space(Rest)
	    }
	->  string(Sentence),
	    [C, Period],
	    { Done = true }
	;   string(Codes),
	    summary(T, Done)
	).
	
string([]) -->
	[].
string([H|T]) -->
	[H],
	string(T).

space([C|_]) :- code_type(C, space), !.
space([]).


		 /*******************************
		 *	      RETRIEVE		*
		 *******************************/

%%	load_man_object(+Obj, -DOM) is semidet.
%
%	load the desription of the  object   matching  Obj from the HTML
%	sources and return the DT/DD pair in DOM.

load_man_object(For, DOM) :-
	object_spec(For, Obj),
	man_index(Obj, _, Path, Position),
	open(Path, read, In, [type(binary)]),
	seek(In, Position, bof, _),
	dtd(html, DTD),
	new_sgml_parser(Parser,
			[ dtd(DTD)
			]),
	set_sgml_parser(Parser, file(Path)),
        set_sgml_parser(Parser, dialect(sgml)),
	set_sgml_parser(Parser, shorttag(false)),
	sgml_parse(Parser,
		   [ document(DT),
		     source(In),
		     parse(element)
		   ]),
	sgml_parse(Parser,
		   [ document(DD),
		     source(In),
		     parse(element)
		   ]),
	free_sgml_parser(Parser),
	close(In),
	append(DT, DD, DOM).


object_spec(Spec, Spec).
object_spec(Atom, Name/Arity) :-
	atom(Atom),
	concat_atom([Name, AA], /, Atom),
	catch(atom_number(AA, Arity), _, fail),
	integer(Arity),
	Arity >= 0.


		 /*******************************
		 *	      EMIT    		*
		 *******************************/

%%	man_page(+Obj, +Options)// is det.
%
%	Produce a Prolog manual page for  Obj.   The  page consists of a
%	link to the section-file and  a   search  field, followed by the
%	predicate description.

man_page(Obj, _Options) -->
	{ load_man_object(Obj, DOM)
	},
	html(\dom_list(DOM)).

dom_list([]) -->
	[].
dom_list([H|T]) -->
	dom(H),
	dom_list(T).

dom(element(Name, Attrs, Content)) --> !,
	{ Begin =.. [Name|Attrs] },
	html_begin(Begin),
	dom_list(Content),
	html_end(Name).
dom(CDATA) -->
	html(CDATA).
