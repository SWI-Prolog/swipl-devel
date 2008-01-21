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

:- module(pldoc_wiki,
	  [ wiki_string_to_dom/3,	% +String, +Args, -DOM
	    wiki_lines_to_dom/3,	% +Lines, +Map, -DOM
	    section_comment_header/3,	% +Lines, -Header, -RestLines
	    summary_from_lines/2,	% +Lines, -Summary
	    indented_lines/3,		% +Text, +PrefixChars, -Lines
	    strip_leading_par/2,	% +DOM0, -DOM
	    normalise_white_space/3,	% -Text, //
	    autolink_file/2		% +FileName, -Type
	  ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(memfile)).
:- use_module(library(pairs)).


		 /*******************************
		 *	    WIKI PARSING	*
		 *******************************/

%%	wiki_lines_to_dom(+Lines:lines, +Args:list(atom), -Term) is det
%
%	Translate a Wiki text into  an   HTML  term suitable for html//1
%	from the html_write library.

wiki_lines_to_dom(Lines, Args, HTML) :-
	tokenize_lines(Lines, Tokens),
	wiki_structure(Tokens, Pars),
	wiki_faces(Pars, Args, HTML).


%%	wiki_string_to_dom(+String, +Args, -DOM) is det.
%
%	Translate a plain text into a DOM term.
%	
%	@param String	Plain text.  Either a string or a list of codes.

wiki_string_to_dom(String, Args, DOM) :-
	string(String), !,
	string_to_list(String, Codes),
	indented_lines(Codes, [], Lines),
	wiki_lines_to_dom(Lines, Args, DOM).
wiki_string_to_dom(Codes, Args, DOM) :-
	indented_lines(Codes, [], Lines),
	wiki_lines_to_dom(Lines, Args, DOM).


%%	wiki_structure(+Lines:lines, -Pars:list(par)) is det
%
%	Get the structure in terms of  paragraphs, lists and tables from
%	the  lines.  This  processing  uses  a  mixture  of  layout  and
%	punctuation.

wiki_structure([], []) :- !.
wiki_structure([_-[]|T], Pars) :- !,	% empty lines
	wiki_structure(T, Pars).
wiki_structure(Lines, [\tags(Tags)]) :-
	tags(Lines, Tags), !.
wiki_structure(Lines, [P1|PL]) :-
	take_par(Lines, P1, RestLines),
	wiki_structure(RestLines, PL).
	
take_par(Lines, List, Rest) :-
	list_item(Lines, Type, Indent, LI, LIT, Rest0), !,
	rest_list(Rest0, Type, Indent, LIT, [], Rest),
	List0 =.. [Type, LI],
	(   ul_to_dl(List0, List)
	->  true
	;   List0 = dl(Items)
	->  List = dl(class=wiki, Items)
	;   List = List0
	).
take_par([N-['|'|RL1]|LT], table(class=wiki, [tr(R0)|RL]), Rest) :-
	phrase(row(R0), RL1),
	rest_table(LT, N, RL, Rest), !.
take_par([_-L1|LT], Section, LT) :-
	section_line(L1, Section), !.
take_par([_-L1|LT], p(Par), Rest) :- !,
	append(L1, PT, Par),
	rest_par(LT, PT, Rest).
take_par([Verb|Lines], Verb, Lines).

%%	list_item(+Lines, ?Type, ?Indent, -LI0, -LIT, -RestLines) is det.
%
%	Create a list-item. Naturally this should produce a single item,
%	but DL lists produce two items, so   we create the list of items
%	as a difference list.

list_item([Indent-Line|LT], Type, Indent, Items, ItemT, Rest) :- !,
	list_item_prefix(Type, Line, L1),
	(   Type == dl
	->  append(DT0, [:|DD1], L1),
	    append(DD1, LIT, DD),
	    strip_ws_tokens(DT0, DT),
	    Items = [dt(DT),dd(DD)|ItemT]
	;   append(L1, LIT, LI0),
	    Items = [li(LI0)|ItemT]
	),
	rest_list_item(LT, Type, Indent, LIT, Rest).
list_item(Lines, _, Indent, [SubList|LIT], LIT, Rest) :-	% sub-list
	nonvar(Indent),
	Lines = [SubIndent-Line|_],
	SubIndent > Indent,
	list_item_prefix(_, Line, _), !,
	take_par(Lines, SubList, Rest).

%%	rest_list_item(+Lines, +Type, +Indent, -RestItem, -RestLines) is det
%
%	Extract the remainder (after the first line) of a list item.

rest_list_item([], _, _, [], []).
rest_list_item([_-[]|L], _, _, [], L) :- !.	% empty line
rest_list_item(L, _, N, [], L) :-		% less indented
	L = [I-_|_], I < N, !.
rest_list_item(L, _, _, [], L) :-		% Start with mark
	L = [_-Line|_],
	list_item_prefix(_, Line, _), !.
rest_list_item([_-L1|L0], Type, N, ['\n'|LI], L) :-
	append(L1, LIT, LI),
	rest_list_item(L0, Type, N, LIT, L).


%%	rest_list(+Lines, +Type, +Indent,
%%		  -Items, -ItemTail, -RestLines) is det.

rest_list(Lines, Type, N, Items, IT, Rest) :-
	list_item(Lines, Type, N, Items, IT0, Rest0), !,
	rest_list(Rest0, Type, N, IT0, IT, Rest).
rest_list(Rest, _, _, IT, IT, Rest).

%%	list_item_prefix(?Type, +Line, -Rest) is det.

list_item_prefix(ul, [*, ' '|T], T) :- !.
list_item_prefix(dl, [$, ' '|T], T) :-
	memberchk(:, T), !.
list_item_prefix(ol, [N, '.', ' '|T], T) :-
	string(N),
	string_to_list(N, [D]),
	between(0'0, 0'9, D).

%%	ul_to_dl(+UL, -DL) is semidet.
%
%	Translate an UL list into a DL list   if  all entries are of the
%	form "* <term> nl, <description>" or all   items  are of the for
%	[[PredicateIndicator]].

ul_to_dl(ul(Items), Description) :-
	term_items(Items, DLItems, []),
	(   terms_to_predicate_includes(DLItems, Preds)
	->  Description = dl(class(predicates), Preds)
	;   Description = dl(class(termlist), DLItems)
	).

term_items([], T, T).
term_items([LI|LIs], DLItems, Tail) :-
	term_item(LI, DLItems, Tail1),
	term_items(LIs, Tail1, Tail).

%%	term_item(+LI, -DLItem, ?Tail) is semidet.
%
%	If LI is of the form <Term> followed  by a newline, return it as
%	dt-dd  tuple.  The  <dt>  item    contains  a  term  \term(Term,
%	Bindings).

term_item(li(Tokens),
	  [ dt(class=term, \term(Term, Bindings)),
	    dd(Descr)
	  | Tail
	  ], Tail) :-
	(   (   append(TermTokens, ['\n'|Descr], Tokens)
	    ->	true
	    ;	TermTokens = Tokens,
		Descr = []
	    )
	->  new_memory_file(MemFile),
	    open_memory_file(MemFile, write, Out),
	    forall(member(T, TermTokens),
		   write(Out, T)),
	    write(Out, ' .\n'),
	    close(Out),
	    open_memory_file(MemFile, read, In),
	    catch(call_cleanup((read_dt_term(In, Term, Bindings),
				read_dt_term(In, end_of_file, [])),
			       (   close(In),
				   free_memory_file(MemFile))), _, fail)
	).

read_dt_term(In, Term, Bindings) :-
	read_term(In, Term,
		  [ variable_names(Bindings),
		    module(pldoc_modes)
		  ]).

terms_to_predicate_includes([], []).
terms_to_predicate_includes([dt(class=term, \term([[PI]], [])), dd([])|T0],
			    [\include(PI, predicate)|T]) :-
	is_pi(PI),
	terms_to_predicate_includes(T0, T).

is_pi(Name/Arity) :-
	atom(Name),
	integer(Arity),
	between(0, 20, Arity).
is_pi(Name//Arity) :-
	atom(Name),
	integer(Arity),
	between(0, 20, Arity).


%%	row(-Cells)// is det.

row([C0|CL]) -->
	cell(C0), !,
	row(CL).
row([]) -->
	[].

cell(td(C)) -->
	string(C0),
	['|'], !,
	{ strip_ws_tokens(C0, C)
	}.

rest_table([N-['|'|RL1]|LT], N, [tr(R0)|RL], Rest) :- !,
	phrase(row(R0), RL1),
	rest_table(LT, N, RL, Rest).
rest_table(Rest, _, [], Rest).

%%	rest_par(+Lines, -Part, -RestLines) is det.

rest_par([], [], []).
rest_par([_-[]|Rest], [], Rest) :- !.
rest_par([_-L1|LT], ['\n'|Par], Rest) :-
	append(L1, PT, Par),
	rest_par(LT, PT, Rest).


%%	section_line(+Tokens, -Section) is det.
%
%	Extract a section using the Twiki   conventions. The section may
%	be preceeded by [Word], in which case we generate an anchor name
%	Word for the section.

section_line([-,-,-|Rest], Section) :-
	plusses(Rest, Section).

plusses([+, ' '|Rest], h1(Attrs, Content)) :-
	hdr_attributes(Rest, Attrs, Content).
plusses([+, +, ' '|Rest], h2(Attrs, Content)) :-
	hdr_attributes(Rest, Attrs, Content).
plusses([+, +, +, ' '|Rest], h3(Attrs, Content)) :-
	hdr_attributes(Rest, Attrs, Content).
plusses([+, +, +, +, ' '|Rest], h4(Attrs, Content)) :-
	hdr_attributes(Rest, Attrs, Content).

hdr_attributes(List, Attrs, Content) :-
	strip_leading_ws(List, List2),
	(   List2 = ['[',Word,']'|List3],
	    atomic(Word)
	->  strip_ws_tokens(List3, Content),
	    string_to_atom(Word, Name),
	    Attrs = [class(wiki), name(Name)]
	;   Attrs = class(wiki),
	    strip_ws_tokens(List, Content)
	).


%%	strip_ws_tokens(+Tokens, -Stripped)
%
%	Strip leading and trailing whitespace from a token list.  Note
%	the the whitespace is already normalised.

strip_ws_tokens([' '|T0], T) :- !,
	strip_ws_tokens(T0, T).
strip_ws_tokens(L0, L) :-
	append(L, [' '], L0), !.
strip_ws_tokens(L, L).


%%	strip_leading_ws(+Tokens, -Stripped) is det.
%
%	Strip leading whitespace from a token list.

strip_leading_ws([' '|T], T) :- !.
strip_leading_ws(T, T).


		 /*******************************
		 *	       TAGS		*
		 *******************************/

%%	tags(+Lines:lines, -Tags) is semidet.
%
%	If the first line is a @tag, read the remainder of the lines to
%	a list of \tag(Name, Value) terms.

tags(Lines, Tags) :-
	collect_tags(Lines, Tags0),
	keysort(Tags0, Tags1),
	pairs_values(Tags1, Tags2),
	combine_tags(Tags2, Tags).

%%	collect_tags(+IndentedLines, -Tags) is det
%
%	Create a list Order-tag(Tag,Tokens) for   each @tag encountered.
%	Order is the desired position as defined by tag_order/2.

collect_tags([], []).
collect_tags([Indent-[@,String|L0]|Lines], [Order-tag(Tag,Value)|Tags]) :-
	tag_name(String, Tag, Order), !,
	strip_leading_ws(L0, L),
	rest_tag(Lines, Indent, VT, RestLines),
	wiki_structure([0-L|VT], Value0),
	strip_leading_par(Value0, Value),
	collect_tags(RestLines, Tags).


%%	tag_name(+String, -Tag:atom, -Order:int) is semidet.
%
%	If String denotes a know tag-name, 

tag_name(String, Tag, Order) :-
	string(String),
	format(atom(Name), '~s', [String]),
	(   renamed_tag(Name, Tag),
	    tag_order(Tag, Order)
	->  print_message(warning, pldoc(deprecated_tag(Name, Tag)))
	;   tag_order(Name, Order)
	->  Tag = Name
	;   print_message(warning, pldoc(unknown_tag(Name))),
	    fail
	).


rest_tag([], _, [], []) :- !.
rest_tag(Lines, Indent, [], Lines) :-
	Lines = [Indent-[@,NameS|_]|_],
	string(NameS), !.
rest_tag([L|Lines0], Indent, [L|VT], Lines) :-
	rest_tag(Lines0, Indent, VT, Lines).


%%	renamed_tag(+DeprecatedTag:atom, -Tag:atom) is semidet.
%
%	Declaration for deprecated tags.

renamed_tag(exception, throws).


%%	tag_order(+Tag:atom, -Order:int) is semidet.
%
%	Both declares the know tags and  their expected order. Currently
%	the tags are forced into  this   order  without  warning. Future
%	versions may issue a warning if the order is inconsistent.

tag_order(param,       1).
tag_order(error,       2).		% same as throw
tag_order(throws,      3).
tag_order(author,      4).
tag_order(version,     5).
tag_order(see,	       6).
tag_order(deprecated,  7).
tag_order(compat,      8).		% PlDoc extension
tag_order(copyright,   9).
tag_order(license,    10).
tag_order(bug,	      11).
tag_order(tbd,	      12).


%%	combine_tags(+Tags:list(tag(Key, Value)), -Tags:list) is det.
%
%	Creates the final tag-list.  Tags is a list of
%	
%		* \params(list(param(Name, Descr)))
%		* \tag(Name, list(Descr))
%	
%	Descr is a list of tokens.

combine_tags([], []).
combine_tags([tag(param, V1)|T0], [\params([P1|PL])|Tags]) :- !,
	param_tag(V1, P1),
	param_tags(T0, PL, T1),
	combine_tags(T1, Tags).
combine_tags([tag(Tag,V0)|T0], [\tag(Tag, [V0|Vs])|T]) :-
	same_tag(Tag, T0, T1, Vs),
	combine_tags(T1, T).

param_tag([PN|Descr0], param(PN, Descr)) :-
	strip_leading_ws(Descr0, Descr).

param_tags([tag(param, V1)|T0], [P1|PL], T) :- !,
	param_tag(V1, P1),
	param_tags(T0, PL, T).
param_tags(T, [], T).

same_tag(Tag, [tag(Tag, V)|T0], T, [V|Vs]) :- !,
	same_tag(Tag, T0, T, Vs).
same_tag(_, L, L, []).


		 /*******************************
		 *	       FACES		*
		 *******************************/

%%	wiki_faces(+Structure, +ArgNames, -HTML) is det.
%
%	Given the wiki structure, analyse the content of the paragraphs,
%	list items and table cells and apply font faces and links.

wiki_faces(DOM0, ArgNames, DOM) :-
	structure_term(DOM0, Functor, Content0), !,
	wiki_faces_list(Content0, ArgNames, Content),
	structure_term(DOM, Functor, Content).
wiki_faces(Verb, _, Verb) :-
	verbatim_term(Verb), !.
wiki_faces(Content0, ArgNames, Content) :-
	assertion(is_list(Content0)),
	phrase(wiki_faces(Content, ArgNames), Content0), !.

wiki_faces_list([], _, []).
wiki_faces_list([H0|T0], Args, [H|T]) :-
	wiki_faces(H0, Args, H),
	wiki_faces_list(T0, Args, T).

%%	structure_term(+Term, -Functor, -Content) is semidet.
%%	structure_term(-Term, +Functor, +Content) is det.
%
%	(Un)pack a term describing structure, so  we can process Content
%	and re-pack the structure.

structure_term(\tags(Tags), tags, [Tags]) :- !.
structure_term(\params(Params), params, [Params]) :- !.
structure_term(param(Name,Descr), param(Name), [Descr]) :- !.
structure_term(\tag(Name,Value), tag(Name), [Value]) :- !.
structure_term(\include(What,Type), include(What,Type), []) :- !.
structure_term(dl(Att, Args), dl(Att), [Args]) :- !.
structure_term(dt(Att, Args), dt(Att), [Args]) :- !.
structure_term(table(Att, Args), table(Att), [Args]) :- !.
structure_term(h1(Att, Args), h1(Att), [Args]) :- !.
structure_term(h2(Att, Args), h2(Att), [Args]) :- !.
structure_term(h3(Att, Args), h3(Att), [Args]) :- !.
structure_term(h4(Att, Args), h4(Att), [Args]) :- !.
structure_term(Term, Functor, Args) :-
	functor(Term, Functor, 1),
	structure_tag(Functor), !,
	Term =.. [Functor|Args].

structure_tag(p).
structure_tag(ul).
structure_tag(ol).
structure_tag(dl).
structure_tag(li).
structure_tag(dt).
structure_tag(dd).
structure_tag(table).
structure_tag(tr).
structure_tag(td).

%%	verbatim_term(?Term) is det
%
%	True if Term must be passes verbatim.

verbatim_term(pre(_,_)).
verbatim_term(\term(_,_)).

%%	wiki_faces(-WithFaces, +ArgNames)// is nondet.
%
%	Apply font-changes and automatic  links   to  running  text. The
%	faces are applied after discovering   the structure (paragraphs,
%	lists, tables, keywords).

wiki_faces([], _) -->
	[].
wiki_faces([H|T], ArgNames) -->
	wiki_face(H, ArgNames),
	wiki_faces(T, ArgNames).

wiki_face(var(Word), ArgNames) -->
	[Word],
	{ string(Word),			% punctuation and blanks are atoms
	  member(Arg, ArgNames),
	  sub_atom(Arg, 0, _, 0, Word)	% match string to atom
	}, !.
wiki_face(b(Bold), _) -->
	[*], word_token(Bold), [*], !.
wiki_face(b(Bold), ArgNames) -->
	[*,'|'], wiki_faces(Bold, ArgNames), ['|',*], !.
wiki_face(i(Italic), _) -->
	['_'], word_token(Italic), ['_'], !.
wiki_face(i(Italic), ArgNames) -->
	['_','|'], wiki_faces(Italic, ArgNames), ['|','_'], !.
wiki_face(code(Code), _) -->
	[=], word_token(Code), [=], !.
wiki_face(code(Code), _) -->
	[=,'|'], wiki_faces(Code, []), ['|',=], !.
wiki_face(\predref(Name/Arity), _) -->
	[ NameS, '/' ], arity(Arity),
	{ functor_name(NameS), !,
	  string_to_atom(NameS, Name)
	}.
wiki_face(\predref(Name/Arity), _) -->
	symbol_string(S), [ '/' ], arity(Arity), !,
	{ atom_chars(Name, S)
	}.
wiki_face(\predref(Name//Arity), _) -->
	[ NameS, '/', '/' ], arity(Arity),
	{ functor_name(NameS), !,
	  string_to_atom(NameS, Name)
	}.
wiki_face(span(class=cvs, CVS), _) -->
	[$, Word, :], {string(Word)}, wiki_faces(CVS0, []), [$], !,
	{ strip_ws_tokens(CVS0, CVS) }.
wiki_face(\include(Name, Type), _) -->
	['[','['], word_token(BaseS), ['.'], word_token(ExtS), [']',']'],
	{  concat_atom([BaseS, '.', ExtS], Name),
	   file_name_extension(_, Ext, Name),
	   autolink_extension(Ext, Type)
	}, !.
wiki_face(\file(Name), _) -->
	word_token(BaseS), ['.'], word_token(ExtS),
	{ concat_atom([BaseS, '.', ExtS], Name),
	  (   autolink_file(Name, _)
	  ;   file_name_extension(_, Ext, Name),
	      autolink_extension(Ext, _)
	  ), !
	}.
wiki_face(\file(Name), _) -->
	word_token(NameS),
	{ autolink_file(Name, _),
	  sub_atom(NameS, 0, _, 0, Name)
	}, !.
wiki_face(a(href=Ref, Ref), _) -->
	word_token(ProtS), [:,/,/], { url_protocol(ProtS) },
	string(Rest), peek_end_url, !,
	{ concat_atom([ProtS, :,/,/ | Rest], Ref) }.
wiki_face(a(href=Ref, Ref), _) -->
	[<], word_token(ProtS), [:], string(Rest), [>], !,
	{ concat_atom([ProtS, : | Rest], Ref) }.
wiki_face(FT, ArgNames) -->
	[T],
	{   atomic(T)
	->  FT = T
	;   wiki_faces(T, ArgNames, FT)
	}.

%%	word_token(-Word:string)// is semidet.
%
%	True if the next token  is  a   string,  which  implies  it is a
%	sequence of alpha-numerical characters.

word_token(Word) -->
	[Word],
	{ string(Word) }.

%%	arity(-Arity:int)// is semidet.
%
%	True if the next token can be  interpreted as an arity. That is,
%	refers to a non-negative integers of at most 20. Although Prolog
%	allows for higher arities, we assume 20   is  a fair maximum for
%	user-created predicates that are documented.

arity(Arity) -->
	[ Word ],
	{ catch(atom_number(Word, Arity), _, fail),
	  Arity >= 0, Arity < 20
	}.

%%	symbol_string(-String)// is nondet
%
%	Accept  a  non-empty  sequence  of   Prolog  symbol  characters,
%	starting with the shortest match.

symbol_string([S]) -->
	[S],
	{ prolog_symbol_char(S) }.
symbol_string([H|T]) -->
	[H],
	{ prolog_symbol_char(H) },
	symbol_string(T).

%%	prolog_symbol_char(?Char)
%
%	True if char is classified by Prolog as a symbol char.

prolog_symbol_char(#).
prolog_symbol_char($).
prolog_symbol_char(&).
prolog_symbol_char(*).
prolog_symbol_char(+).
prolog_symbol_char(-).
prolog_symbol_char(.).
prolog_symbol_char(/).
prolog_symbol_char(:).
prolog_symbol_char(<).
prolog_symbol_char(=).
prolog_symbol_char(>).
prolog_symbol_char(?).
prolog_symbol_char(@).
prolog_symbol_char(\).
prolog_symbol_char(^).
prolog_symbol_char(`).
prolog_symbol_char(~).


functor_name(String) :-
	sub_atom(String, 0, 1, _, Char),
	char_type(Char, lower).

url_protocol(String) :-	sub_atom(String, 0, _, 0, http).
url_protocol(String) :-	sub_atom(String, 0, _, 0, ftp).
url_protocol(String) :-	sub_atom(String, 0, _, 0, mailto).


peek_end_url -->
	peek(End),
	{ space_atom(End) }, !.
peek_end_url -->
	eos, !.

space_atom(' ').
space_atom('\r').
space_atom('\n').

%%	autolink_extension(?Ext, ?Type) is nondet.
%
%	True if Ext is a filename extensions that create automatic links
%	in the documentation.

autolink_extension(pl, prolog).
autolink_extension(txt, wiki).
autolink_extension(gif, image).
autolink_extension(png, image).
autolink_extension(jpg, image).
autolink_extension(jpeg, image).

%%	autolink_file(?File, -Type) is nondet.
%
%	Files to which we automatically create links, regardless of the
%	extension.

autolink_file('README', wiki).
autolink_file('TODO', wiki).
autolink_file('ChangeLog', wiki).

		 /*******************************
		 *	     SECTIONS		*
		 *******************************/

%%	section_comment_header(+Lines, -Header, -RestLines) is semidet.
%
%	Processes   /**   <section>   comments.   Header   is   a   term
%	\section(Type, Title), where  Title  is   a  string  holding the
%	section title and Type is an atom holding the text between <>.
%	
%	@param Lines	List of Indent-Codes.
%	@param Header	DOM term of the format \section(Type, Title),
%			where Type is an atom from <type> and Title is
%			a string holding the type.

section_comment_header([_-Line|Lines], Header, Lines) :-
	phrase(section_line(Header), Line).

section_line(\section(Type, Title)) -->
	ws, "<", word(Codes), ">", normalise_white_space(TitleCodes),
	{ atom_codes(Type, Codes),
	  string_to_list(Title, TitleCodes)
	}.


%%	normalise_white_space(-Text)// is det.
%
%	Text is input after deleting leading   and  trailing white space
%	and mapping all internal white space to a single space.

normalise_white_space(Text) -->
	ws,
	normalise_white_space2(Text).

normalise_white_space2(Text) -->
	non_ws(Text, Tail),
	ws,
	(   eos
	->  { Tail = "" }
	;   { Tail = [32|T2] },
	    normalise_white_space2(T2)
	).


		 /*******************************
		 *	     TOKENIZER		*
		 *******************************/

%%	tokenize_lines(+Lines:lines, -TokenLines) is det
%
%	Convert Indent-Codes into Indent-Tokens

tokenize_lines([], []) :- !.
tokenize_lines(Lines, [Pre|T]) :-
	verbatim(Lines, Pre, RestLines), !,
	tokenize_lines(RestLines, T).
tokenize_lines([I-H0|T0], [I-H|T]) :-
	phrase(tokens(H), H0),
	tokenize_lines(T0, T).


%%	tokens(-Tokens:list)// is det.
%
%	Create a list of tokens, where  is  token   is  either  a ' ' to
%	denote spaces, a string denoting a word   or  an atom denoting a
%	punctuation character.

tokens([H|T]) -->
	token(H), !,
	tokens(T).
tokens([]) -->
	[].

token(T) -->
	[C],
	(   { code_type(C, space) }
	->  ws,
	    { T = ' ' }
	;   { code_type(C, alnum) },
	    word(Rest),
	    { string_to_list(T, [C|Rest]) }
	;   { char_code(T, C) }
	).

word([C0|T]) -->
	[C0],  { code_type(C0, alnum) }, !,
	word(T).
word([0'_, C1|T]) -->
	[0'_, C1],  { code_type(C1, alnum) }, !,
	word(T).
word([]) -->
	[].


%%	verbatim(+Lines, -Pre, -RestLines) is det.
%
%	Extract a verbatim environment.  The  returned   Pre  is  of the
%	format pre(Class, String). The indentation of  the leading == is
%	substracted from the indentation of the verbatim lines.
%
%	Verbatim environment is delimited as
%	
%	==
%		...,
%		verbatim(Lines, Pre, Rest)
%		...,
%	==

verbatim([Indent-"=="|Lines], pre(class(code),Pre), RestLines) :-
	verbatim_body(Lines, Indent, [10|PreCodes], [],
		      [Indent-"=="|RestLines]), !,
	string_to_list(Pre, PreCodes).

verbatim_body(Lines, _, PreT, PreT, Lines).
verbatim_body([I-L|Lines], Indent, [10|Pre], PreT, RestLines) :-
	PreI is I - Indent,
	pre_indent(PreI, Pre, PreT0),
	verbatim_line(L, PreT0, PreT1),
	verbatim_body(Lines, Indent, PreT1, PreT, RestLines).

%%	pre_indent(+Indent)// is det.
%
%	Insert Indent leading spaces.  Note we cannot use tabs as these
%	are not expanded by the HTML <pre> element.

pre_indent(Indent, Pre, PreT) :-
	format(codes(Pre, PreT), '~*c', [Indent, 32]).

verbatim_line(Line, Pre, PreT) :-
	append(Line, PreT, Pre).


		 /*******************************
		 *	      SUMMARY		*
		 *******************************/

%%	summary_from_lines(+Lines:lines, -Summary:string) is det.
%
%	Produce a summary for Lines. Similar  to JavaDoc, the summary is
%	defined as the first sentence of the documentation. In addition,
%	a sentence is also ended by an  empty   line  or  the end of the
%	comment.

summary_from_lines(Lines, Summary) :-
	skip_empty_lines(Lines, Lines1),
	summary2(Lines1, Sentence0),
	end_sentence(Sentence0, Sentence),
	string_to_list(Summary, Sentence).

summary2(_, Sentence) :-
	Sentence == [], !.		% we finished our sentence
summary2([], []) :- !.
summary2([_-[]|_], []) :- !.		% empty line
summary2([_-[0'@|_]|_], []) :- !.	% keyword line
summary2([_-L0|Lines], Sentence) :-
	phrase(sentence(Sentence, Tail), L0, _),
	summary2(Lines, Tail).

sentence([C,End], []) -->
	[C,End],
	{ \+ code_type(C, period),
	  code_type(End, period)		% ., !, ?
	},
	white, !.
sentence([0' |T0], T) -->
	space, !,
	ws,
	sentence(T0, T).
sentence([H|T0], T) -->
	[H],
	sentence(T0, T).
sentence([0' |T], T) -->
	eos.

white -->
	space.
white -->
	eos.

skip_empty_lines([], []).
skip_empty_lines([_-""|Lines0], Lines) :- !,
	skip_empty_lines(Lines0, Lines).
skip_empty_lines(Lines, Lines).

end_sentence("", "").
end_sentence(" ", ".") :- !.
end_sentence([H|T0], [H|T]) :-
	end_sentence(T0, T).


		 /*******************************
		 *	  CREATE LINES		*
		 *******************************/

%%	indented_lines(+Text:string, +Prefixes:list(codes), -Lines:list) is det.
%
%	Extract a list of lines  without   leading  blanks or characters
%	from Prefix from Text. Each line   is a term Indent-Codes, where
%	Indent specifies the line_position of the real text of the line.

indented_lines(Comment, Prefixes, Lines) :-
	string_to_list(Comment, List),
	phrase(split_lines(Prefixes, Lines), List).

split_lines(_, []) -->
	end_of_comment, !.
split_lines(Prefixes, [Indent-L1|Ls]) -->
	take_prefix(Prefixes, 0, Indent0),
	white_prefix(Indent0, Indent),
	take_line(L1),
	split_lines(Prefixes, Ls).


%%	end_of_comment// is det.
%
%	Succeeds if we hit the end of the comment.
%	
%	@bug	%*/ will be seen as the end of the comment.

end_of_comment -->
	eos, !.
end_of_comment -->
	ws, stars, "*/", !.

stars --> [].
stars --> "*", !, stars.


%%	take_prefix(+Prefixes:list(codes), +Indent0:int, -Indent:int)// is det.
%
%	Get the leading characters  from  the   input  and  compute  the
%	line-position at the end of the leading characters.

take_prefix(Prefixes, I0, I) -->
	{ member(Prefix, Prefixes) },
	string(Prefix), !,
	{ string_update_linepos(Prefix, I0, I) }.
take_prefix(_, I, I) -->
	[].

white_prefix(I0, I) -->
	[C],
	{  code_type(C, white), !,
	   update_linepos(C, I0, I1)
	},
	white_prefix(I1, I).
white_prefix(I, I) -->
	[].

%%	string_update_linepos(+Codes, +Pos0, -Pos) is det.
%
%	Update line-position after adding Codes at Pos0.

string_update_linepos([], I, I).
string_update_linepos([H|T], I0, I) :-
	update_linepos(H, I0, I1),
	string_update_linepos(T, I1, I).

%%	update_linepos(+Code, +Pos0, -Pos) is det.
%
%	Update line-position after adding Code.
%	
%	@tbd	Currently assumes tab-width of 8.

update_linepos(0'\t, I0, I) :- !,
	I is (I0\/7)+1.
update_linepos(0'\b, I0, I) :- !,
	I is max(0, I0-1).
update_linepos(0'\r, _, 0) :- !.
update_linepos(0'\n, _, 0) :- !.
update_linepos(_, I0, I) :-
	I is I0 + 1.

%%	take_line(-Line:codes)// is det.
%
%	Take  a  line  from  the  input.   Line  does  not  include  the
%	terminating \r or \n character(s), nor trailing whitespace.

take_line([]) -->
	"\r\n", !.			% DOS file
take_line([]) -->
	"\n", !.			% Unix file
take_line(Line) -->
	[H], { code_type(H, white) }, !,
	take_white(White, WT),
	(   nl
	->  { Line = [] }
	;   { Line = [H|White] },
	    take_line(WT)
	).
take_line([H|T]) -->
	[H], !,
	take_line(T).
take_line([]) -->			% end of string
	[].

take_white([H|T0], T) -->
	[H],  { code_type(H, white) }, !,
	take_white(T0, T).
take_white(T, T) -->
	[].



		 /*******************************
		 *	       MISC		*
		 *******************************/

%%	strip_leading_par(+Dom0, -Dom) is det.
%
%	Remove the leading paragraph for  environments where a paragraph
%	is not required.

strip_leading_par([p(C)|T], L) :- !,
	append(C, T, L).
strip_leading_par(L, L).


		 /*******************************
		 *	     DCG BASICS		*
		 *******************************/

%%	eos// is det
%
%	Peek at end of input

eos([], []).

%%	ws// is det
%
%	Eagerly skip layout characters

ws -->
	[C], {code_type(C, space)}, !,
	ws.
ws -->
	[].

%	space// is det
%	
%	True if then next code is layout.

space -->
	[C],
	{code_type(C, space)}.

%%	non_ws(-Text, ?Tail) is det.
%
%	True if the  difference  list  Text-Tail   is  the  sequence  of
%	non-white-space characters.

non_ws([H|T0], T) -->
	[H],
	{ \+ code_type(H, space) }, !,
	non_ws(T0, T).
non_ws(T, T) -->
	[].


%%	nl//
%
%	Get end-of-line

nl -->
	"\r\n", !.
nl -->
	"\n".

%%	peek(H)//
%
%	True if next token is H without eating it.

peek(H, L, L) :-
	L = [H|_].

%%	string(-Tokens:list)// is nondet.
%
%	Defensively take tokens from the input.  Backtracking takes more
%	tokens.

string([]) --> [].
string([H|T]) --> [H], string(T).


