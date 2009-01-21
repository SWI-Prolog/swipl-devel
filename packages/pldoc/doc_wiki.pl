/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006-2009, University of Amsterdam

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
:- use_module(library(option)).


		 /*******************************
		 *	    WIKI PARSING	*
		 *******************************/

%%	wiki_lines_to_dom(+Lines:lines, +Args:list(atom), -Term) is det
%
%	Translate a Wiki text into  an   HTML  term suitable for html//1
%	from the html_write library.

wiki_lines_to_dom(Lines, Args, HTML) :-
	tokenize_lines(Lines, Tokens0),
	normalise_indentation(Tokens0, Tokens),
	wiki_structure(Tokens, -1, Pars),
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


%%	wiki_structure(+Lines:lines, +BaseIndent,
%%		       -Blocks:list(block)) is det
%
%	Get the structure in terms  of block-level elements: paragraphs,
%	lists and tables. This processing uses   a mixture of layout and
%	punctuation.

wiki_structure([], _, []) :- !.
wiki_structure([_-[]|T], BI, Pars) :- !,	% empty lines
	wiki_structure(T, BI, Pars).
wiki_structure(Lines, _, [\tags(Tags)]) :-
	tags(Lines, Tags), !.
wiki_structure(Lines, BI, [P1|PL]) :-
	take_block(Lines, BI, P1, RestLines),
	wiki_structure(RestLines, BI, PL).
	
%%	take_block(+Lines, +BaseIndent, ?Block, -RestLines) is semidet.
%
%	Take a block-structure from the input.  Defined block elements
%	are lists, table, hrule, section header and paragraph.

take_block([_-[]|Lines], BaseIndent, Block, Rest) :- !,
	take_block(Lines, BaseIndent, Block, Rest).
take_block([N-_|_], BaseIndent, _, _) :-
	N < BaseIndent, !,
	fail.				% less indented
take_block(Lines, BaseIndent, List, Rest) :-
	list_item(Lines, Type, Indent, LI, LIT, Rest0), !,
	Indent > BaseIndent,
	rest_list(Rest0, Type, Indent, LIT, [], Rest),
	List0 =.. [Type, LI],
	(   ul_to_dl(List0, List)
	->  true
	;   List0 = dl(Items)
	->  List = dl(class=wiki, Items)
	;   List = List0
	).
take_block([N-['|'|RL1]|LT], _, Table, Rest) :-
	phrase(row(R0), RL1),
	rest_table(LT, N, RL, Rest), !,
	Table = table(class=wiki, [tr(R0)|RL]).
take_block([0-[-,-|More]|LT], _, Block, LT) :-	% seperation line
	maplist(=(-), More), !,
	Block = hr([]).
take_block([_-[@|_]], _, _, _) :- !,		% starts @tags section
	fail.
take_block([_-L1|LT], _, Section, LT) :-
	section_line(L1, Section), !.
take_block([_-Verb|Lines], _, Verb, Lines) :-
	verbatim_term(Verb), !.
take_block([I-L1|LT], BaseIndent, Elem, Rest) :- !,
	append(L1, PT, Par),
	rest_par(LT, PT, I, BaseIndent, MaxI, Rest),
	(   MaxI >= BaseIndent+16
	->  Elem = center(Par)
	;   MaxI >= BaseIndent+4
	->  Elem = blockquote(Par)
	;   Elem = p(Par)
	).
take_block([Verb|Lines], _, Verb, Lines).


%%	list_item(+Lines, ?Type, ?Indent, -LI0, -LIT, -RestLines) is det.
%
%	Create a list-item. Naturally this should produce a single item,
%	but DL lists produce two items, so   we create the list of items
%	as a difference list.
%	
%	@tbd	Pass base-indent

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

%%	rest_list_item(+Lines, +Type, +Indent, -RestItem, -RestLines) is det
%
%	Extract the remainder (after the first line) of a list item.

rest_list_item(Lines, _Type, Indent, RestItem, RestLines) :-
	take_blocks_at_indent(Lines, Indent, Blocks, RestLines),
	(   Blocks = [p(Par)|MoreBlocks]
	->  append(['\n'|Par], MoreBlocks, RestItem)
	;   RestItem = Blocks
	).

%%	take_blocks_at_indent(+Lines, +Indent, -Pars, -RestLines) is det.
%
%	Process paragraphs and verbatim blocks (==..==) in bullet-lists.

take_blocks_at_indent(Lines, N, [Block|RestBlocks], RestLines) :-
	take_block(Lines, N, Block, Rest0), !,
	take_blocks_at_indent(Rest0, N, RestBlocks, RestLines).
take_blocks_at_indent(Lines, _, [], Lines).


%%	rest_list(+Lines, +Type, +Indent,
%%		  -Items, -ItemTail, -RestLines) is det.

rest_list(Lines, Type, N, Items, IT, Rest) :-
	skip_empty_lines(Lines, Lines1),
	list_item(Lines1, Type, N, Items, IT0, Rest0), !,
	rest_list(Rest0, Type, N, IT0, IT, Rest).
rest_list(Rest, _, _, IT, IT, Rest).

%%	list_item_prefix(?Type, +Line, -Rest) is det.

list_item_prefix(ul, [*, ' '|T], T) :- !.
list_item_prefix(ul, [-, ' '|T], T) :- !.
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
			    [\include(PI, predicate, [])|T]) :-
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

%%	rest_par(+Lines, -Par,
%%		 +BaseIndent, +MaxI0, -MaxI, -RestLines) is det.
%
%	Take the rest of a paragraph. Paragraphs   are  ended by a blank
%	line or the start of a list-item.   The latter is a bit dubious.
%	Why not a  general  block-level   object?  The  current defition
%	allows for writing lists without a blank line between the items.

rest_par([], [], _, MaxI, MaxI, []).
rest_par([_-[]|Rest], [], _, MaxI, MaxI, Rest) :- !.
rest_par([I-L|Rest], [], I, MaxI, MaxI, [I-L|Rest]) :-
	list_item_prefix(_, L, _), !.
rest_par([I-L1|LT], ['\n'|Par], BI, MaxI0, MaxI, Rest) :-
	append(L1, PT, Par),
	MaxI1 is max(I, MaxI0),
	rest_par(LT, PT, BI, MaxI1, MaxI, Rest).


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

%%	collect_tags(+IndentedLines, -Tags) is semidet
%
%	Create a list Order-tag(Tag,Tokens) for   each @tag encountered.
%	Order is the desired position as defined by tag_order/2.
%	
%	@tbd Tag content is  often  poorly   aligned.  We  now  find the
%	alignment of subsequent lines  and  assume   the  first  line is
%	alligned with the remaining lines.

collect_tags([], []).
collect_tags([Indent-[@,String|L0]|Lines], [Order-tag(Tag,Value)|Tags]) :-
	tag_name(String, Tag, Order), !,
	strip_leading_ws(L0, L),
	rest_tag(Lines, Indent, VT, RestLines),
	normalise_indentation(VT, VT1),
	wiki_structure([0-L|VT1], -1, Value0),
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
structure_term(\include(What,Type,Opts), include(What,Type,Opts), []) :- !.
structure_term(dl(Att, Args), dl(Att), [Args]) :- !.
structure_term(dt(Att, Args), dt(Att), [Args]) :- !.
structure_term(table(Att, Args), table(Att), [Args]) :- !.
structure_term(h1(Att, Args), h1(Att), [Args]) :- !.
structure_term(h2(Att, Args), h2(Att), [Args]) :- !.
structure_term(h3(Att, Args), h3(Att), [Args]) :- !.
structure_term(h4(Att, Args), h4(Att), [Args]) :- !.
structure_term(hr(Att), hr(Att), []) :- !.
structure_term(p(Args), p, [Args]) :- !.
structure_term(Term, Functor, Args) :-
	functor(Term, Functor, 1),
	structure_tag(Functor), !,
	Term =.. [Functor|Args].

structure_tag(ul).
structure_tag(ol).
structure_tag(dl).
structure_tag(li).
structure_tag(dt).
structure_tag(dd).
structure_tag(table).
structure_tag(tr).
structure_tag(td).
structure_tag(blockquote).
structure_tag(center).

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
	prolog_symbol_char(S0),
	symbol_string(SRest), [ '/' ], arity(Arity), !,
	{ atom_chars(Name, [S0|SRest])
	}.
wiki_face(\predref(Name//Arity), _) -->
	[ NameS, '/', '/' ], arity(Arity),
	{ functor_name(NameS), !,
	  string_to_atom(NameS, Name)
	}.
wiki_face(\include(Name, Type, Options), _) -->
	['[','['], file_name(Base, Ext), [']',']'],
	{ autolink_extension(Ext, Type), !,
	  file_name_extension(Base, Ext, Name),
	  resolve_file(Name, Options, [])
	}, !.
wiki_face(Link, _ArgNames) -->		% [[Label][Link]]
	['[','['],
	string(LabelParts),
	[']','['],
	wiki_link(Link, [label(Label), relative(true), end(']')]),
	[']',']'], !,
	{ make_label(LabelParts, Label) }.
wiki_face(Link, _ArgNames) -->
	wiki_link(Link, []), !.
wiki_face(FT, ArgNames) -->
	[T],
	{   atomic(T)
	->  FT = T
	;   wiki_faces(T, ArgNames, FT)
	}.

%%	make_label(+Parts, -Label) is det.
%
%	Translate the [[Parts][...] into a label

make_label(Parts, Label) :-
	phrase(image_label(Label), Parts), !.
make_label(Parts, Label) :-
	concat_atom(Parts, Label).

image_label(\include(Name, image, Options)) -->
	file_name(Base, Ext),
	{ autolink_extension(Ext, image),
	  file_name_extension(Base, Ext, Name),
	  resolve_file(Name, Options, RestOptions)
	},
	file_options(RestOptions).


%%	file_options(-Options) is det.
%
%	Extracts additional processing options for  files. The format is
%	;name="value",name2=value2,... Spaces are not allowed.

file_options(Options) -->
	[;], nv_pairs(Options), !.
file_options([]) -->
	[].

nv_pairs([H|T]) -->
	nv_pair(H),
	(   [',']
	->  nv_pairs(T)
	;   {T=[]}
	).

nv_pair(Option) -->
	word_token(NameS), [=,'"'], string(ValueS), ['"'], !,
	{ concat_atom([NameS], Name),
	  concat_atom(ValueS, Value0),
	  catch(atom_number(Value0, Value), _, Value=Value0),
	  Option =.. [Name,Value]
	}.


%%	wiki_link(-Link, +Options)// is semidet.
%
%	True if we can find a link to a file or URL. Links are described
%	as one of:
%	
%	    $ filename :
%	    A filename defined using autolink_file/2 or
%	    autolink_extension/2
%	    $ <url-protocol>://<rest-url> :
%	    A fully qualified URL
%	    $ '<' URL '>' :
%	    Be more relaxed on the URL specification.

wiki_link(\file(Name, FileOptions), Options) -->
	file_name(Base, Ext),
	{ file_name_extension(Base, Ext, Name),
	  (   autolink_file(Name, _)
	  ;   autolink_extension(Ext, _)
	  ), !,
	  resolve_file(Name, FileOptions, Options)
	}.
wiki_link(\file(Name, FileOptions), Options) -->
	word_token(NameS),
	{ autolink_file(Name, _),
	  sub_atom(NameS, 0, _, 0, Name), !,
	  resolve_file(Name, FileOptions, Options)
	}, !.
wiki_link(a(href(Ref), Label), Options) -->
	word_token(ProtS), [:,/,/], { url_protocol(ProtS) },
	{ option(end(End), Options, space)
	},
	string_no_whitespace(Rest), peek_end_url(End), !,
	{ concat_atom([ProtS, :,/,/ | Rest], Ref),
	  option(label(Label), Options, Ref)
	}.
wiki_link(a(href(Ref), Label), Options) -->
	[<], word_token(AliasS), [:],
	{ concat_atom([AliasS], Alias),
	  user:url_path(Alias, _)
	},
	string_no_whitespace(Rest), [>],
	{ concat_atom(Rest, Local),
	  (   Local == ''
	  ->  Term =.. [Alias,'.']
	  ;   Term =.. [Alias,Local]
	  ),
	  expand_url_path(Term, Ref),
	  option(label(Label), Options, Ref)
	}.
wiki_link(a(href(Ref), Label), Options) -->
	[<], 
	(   { option(relative(true), Options),
	      Parts = Rest
	    }
	->  string_no_whitespace(Rest)
	;   { Parts = [ProtS, : | Rest]
	    },
	    word_token(ProtS), [:], string_no_whitespace(Rest)
	),
	[>], !,
	{ concat_atom(Parts, Ref),
	  option(label(Label), Options, Ref)
	}.


%%	filename(-Name:atom, -Ext:atom)// is semidet.
%
%	Matches a filename.  A filename is defined as a	sequence
%	<segment>{/<segment}.<ext>.

file_name(FileBase, Extension) -->
	segment(S1),
	segments(List),
	['.'], file_extension(Extension), !,
	{ concat_atom([S1|List], '/', FileBase) }.

segment(..) -->
	['.','.'], !.
segment(Word) -->
	word_token(Word).

segments([H|T]) -->
	['/'], !,
	segment(H),
	segments(T).
segments([]) -->
	[].

file_extension(Ext) -->
	word_token(String),
	{ concat_atom([String], Ext),
	  autolink_extension(Ext, _)
	}.


%%	resolve_file(+Name, -Options, ?RestOptions) is det.
%
%	Find the actual file based on the pldoc_file global variable. If
%	present  and  the   file   is    resolvable,   add   an   option
%	absolute_path(Path) that reflects the current   location  of the
%	file.

resolve_file(Name, Options, Rest) :-
	nb_current(pldoc_file, RelativeTo),
	RelativeTo \== [],
	absolute_file_name(Name, Path,
			   [ relative_to(RelativeTo),
			     access(read),
			     file_errors(fail)
			   ]), !,
	Options = [ absolute_path(Path) | Rest ].
resolve_file(_, Options, Options).


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
%	Accept a sequence of Prolog symbol characters, starting with the
%	shortest (empty) match.

symbol_string([]) -->
	[].
symbol_string([H|T]) -->
	[H],
	{ prolog_symbol_char(H) },
	symbol_string(T).

prolog_symbol_char(C) -->
	[C],
	{ prolog_symbol_char(C) }.

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


peek_end_url(space) -->
	peek(End),
	{ space_atom(End) }, !.
peek_end_url(space) -->
	eos, !.
peek_end_url(Token) -->
	peek(Token), !.

space_atom(' ').
space_atom('\r').
space_atom('\n').

%%	autolink_extension(?Ext, ?Type) is nondet.
%
%	True if Ext is a filename extensions that create automatic links
%	in the documentation.

autolink_extension(Ext, prolog) :-
	prolog_file_type(Ext,prolog).
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

verbatim([Indent-"=="|Lines], Indent-pre(class(code),Pre), RestLines) :-
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
sentence([0' |T], T) -->		% '
	eos.

white -->
	space.
white -->
	eos.

%%	skip_empty_lines(+LinesIn, -LinesOut) is det.
%
%	Remove empty lines from the start of the input.  Note that
%	this is used both to process character and token data.

skip_empty_lines([], []).
skip_empty_lines([_-[]|Lines0], Lines) :- !,
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

%%	normalise_indentation(+LinesIn, -LinesOut) is det.
%
%	Re-normalise the indentation, such that the  lef-most line is at
%	zero.  Note that we skip empty lines in the computation.

normalise_indentation(Lines0, Lines) :-
	skip_empty_lines(Lines0, Lines1),
	Lines1 = [I0-_|Lines2], !,
	smallest_indentation(Lines2, I0, Subtract),
	(   Subtract == 0
	->  Lines = Lines0
	;   maplist(substract_indent(Subtract), Lines0, Lines)
	).
normalise_indentation(Lines, Lines).

smallest_indentation([], I, I).
smallest_indentation([_-[]|T], I0, I) :- !,
	smallest_indentation(T, I0, I).
smallest_indentation([X-_|T], I0, I) :-
	I1 is min(I0, X),
	smallest_indentation(T, I1, I).

substract_indent(Subtract, I0-L, I-L) :-
	I is max(0,I0-Subtract).


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

%%	string_no_whitespace(-Tokens:list)// is nondet.
%
%	Defensively take tokens from the input.  Backtracking takes more
%	tokens.  Tokens cannot include whitespace.

string_no_whitespace([]) -->
	[].
string_no_whitespace([H|T]) --> [H],
	{ \+ space_atom(H) },
	string_no_whitespace(T).


