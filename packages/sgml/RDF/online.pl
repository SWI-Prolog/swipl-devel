/*  $Id$

    Part of SWI-Prolog RDF parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/


:- use_module(library(cgi)).
:- use_module(library(sgml)).
:- use_module(rdf).
:- use_module(rdf_parser).
:- use_module(rewrite).
:- use_module(pretty_print).

term_expansion(F, T) :- rew_term_expansion(F, T).
goal_expansion(F, T) :- rew_goal_expansion(F, T).

:- dynamic new_rdf_namespace/1.

parse(Text, RDFTerm, Triples) :-
	parse_atom(Text, Term),
	(   find_rdf(Term, RDFTerm)
	->  true
	;   RDFTerm = Term
	),
	xml_to_rdf(RDFTerm, [], Triples).

find_rdf(Term, RDFTerm) :-
	RDFTerm = element(NS:'RDF', _, _),
	term_member(RDFTerm, Term), !,
	(   rdf_name_space(NS)
	->  true
	;   assert(rdf_parser:rdf_name_space(NS)),
	    assert(new_rdf_namespace(NS))
	).

term_member(X, X).
term_member(X, Compound) :-
	compound(Compound),
	arg(_, Compound, Arg),
	term_member(X, Arg).

%	parse_atom(+Atom, -Term, +Options
%
%	Parse and atom into a structured term

parse_atom(Atom, Term) :-
	new_sgml_parser(Parser, []),
	set_sgml_parser(Parser, dialect(xmlns)),
	sgml_parse(Parser,
		   [ document(Term),
		     goal(provide_atom(Atom))
		   ]),
	free_sgml_parser(Parser).

provide_atom(Atom, ParserStream) :-
	write(ParserStream, Atom).


		 /*******************************
		 *	 HTML GENERATION	*
		 *******************************/

:- op(100, fx, #).
:- op(110, xfx, ::).

emit([]) :- !.
emit([H|T]) :- !,
	emit(H),
	emit(T).
emit(Fmt-Args) :- !,
	format(Fmt, Args),
	retractall(nl_done(_)).
emit(#Term) :- !,
	#Term.
emit(#Term::Content) :- !,
	#Term::Content.
emit(Atom) :-
	write(Atom),
	retractall(nl_done(_)).

#Term::Content :-
	Term =.. [Name|Attributes],
	layout(before(open, Name)),
	format('<~w', [Name]),
	attlist(Attributes),
	format('>', []),
	retractall(nl_done(_)),
	layout(after(open, Name)),
	emit(Content),
	end_tag(Name).
#pre(Text) :- !,
	sgml_quote(Text, Quoted),
	#pre::Quoted.
#box(Text) :- !,
	box(Text, '#e0e0e0').
#box(Text, Colour) :- !,
	box(Text, Colour).
#Term :-
	Term =.. [Name|Attributes],
	layout(before(open, Name)),
	format('<~w', [Name]),
	attlist(Attributes),
	format('>', []),
	retractall(nl_done(_)),
	layout(after(open, Name)),
	end_tag(Name).

end_tag(Name) :-
	blines(Name, _, o), !.
end_tag(Name) :-
	layout(before(close, Name)),
	format('</~w>', [Name]),
	retractall(nl_done(_)),
	layout(after(close, Name)).


layout(before(open, Name)) :-
	blines(Name, N-_, _), !,
	nls(N).
layout(after(open, Name)) :-
	blines(Name, _-N, _), !,
	nls(N).
layout(before(close, Name)) :-
	blines(Name, _, N-_), !,
	nls(N).
layout(after(close, Name)) :-
	blines(Name, _, _-N), !,
	nls(N).
layout(_) :-
	retractall(nl_done(_)).

:- dynamic
	nl_done/1.

nls(N) :-
	(   nl_done(Done)
	->  true
	;   Done = 0
	),
	ToDo is N - Done,
	New is max(N, Done),
	retractall(nl_done(Done)),
	assert(nl_done(New)),
	do_nl(ToDo).

do_nl(N) :-
	N > 0, !,
	nl,
	NN is N - 1,
	do_nl(NN).
do_nl(_).

blines(tr,    1-0, 0-0).
blines(table, 2-1, 1-1).
blines(form,  2-1, 1-1).
blines(h1,    2-0, 0-1).
blines(h2,    2-0, 0-2).
blines(h3,    2-0, 0-2).
blines(h4,    2-0, 0-2).
blines(p,     2-1, o).			% omitted end-tag

attlist([]).
attlist([Name=Value|T]) :- !,
	sgml_quote_value(Value, Quoted),
	format(' ~w=~w', [Name, Quoted]),
	attlist(T).
attlist([Name|T]) :-
	format(' ~w', [Name]),
	attlist(T).

head(Title) :-
	emit([ 'Content-type: text/html\n\n',
	       '<html>\n',
	       '<head>\n',
	       '<title>~w</title>~n'-[Title],
	       '</head>\n\n',
	       '<body bgcolor="white">\n'
	     ]).
foot :-
	emit([ '</body>\n',
	       '</html>\n'
	     ]).
	     

pre(Text) :-
	sgml_quote(Text, Quoted),
	#pre::Quoted.

box(Text, Colour) :-
	emit('<p>\n'),
	#table(width='80%', align=center, border=6, bgcolor=Colour)::
	  [#tr::[#td(nowrap)::[#pre(Text)]]].


		 /*******************************
		 *	      QUOTING		*
		 *******************************/

sgml_quote_value(Value, Arg) :-
	atom_chars(Value, Chars),
	(   name_chars(Chars)
	->  Arg = Value
	;   sgml_quote_chars(Chars, Quoted),
	    atom_chars(Arg, Quoted)
	).

name_chars([H|T]) :-
	char_type(H, alpha),
	all_alnum(T).

all_alnum([]).
all_alnum([H|T]) :-
	char_type(H, csymf),
	all_alnum(T).
	    
sgml_quote_chars(L, ['"'|T]) :-
	sgml_quote2(L, T, ['"']).

sgml_quote2([], T, T).
sgml_quote2([H|T0], List, Rest) :-
	sgml_quote_char(H, List, T), !,
	sgml_quote2(T0, T, Rest).
sgml_quote2([H|T0], [H|T], Rest) :-
	sgml_quote2(T0, T, Rest).

sgml_quote_char('<', [&, l, t, ;|T], T).	
sgml_quote_char('>', [&, g, t, ;|T], T).	
sgml_quote_char('&', [&, a, m, p, ;|T], T).	
sgml_quote_char('"', [&, q, u, o, t, ;|T], T).	
%sgml_quote_char('\'', [&, a, p, o, s, ;|T], T).	

sgml_quote(Text, Quoted) :-
	atom_chars(Text, Chars),
	sgml_quote2(Chars, QuotedChars, []),
	atom_chars(Quoted, QuotedChars).


		 /*******************************
		 *	  PAGE GENERATION	*
		 *******************************/

parsed(Time, Triples) :-
	length(Triples, Len),
	#h2::'RDF statement parsed successfully',
	#p::[ 'Your RDF statement has been parsed in ~2f seconds, '-[Time],
	      'creating ', #b::Len, ' triples. ',
	      'Please find the created triples in the table below.'
	    ],
	(   getenv('HTTP_REFERER', Referer)
	->  #p::[ 'If you want to try another RDF statement, please go ',
		  'back to ', #a(href=Referer)::'the request form', '.'
		]
	;   true
	).

rdf_table(Triples) :-
	maplist(triple_row, Triples, TripleRows),
	#p,
	#table(caption='RDF triples',
	       align=center, border=2, cellpadding=3)::
	  [ #tr::[#th::'Subject', #th::'Predicate', #th::'Object']
	  | TripleRows
	  ].

triple_row(rdf(Subj, Pred, Obj), #tr::[#td::S,#td::P,#td::O]) :-
	cell(Subj, S),
	cell(Pred, P),
	cell(Obj, O).

cell(rdf:Local,  [#em::rdf, :, #b::Local]) :- !.
cell(literal(X), [#b::'literal(', X, #b::')']) :- !.
cell(each(X),    [#b::'each(', X, #b::')']) :- !.
cell(pefix(X),   [#b::'prefix(', X, #b::')']) :- !.
cell(NS:Local,   [NS, :, #b::Local]) :- !.
cell(V,		 [T]) :-
	sformat(T, '~p', [V]).

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

show_errors :-
	getenv('ERROR_FILE', File),
	size_file(File, Size),
	Size > 0, !,
	read_file(File, Data),
	#h4::[#font(color=red)::
	      'The following errors occurred while processing your request'],
	#p,
	#box(Data, '#ff8c00').
show_errors.

show_new_namepace :-
	new_rdf_namespace(NS), !,
	#h4::[#font(color=red)::'Warning: unofficial RDF Namespace'],
	#p::['It appears your RDF description uses the unofficial ',
	     'name space ', #b::NS, '. ',
	     'This name space has been added for RDF.'
	    ].
show_new_namepace.


		 /*******************************
		 *	       COMMENT		*
		 *******************************/

comment(TextId) :-
	#h4::'<hr>Comment',
	#p::[ 'If you do not agree with the output or have other comments, ',
	      'Please write them in the text-area below and submit them'
	    ],
	getenv('REQUEST_URI', Script),
	#form(method=post, action=Script)::
	  [ #input(type=hidden, name=id, value=TextId),
	    #table(align=center)::
	      [ #tr::[#td::[#textarea(name=comment, cols=64, rows=10)]],
		#tr::[#td(align=right)::['E-mail: ', #input(name=mail)]],
		#tr::[#td(align=right)::[#input(type=submit)]]
	      ]
	  ].


		 /*******************************
		 *	       REQUEST		*
		 *******************************/

request_location('Online-requests').

%	Save the request and return a local identifier for it.

save_request(Text, Id) :-
	request_dir(Dir, Date),
	concat_atom([Dir, /, Date], DateDir),
	ensure_dir(DateDir),
	between(1, 10000, N),
	    concat_atom([DateDir, /, N, '.rdf'], File),
	    \+ exists_file(File), !,
	    open(File, write, Fd),
	    format(Fd, '~w~n', [Text]),
	    close(Fd),
	    concat_atom([Date, /, N], Id).
	    
request_dir(BaseDir, Date) :-
	get_time(Time),
	convert_time(Time, Y, M, D, _, _, _, _),
	request_location(BaseDir),
	concat_atom([D, -, M, -, Y], Date).

ensure_dir(Dir) :-
	exists_directory(Dir), !.
ensure_dir(Dir) :-
	make_directory(Dir).

save_comment(Id, Mail, Comment) :-
	request_location(Base),
	concat_atom([Base, '/', Id], FileBase),
	absolute_file_name(FileBase, AbsFileBase),
	absolute_file_name(Base, AbsBase),
	sub_atom(AbsFileBase, 0, _, _, AbsBase), 	% verify in tree
	atom_concat(AbsFileBase, '.cmt', CmtFile),
	open(CmtFile, write, Fd),
	format(Fd, 'E-mail: ~w~n~n~w~n', [Mail, Comment]),
	close(Fd).


		 /*******************************
		 *	       ENTRY		*
		 *******************************/

main :-
	cgi_get_form(Arguments),
	(   (   memberchk(attachment(Text), Arguments),
		Text \== ''
	    ;   memberchk(rdf(Text), Arguments)
	    )
	->  save_request(Text, TextId),
	    (	OldTime is cputime,
		parse(Text, _Prolog, Triples),
		Time is cputime - OldTime
	    ->  head('RDF Triples'),
		parsed(Time, Triples),
		show_errors,
		show_new_namepace,
		rdf_table(Triples),
		comment(TextId),
		foot
	    ;	head('Failed to parse'),
		#p::[ 'I failed to parse your request' ],
		show_errors,
		comment(TextId),
		foot
	    ),
	    halt
	;   memberchk(comment(Comment), Arguments),
	    memberchk(id(Id), Arguments),
	    memberchk(mail(Mail), Arguments)
	->  save_comment(Id, Mail, Comment),
	    head('Thanks for comment'),
	    #p::'Thank you for your comments',
	    foot,
	    halt
	).
main :-
	head('Failed'),
	#p::[ 'This CGI-script failed to understand your request' ],
	foot,
	halt.

go :-
	catch(main, E, error(E)).

error(E) :-
	message_to_string(E, Msg),
	head('Failed to parse'),
	show_errors,
	#p::[ 'An exception was raised while parsing your request:' ],
	#pre(Msg),
	foot,
	halt.


		 /*******************************
		 *	       TEST		*
		 *******************************/

test :-
	read_file('suite/t1.rdf', Text),
	catch(parse(Text, _Prolog, Triples), E, error(E)),
	head('RDF Triples'),
	rdf_table(Triples),
	foot.


		 /*******************************
		 *	       UTIL		*
		 *******************************/

read_file(File, Atom) :-
	open(File, read, Fd),
	get_code(Fd, C),
	read_stream(C, Fd, Chars),
	close(Fd),
	atom_codes(Atom, Chars).

read_stream(-1, _, []) :- !.
read_stream(C0, Fd, [C0|T]) :-
	get_code(Fd, C),
	read_stream(C, Fd, T).
