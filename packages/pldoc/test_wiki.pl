:- use_module(pldoc).
:- use_module(wiki).
:- use_module(modes).
:- use_module(html).
:- use_module(http).
:- use_module(library('http/html_write')).

process_comment(File, Pos-String, DOM) :-
	stream_position_data(line_count, Pos, Line),
	FilePos = File:Line,
	is_structured_comment(String, Prefixes),
	indented_lines(String, Prefixes, Lines),
	(   section_comment_header(Lines, Header, Lines1)
	->  DOM = [Header|DOM1],
	    Args = []
	;   process_modes(Lines, FilePos, Modes, Args, Lines1)
	->  DOM = [\pred_dt(Modes), dd(class=defbody, DOM1)]
	),
	wiki_lines_to_dom(Lines1, Args, DOM0),
	strip_leading_par(DOM0, DOM1).

%%	process_comments(+Comments, +File, -DOM) is det.
%
%	@param Mode	Enclosing environment, =body= or =dl=

process_comments(Comments, File, DOM) :-
	maplist(process_comment(File), Comments, DOMList),
	phrase(missing_tags(DOMList, body), DOM).

missing_tags([], _) -->
	[].
missing_tags([H|T0], Outer) -->
	{ requires(H, Tag), Tag \== Outer, !,
	  Env =.. [Tag,C],
	  phrase(in_tag(T0, T, Tag), C)
	},
	[Env],
	missing_tags(T, Outer).
missing_tags([H|T], Outer) -->
	H,
	missing_tags(T, Outer).
	
in_tag([], [], _) --> !,
	[].
in_tag(L, L, Tag) -->
	{ L = [H|_],
	  \+ requires(H,Tag)
	}, !,
	[].
in_tag([H|T0], T, Tag) -->
	H,
	in_tag(T0, T, Tag).


requires([\pred_dt(_)|_], dl).

test :-
	test('wiki_test_data').

test(Spec) :-
	absolute_file_name(Spec, File, [file_type(prolog)]),
	read_structured_comments(File, Comments),
	process_comments(Comments, File, DOM),
	doc_file_name(File, DocFile, [format(html)]),
	open(DocFile, write, Out),
	call_cleanup(doc_write_html(Out, File, DOM),
		     close(Out)).

doc :-
	Port = 4000,
	doc_server(Port),
	format(atom(URL), 'http://localhost:~w/', [Port]),
	www_open_url(URL).
