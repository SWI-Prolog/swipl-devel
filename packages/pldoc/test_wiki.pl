:- use_module(pldoc).
:- use_module(wiki).
:- use_module(modes).
:- use_module(library('http/html_write')).

process_wiki(String, DOM) :-
	indented_lines(String, ["%"], Lines),
	process_modes(Lines, _Modes, Args, Lines1),
	wiki_lines_to_dom(Lines1, Args, DOM0),
	strip_leading_par(DOM0, DOM).

test :-
	test('wiki_test_data.pl').

test(File) :-
	open(File, read, In),
	read_term(In, _Term,
		  [ comments(Comments)
		  ]),
	close(In),
	forall(member(P-C, Comments),
	       test_wiki(P, C)).

test_wiki(P, C) :-
	sub_string(C, 0, _, _, '%%'), !,
	stream_position_data(line_count, P, LNo),
	process_wiki(C, Doc),
	format('~`#t From line ~D ~`#t ~72|~n', [LNo]),
	(   true
	->  write_html(Doc)
	;   pp(Doc)
	), nl, nl.
test_wiki(_, _).

write_html(Doc) :-
	phrase(html(Doc), Tokens),
	print_html(Tokens).

tags(Tags) -->
	html(dl(Tags)).

tag(Name, Value) -->
	html([dt(Name), dd(Value)]).
