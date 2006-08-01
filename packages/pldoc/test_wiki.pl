:- use_module(pldoc).
:- use_module(wiki).
:- use_module(modes).
:- use_module(html).
:- use_module(library('http/html_write')).

process_wiki(_Pos-String, DOM) :- !,
	process_wiki(String, DOM).
process_wiki(String, DOM) :-
	DOM = [\pred_dt(Modes), dd(class=defbody, DOM1)],
	is_structured_comment(String, Prefixes),
	indented_lines(String, Prefixes, Lines),
	process_modes(Lines, Modes, Args, Lines1),
	wiki_lines_to_dom(Lines1, Args, DOM0),
	strip_leading_par(DOM0, DOM1).

process_wikis([], T, T).
process_wikis([DH|DT], T0, T) :-
	process_wiki(DH, DOM),
	append(DOM, T1, T0),
	process_wikis(DT, T1, T).

test :-
	test('wiki_test_data.pl').

test(File) :-
	read_structured_comments(File, Comments),
	process_wikis(Comments, DOM, []),
	doc_file_name(File, DocFile, [format(html)]),
	open(DocFile, write, Out),
	call_cleanup(doc_write_html(Out, File, dl(DOM)),
		     close(Out)).
