:- module(test_http,
	  [ test_http/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_stream)).
:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(library(lists)).

test_http :-
	run_tests([ http_open,
		    http_get
		  ]).


:- begin_tests(http_open).

test(gollem, true) :-
     http_open('http://gollem.science.uva.nl/', In, []),
     read_stream_to_codes(In, Codes),
     close(In),
     appendchk(_, "http://www.swi-prolog.org/", _, Codes).
test(gollem_redirect, true) :-
     http_open('http://gollem.science.uva.nl', In, []),
     read_stream_to_codes(In, Codes),
     close(In),
     appendchk(_, "http://www.swi-prolog.org/", _, Codes).
test(gollem_chunked, true(Codes == Ref)) :-
     http_open('http://gollem.science.uva.nl/cgi-bin/chunked', In, []),
     read_stream_to_codes(In, Codes),
     close(In),
     chunked_data(Ref).

:- end_tests(http_open).

:- begin_tests(http_get).

test(gollem, true) :-
     http_get('http://gollem.science.uva.nl/', Data, [to(codes)]),
     appendchk(_, "http://www.swi-prolog.org/", _, Data).

test(gollem_chunked, true(Data == Ref)) :-
     http_get('http://gollem.science.uva.nl/cgi-bin/chunked',
	      Data, [to(codes)]),
     chunked_data(Ref).

:- end_tests(http_get).

		 /*******************************
		 *	       UTIL		*
		 *******************************/

read_file_to_codes(File, Codes) :-
	open(File, read, In),
	call_cleanup(read_stream_to_codes(In, Codes), close(In)).

appendchk(Pre, Middle, Post, List) :-
	append(Pre, Rest, List),
	append(Middle, Post, Rest), !.

%%	chunked_data(-String) is det.
%
%	Content of the chunked data that is sent by cgi-bin/chunked.

chunked_data(S) :-
	findall(C,
		(   between(1, 1000, X),
		    C is "a" + X mod 26
		), S).


