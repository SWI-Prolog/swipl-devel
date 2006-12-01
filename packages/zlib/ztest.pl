:- module(ztest,
	  [
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(user:library(zlib)).
:- use_module(user:library(plunit)).
:- use_module(user:library(readutil)).

:- begin_tests(zlib).

test(gunzip,
     [ setup(shell('gzip < ztest.pl > plunit-tmp.gz'))%,
       %cleanup(delete_file('plunit-tmp.gz'))
     ]) :-
	gzopen('plunit-tmp.gz', read, ZIn),
	call_cleanup(read_stream_to_codes(ZIn, Codes0), close(ZIn)),
	open('ztest.pl', read, In),
	call_cleanup(read_stream_to_codes(In, Codes1), close(In)),
	Codes0 == Codes1.
	
:- end_tests(zlib).
