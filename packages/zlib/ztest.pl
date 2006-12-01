:- module(ztest,
	  [
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(user:library(zlib)).
:- use_module(user:library(plunit)).
:- use_module(user:library(readutil)).

read_file_to_codes(File, Codes) :-
	open(File, read, In),
	call_cleanup(read_stream_to_codes(In, Codes), close(In)).

:- begin_tests(zlib).

test(gunzip,
     [ setup(shell('gzip < ztest.pl > plunit-tmp.gz')),
       cleanup(delete_file('plunit-tmp.gz'))
     ]) :-
	gzopen('plunit-tmp.gz', read, ZIn),
	call_cleanup(read_stream_to_codes(ZIn, Codes0), close(ZIn)),
	read_file_to_codes('ztest.pl', Codes1),
	Codes0 == Codes1.
	
test(gzip,
     [ cleanup(delete_file('plunit-tmp.gz'))
     ]) :-
	read_file_to_codes('ztest.pl', Codes),
	gzopen('plunit-tmp.gz', write, ZOut),
	format(ZOut, '~s', [Codes]),
	close(ZOut),
	read_file_to_codes(pipe('gunzip < plunit-tmp.gz'), Codes1),
	Codes == Codes1.

:- end_tests(zlib).
