#!/staff/jan/bin/pl -f none -q -g main -s

:- doc_collect(true).
:- use_module(library('pldoc/doc_library')).
:- use_module(doc_log).

:- doc_load_library.

%%	main
%
%	Start the documentation server and wait. Does not provide access
%	to the toplevel, so it can be run as a background process.

main :-
	start_server,
	wait.

start_server :-
	doc_log_requests('Requests.log'),
	doc_server(4000,
		   [
		   ]).

wait :-
	thread_get_message(_),
	halt.
	
	
