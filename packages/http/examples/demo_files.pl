:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dirindex)).

:- http_handler(root(.), serve_files, [prefix]).

%%	server(+Port, +DirSpec) is det.
%
%	Start  the  server  at  port  Port,  serving  directories  below
%	DirSpec. DirSpec may contain ~ and $var.
%
%	This simple example defines  a   complete  web-server for static
%	pages. Note that more specific  handlers   than  the  bave (i.e.
%	using a longer path) have  priority   over  this handler and can
%	thus be used to add dynamic parts to your server.

server(Port, DirSpec) :-
	expand_file_name(DirSpec, [Dir]),
	assert(user:file_search_path(document_root, Dir)),
	http_server(http_dispatch, [port(Port)]).

%%	serve_files(Request)
%
%	Server a file or directory  according   to  the path_info field,
%	which contains the path *after* the http-location matched by the
%	handler. If the  handler  is   matched  *exactly*,  path_info is
%	missing.
%
%	http_safe_file/1 checks the path for attempts to escape from the
%	hierarchy defined by the =document_root= alias. We find the path
%	before calling one of the two reply functions because we want to
%	know whether we are dealing with a   directory  or a file. After
%	that, the path is absolute  and   we  must  pass unsafe(true) to
%	avoid the path-checker in the reply-functions complaining.

serve_files(Request) :-
	(   memberchk(path_info(PathInfo), Request)
	->  true
	;   PathInfo = './'
	),
	http_safe_file(document_root(PathInfo), []),
	absolute_file_name(document_root(PathInfo), Path,
			   [ access(read)] ),
	(   exists_directory(Path)
	->  http_reply_dirindex(Path, [unsafe(true)], Request)
	;   http_reply_file(Path, [unsafe(true)], Request)
	).


