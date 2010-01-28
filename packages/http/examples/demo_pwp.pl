:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_pwp)).

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(http_demo, Dir)).

user:file_search_path(pwp_demo, http_demo(pwp)).

:- http_handler(root(.),
		pwp_handler([path_alias(pwp_demo), view(true)]),
		[prefix]).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).


