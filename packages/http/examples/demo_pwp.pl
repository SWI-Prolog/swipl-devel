:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_pwp)).

user:file_search_path(pwp, pwp).

:- http_handler(root(.), pwp_handler([path_alias(pwp), view(true)]), [prefix]).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).


