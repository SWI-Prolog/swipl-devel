% Provide an HTTP server to make   the  various components available. To
% use it, build the wasm version  in   e.g.,  `build.wasm` and from this
% directory, run
%
%     swipl ../src/wasm/server.pl
%
:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(main)).
:- use_module(library(option)).

user:file_search_path(web, '../src/wasm').
user:file_search_path(web, 'src').
user:file_search_path(scasp,  Dir) :-
    getenv('SCASP_HOME', Dir).

:- http_handler('/', http_redirect(see_other, '/wasm/'), []).
:- http_handler('/wasm/shell',
                http_reply_file(
                    web('shell.html'),
                    [ headers([ 'Cross-Origin-Opener-Policy'('same-origin'),
                                'Cross-Origin-Embedder-Policy'('require-corp')
                              ])]),
                []).
:- http_handler('/wasm/test',
                http_reply_file(
                    web('test.html'),
                    [ headers([ 'Cross-Origin-Opener-Policy'('same-origin'),
                                'Cross-Origin-Embedder-Policy'('require-corp')
                              ])]),
                []).
:- http_handler('/wasm/cbg',
                http_reply_file(
                    web('cbg.html'),
                    [ headers([ 'Cross-Origin-Opener-Policy'('same-origin'),
                                'Cross-Origin-Embedder-Policy'('require-corp')
                              ])]),
                []).
:- http_handler('/wasm/',
                http_reply_from_files(web(.), []), [prefix]).


:- if(absolute_file_name(scasp(.), _, [file_type(directory), file_errors(fail)])).
:- http_handler('/wasm/scasp/', http_reply_from_files(scasp(.), []), [prefix]).
:- endif.


:- initialization(server_loop, main).

opt_type(port, port, nonneg).
opt_help(port, "Port to listen to (default 8080)").

server :-
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _Positonal, Options),
    merge_options(Options, [port(8080)], Options1),
    http_server(Options1).

server_loop :-
    server,
    thread_get_message(quit).

