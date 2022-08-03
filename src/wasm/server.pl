% Provide an HTTP server to make   the  various components available. To
% use it, build the wasm version  in   e.g.,  `build.wasm` and from this
% directory, run
%
%     swipl ../src/wasm/server.pl
%
:- use_module(library(http/http_server)).
:- use_module(library(main)).
:- use_module(library(option)).

user:file_search_path(source, '../src/wasm').
user:file_search_path(wasm,   'src').

:- http_handler('/', http_redirect(see_other, '/wasm/shell'), []).
:- http_handler('/wasm/', http_redirect(see_other, '/wasm/shell'), []).
:- http_handler('/wasm/shell',
                http_reply_file(
                    source('shell.html'),
                    [ headers([ 'Cross-Origin-Opener-Policy'('same-origin'),
                                'Cross-Origin-Embedder-Policy'('require-corp')
                              ])]),
                []).
:- http_handler('/wasm/swipl-web.js',
                http_reply_file(wasm('swipl-web.js'), []), []).
:- http_handler('/wasm/swipl-web.worker.js',
                http_reply_file(wasm('swipl-web.worker.js'), []), []).
:- http_handler('/wasm/swipl-web.data',
                http_reply_file(wasm('swipl-web.data'), []), []).
:- http_handler('/wasm/swipl-web.wasm',
                http_reply_file(wasm('swipl-web.wasm'), []), []).

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

