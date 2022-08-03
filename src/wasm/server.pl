% Provide an HTTP server to make   the  various components available. To
% use it, build the wasm version  in   e.g.,  `build.wasm` and from this
% directory, run
%
%     swipl ../src/wasm/server.pl
%
:- use_module(library(http/http_server)).

user:file_search_path(source, '../src/wasm').
user:file_search_path(wasm,   'src').

:- http_handler('/', http_redirect(see_other, '/wasm/shell'), []).
:- http_handler('/wasm/shell',
                http_reply_file(source('shell.html'), []), []).
:- http_handler('/wasm/swipl-web.js',
                http_reply_file(wasm('swipl-web.js'), []), []).
:- http_handler('/wasm/swipl-web.data',
                http_reply_file(wasm('swipl-web.data'), []), []).
:- http_handler('/wasm/swipl-web.wasm',
                http_reply_file(wasm('swipl-web.wasm'), []), []).

:- initialization(server_loop, main).

server :-
    http_server([port(8080)]).

server_loop :-
    server,
    thread_get_message(quit).

