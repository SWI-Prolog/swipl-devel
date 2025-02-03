/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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
:- http_handler('/wasm/shell', reply_html_test('shell.html'), []).
:- http_handler('/wasm/test',  reply_html_test('test.html'), []).
:- http_handler('/wasm/cbg',   reply_html_test('cbg.html'), []).
:- http_handler('/wasm/',
                http_reply_from_files(web(.), [static_gzip(true)]), [prefix]).


reply_html_test(File, Request) :-
    http_reply_file(web(File),
                    [ headers([ 'Cross-Origin-Opener-Policy'('same-origin'),
                                'Cross-Origin-Embedder-Policy'('require-corp')
                              ])],
                    Request).

:- if(absolute_file_name(scasp(.), _, [file_type(directory), file_errors(fail)])).
:- http_handler('/wasm/scasp/', http_reply_from_files(scasp(.), []), [prefix]).
:- endif.


:- initialization(main, main).

opt_type(port,        port,        nonneg).
opt_type(p,           port,        nonneg).
opt_type(interactive, interactive, boolean).
opt_type(i,           i,           boolean).

opt_help(port, "Port to listen to (default 8080)").
opt_help(interactive, "Become interactive").

server(Options) :-
    merge_options(Options, [port(8080)], Options1),
    http_server(Options1).

main(Argv) :-
    argv_options(Argv, _Pos, Options),
    server(Options),
    (   option(interactive(true), Options)
    ->  cli_enable_development_system
    ;   thread_get_message(quit)
    ).
