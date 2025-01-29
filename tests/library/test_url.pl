/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2006-2010, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_url,
	  [ test_url/0
	  ]).

:- encoding(utf8).
:- use_module(library(url)).

:- thread_local
	error/2.

test_url :-
	retractall(error(_,_)),
	forall(test_p(URL, Parts),
	       test_parse(URL, Parts)),
	forall(test_b(URL, Parts),
	       (   test_parse(URL, Parts),
		   test_gen(URL, Parts))),
	forall(abs(Rel, Base, Abs),
	       test_abs(Rel, Base, Abs)),
	\+ error(_,_).

test_parse(URL, Parts) :-
	parse_url(URL, P0),
	sort(P0, P1),
	sort(Parts, P2),
	P1 == P2, !.
test_parse(URL, _) :-
	fmt_error('FAILED: parse_url(~q, Parts)~n', [URL]).

test_gen(URL, Parts) :-
	(   parse_url(URL1, Parts)
	->  (   URL1 == URL
	    ->  true
	    ;   fmt_error('FAILED: parse_url(URL, ~q)~n', [Parts]),
		fmt_error('~q \\== ~q~n', [URL, URL1])
	    )
	;   fmt_error('FAILED: parse_url(URL, ~q)~n', [Parts])
	).

test_abs(Rel, Base, Abs) :-
	(   global_url(Rel, Base, Abs0),
	    Abs0 == Abs
	->  true
	;   fmt_error('FAILED: global_url(~q, ~q, ABS)~n', [Rel, Base])
	).


%	test_p(URL, Parts)
%
%	Parse-only tests

test_p('localhost',
       [protocol(http), host(localhost), path(/)]).
test_p('http://localhost',
       [protocol(http), host(localhost), path(/)]).
test_p('http://gollem?name=value',
       [protocol(http), host(gollem), path(/), search([name=value])]).

%	test_b(URL, Parts)
%
%	Bi-directional tests (canonical URIs)

test_b('http://localhost/',
       [protocol(http), host(localhost), path(/)]).
test_b('http://gollem.science.uva.nl/',
       [protocol(http), host('gollem.science.uva.nl'), path(/)]).
test_b('http://146.50.26.20/',
       [protocol(http), host('146.50.26.20'), path(/)]).
test_b('http://%d0%b5%d0%b7%d1%83%d0%bf%d1%80.com/',
       [protocol(http), host('езупр.com'), path(/)]).
test_b('http://jan@gollem.science.uva.nl/',
       [protocol(http), host('gollem.science.uva.nl'), path(/), user(jan)]).
test_b('http://gollem/index.html',
       [protocol(http), host('gollem'), path('/index.html')]).
test_b('http://gollem/top/index.html',
       [protocol(http), host('gollem'), path('/top/index.html')]).
test_b('http://gollem/%d0%b5%d0%b7%d1%83%d0%bf%d1%80',
       [protocol(http), host('gollem'), path('/езупр')]).
test_b('http://gollem/?name=value',
       [protocol(http), host(gollem), path(/), search([name=value])]).
test_b('http://gollem/?name=w1%20w2',
       [protocol(http), host(gollem), path(/), search([name='w1 w2'])]).
test_b('http://gollem/?name=w1%20w2&a=b',
       [protocol(http), host(gollem), path(/), search([name='w1 w2',a=b])]).
test_b('file:///dir/file.xml',
       [protocol(file), path('/dir/file.xml')]).
test_b('file://u:/dir/file.xml',
       [protocol(file), path('u:/dir/file.xml')]).


abs('file.html',
    'http://gollem/top/index.html',
    'http://gollem/top/file.html').
abs('file.html',
    'http://gollem/top/index.html?a=b',
    'http://gollem/top/file.html').
abs('file.html?name=value',
    'http://gollem/top/index.html?a=b',
    'http://gollem/top/file.html?name=value').

fmt_error(Fmt, Args) :-
	format(user_error, Fmt, Args),
	assert(error(Fmt, Args)).

