/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009, University of Amsterdam
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

:- module(dmalloc,
	  [ dmalloc/1,			% :Goal
	    dmalloc/3,			% :Goal, +Format, +Args
	    dmalloc_mark/1,		% -Mark
	    dmalloc_log_changed/4,	% +Mark, +NotFreed, +Freed, +Details
	    dmalloc_message/2		% +Format, +Args
	  ]).

/** <module> DMALLOC utility predicates

Usage: compile dmalloc.c using the  command   below  before loading this
module.

    ==
    % swipl-ld --shared -o dmalloc dmalloc.c
    ==

Prolog must be compiled  with  dmalloc   support  enabled,  which can be
achieved using =|configure --enable-dmalloc|= or by adding =|-DDMALLOC|=
to COFLAGS and =|-ldmalloc|= to LIBS in the generated Makefile.

Now, configure dmalloc for the shell, start Prolog and load this module.
E.g.

    ==
    % dmalloc -l logfile low
    % pl
    % ?- [dmalloc].
    % ?- dmalloc(go).
    ==

Note that typically, you should run the  predicate for which you want to
do leak-testing twice. The first run may  show `leaks' due to autoloaded
predicates, lazily generated clause indices, etc.

@see	The Dmalloc home-page at http://dmalloc.com/
*/

:- use_foreign_library(dmalloc).
:- meta_predicate
	dmalloc(0),
	dmalloc(0, +, +).

%%	dmalloc(:Goal).
%%	dmalloc(:Goal, +Format, +Args).
%
%	Run Goal and  print  incremental   dmalloc  diagnostics  to  the
%	current malloc logfile.

dmalloc(G) :-
	dmalloc(G, '', []).

dmalloc(G, Fmt, Args) :-
	garbage_collect,
	garbage_collect_atoms,
	trim_stacks,
	setup_call_cleanup(dmalloc_mark(Mark),
			   G,
			   end_dmalloc(Mark, Fmt, Args)).

end_dmalloc(Mark, Fmt, Args) :-
	(   Fmt \== ''
	->  dmalloc_message(Fmt, Args)
	;   true
	),
	garbage_collect,
	garbage_collect_atoms,
	trim_stacks,
	dmalloc_log_changed(Mark, true, false, true).

dmalloc_message(Format, Args) :-
	format(string(S), Format, Args),
	'_dmalloc_message'(S).
