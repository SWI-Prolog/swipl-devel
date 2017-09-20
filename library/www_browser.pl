/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2015, University of Amsterdam
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

:- module(www_browser,
          [ www_open_url/1,             % +UrlOrSpec
            expand_url_path/2           % +Spec, -URL
          ]).
:- use_module(library(lists)).
:- if(exists_source(library(process))).
:- use_module(library(process)).
:- endif.

:- multifile
    known_browser/2.

/** <module> Open a URL in the users browser

This library deals with the highly platform   specific task of opening a
web  page.  In  addition,   is   provides    a   mechanism   similar  to
absolute_file_name/3 that expands compound terms   to concrete URLs. For
example, the SWI-Prolog home page can be opened using:

  ==
  ?- www_open_url(swipl(.)).
  ==
*/

%!  www_open_url(+Url)
%
%   Open URL in running version of the users' browser or start a new
%   browser.  This predicate tries the following steps:
%
%     1. If a prolog flag (see set_prolog_flag/2) =browser= is set
%     and this is the name of a known executable, use this.  The
%     flag may be set to `Command-Mode`, where mode is one of `fg`
%     or `bg`, requesting Command to run in foreground or background
%     mode.  Default is `bg`.
%
%     2. On Windows, use win_shell(open, URL)
%
%     3. Find a generic `open' comment.  Candidates are =xdg-open=,
%     =open= or =|gnome-open|=.
%
%     4. If a environment variable =BROWSER= is set
%     and this is the name of a known executable, use this.
%
%     5. Try to find a known browser.
%
%     @tbd  Figure out the right tool in step 3 as it is not
%           uncommon that multiple are installed.

www_open_url(Spec) :-                   % user configured
    expand_url_path(Spec, URL),
    open_url(URL).

open_url(URL) :-
    current_prolog_flag(browser, Browser),
    expand_browser_flag(Browser, Command, Mode),
    has_command(Command),
    !,
    run_command(Command, [URL], Mode).
:- if(current_predicate(win_shell/2)).
open_url(URL) :-                        % Windows shell
    win_shell(open, URL).
:- endif.
open_url(URL) :-                        % Unix `open document'
    open_command(Open),
    has_command(Open),
    !,
    run_command(Open, [URL], fg).
open_url(URL) :-                        % user configured
    getenv('BROWSER', Browser),
    has_command(Browser),
    !,
    run_browser(Browser, URL).
open_url(URL) :-                        % something we know
    known_browser(Browser, _),
    has_command(Browser),
    !,
    run_browser(Browser, URL).

expand_browser_flag(Command-Mode, Command, Mode) :- !.
expand_browser_flag(Command, Command, bg) :- atomic(Command).

open_command(open) :-                   % Apples open command
    current_prolog_flag(apple, true).
open_command('xdg-open').               % Free desktop
open_command('gnome-open').             % Gnome (deprecated in favour of xdg-open
open_command(open).                     % Who knows

%!  run_browser(+Browser, +URL) is det.
%
%   Open a page using a browser.

run_browser(Browser, URL) :-
    run_command(Browser, [URL], bg).

%!  run_command(+Command, +Args, +Background)
%
%   Run OS command Command using Args,   silencing  the error output
%   because many browsers are rather verbose.

:- if(current_predicate(process_create/3)).
run_command(Command, Args, fg) :-
    !,
    process_create(path(Command), Args, [stderr(null)]).
:- endif.
:- if(current_prolog_flag(unix, true)).
run_command(Command, [Arg], fg) :-
    format(string(Cmd), "\"~w\" \"~w\" &> /dev/null", [Command, Arg]),
    shell(Cmd).
run_command(Command, [Arg], bg) :-
    format(string(Cmd), "\"~w\" \"~w\" &> /dev/null &", [Command, Arg]),
    shell(Cmd).
:- else.
run_command(Command, [Arg], fg) :-
    format(string(Cmd), "\"~w\" \"~w\"", [Command, Arg]),
    shell(Cmd).
run_command(Command, [Arg], bg) :-
    format(string(Cmd), "\"~w\" \"~w\" &", [Command, Arg]),
    shell(Cmd).
:- endif.

%!  known_browser(+FileBaseName, -Compatible)
%
%   True if browser FileBaseName has a remote protocol compatible to
%   Compatible.

known_browser(firefox,   netscape).
known_browser(mozilla,   netscape).
known_browser(netscape,  netscape).
known_browser(konqueror, -).
known_browser(opera,     -).


%!  has_command(+Command)
%
%   Succeeds if Command is in  $PATH.   Works  for Unix systems. For
%   Windows we have to test for executable extensions.

:- dynamic
    command_cache/2.
:- volatile
    command_cache/2.

has_command(Command) :-
    command_cache(Command, Path),
    !,
    Path \== (-).
has_command(Command) :-
    (   getenv('PATH', Path),
        (   current_prolog_flag(windows, true)
        ->  Sep = (;)
        ;   Sep = (:)
        ),
        atomic_list_concat(Parts, Sep, Path),
        member(Part, Parts),
        prolog_to_os_filename(PlPart, Part),
        atomic_list_concat([PlPart, Command], /, Exe),
        access_file(Exe, execute)
    ->  assert(command_cache(Command, Exe))
    ;   assert(command_cache(Command, -)),
        fail
    ).


                 /*******************************
                 *            NET PATHS         *
                 *******************************/

%!  url_path(+Alias, -Expansion) is nondet.
%
%   Define URL path aliases. This multifile  predicate is defined in
%   module =user=. Expansion is either a URL, or a term Alias(Sub).

:- multifile
    user:url_path/2.

user:url_path(swipl,          'http://www.swi-prolog.org').
user:url_path(swipl_book,     'http://books.google.nl/books/about/\c
                               SWI_Prolog_Reference_Manual_6_2_2.html?\c
                               id=q6R3Q3B-VC4C&redir_esc=y').

user:url_path(swipl_faq,      swipl('FAQ')).
user:url_path(swipl_man,      swipl('pldoc/doc_for?object=manual')).
user:url_path(swipl_mail,     swipl('Mailinglist.html')).
user:url_path(swipl_download, swipl('Download.html')).
user:url_path(swipl_pack,     swipl('pack/list')).
user:url_path(swipl_bugs,     swipl('bugzilla/')).
user:url_path(swipl_quick,    swipl('man/quickstart.html')).

%!  expand_url_path(+Spec, -URL)
%
%   Expand URL specifications similar   to absolute_file_name/3. The
%   predicate url_path/2 plays the role of file_search_path/2.
%
%   @error  existence_error(url_path, Spec) if the location is not
%           defined.

expand_url_path(URL, URL) :-
    atomic(URL),
    !.                 % Allow atom and string
expand_url_path(Spec, URL) :-
    Spec =.. [Path, Local],
    (   user:url_path(Path, Spec2)
    ->  expand_url_path(Spec2, URL0),
        (   Local == '.'
        ->  URL = URL0
        ;   sub_atom(Local, 0, _, _, #)
        ->  atom_concat(URL0, Local, URL)
        ;   atomic_list_concat([URL0, Local], /, URL)
        )
    ;   throw(error(existence_error(url_path, Path), expand_url_path/2))
    ).

