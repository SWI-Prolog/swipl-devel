/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(www_browser,
	  [ www_open_url/1,		% +UrlOrSpec
	    expand_url_path/2		% +Spec, -URL
	  ]).
:- use_module(library(lists)).
:- use_module(library(readutil)).

:- multifile
	known_browser/2.

%%	www_open_url(+Url)
%
%	Open URL in running version of the users' browser or start a new
%	browser.  This predicate tries the following steps:
%
%	  1. If a prolog flag (see set_prolog_flag/2) =browser= is set
%	  or the environment =BROWSER= and this is the name of a known
%	  executable, use this.  This uses www_open_url/2.
%
%	  2. On Windows, use win_shell(open, URL)
%
%	  3. Find a generic `open' comment.  Candidates are =open=,
%	  =|gnome-open|=, =kfmclient=.
%
%	  4. Try to find a known browser.
%
%	  @tbd	Figure out the right tool in step 3 as it is not
%		uncommon that multiple are installed.

www_open_url(Spec) :-			% user configured
	(   current_prolog_flag(browser, Browser)
	;   getenv('BROWSER', Browser)
	),
	has_command(Browser), !,
	expand_url_path(Spec, URL),
	www_open_url(Browser, URL).
:- if(current_predicate(win_shell/2)).
www_open_url(Spec) :-			% Windows shell
	expand_url_path(Spec, URL),
	win_shell(open, URL).
:- endif.
www_open_url(Spec) :-			% Unix `open document'
	open_command(Open),
	has_command(Open), !,
	expand_url_path(Spec, URL),
	format(string(Cmd), '~w "~w"', [Open, URL]),
	shell(Cmd).
www_open_url(Spec) :-			% KDE client
	has_command(kfmclient), !,
	expand_url_path(Spec, URL),
	format(string(Cmd), 'kfmclient openURL "~w"', [URL]),
	shell(Cmd).
www_open_url(Spec) :-			% something we know
	known_browser(Browser, _),
	has_command(Browser), !,
	expand_url_path(Spec, URL),
	www_open_url(Browser, URL).

open_command('gnome-open').
open_command(open).
open_command('xdg-open').

%%	www_open_url(+Browser, +URL) is det.
%
%	Open a page using  a  browser.   Preferably  we  use an existing
%	browser to to the job. Currently   only supports browsers with a
%	netscape compatible remote interface.
%
%	@see http://www.mozilla.org/unix/remote.html

www_open_url(Browser, URL) :-
	compatible(Browser, netscape),
	netscape_remote(Browser, 'ping()', []), !,
	netscape_remote(Browser, 'openURL(~w,new-window)', [URL]).
www_open_url(Browser, URL) :-
	format(string(Cmd), '"~w" "~w" &', [Browser, URL]),
	shell(Cmd).

%%	netscape_remote(+Browser, +Format, +Args) is semidet.
%
%	Execute netscape remote command using =|-remote|=. Create the
%	remote command using format/3 from Format and Args.
%
%	@bug	At least firefox gives always 0 exit code on -remote,
%		so we must check the error message.  Grrrr.

netscape_remote(Browser, Fmt, Args) :-
	format(string(RCmd), Fmt, Args),
	format(string(Cmd), '"~w" -remote "~w" 2>&1', [Browser, RCmd]),
	open(pipe(Cmd), read, In),
	call_cleanup(read_stream_to_codes(In, Codes),
		     close(In)),
	(   append("Error:", _, Codes)
	->  !, fail
	;   true
	).


compatible(Browser, With) :-
	file_base_name(Browser, Base),
	known_browser(Base, With).

%%	known_browser(+FileBaseName, -Compatible)
%
%	True if browser FileBaseName has a remote protocol compatible to
%	Compatible.

known_browser(firefox,   netscape).
known_browser(mozilla,   netscape).
known_browser(netscape,  netscape).
known_browser(konqueror, -).
known_browser(opera,     -).


%%	has_command(+Command)
%
%	Succeeds if Command is in  $PATH.   Works  for Unix systems. For
%	Windows we have to test for executable extensions.

:- dynamic
	command_cache/2.
:- volatile
	command_cache/2.

has_command(Command) :-
	command_cache(Command, Path), !,
	Path \== (-).
has_command(Command) :-
	(   getenv('PATH', Path),
	    (	current_prolog_flag(windows, true)
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
		 *	      NET PATHS		*
		 *******************************/

%%	url_path(+Alias, -Expansion) is nondet.
%
%	Define URL path aliases. This multifile  predicate is defined in
%	module =user=. Expansion is either a URL, or a term Alias(Sub).

:- multifile
	user:url_path/2.

user:url_path(swipl,	      'http://www.swi-prolog.org').
user:url_path(swipl_book,     'http://books.google.nl/books/about/\c
			       SWI_Prolog_Reference_Manual_6_2_2.html?\c
			       id=q6R3Q3B-VC4C&redir_esc=y').

user:url_path(swipl_faq,      swipl('FAQ')).
user:url_path(swipl_man,      swipl('pldoc/index.html')).
user:url_path(swipl_mail,     swipl('Mailinglist.html')).
user:url_path(swipl_download, swipl('Download.html')).
user:url_path(swipl_pack,     swipl('pack/list')).
user:url_path(swipl_bugs,     swipl('bugzilla/')).
user:url_path(swipl_quick,    swipl('man/quickstart.html')).

%%	expand_url_path(+Spec, -URL)
%
%	Expand URL specifications similar   to absolute_file_name/3. The
%	predicate url_path/2 plays the role of file_search_path/2.
%
%	@error	existence_error(url_path, Spec) if the location is not
%		defined.

expand_url_path(URL, URL) :-
	atomic(URL), !.			% Allow atom and string
expand_url_path(Spec, URL) :-
	Spec =.. [Path, Local],
	(   user:url_path(Path, Spec2)
	->  expand_url_path(Spec2, URL0),
	    (	Local == '.'
	    ->	URL = URL0
	    ;	sub_atom(Local, 0, _, _, #)
	    ->	atom_concat(URL0, Local, URL)
	    ;	atomic_list_concat([URL0, Local], /, URL)
	    )
	;   throw(error(existence_error(url_path, Path), expand_url_path/2))
	).

