/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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

%%	www_open_url(+Url)
%
%	Open URL in running version of netscape or start a new netscape.
%	Based on a windows-only version by Bob Wielinga.
%
%	On Unix life is a bit harder as there is no established standard
%	to figure out which browser to open.  We try:
%	
%		* The start-program `open' (Mac and some Unix systems).
%		* Prolog flag `browser'
%		* Variable BROWSER
%		* All known browsers

www_open_url(Spec) :-			% user configured
	(   current_prolog_flag(browser, Browser)
	;   getenv('BROWSER', Browser)
	),
	has_command(Browser), !,
	expand_url_path(Spec, URL),
	www_open_url(Browser, URL).
www_open_url(Spec) :-			% Windows shell
	current_prolog_flag(windows, true), !,
	expand_url_path(Spec, URL),
	call(win_shell(open, URL)).	% fool xref
www_open_url(Spec) :-			% Unix `open document'
	has_command(open), !,
	expand_url_path(Spec, URL),
	sformat(Cmd, 'open "~w"', [URL]),
	shell(Cmd).
www_open_url(Spec) :-			% something we know
	known_browser(Browser, _),
	has_command(Browser), !,
	expand_url_path(Spec, URL),
	www_open_url(Browser, URL).

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

netscape_remote(Browser, Fmt, Args) :-
	format(string(RCmd), Fmt, Args),
	format(string(Cmd), '"~w" -remote "~w"', [Browser, RCmd]),
	shell(Cmd, 0).

compatible(Browser, With) :-
	file_base_name(Browser, Base),
	known_browser(Base, With).

:- multifile
	known_browser/2.

%%	known_browser(+FileBaseName, -Compatible)
%
%	True if browser FileBaseName has a remote protocol compatible to
%	Compatible.

known_browser(firefox,   netscape).
known_browser(mozilla,   netscape).
known_browser(netscape,  netscape).
known_browser(konquerer, -).
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
	    concat_atom(Parts, Sep, Path),
	    member(Part, Parts),
	    prolog_to_os_filename(PlPart, Part),
	    concat_atom([PlPart, Command], /, Exe),
	    access_file(Exe, execute)
	->  assert(command_cache(Command, Exe))
	;   assert(command_cache(Command, -)),
	    fail
	).


		 /*******************************
		 *	      NET PATHS		*
		 *******************************/

:- multifile
	user:url_path/2.

user:url_path(swi,	   'http://hcs.science.uva.nl').
user:url_path(pl,	   'http://www.swi-prolog.org').
user:url_path(pl_twiki,	   'http://gollem.science.uva.nl/twiki/pl/bin/view').

user:url_path(pl_at_swi,   swi('projects/SWI-Prolog')).
user:url_path(pl_faq,	   pl_twiki('FAQ/WebHome')).
user:url_path(pl_man,	   pl_at_swi('Manual')).
user:url_path(pl_mail,	   pl('mailinglist.html')).
user:url_path(pl_download, pl('download.html')).
user:url_path(pl_bugs,	   pl('bugreport.html')).
user:url_path(pl_quick,	   pl_man('quickstart.html')).
user:url_path(pl_donate,   pl('donate.html')).

user:url_path(xpce,	   pl('packages/xpce')).
user:url_path(xpce_man,	   swi('projects/xpce/UserGuide')).

%%	expand_url_path(+Spec, -URL)
%
%	Expand URL specifications similar   to absolute_file_name/3. The
%	predicate url_path/2 plays the role of file_name_expansion/2.

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
	    ;	concat_atom([URL0, Local], /, URL)
	    )
	;   throw(error(existence_error(url_path, Path), expand_url_path/2))
	).

