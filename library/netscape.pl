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

:- module(netscape,
	  [ www_open_url/1,		% +UrlOrSpec
	    expand_url_path/2		% +Spec, -URL
	  ]).

%	www_open_url(+Url)
%
%	Open URL in running version of netscape or start a new netscape.
%	Based on a windows-only version by Bob Wielinga.
%
%	On Unix life is a bit harder as there is no established standard
%	to figure out which browser to open.

www_open_url(Spec) :-
	current_prolog_flag(windows, true), !,
	expand_url_path(Spec, URL),
	call(win_shell(open, URL)).	% fool xref
www_open_url(Spec) :-
	(   getenv('BROWSER', Browser)
	->  true
	;   Browser = netscape
	),
	expand_url_path(Spec, URL),
	www_open_url(Browser, URL).

www_open_url(Browser, URL) :-
	netscape_compatible(Browser), !,
	sformat(Cmd0, '~w -remote "xfeDoCommand(openBrowser)"', [Browser]),
	(   shell(Cmd0, 0)
	->  sformat(Cmd, '~w -remote "openURL(~w)" &', [Browser, URL]),
	    shell(Cmd)
	;   sformat(Cmd, '~w "~w" &', [Browser, URL]),
	    shell(Cmd)
	).
www_open_url(Browser, URL) :-
	sformat(Cmd, '~w "~w" &', [Browser, URL]),
	shell(Cmd).

netscape_compatible(Browser) :-
	file_base_name(Browser, Base),
	netscape_base(Base).

netscape_base(netscape).
netscape_base(mozilla).


		 /*******************************
		 *	      NET PATHS		*
		 *******************************/

:- multifile
	user:url_path/2.

user:url_path(swi,	   'http://www.swi.psy.uva.nl').
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

expand_url_path(URL, URL) :-
	atom(URL), !.
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

