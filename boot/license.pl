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

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(license,
	  [ license/1,			% +LicenseId
	    license/2,			% +LicenseId, +ModuleId
	    eval_license/0		% Eval current situation
	  ]).

:- dynamic
	licensed/2.			% +Id, +Module

:- multifile
	license/3.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
License  definitions.  This  data  is   (still  incomplete)  taken  from
http://www.fsf.org/licenses/license-list.html. The first argument is the
license identifier. The second tells us with  which of the two principal
licenses (GPL and LGPL) the license is   compatible.  The remainder is a
list  of  properties  that   can   be    used   with   more   high-level
license-information tools.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

license(gpl, gpl,
        [ comment('GNU General Public License'),
	  url('http://www.fsf.org/copyleft/gpl.html')
	]).
license(lgpl, lgpl,
	[ comment('GNU Lesser General Public License'),
	  url('http://www.fsf.org/copyleft/lesser.html')
	]).

license(swipl, lgpl,
	[ comment('SWI-Prolog Prolog Source License'),
	  url('http://www.swi-prolog.org/license.html')
	]).

%	Other GPL/LGPL compatible licenses.

license(guile, lgpl,
	[ comment('License for Guile')
	]).
license(gnu_ada, lgpl,
	[ comment('The license of the run-time units of the GNU Ada compiler')
	]).
license(x11, lgpl,
	[ comment('The X11 licens'),
	  url('http://www.x.org/terms.htm')
	]).
license(expat, lgpl,
	[ comment('Expat license'),
	  url('http://www.jclark.com/xml/copying.txt')
	]).
license(sml, lgpl,
	[ comment('Standard ML of New Jersey Copyright License'),
	  url('http://cm.bell-labs.com/cm/cs/what/smlnj/license.html')
	]).
license(public_domain, lgpl,
	[ comment('Unrestricted Public domain')
	]).
license(cryptix, lgpl,
	[ comment('The Cryptix General License'),
	  url('http://www.cryptix.org/docs/license.html')
	]).
license(bsd, lgpl,
	[ comment('The modified BSD license'),
	  url('http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5')
	]).
license(zlib, gpl,
	[ comment('The license of ZLib'),
	  url('http://www.gzip.org/zlib/zlib_license.html')
	]).

license(lgpl_compatible, lgpl,
	[ comment('Other LGPL compatible license')
	]).
license(gpl_compatible, gpl,
	[ comment('Other GPL and not LGPL compatible license')
	]).


%	license(+License, [+ModuleId])
%
%	Register the current file under the given license restriction.

license(License) :-
	(   prolog_load_context(file, File)
	->  true
	;   File = '<unknown file>'
	),
	license(License, File).

license(License, File) :-
	warn_if_unknown(License),
	assert(licensed(License, File)).

warn_if_unknown(License) :-
	license(License, _, _), !.
warn_if_unknown(License) :-
	print_message(warning, unknown_license(License)).

%	eval_license
%
%	Report current license situation

eval_license :-
	report_gpl,
	report_proprietary.

report_gpl :-
	setof(Module, gpled(Module), Modules), !,
	print_message(informational, license(gpl, Modules)).
report_gpl :-
	print_message(informational, license(lgpl)).

gpled(Module) :-
	licensed(X, Module),
	license(X, gpl, _).

report_proprietary :-
	(   setof(Module, proprietary(Module, L), Modules),
	    print_message(informational, license(proprierary(L), Modules)),
	    fail
	;   true
	).

proprietary(Module, L) :-
	licensed(L, Module),
	license(L, C, _),
	C \== gpl,
	C \== lgpl.


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(unknown_license(License)) -->
	[ 'Unknown license: ~w.  Known licenses are:'-[License], nl ],
	license_list.
prolog:message(license(gpl, Modules)) -->
	[ 'This system may only distributed using the GNU General Public License', nl,
	  'because the following components contain GPL-ed code:', nl, nl
	],
	file_list(Modules),
	see_also.
prolog:message(license(lgpl)) -->
	[ 'This program may be distributed under any license, provided all', nl,
	  'conditions implied by the GNU Lesser General Public License', nl,
	  'are satisfied.  In particular, this implies the source code', nl,
	  'to any modification in SWI-Prolog or one of the used libraries', nl,
	  'must be made available.', nl
	],
	see_also.
prolog:message(license(proprierary(L), Modules)) -->
	{ license(L, _, Att) },
	{   memberchk(comment(C), Att)
	->  true
	;   C = L
	},
	[ nl,
	  'The program contains modules covered by the "~w" license'-[C], nl
	],
	(   { memberchk(url(URL), Att) }
	->  [ 'See ~w'-[URL], nl ]
	;   []
	),
	[ nl ],
	file_list(Modules).

see_also -->
	[ nl,
	  'See http://www.swi-prolog.org/license.html for details on', nl,
	  'SWI-Prolog licensing policies supporting both free and non-free',nl,
	  'Software.'
	].

license_list -->
	{ findall(X, license(X, _, _), Pairs)
	},
	license_list(Pairs).

license_list([]) -->
	[].
license_list([L|T]) -->
	{ license(L, _, Att) },
	(   { memberchk(comment(C), Att)
	    ; memberchk(url(C), Att)
	    }
	->  [ '    ~w~t16~|~w'-[X, C], nl ]
	;   [ '	   ~w'-[X, C], nl ]
	),
	license_list(T).

file_list([]) -->
	[].
file_list([H|T]) -->
	[ '    ~w'-[H], nl ],
	file_list(T).
