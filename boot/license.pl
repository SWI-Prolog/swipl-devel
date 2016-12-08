/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2016, University of Amsterdam
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

:- module(license,
	  [ license/1,			% +LicenseId
	    license/2,			% +LicenseId, +ModuleId
	    license/0			% Current situation
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
license(gplv2, gpl,
        [ comment('GNU General Public License, version 2'),
	  url('http://www.fsf.org/copyleft/gpl.html')
	]).
license('gplv2+', gpl,
        [ comment('GNU General Public License, version 2 or later'),
	  url('http://www.fsf.org/copyleft/gpl.html')
	]).
license(gplv3, gpl,
        [ comment('GNU General Public License, version 3'),
	  url('http://www.fsf.org/copyleft/gpl.html')
	]).
license(lgpl, lgpl,
	[ comment('GNU Lesser General Public License'),
	  url('http://www.fsf.org/copyleft/lesser.html')
	]).
license(lgplv2, lgpl,
	[ comment('GNU Lesser General Public License, version 2'),
	  url('http://www.fsf.org/copyleft/lesser.html')
	]).
license('lgplv2+', lgpl,
	[ comment('GNU Lesser General Public License, version 2 or late'),
	  url('http://www.fsf.org/copyleft/lesser.html')
	]).
license(lgplv3, lgpl,
	[ comment('GNU Lesser General Public License, version 3'),
	  url('http://www.fsf.org/copyleft/lesser.html')
	]).
license(gpl_swipl, lgpl,
	[ comment('SWI-Prolog Prolog Source License for versions up to 7.3.32'),
	  url('http://www.swi-prolog.org/license-old.html')
	]).
license(swipl, lgpl,
	[ comment('SWI-Prolog Prolog Source License for versions up to 7.3.32'),
	  url('http://www.swi-prolog.org/license-old.html')
	]).

%	Other GPL/LGPL compatible licenses.
%	TBD: Check permissive status of these licenses

license(guile, lgpl,
	[ comment('License for Guile'),
	  url('https://www.gnu.org/software/guile/docs/docs-1.6/guile-ref/Guile-License.html')
	]).
license(gnu_ada, lgpl,
	[ comment('The license of the run-time units of the GNU Ada compiler'),
	  url('https://en.wikipedia.org/wiki/GNAT#License')
	]).
license(x11, permissive,
	[ comment('The X11 license'),
	  url('http://www.x.org/terms.htm')
	]).
license(expat, permissive,
	[ comment('Expat license'),
	  url('http://www.jclark.com/xml/copying.txt')
	]).
license(sml, permissive,
	[ comment('Standard ML of New Jersey Copyright License'),
	  url('http://cm.bell-labs.com/cm/cs/what/smlnj/license.html')
	]).
license(public_domain, permissive,
	[ comment('Unrestricted Public domain')
	]).
license(cryptix, permissive,
	[ comment('The Cryptix General License'),
	  url('http://www.cryptix.org/docs/license.html')
	]).
license(bsd, permissive,
	[ comment('The modified BSD license'),
	  url('http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5')
	]).
license(mit, permissive,
	[ comment('The MIT License'),
	  url('https://en.wikipedia.org/wiki/MIT_License')
	]).
license(zlib, permissive,
	[ comment('The license of ZLib'),
	  url('http://www.gzip.org/zlib/zlib_license.html')
	]).
license(agpl, gpl,
	[ comment('Affero General Public License'),
	  url('http://www.gnu.org/licenses/agpl-3.0.en.html')
	]).
license(lgpl_compatible, lgpl,
	[ comment('Other LGPL compatible license')
	]).
license(gpl_compatible, gpl,
	[ comment('Other GPL and not LGPL compatible license')
	]).
license(permissive, permissive,
	[ comment('Other permissive license')
	]).
license(asl2, permissive,
	[ comment('Apache License 2.0'),
	  url('http://www.apache.org/licenses/LICENSE-2.0')
	]).


%%	license(+License, [+ModuleId])
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
	assertz(licensed(License, File)).

warn_if_unknown(License) :-
	license(License, _, _), !.
warn_if_unknown(License) :-
	print_message(warning, unknown_license(License)).

%%	license is det.
%
%	Report current license situation

license :-
	report_gpl,
	report_proprietary.

report_gpl :-
	setof(Module, gpled(Module), Modules), !,
	print_message(informational, license(gpl, Modules)).
report_gpl :-
	setof(Module, lgpled(Module), Modules), !,
	print_message(informational, license(lgpl, Modules)).
report_gpl :-
	print_message(informational, license(permissive)).

gpled(Module) :-
	licensed(X, Module),
	license(X, gpl, _).
lgpled(Module) :-
	licensed(X, Module),
	license(X, lgpl, _).

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
	[ 'This executable may only distributed using the GNU General Public License', nl,
	  'because the following components contain GPL-ed code:', nl, nl
	],
	file_list(Modules),
	see_also.
prolog:message(license(lgpl, Modules)) -->
	[ 'This executable may be distributed under any license, provided all', nl,
	  'conditions implied by the GNU Lesser General Public License', nl,
	  'are satisfied.', nl
	],
	file_list(Modules),
	see_also.
prolog:message(license(permissive)) -->
	[ 'No modules with restrictive conditions have been registered.', nl,
	  'This executable is covered by several permissive licenses.', nl,
	  'SWI-Prolog itself is coverned by the Simplified BSD license.', nl
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
	  'The SWI-Prolog source is distributed under the Simplified BSD', nl,
	  'license. See http://www.swi-prolog.org/license.html for details.'
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
	->  [ '  ~|~w~t~20+~w'-[L, C], nl ]
	;   [ '  ~|~w'-[L], nl ]
	),
	license_list(T).

file_list([]) -->
	[].
file_list([H|T]) -->
	[ '    ~w'-[H], nl ],
	file_list(T).
