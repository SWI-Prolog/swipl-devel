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
          [ license/1,                  % +LicenseId
            license/2,                  % +LicenseId, +ModuleId
            license/0,                  % Current situation

            known_licenses/0
          ]).

:- dynamic
    licensed/2.                     % +Id, +Module

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
        [ comment('GNU Lesser General Public License, version 2 or later'),
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

%       Other GPL/LGPL compatible licenses.
%       TBD: Check permissive status of these licenses

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


%!  license(+License) is det.
%!  license(+License, +ModuleId) is det.
%
%   Register the current file under the given license restriction.

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
    license(License, _, _),
    !.
warn_if_unknown(License) :-
    print_message(warning, unknown_license(License)).

%!  license is det.
%
%   Report current license situation

license :-
    (setof(Module, gpled(Module), GPL)   -> true ; GPL  = []),
    (setof(Module, lgpled(Module), LGPL) -> true ; LGPL = []),
    findall(L-Modules,
            setof(Module, permissive(Module, L), Modules),
            Permissive),
    findall(L-Modules,
            setof(Module, proprietary(Module, L), Modules),
            Proprietary),
    print_message(informational, license(GPL,LGPL,Permissive,Proprietary)).

gpled(Module) :-
    licensed(X, Module),
    license(X, gpl, _).

lgpled(Module) :-
    licensed(X, Module),
    license(X, lgpl, _).

permissive(Module, L) :-
    licensed(L, Module),
    license(L, permissive, _).

proprietary(Module, L) :-
    licensed(L, Module),
    (   license(L, C, _)
    ->  C \== gpl,
        C \== lgpl,
        C \== permissive
    ;   true
    ).

%!  known_licenses
%
%   Print all known licenses.

known_licenses :-
    findall(license(Id,Compat,Atts),
            license(Id,Compat,Atts),
            Licenses),
    print_message(informational, known_licenses(Licenses)).


                 /*******************************
                 *             MESSAGES         *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(license(GPL,LGPL,Permissive,Proprietary)) -->
    license_message(GPL,LGPL,Permissive,Proprietary).
prolog:message(unknown_license(License)) -->
    [ 'The license "~w" is not known.  You can list the known '-[License], nl,
      'licenses using ?- known_licenses. or add information about this ',
      'license by extending license:license/3.'
    ].
prolog:message(known_licenses(Licenses)) -->
    [ 'The following license identifiers may be used in license/2',
      'and PL_license()'
    ],
    known_licenses(Licenses).

%!  license_message(+GPL, +LGPL, +Proprietary)//

license_message(GPL, LGPL, Permissive, Proprietary) -->
    license_message(GPL, LGPL, Permissive),
    proprietary_licenses(Proprietary).

license_message([], [], Permissive) -->
    !,
    [ 'This program contains only components covered by permissive license', nl,
      'conditions. SWI-Prolog is covered by the Simplified BSD license:',
      nl, nl
    ],
    bsd2_license,
	permissive_licenses(Permissive).
license_message(GPL, _, _) -->
    { GPL \== [] },
    !,
    [ 'SWI-Prolog is covered by the Simplified BSD license:', nl, nl ],
    bsd2_license, [nl, nl],
    warn([ 'This program contains components covered by the GNU General', nl,
           'Public License, which therefore apply to the entire program.', nl,
           'These components are:', nl, nl
         ]),
    file_list(GPL).
license_message([], LGPL, _) -->
    !,
    [ 'SWI-Prolog is covered by the Simplified BSD license:', nl, nl ],
    bsd2_license, [nl, nl],
    warn([ 'This program contains components covered by the GNU Lesser', nl,
           'Public License.  Distribution of this program is subject to',  nl,
           'additional conditions.  These components are:', nl, nl
         ]),
    file_list(LGPL).


bsd2_license -->
    [ 'Redistribution and use in source and binary forms, with or without', nl,
      'modification, are permitted provided that the following conditions', nl,
      'are met:', nl,
      nl,
      '1. Redistributions of source code must retain the above copyright', nl,
      '   notice, this list of conditions and the following disclaimer.', nl,
      nl,
      '2. Redistributions in binary form must reproduce the above copyright', nl,
      '   notice, this list of conditions and the following disclaimer in', nl,
      '   the documentation and/or other materials provided with the', nl,
      '   distribution.', nl,
      nl,
      'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS', nl,
      '"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT', nl,
      'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS', nl,
      'FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE', nl,
      'COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,', nl,
      'INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,', nl,
      'BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;', nl,
      'LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER', nl,
      'CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT', nl,
      'LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN', nl,
      'ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE', nl,
      'POSSIBILITY OF SUCH DAMAGE.'
    ].

permissive_licenses([]) --> !.
permissive_licenses([LM| LMs]) -->
    [ nl, nl,
      'This program contains other components with permissive licenses:',
      nl, nl
    ],
    permissive([LM| LMs]).

permissive([]) --> [].
permissive([License-Modules|T]) -->
    license_title(License),
    license_url(License),
    [nl],
    file_list(Modules),
    (   {T==[]}
    ->  []
    ;   [nl],
        permissive(T)
    ).

proprietary_licenses([]) --> !.
proprietary_licenses(List) -->
    warn([ nl,
           'This program contains components with proprietary licenses:',
           nl, nl
         ]),
    proprietary(List).

proprietary([]) --> [].
proprietary([License-Modules|T]) -->
    license_title(License),
    license_url(License),
    [nl],
    file_list(Modules),
    (   {T==[]}
    ->  []
    ;   [nl],
        proprietary(T)
    ).

license_title(License) -->
    {   license(License, _, Att),
        memberchk(comment(C), Att)
    ->  true
    ;   C = License
    },
    [ '  The following components are covered by the "~w" license'-[C] ].

license_url(License) -->
    { license(License, _, Att),
      memberchk(url(URL), Att)
    },
    !,
    [ nl, '    (see ~w)'-[URL] ].
license_url(_) --> [].

file_list([]) -->
    [].
file_list([H|T]) -->
    [ '    ~w'-[H], nl ],
    file_list(T).

known_licenses([]) --> [].
known_licenses([H|T]) --> [nl,nl], known_license(H), known_licenses(T).

known_license(license(ID, Compat, Atts)) -->
    { memberchk(comment(Comment), Atts) },
    !,
    [ '  ~w (category ~w): ~w'-[ID, Compat, Comment] ],
    license_url(ID).
known_license(license(ID, Compat, _)) -->
    [ '  ~w (category ~w)'-[ID, Compat] ],
    license_url(ID).

warn([]) --> [].
warn([H|T]) --> warn1(H), warn(T).

warn1(nl) --> !, [nl].
warn1(Line) --> [ansi([fg(red)], Line, [])].
warn1(Line-Args) --> [ansi([fg(red)], Line, Args)].
