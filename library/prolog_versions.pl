/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(prolog_versions,
          [ require_prolog_version/2    % +Required:atom, +Features:list
          ]).
:- autoload(library(apply), [maplist/2, maplist/3]).

/** <module> Demand specific Prolog versions

This library is provided to make  it   easier  to  reason about software
versions, in particular require that that   hosting  Prolog system is of
the right version and provides the features   required by the library or
application.

@tbd Not only provide a minimal version but a more version ranges,
     exclude certain versions, etc.
@tbd More features and better messages to help the user resolving
     problems.
*/

%!  require_prolog_version(+Required:atom, +Features:list) is det.
%
%   Claim that the running Prolog version   is at least version Required
%   and provides the requested Features. Required   is an atom or string
%   that provides the version as
%
%	Major.Minor[[.Patch][[-GitRev],-GitHash]]]
%
%   Example     strings     are     '8.5',      '8.5.0',     '8.5.0-50',
%   '8.5.0-69-gad38e8ad8`. The last two  require   fetching  the sources
%   from git or using the Windows daily builds.
%
%   Features is a list of  required   or  preferred features. Individual
%   features are:
%
%     - warning(Feature)
%       Only print a warning instead of throwing an error.
%     - threaded
%       Demand support for multi-threading
%     - rational
%       Demand native support for rational numbers
%     - library(Lib)
%       Demand library(Lib) to be present.  Thde library not being
%       there may indicate an incomplete installation.  For example
%       library(pce) to demand xpce graphics support.
%     - Flag
%       Demand current_prolog_flag(Flag, true) to be true.
%     - FlagValue
%       If FlagValue is Flag(Value), demand current_prolog_flag(Flag,
%       Value) to be true.
%
%    @error prolog_version(Required, Found)
%    @error prolog_feature(Feature))

require_prolog_version(Required, Features) :-
    require_prolog_version(Required),
    maplist(check_feature, Features).

require_prolog_version(Required) :-
    parse_version(Required, ReqNumbers, GitReq),
    current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
    VNumbers = [Major, Minor, Patch],
    atomic_list_concat(VNumbers, '.', VAtom),
    (   ReqNumbers @< VNumbers
    ->  true
    ;   GitReq = git(ReqRev, ReqHash),
        ReqNumbers == VNumbers
    ->  (   current_prolog_flag(version_git, GitVersion)
        ->  parse_version(GitVersion, _, git(Rev, Hash)),
            (   ReqRev < Rev
            ->  true
            ;   ReqRev == Rev,
                (   (   ReqHash == '-' ;
                        ReqHash == Hash
                    )
                ->  true
                ;   prolog_version_error(Required, GitVersion)
                )
            ->  true
            ;   prolog_version_error(Required, GitVersion)
            )
        ;   prolog_version_error(Required, VAtom)
        )
    ;   ReqNumbers == VNumbers
    ->  true
    ;   prolog_version_error(Required, VAtom)
    ).

prolog_version_error(Required, Found) :-
    throw(error(prolog_version(Required, Found), _)).

parse_version(String, ReqNumbers, git(GitRev, GitHash)) :-
    split_string(String, "-", "", [NumberS,GitRevS|Hash]),
    !,
    split_string(NumberS, ".", "", List),
    maplist(number_string, ReqNumbers, List),
    number_string(GitRev, GitRevS),
    (   Hash = [HashS]
    ->  atom_string(GitHash, HashS)
    ;   GitHash = '-'
    ).
parse_version(String, ReqNumbers, -) :-
    split_string(String, ".", "", List),
    maplist(number_string, ReqNumbers, List).

check_feature(warning(Flag)) :-
    !,
    (   has_feature(Flag)
    ->  true
    ;   print_message(warning, error(prolog_feature(warning(Flag)), _))
    ).
check_feature(Flag) :-
    has_feature(Flag),
    !.
check_feature(Flag) :-
    throw(error(prolog_feature(Flag), _)).

has_feature(rational) =>
    current_prolog_flag(bounded, false).
has_feature(library(Lib)) =>
    exists_source(library(Lib)).
has_feature(Flag), atom(Flag) =>
    current_prolog_flag(Flag, true).
has_feature(Flag), Flag =.. [Name|Arg] =>
    current_prolog_flag(Name, Arg).

		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:error_message//1.

prolog:error_message(prolog_version(Required, Found)) -->
    { current_prolog_flag(executable, Exe) },
    [ 'Application requires at least SWI-Prolog ',
      ansi(code, '~p', [Required]), ',', nl, '   ',
      ansi(code, '~p', [Exe]), ' has version ',
      ansi(code, '~p', [Found])
    ].
prolog:error_message(prolog_feature(warning(Feature))) -->
    [ 'This version of SWI-Prolog does not optimally support your application because', nl,
      '   '
    ],
    missing_feature(Feature).
prolog:error_message(prolog_feature(Feature)) -->
    [ 'This version of SWI-Prolog cannot run your application because', nl,
      '   '
    ],
    missing_feature(Feature).

missing_feature(threads) -->
    [ 'multi-threading is not available' ].
missing_feature(rational) -->
    [ 'it has no support for rational numbers' ].
missing_feature(bounded(false)) -->
    [ 'it has no support for unbounded arithmetic' ].
missing_feature(library(Lib)) -->
    [ 'it does not provide library(~q)'-[Lib] ].
missing_feature(Feature) -->
    [ 'it does not support ~p'-[Feature] ].
