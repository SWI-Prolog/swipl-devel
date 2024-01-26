/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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
          [ require_prolog_version/2,   % +Required, +Features:list
            require_version/3,          % +Component, +Available, +Required
            cmp_versions/3              % ?Cmp, +Version1, +Version2
          ]).
:- autoload(library(apply), [maplist/2, maplist/3]).
:- autoload(library(error), [domain_error/2, existence_error/2, type_error/2]).

/** <module> Demand specific (Prolog) versions

This library is provided to make  it   easier  to  reason about software
versions, in particular require that that   hosting  Prolog system is of
the right version and provides the features   required by the library or
application.

@tbd Not only provide a minimal version but a more version ranges,
     exclude certain versions, etc.
@tbd More features and better messages to help the user resolving
     problems.
*/

%!  require_prolog_version(+Required, +Features:list) is det.
%
%   Claim that the running Prolog version   is at least version Required
%   and provides the requested Features. Required   is  an expression of
%   versions. At the lowest level, a version   is an atom or string that
%   provides the version as
%
%	Major.Minor[[.Patch][[-GitRev],-GitHash]]]
%
%   Example     strings     are     '8.5',      '8.5.0',     '8.5.0-50',
%   '8.5.0-69-gad38e8ad8`. The last two  require   fetching  the sources
%   from git or using the Windows daily builds.
%
%   Versions may be embedded in a   comparison operator (`<`, `=<`, `=`,
%   `>=` or `>`), e.g., `=<('9.1')`. Versions  are considered to compare
%   equal only on the components of  the Required version. I.e., `'9.1'`
%   compares equal to `'9.1.2'`.
%
%   Version expressions can be  constructed   from  the Prolog operators
%   ','/2, ';'/2 and '\+'/1. An example   of a complicated expression is
%   below, which demands major  version  9,   but  considers  9.1.2  not
%   suitable.
%
%       (>=('9'), \+(=('9.1.2')))
%
%   Features is a list of  required   or  preferred features. Individual
%   features are:
%
%     - warning(Feature)
%       Only print a warning instead of throwing an error.
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
%    @error version_error('SWI-Prolog', PrologVersion, Cmp, Required)
%    @error existence_error(prolog_feature, Feature)

require_prolog_version(Required, Features) :-
    require_prolog_version(Required),
    maplist(check_feature, Features).

require_prolog_version(Required) :-
    prolog_version(Available),
    require_version('SWI-Prolog', Available, Required).

prolog_version(Version) :-
    current_prolog_flag(version_git, Version),
    !.
prolog_version(Version) :-
    current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
    VNumbers = [Major, Minor, Patch],
    atomic_list_concat(VNumbers, '.', Version).

%!  require_version(+Component, +Available, +CmpRequired) is det.
%
%   Require Component to have version   CmpRequired,  while Component is
%   know to have version Available.
%
%   @error version_error(Component, Required, Cmp, Available)

require_version(Component, Available, CmpRequired) :-
    parse_version(Available, AvlNumbers, AvlGit),
    (   require_version_(AvlNumbers, AvlGit, CmpRequired)
    ->  true
    ;   throw(error(version_error(Component, Available, CmpRequired), _))
    ).

require_version_(AvlNumbers, AvlGit, (V1;V2)) =>
    (   require_version_(AvlNumbers, AvlGit, V1)
    ->  true
    ;   require_version_(AvlNumbers, AvlGit, V2)
    ).
require_version_(AvlNumbers, AvlGit, (V1,V2)) =>
    (   require_version_(AvlNumbers, AvlGit, V1)
    ;   require_version_(AvlNumbers, AvlGit, V2)
    ).
require_version_(AvlNumbers, AvlGit, \+V1) =>
    \+ require_version_(AvlNumbers, AvlGit, V1).
require_version_(AvlNumbers, AvlGit, Required) =>
    parse_version(Required,  ReqNumbers, ReqGit, Cmp, _),
    cmp_versions(Cmp, AvlNumbers, AvlGit, ReqNumbers, ReqGit).

%!  cmp_versions(?Cmp, +V1, +V2) is semidet.
%
%   Compare to versions. Cmp is one of `<`,   `=<`, `=`, `>=` or `>`. If
%   Cmp is unbound we check whether `<` or  `>` hold or else bind Cmp to
%   `=`.
%
%   When comparing for equality (`=`), the versions are considered equal
%   if they compare equal up to the detail level of the least specified.
%   E.g, '9.1.2' is considered equal to '9.1'.

cmp_versions(Cmp, V1, V2) :-
    parse_version(V1, V1_Numbers, V1_Git),
    parse_version(V2, V2_Numbers, V2_Git),
    (   nonvar(Cmp)
    ->  cmp_versions(Cmp, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ;   cmp_versions(<, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ->  Cmp = (<)
    ;   cmp_versions(>, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ->  Cmp = (<)
    ;   Cmp = (=)
    ).

cmp_versions(=<, V1_Numbers, V1_Git, V2_Numbers, V2_Git) =>
    (   cmp_versions(<, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ->  true
    ;   cmp_versions(=, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ).
cmp_versions(>=, V1_Numbers, V1_Git, V2_Numbers, V2_Git) =>
    (   cmp_versions(>, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ->  true
    ;   cmp_versions(=, V1_Numbers, V1_Git, V2_Numbers, V2_Git)
    ).
cmp_versions(<, V1_Numbers, V1_Git, V2_Numbers, V2_Git) =>
    (   cmp_num_version(<, V1_Numbers, V2_Numbers)
    ->  true
    ;   V1_Numbers == V2_Numbers,
        cmp_git_version(<, V1_Git, V2_Git)
    ).
cmp_versions(>, V1_Numbers, V1_Git, V2_Numbers, V2_Git) =>
    (   cmp_num_version(>, V1_Numbers, V2_Numbers)
    ->  true
    ;   V1_Numbers == V2_Numbers,
        cmp_git_version(>, V1_Git, V2_Git)
    ).
cmp_versions(=, V1_Numbers, V1_Git, V2_Numbers, V2_Git) =>
    cmp_num_version(=, V1_Numbers, V2_Numbers),
    cmp_git_version(=, V1_Git, V2_Git).

cmp_num_version(Cmp, V1_Numbers, V2_Numbers) :-
    shortest(V1_Numbers, V2_Numbers, V1, V2),
    compare(Cmp, V1, V2).

shortest([H1|T1], [H2|T2], [H1|R1], [H2|R2]) :-
    !,
    shortest(T1, T2, R1, R2).
shortest(_,_, [], []).


cmp_git_version(<, -, -) => fail.
cmp_git_version(>, -, -) => fail.
cmp_git_version(=, -, -) => true.
cmp_git_version(<, _, -) => true.
cmp_git_version(>, -, _) => true.
cmp_git_version(=, -, _) => true.
cmp_git_version(=, _, -) => true.
cmp_git_version(=, git(V,-), git(V,_)) => true.
cmp_git_version(=, git(V,_), git(V,-)) => true.
cmp_git_version(<, git(V1, _V1_Hash), git(V2, _V2_Hash)) =>
    V1 < V2.
cmp_git_version(>, git(V1, _V1_Hash), git(V2, _V2_Hash)) =>
    V1 > V2.
cmp_git_version(=, V1, V2) => V1 == V2.

%!  parse_version(+Spec, -Version:list(integer), -GIT, -Cmp, -Plain) is det.

parse_version(Spec, VNumbers, GitVersion, Cmp, VString) :-
    spec_cmp_version(Spec, Cmp, VString),
    parse_version(VString, VNumbers, GitVersion).

spec_cmp_version(Spec, Cmp, Version),
    compound(Spec), compound_name_arity(Spec, Cmp, 1) =>
    (   is_cmp(Cmp)
    ->  true
    ;   domain_error(comparison_operator, Cmp)
    ),
    arg(1, Spec, Version).
spec_cmp_version(Spec, Cmp, Version), atom(Spec) =>
    Cmp = (>=),
    Version = Spec.
spec_cmp_version(Spec, Cmp, Version), string(Spec) =>
    Cmp = (>=),
    atom_string(Version, Spec).
spec_cmp_version(Spec, _Cmp, _Version) =>
    type_error(version, Spec).

is_cmp(=<).
is_cmp(<).
is_cmp(>=).
is_cmp(>).
is_cmp(=).
is_cmp(>=).

parse_version(String, VNumbers, VGit) :-
    (   parse_version_(String, VNumbers, VGit)
    ->  true
    ;   domain_error(version_string, String)
    ).

parse_version_(String, VNumbers, git(GitRev, GitHash)) :-
    split_string(String, "-", "", [NumberS,GitRevS|Hash]),
    !,
    split_string(NumberS, ".", "", List),
    maplist(number_string, VNumbers, List),
    (   GitRevS == "DIRTY"
    ->  GitRev = 0,
        GitHash = 'DIRTY'
    ;   number_string(GitRev, GitRevS),
        (   Hash = [HashS]
        ->  atom_string(GitHash, HashS)
        ;   GitHash = '-'
        )
    ).
parse_version_(String, VNumbers, -) :-
    split_string(String, ".", "", List),
    maplist(number_string, VNumbers, List).

%!  check_feature(+Feature) is det.
%
%   Verify that the running Prolog process has a certain feature.

check_feature(warning(Flag)) :-
    !,
    (   has_feature(Flag)
    ->  true
    ;   print_message(
            warning,
            error(existence_error(prolog_feature, warning(Flag)), _))
    ).
check_feature(Flag) :-
    has_feature(Flag),
    !.
check_feature(Flag) :-
    existence_error(prolog_feature, Flag).

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

prolog:error_message(version_error(Component, Found, Required)) -->
    { current_prolog_flag(executable, Exe) },
    [ 'Application requires ~w '-[Component] ], req_msg(Required),
    [ ',', nl, '   ',
      ansi(code, '~w', [Exe]), ' has version ',
      ansi(code, '~w', [Found])
    ].
prolog:error_message(existence_error(prolog_feature, Feature)) -->
    missing_feature(Feature).

req_msg((A,B)) --> req_msg(A), [' and '], req_msg(B).
req_msg((A;B)) --> req_msg(A), [' or '], req_msg(B).
req_msg(\+(A)) --> ['not '], req_msg(A).
req_msg(V) --> { spec_cmp_version(V, Cmp, Version) }, !, cmp_msg(Cmp), [' '],
    [ ansi(code, '~w', [Version]) ].

cmp_msg(<)  --> ['before'].
cmp_msg(=<) --> ['at most'].
cmp_msg(=)  --> ['exactly'].
cmp_msg(>=) --> ['at least'].
cmp_msg(>)  --> ['after'].

missing_feature(warning(Feature)) -->
    [ 'This version of SWI-Prolog does not optimally support your \c
       application because',
      nl, '   '
    ],
    missing_feature_(Feature).
missing_feature(warning(Feature)) -->
    [ 'This version of SWI-Prolog cannot run your application because',
      nl, '   '
    ],
    missing_feature_(Feature).

missing_feature_(threads) -->
    [ 'multi-threading is not available' ].
missing_feature_(rational) -->
    [ 'it has no support for rational numbers' ].
missing_feature_(bounded(false)) -->
    [ 'it has no support for unbounded arithmetic' ].
missing_feature_(library(Lib)) -->
    [ 'it does not provide library(~q)'-[Lib] ].
missing_feature_(Feature) -->
    [ 'it does not support ~p'-[Feature] ].
