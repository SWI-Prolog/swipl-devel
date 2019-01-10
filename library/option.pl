/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2018, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(swi_option,
          [ option/2,                   % +Term, +List
            option/3,                   % +Term, +List, +Default
            select_option/3,            % +Term, +Options, -RestOpts
            select_option/4,            % +Term, +Options, -RestOpts, +Default
            merge_options/3,            % +New, +Old, -Merged
            meta_options/3,             % :IsMeta, :OptionsIn, -OptionsOut
            dict_options/2              % ?Dict, ?Options
          ]).
:- use_module(library(lists)).
:- use_module(library(error)).
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    meta_options(1, :, -).

/** <module> Option list processing

The library(option) provides some utilities for processing option lists.
Option lists are commonly used  as   an  alternative for many arguments.
Examples of built-in predicates are open/4  and write_term/3. Naming the
arguments results in more readable code, and   the  list nature makes it
easy to extend the list of options accepted by a predicate. Option lists
come in two styles, both of which are handled by this library.

        $ Name(Value) :
        This is the preferred style.

        $ Name = Value :
        This is often used, but deprecated.

Processing options inside time-critical code   (loops) can cause serious
overhead. One possibility is to define   a  record using library(record)
and initialise this using make_<record>/2. In addition to providing good
performance, this also provides type-checking and central declaration of
defaults.

  ==
  :- record atts(width:integer=100, shape:oneof([box,circle])=box).

  process(Data, Options) :-
          make_atts(Options, Attributes),
          action(Data, Attributes).

  action(Data, Attributes) :-
          atts_shape(Attributes, Shape),
          ...
  ==

Options typically have exactly one argument.   The  library does support
options  with  0  or  more  than    one   argument  with  the  following
restrictions:

  - The predicate option/3 and select_option/4, involving default are
    meaningless. They perform an arg(1, Option, Default), causing
    failure without arguments and filling only the first option-argument
    otherwise.
  - meta_options/3 can only qualify options with exactly one argument.

@tbd    We should consider putting many options in an assoc or record
        with appropriate preprocessing to achieve better performance.
@see    library(record)
@see    Option processing capabilities may be declared using the
        directive predicate_options/3.
*/

%!  option(?Option, +OptionList, +Default) is semidet.
%
%   Get  an  Option  from  OptionList.  OptionList  can  use  the
%   Name=Value as well as the Name(Value) convention.
%
%   @param Option   Term of the form Name(?Value).

option(Opt, Options, Default) :-
    is_dict(Options),
    !,
    functor(Opt, Name, 1),
    (   get_dict(Name, Options, Val)
    ->  true
    ;   Val = Default
    ),
    arg(1, Opt, Val).
option(Opt, Options, Default) :-        % make option processing stead-fast
    functor(Opt, Name, Arity),
    functor(GenOpt, Name, Arity),
    (   get_option(GenOpt, Options)
    ->  Opt = GenOpt
    ;   arg(1, Opt, Default)
    ).


%!  option(?Option, +OptionList) is semidet.
%
%   Get an Option from OptionList. OptionList can use the Name=Value
%   as well as the Name(Value)  convention.   Fails  silently if the
%   option does not appear in OptionList.
%
%   @param Option   Term of the form Name(?Value).

option(Opt, Options) :-                 % make option processing stead-fast
    is_dict(Options),
    !,
    functor(Opt, Name, 1),
    get_dict(Name, Options, Val),
    arg(1, Opt, Val).
option(Opt, Options) :-                 % make option processing stead-fast
    functor(Opt, Name, Arity),
    functor(GenOpt, Name, Arity),
    get_option(GenOpt, Options),
    !,
    Opt = GenOpt.

get_option(Opt, Options) :-
    memberchk(Opt, Options),
    !.
get_option(Opt, Options) :-
    functor(Opt, OptName, 1),
    arg(1, Opt, OptVal),
    memberchk(OptName=OptVal, Options),
    !.


%!  select_option(?Option, +Options, -RestOptions) is semidet.
%
%   Get and remove Option from an option list. As option/2, removing
%   the matching option from  Options   and  unifying  the remaining
%   options with RestOptions.

select_option(Opt, Options0, Options) :-
    is_dict(Options0),
    !,
    functor(Opt, Name, 1),
    get_dict(Name, Options0, Val),
    arg(1, Opt, Val),
    del_dict(Name, Options0, Val, Options).
select_option(Opt, Options0, Options) :-        % stead-fast
    functor(Opt, Name, Arity),
    functor(GenOpt, Name, Arity),
    get_option(GenOpt, Options0, Options),
    Opt = GenOpt.

get_option(Opt, Options0, Options) :-
    selectchk(Opt, Options0, Options),
    !.
get_option(Opt, Options0, Options) :-
    functor(Opt, OptName, 1),
    arg(1, Opt, OptVal),
    selectchk(OptName=OptVal, Options0, Options).

%!  select_option(?Option, +Options, -RestOptions, +Default) is det.
%
%   Get and remove Option with   default  value. As select_option/3,
%   but if Option is not  in  Options,   its  value  is unified with
%   Default and RestOptions with Options.

select_option(Option, Options, RestOptions, Default) :-
    is_dict(Options),
    !,
    functor(Option, Name, 1),
    (   del_dict(Name, Options, Val, RestOptions)
    ->  true
    ;   Val = Default,
        RestOptions = Options
    ),
    arg(1, Option, Val).
select_option(Option, Options, RestOptions, Default) :-
    functor(Option, Name, Arity),
    functor(GenOpt, Name, Arity),
    (   get_option(GenOpt, Options, RestOptions)
    ->  Option = GenOpt
    ;   RestOptions = Options,
        arg(1, Option, Default)
    ).


%!  merge_options(+New, +Old, -Merged) is det.
%
%   Merge two option lists. Merged is a sorted list of options using
%   the canonical format Name(Value) holding   all  options from New
%   and Old, after removing conflicting options from Old.
%
%   Multi-values options (e.g.,  proxy(Host,   Port))  are  allowed,
%   where both option-name and arity  define   the  identity  of the
%   option.

merge_options([], Old, Merged) :-
    !,
    canonicalise_options(Old, Merged).
merge_options(New, [], Merged) :-
    !,
    canonicalise_options(New, Merged).
merge_options(New, Old, Merged) :-
    canonicalise_options(New, NCanonical),
    canonicalise_options(Old, OCanonical),
    sort(NCanonical, NSorted),
    sort(OCanonical, OSorted),
    ord_merge(NSorted, OSorted, Merged).

ord_merge([], L, L) :- !.
ord_merge(L, [], L) :- !.
ord_merge([NO|TN], [OO|TO], Merged) :-
    sort_key(NO, NKey),
    sort_key(OO, OKey),
    compare(Diff, NKey, OKey),
    ord_merge(Diff, NO, NKey, OO, OKey, TN, TO, Merged).

ord_merge(=, NO, _, _, _, TN, TO, [NO|T]) :-
    ord_merge(TN, TO, T).
ord_merge(<, NO, _, OO, OKey, TN, TO, [NO|T]) :-
    (   TN = [H|TN2]
    ->  sort_key(H, NKey),
        compare(Diff, NKey, OKey),
        ord_merge(Diff, H, NKey, OO, OKey, TN2, TO, T)
    ;   T = [OO|TO]
    ).
ord_merge(>, NO, NKey, OO, _, TN, TO, [OO|T]) :-
    (   TO = [H|TO2]
    ->  sort_key(H, OKey),
        compare(Diff, NKey, OKey),
        ord_merge(Diff, NO, NKey, H, OKey, TN, TO2, T)
    ;   T = [NO|TN]
    ).

sort_key(Option, Name-Arity) :-
    functor(Option, Name, Arity).

%!  canonicalise_options(+OptionsIn, -OptionsOut) is det.
%
%   Rewrite option list from possible Name=Value to Name(Value)

canonicalise_options(Dict, Out) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, _, Pairs),
    canonicalise_options2(Pairs, Out).
canonicalise_options(In, Out) :-
    memberchk(_=_, In),            % speedup a bit if already ok.
    !,
    canonicalise_options2(In, Out).
canonicalise_options(Options, Options).

canonicalise_options2([], []).
canonicalise_options2([H0|T0], [H|T]) :-
    canonicalise_option(H0, H),
    canonicalise_options2(T0, T).

canonicalise_option(Name=Value, H) :-
    !,
    H =.. [Name,Value].
canonicalise_option(Name-Value, H) :-
    !,
    H =.. [Name,Value].
canonicalise_option(H, H).


%!  meta_options(+IsMeta, :Options0, -Options) is det.
%
%   Perform meta-expansion on  options   that  are module-sensitive.
%   Whether an option name  is   module-sensitive  is  determined by
%   calling call(IsMeta, Name). Here is an example:
%
%   ==
%           meta_options(is_meta, OptionsIn, Options),
%           ...
%
%   is_meta(callback).
%   ==
%
%   Meta-options must have exactly one  argument. This argument will
%   be qualified.
%
%   @tbd    Should be integrated with declarations from
%           predicate_options/3.

meta_options(IsMeta, Context:Options0, Options) :-
    is_dict(Options0),
    !,
    dict_pairs(Options0, Class, Pairs0),
    meta_options(Pairs0, IsMeta, Context, Pairs),
    dict_pairs(Options, Class, Pairs).
meta_options(IsMeta, Context:Options0, Options) :-
    must_be(list, Options0),
    meta_options(Options0, IsMeta, Context, Options).

meta_options([], _, _, []).
meta_options([H0|T0], IM, Context, [H|T]) :-
    meta_option(H0, IM, Context, H),
    meta_options(T0, IM, Context, T).

meta_option(Name=V0, IM, Context, Name=(M:V)) :-
    call(IM, Name),
    !,
    strip_module(Context:V0, M, V).
meta_option(Name-V0, IM, Context, Name-(M:V)) :-
    call(IM, Name),
    !,
    strip_module(Context:V0, M, V).
meta_option(O0, IM, Context, O) :-
    compound(O0),
    O0 =.. [Name,V0],
    call(IM, Name),
    !,
    strip_module(Context:V0, M, V),
    O =.. [Name,M:V].
meta_option(O, _, _, O).

%!  dict_options(?Dict, ?Options) is det.
%
%   Convert between an option list  and   a  dictionary.  One of the
%   arguments must be instantiated. If the   option list is created,
%   it is created in canonical form,  i.e., using Option(Value) with
%   the Options sorted in the standard order of terms. Note that the
%   conversion is not always possible   due to different constraints
%   and convertion may thus lead to (type) errors.
%
%     - Dict keys can be integers. This is not allowed in canonical
%       option lists.
%     - Options can hold multiple options with the same key. This is
%       not allowed in dicts.
%     - Options can have more than one value (name(V1,V2)).  This is
%       not allowed in dicts.
%
%   Also note that most system predicates  and predicates using this
%   library for processing the option argument   can  both work with
%   classical Prolog options and dicts objects.

dict_options(Dict, Options) :-
    nonvar(Dict),
    !,
    dict_pairs(Dict, _, Pairs),
    canonicalise_options2(Pairs, Options).
dict_options(Dict, Options) :-
    dict_create(Dict, _, Options).
