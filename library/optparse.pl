/*  Part of SWI-Prolog

    Author:        Marcus Uneson
    E-mail:        marcus.uneson@ling.lu.se
    WWW:           http://person.sol.lu.se/MarcusUneson/
    Copyright (c)  2011-2015, Marcus Uneson
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

:- module(optparse,
    [  opt_parse/5,     %+OptsSpec, +CLArgs, -Opts, -PositionalArgs,-ParseOptions
       opt_parse/4,     %+OptsSpec, +CLArgs, -Opts, -PositionalArgs,
       opt_arguments/3, %+OptsSpec, -Opts, -PositionalArgs
       opt_help/2       %+OptsSpec, -Help
    ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(error)).
:- set_prolog_flag(double_quotes, codes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXPORTS

/** <module> command line parsing

This  module  helps  in  building  a    command-line   interface  to  an
application. In particular, it provides functions   that  take an option
specification and a list of atoms, probably  given to the program on the
command line, and  return  a  parsed   representation  (a  list  of  the
customary Key(Val) by default; or optionally,   a list of Func(Key, Val)
terms in the style of current_prolog_flag/2).   It can also synthesize a
simple help text from the options specification.

The terminology in the following  is   partly  borrowed from python, see
http://docs.python.org/library/optparse.html#terminology . Very briefly,
_arguments_ is what you provide on the command line and for many prologs
show up as a  list  of   atoms  =|Args|=  in =|current_prolog_flag(argv,
Args)|=. For a typical prolog incantation, they can be divided into

    * _|runtime arguments|_, which controls the prolog runtime;
    conventionally, they are ended by '--';
    * _options_, which are key-value pairs (with a boolean value
    possibly implicit) intended to control your program in one way
    or another; and
    * _|positional arguments|_, which is what remains after
    all runtime arguments and options have been removed (with
    implicit arguments -- true/false for booleans -- filled in).

Positional arguments are in  particular   used  for  mandatory arguments
without which your program  won't  work  and   for  which  there  are no
sensible defaults (e.g,, input file names).  Options, by contrast, offer
flexibility by letting  you  change  a   default  setting.  Options  are
optional not only by etymology: this library  has no notion of mandatory
or required options (see  the  python   docs  for  other rationales than
laziness).

The command-line arguments enter your program as   a  list of atoms, but
the programs perhaps expects booleans, integers,   floats or even prolog
terms. You tell the parser so by providing an _|options specification|_.
This is just a list of individual   option specifications. One of those,
in turn, is a list of ground   prolog terms in the customary Name(Value)
format. The following terms are recognized (any others raise error).

        * opt(Key)
        Key is what the option later will be accessed by, just like for
        current_prolog_flag(Key, Value). This term is mandatory (an error is
        thrown if missing).

        * shortflags(ListOfFlags)
        ListOfFlags denotes any single-dashed, single letter args specifying the
        current option (=|-s , -K|=, etc). Uppercase letters must be quoted.
        Usually ListOfFlags will be a singleton list, but sometimes aliased flags
        may be convenient.

        * longflags(ListOfFlags)
        ListOfFlags denotes any double-dashed arguments specifying
        the current option (=|--verbose, --no-debug|=, etc). They are
        basically a more readable alternative to short flags, except

        1. long flags can be specified as =|--flag value|= or
        =|--flag=value|= (but not as =|--flagvalue|=); short flags as
        =|-f val|= or =|-fval|= (but not =|-f=val|=)
        2. boolean long flags can be specified as =|--bool-flag|=
        or =|--bool-flag=true|= or =|--bool-flag true|=; and they can be
        negated as =|--no-bool-flag|= or =|--bool-flag=false|= or
        =|--bool-flag false|=.

        Except that shortflags must be single characters, the
        distinction between long and short is in calling convention, not
        in namespaces. Thus, if you have shortflags([v]), you can use it
        as =|-v2|= or =|-v 2|= or =|--v=2|= or =|--v 2|= (but not
        =|-v=2|= or =|--v2|=).

        Shortflags and longflags both default to =|[]|=. It can be useful to
        have flagless options -- see example below.

        * meta(Meta)
        Meta is optional and only relevant for the synthesized usage message
        and is the name (an atom) of the metasyntactic variable (possibly)
        appearing in it together with type and default value (e.g,
        =|x:integer=3|=, =|interest:float=0.11|=). It may be useful to
        have named variables (=|x|=, =|interest|=) in case you wish to
        mention them again in the help text. If not given the =|Meta:|=
        part is suppressed -- see example below.

        * type(Type)
        Type is one of =|boolean, atom, integer, float, term|=.
        The corresponding argument will be parsed appropriately. This
        term is optional; if not given, defaults to =|term|=.

        * default(Default)
        Default value. This term is optional; if not given, or if given the
        special value '_', an uninstantiated variable is created (and any
        type declaration is ignored).

        * help(Help)
        Help is (usually) an atom of text describing the option in the
        help text. This term is optional (but obviously strongly recommended
        for all options which have flags).

        Long lines are subject to basic word wrapping -- split on white
        space, reindent, rejoin. However, you can get more control by
        supplying the line breaking yourself: rather than a single line of
        text, you can provide a list of lines (as atoms). If you do, they
        will be joined with the appropriate indent but otherwise left
        untouched (see the option =mode= in the example below).

Absence of mandatory option specs or the presence of more than one for a
particular option throws an error, as do unknown or incompatible types.

As a concrete example from a fictive   application,  suppose we want the
following options to be read from the  command line (long flag(s), short
flag(s), meta:type=default, help)

==
--mode                  -m     atom=SCAN       data gathering mode,
                                               one of
                                                SCAN: do this
                                                READ: do that
                                                MAKE: make numbers
                                                WAIT: do nothing
--rebuild-cache         -r     boolean=true    rebuild cache in
                                               each iteration
--heisenberg-threshold  -t,-h  float=0.1       heisenberg threshold
--depths, --iters       -i,-d  K:integer=3     stop after K
                                               iterations
--distances                    term=[1,2,3,5]  initial prolog term
--output-file           -o     FILE:atom=_     write output to FILE
--label                 -l     atom=REPORT     report label
--verbosity             -v     V:integer=2     verbosity level,
                                               1 <= V <= 3
==

We may also have some configuration  parameters which we currently think
not   needs   to   be   controlled   from    the   command   line,   say
path('/some/file/path').

This interface is  described  by   the  following  options specification
(order between the specifications of a particular option is irrelevant).

==
ExampleOptsSpec =
    [ [opt(mode    ), type(atom), default('SCAN'),
        shortflags([m]),   longflags(['mode'] ),
        help([ 'data gathering mode, one of'
             , '  SCAN: do this'
             , '  READ: do that'
             , '  MAKE: fabricate some numbers'
             , '  WAIT: don''t do anything'])]

    , [opt(cache), type(boolean), default(true),
        shortflags([r]),   longflags(['rebuild-cache']),
        help('rebuild cache in each iteration')]

    , [opt(threshold), type(float), default(0.1),
        shortflags([t,h]),  longflags(['heisenberg-threshold']),
        help('heisenberg threshold')]

    , [opt(depth), meta('K'), type(integer), default(3),
        shortflags([i,d]),longflags([depths,iters]),
        help('stop after K iterations')]

    , [opt(distances), default([1,2,3,5]),
        longflags([distances]),
        help('initial prolog term')]

    , [opt(outfile), meta('FILE'), type(atom),
        shortflags([o]),  longflags(['output-file']),
        help('write output to FILE')]

    , [opt(label), type(atom), default('REPORT'),
        shortflags([l]), longflags([label]),
        help('report label')]

    , [opt(verbose),  meta('V'), type(integer), default(2),
        shortflags([v]),  longflags([verbosity]),
        help('verbosity level, 1 <= V <= 3')]

    , [opt(path), default('/some/file/path/')]
    ].
==

The  help  text  above  was   accessed  by  =|opt_help(ExamplesOptsSpec,
HelpText)|=. The options appear in the same order as in the OptsSpec.

Given  =|ExampleOptsSpec|=,  a  command   line  (somewhat  syntactically
inconsistent, in order to demonstrate different calling conventions) may
look as follows

==
ExampleArgs = [ '-d5'
              , '--heisenberg-threshold', '0.14'
              , '--distances=[1,1,2,3,5,8]'
              , '--iters', '7'
              , '-ooutput.txt'
              , '--rebuild-cache', 'true'
              , 'input.txt'
              , '--verbosity=2'
              ].
==

opt_parse(ExampleOptsSpec, ExampleArgs, Opts, PositionalArgs) would then
succeed with

==
Opts =    [ mode('SCAN')
          , label('REPORT')
          , path('/some/file/path')
          , threshold(0.14)
          , distances([1,1,2,3,5,8])
          , depth(7)
          , outfile('output.txt')
          , cache(true)
          , verbose(2)
          ],
PositionalArgs = ['input.txt'].
==

Note that path('/some/file/path') showing up in Opts has a default value
(of the implicit type 'term'), but   no corresponding flags in OptsSpec.
Thus it can't be set from the  command   line.  The rest of your program
doesn't need to know that, of course.   This  provides an alternative to
the common practice of asserting  such   hard-coded  parameters  under a
single predicate (for instance   setting(path, '/some/file/path')), with
the advantage that you  may  seamlessly   upgrade  them  to command-line
options, should you  one  day  find  this   a  good  idea.  Just  add an
appropriate flag or two and a line  of help text. Similarly, suppressing
an option in a cluttered interface amounts to commenting out the flags.

opt_parse/5 allows more control through an   additional argument list as
shown in the example below.

==
?- opt_parse(ExampleOptsSpec, ExampleArgs,  Opts, PositionalArgs,
             [ output_functor(appl_config)
             ]).

Opts =    [ appl_config(verbose, 2),
          , appl_config(label, 'REPORT')
          ...
          ]
==

This representation may be preferable  with the empty-flag configuration
parameter style above (perhaps with asserting appl_config/2).

## Notes and tips {#optparse-notes}

    * In the example we were mostly explicit about the types. Since the
    default is =|term|=, which subsumes =|integer, float, atom|=, it
    may be possible to get away cheaper (e.g., by only giving booleans).
    However, it is recommended practice to always specify types:
    parsing becomes more reliable and error messages will be easier to interpret.


    * Note that =|-sbar|= is taken to mean =|-s bar|=, not =|-s -b -a -r|=,
    that is, there is no clustering of flags.

    * =|-s=foo|= is disallowed. The rationale is that although some
    command-line parsers will silently interpret this as =|-s =foo|=, this is very
    seldom what you want. To have an option argument start with '=' (very
    un-recommended), say so explicitly.

    * The example specifies the option =|depth|= twice: once as
    =|-d5|= and once as =|--iters 7|=. The default when encountering duplicated
    flags is to =|keeplast|= (this behaviour can be controlled, by ParseOption
    duplicated_flags).

    * The order of the options returned by the parsing functions is the same as
    given on the command
    line, with non-overridden defaults prepended and duplicates removed
    as in previous item. You should not rely on this, however.

    * Unknown flags (not appearing in OptsSpec) will throw errors. This
    is usually a Good Thing. Sometimes, however, you may wish to pass
    along flags to an external program (say, one called by shell/2), and
    it means duplicated effort and a maintenance headache to have to
    specify all possible flags for the external program explicitly (if
    it even can be done). On the other hand, simply taking all unknown
    flags as valid makes error checking much less efficient and
    identification of positional arguments uncertain. A better solution
    is to collect all arguments intended for passing along to an
    indirectly called program as a single argument, probably as an atom
    (if you don't need to inspect them first) or as a prolog term (if
    you do).

@author Marcus Uneson
@version 0.20 (2011-04-27)
@tbd: validation? e.g, numbers; file path existence; one-out-of-a-set-of-atoms
*/

:- predicate_options(opt_parse/5, 5,
                     [ allow_empty_flag_spec(boolean),
                       duplicated_flags(oneof([keepfirst,keeplast,keepall])),
                       output_functor(atom),
                       suppress_empty_meta(boolean)
                     ]).

:- multifile
    error:has_type/2,
    parse_type/3.

%%   opt_arguments(+OptsSpec, -Opts, -PositionalArgs) is det
%
%    Extract  commandline  options   according    to   a  specification.
%    Convenience predicate, assuming that command-line  arguments can be
%    accessed by current_prolog_flag/2 (as  in   swi-prolog).  For other
%    access mechanisms and/or more control, get   the args and pass them
%    as a list of atoms to opt_parse/4 or opt_parse/5 instead.
%
%    Opts is a list of parsed  options   in  the form Key(Value). Dashed
%    args not in OptsSpec are not permitted   and  will raise error (see
%    tip on how to  pass  unknown   flags  in  the  module description).
%    PositionalArgs are the remaining non-dashed   args  after each flag
%    has taken its argument (filling in =true= or =false= for booleans).
%    There are no restrictions on non-dashed   arguments and they may go
%    anywhere (although it is  good  practice   to  put  them last). Any
%    leading arguments for the runtime (up   to  and including '--') are
%    discarded.

opt_arguments(OptsSpec, Opts, PositionalArgs) :-
    current_prolog_flag(argv, Argv),
    opt_parse(OptsSpec, Argv, Opts, PositionalArgs).

%%   opt_parse(+OptsSpec, +ApplArgs, -Opts, -PositionalArgs) is det
%
%    Equivalent to opt_parse(OptsSpec, ApplArgs, Opts, PositionalArgs, []).


opt_parse(OptsSpec, ApplArgs, Opts, PositionalArgs) :-
    opt_parse(OptsSpec, ApplArgs, Opts, PositionalArgs, []).

%%   opt_parse(+OptsSpec, +ApplArgs, -Opts, -PositionalArgs, +ParseOptions) is det
%
%    Parse the arguments Args (as list  of atoms) according to OptsSpec.
%    Any runtime arguments (typically terminated by '--') are assumed to
%    be removed already.
%
%    Opts is a list of parsed options   in the form Key(Value), or (with
%    the option functor(Func)  given)  in   the  form  Func(Key, Value).
%    Dashed args not in OptsSpec are not  permitted and will raise error
%    (see tip on how to pass unknown   flags in the module description).
%    PositionalArgs are the remaining non-dashed   args  after each flag
%    has taken its argument (filling in =true= or =false= for booleans).
%    There are no restrictions on non-dashed   arguments and they may go
%    anywhere  (although  it  is  good  practice   to  put  them  last).
%    ParseOptions are
%
%    * output_functor(Func)
%      Set the functor Func of the returned options Func(Key,Value).
%      Default is the special value 'OPTION' (upper-case), which makes
%      the returned options have form Key(Value).
%
%    * duplicated_flags(Keep)
%      Controls how to handle options given more than once on the commad line.
%      Keep is one of  =|keepfirst, keeplast, keepall|= with the obvious meaning.
%      Default is =|keeplast|=.
%
%    * allow_empty_flag_spec(Bool)
%      If true (default), a flag specification is not required (it is allowed
%      that both shortflags and longflags be either [] or absent).
%      Flagless options cannot be manipulated from the command line
%      and will not show up in the generated help. This is useful when you
%      have (also) general configuration parameters in
%      your OptsSpec, especially if you think they one day might need to be
%      controlled externally. See example in the module overview.
%      allow_empty_flag_spec(false) gives the more customary behaviour of
%      raising error on empty flags.


opt_parse(OptsSpec, ApplArgs, Opts, PositionalArgs, ParseOptions) :-
    opt_parse_(OptsSpec, ApplArgs, Opts, PositionalArgs, ParseOptions).


%%   opt_help(+OptsSpec, -Help:atom) is det
%
%    True when Help is a help string synthesized from OptsSpec.

opt_help(OptsSpec, Help) :-
    opt_help(OptsSpec, Help, []).

% semi-arbitrary default format settings go here;
% if someone needs more control one day, opt_help/3 could be exported
opt_help(OptsSpec, Help, HelpOptions0) :-
    Defaults = [ line_width(80)
               , min_help_width(40)
               , break_long_flags(false)
               , suppress_empty_meta(true)
               ],
    merge_options(HelpOptions0, Defaults, HelpOptions),
    opt_help_(OptsSpec, Help, HelpOptions).


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPT_PARSE

opt_parse_(OptsSpec0, Args0, Opts, PositionalArgs, ParseOptions) :-
    must_be(list(atom), Args0),

    check_opts_spec(OptsSpec0, ParseOptions, OptsSpec),

    maplist(atom_codes, Args0, Args1),
    parse_options(OptsSpec, Args1, Args2, PositionalArgs),
    add_default_opts(OptsSpec, Args2, Args3),

    option(duplicated_flags(Keep), ParseOptions, keeplast),
    remove_duplicates(Keep, Args3, Args4),

    option(output_functor(Func), ParseOptions, 'OPTION'),
    refunctor_opts(Func, Args4, Opts). %}}}



%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAKE HELP
opt_help_(OptsSpec0, Help, HelpOptions) :-
    check_opts_spec(OptsSpec0, HelpOptions, OptsSpec1),
    include_in_help(OptsSpec1, OptsSpec2),
    format_help_fields(OptsSpec2, OptsSpec3),
    col_widths(OptsSpec3, [shortflags, metatypedef], CWs),
    long_flag_col_width(OptsSpec3, LongestFlagWidth),
    maplist(format_opt(LongestFlagWidth, CWs, HelpOptions), OptsSpec3, Lines),
    atomic_list_concat(Lines, Help).

include_in_help([], []).
include_in_help([OptSpec|OptsSpec], Result) :-
    (  flags(OptSpec, [_|_])
    -> Result = [OptSpec|Rest]
    ;  Result = Rest
    ),
    include_in_help(OptsSpec, Rest).

format_help_fields(OptsSpec0, OptsSpec) :-
    maplist(embellish_flag(short), OptsSpec0, OptsSpec1),
    maplist(embellish_flag(long), OptsSpec1, OptsSpec2),
    maplist(merge_meta_type_def, OptsSpec2, OptsSpec).

merge_meta_type_def(OptSpecIn, [metatypedef(MTD)|OptSpecIn]) :-
    memberchk(meta(Meta), OptSpecIn),
    memberchk(type(Type), OptSpecIn),
    memberchk(default(Def), OptSpecIn),
    atom_length(Meta, N),
    (  N > 0
    -> format(atom(MTD), '~w:~w=~w', [Meta, Type, Def])
    ;  format(atom(MTD), '~w=~w', [Type, Def])
    ).
embellish_flag(short, OptSpecIn, OptSpecOut) :-
    memberchk(shortflags(FlagsIn), OptSpecIn),
    maplist(atom_concat('-'), FlagsIn, FlagsOut0),
    atomic_list_concat(FlagsOut0, ',',  FlagsOut),
    merge_options([shortflags(FlagsOut)], OptSpecIn, OptSpecOut).
embellish_flag(long, OptSpecIn, OptSpecOut) :-
    memberchk(longflags(FlagsIn), OptSpecIn),
    maplist(atom_concat('--'), FlagsIn, FlagsOut),
    merge_options([longflags(FlagsOut)], OptSpecIn, OptSpecOut).

col_widths(OptsSpec, Functors, ColWidths) :-
    maplist(col_width(OptsSpec), Functors, ColWidths).
col_width(OptsSpec, Functor, ColWidth) :-
    findall(N,
            ( member(OptSpec, OptsSpec),
              M =.. [Functor, Arg],
              member(M, OptSpec),
              format(atom(Atom), '~w', [Arg]),
              atom_length(Atom, N0),
              N is N0 + 2     %separate cols with two spaces
            ),
            Ns),
    max_list([0|Ns], ColWidth).

long_flag_col_width(OptsSpec, ColWidth) :-
    findall(FlagLength,
           ( member(OptSpec, OptsSpec),
             memberchk(longflags(LFlags), OptSpec),
             member(LFlag, LFlags),
             atom_length(LFlag, FlagLength)
             ),
            FlagLengths),
    max_list([0|FlagLengths], ColWidth).


format_opt(LongestFlagWidth, [SFlagsCW, MTDCW], HelpOptions, Opt, Line) :-
    memberchk(shortflags(SFlags), Opt),

    memberchk(longflags(LFlags0), Opt),
    group_length(LongestFlagWidth, LFlags0, LFlags1),
    LFlagsCW is LongestFlagWidth + 2, %separate with comma and space
    option(break_long_flags(BLF), HelpOptions, true),
    (  BLF
    -> maplist(atomic_list_concat_(',\n'), LFlags1, LFlags2)
    ;  maplist(atomic_list_concat_(', '), LFlags1, LFlags2)
    ),
    atomic_list_concat(LFlags2, ',\n', LFlags),

    memberchk(metatypedef(MetaTypeDef), Opt),

    memberchk(help(Help), Opt),
    HelpIndent is LFlagsCW + SFlagsCW + MTDCW + 2,
    option(line_width(LW), HelpOptions, 80),
    option(min_help_width(MHW), HelpOptions, 40),
    HelpWidth is max(MHW, LW - HelpIndent),
    (  atom(Help)
    -> line_breaks(Help, HelpWidth, HelpIndent, BrokenHelp)
    ;  assertion(is_list_of_atoms(Help))
    -> indent_lines(Help, HelpIndent, BrokenHelp)
    ),
    format(atom(Line), '~w~t~*+~w~t~*+~w~t~*+~w~n',
      [LFlags, LFlagsCW, SFlags, SFlagsCW, MetaTypeDef, MTDCW, BrokenHelp]).


line_breaks(TextLine, LineLength, Indent, TextLines) :-
    atomic_list_concat(Words, ' ', TextLine),
    group_length(LineLength, Words, Groups0),
    maplist(atomic_list_concat_(' '), Groups0, Groups),
    indent_lines(Groups, Indent, TextLines).

indent_lines(Lines, Indent, TextLines) :-
    format(atom(Separator), '~n~*|', [Indent]),
    atomic_list_concat(Lines, Separator, TextLines).

atomic_list_concat_(Separator, List, Atom) :-
    atomic_list_concat(List, Separator, Atom).

%group_length(10,
%             [here, are, some, words, you, see],
%             [[here are], [some words], [you see]]) %each group >= 10F
group_length(LineLength, Words, Groups) :-
    group_length_(Words, LineLength, LineLength, [], [], Groups).

group_length_([], _, _, ThisLine, GroupsAcc, Groups) :-
    maplist(reverse, [ThisLine|GroupsAcc], GroupsAcc1),
    reverse(GroupsAcc1, Groups).
group_length_([Word|Words], LineLength, Remains, ThisLine, Groups, GroupsAcc) :-
    atom_length(Word, K),
    (  (Remains >= K; ThisLine = [])  %Word fits on ThisLine, or too long too fit
    -> Remains1 is Remains - K - 1,  %even on a new line
     group_length_(Words, LineLength, Remains1, [Word|ThisLine], Groups, GroupsAcc)

                                     %Word doesn't fit on ThisLine (non-empty)
    ;  group_length_([Word|Words], LineLength, LineLength, [], [ThisLine|Groups], GroupsAcc)
    ).


%}}}


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPTSSPEC DEFAULTS


add_default_defaults(OptsSpec0, OptsSpec, Options) :-
    option(suppress_empty_meta(SEM), Options, true),
    maplist(default_defaults(SEM), OptsSpec0, OptsSpec).

default_defaults(SuppressEmptyMeta, OptSpec0, OptSpec) :-
    (  SuppressEmptyMeta
    -> Meta = ''
    ;  memberchk(type(Type), OptSpec0)
    -> meta_placeholder(Type, Meta)
    ;  Meta = 'T'
    ),

    Defaults = [ help('')
             , type(term)
             , shortflags([])
             , longflags([])
             , default('_')
             , meta(Meta)
             ],
    merge_options(OptSpec0, Defaults, OptSpec).
    %merge_options(+New, +Old, -Merged)


meta_placeholder(boolean, 'B').
meta_placeholder(atom, 'A').
meta_placeholder(float, 'F').
meta_placeholder(integer, 'I').
meta_placeholder(term, 'T').



%}}}


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPTSSPEC VALIDATION

%this is a bit paranoid, but OTOH efficiency is no issue
check_opts_spec(OptsSpec0, Options, OptsSpec) :-
    validate_opts_spec(OptsSpec0, Options),
    add_default_defaults(OptsSpec0, OptsSpec, Options),
    validate_opts_spec(OptsSpec, Options).

validate_opts_spec(OptsSpec, ParseOptions) :-
    \+ invalidate_opts_spec(OptsSpec, ParseOptions).

invalidate_opts_spec(OptsSpec, _ParseOptions) :-
    %invalid if not ground -- must go first for \+ to be sound
    ( \+ ground(OptsSpec)
    -> throw(error(instantiation_error,
                   context(validate_opts_spec/1, 'option spec must be ground')))

    %invalid if conflicting flags
    ; ( member(O1, OptsSpec), flags(O1, Flags1), member(F, Flags1),
        member(O2, OptsSpec), flags(O2, Flags2), member(F, Flags2),
        O1 \= O2)
    -> throw(error(domain_error(unique_atom, F),
                   context(validate_opts_spec/1, 'ambiguous flag')))

    %invalid if unknown opt spec
    ; ( member(OptSpec, OptsSpec),
        member(Spec, OptSpec),
        functor(Spec, F, _),
        \+ member(F, [opt, shortflags, longflags, type, help, default, meta]) )
    ->  throw(error(domain_error(opt_spec, F),
                   context(validate_opts_spec/1, 'unknown opt spec')))

    %invalid if mandatory option spec opt(ID) is not unique in the entire Spec
    ; ( member(O1, OptsSpec), member(opt(Name), O1),
        member(O2, OptsSpec), member(opt(Name), O2),
        O1 \= O2)
    -> throw(error(domain_error(unique_atom, Name),
                   context(validate_opts_spec/1, 'ambiguous id')))
    ).

invalidate_opts_spec(OptsSpec, _ParseOptions) :-
    member(OptSpec, OptsSpec),
    \+ member(opt(_Name), OptSpec),
    %invalid if mandatory option spec opt(ID) is absent
    throw(error(domain_error(unique_atom, OptSpec),
                context(validate_opts_spec/1, 'opt(id) missing'))).

invalidate_opts_spec(OptsSpec, ParseOptions) :-
    member(OptSpec, OptsSpec), %if we got here, OptSpec has a single unique Name
    member(opt(Name), OptSpec),

    option(allow_empty_flag_spec(AllowEmpty), ParseOptions, true),

    %invalid if allow_empty_flag_spec(false) and no flag is given
    ( (\+ AllowEmpty, \+ flags(OptSpec, [_|_]))
    -> format(atom(Msg), 'no flag specified for option ''~w''', [Name]),
       throw(error(domain_error(unique_atom, _),
                context(validate_opts_spec/1, Msg)))

    %invalid if any short flag is not actually single-letter
    ; ( memberchk(shortflags(Flags), OptSpec),
        member(F, Flags),
        atom_length(F, L),
        L > 1)
    ->  format(atom(Msg), 'option ''~w'': flag too long to be short', [Name]),
        throw(error(domain_error(short_flag, F),
                context(validate_opts_spec/1, Msg)))

    %invalid if any option spec is given more than once
    ; duplicate_optspec(OptSpec,
      [type,opt,default,help,shortflags,longflags,meta])
    ->  format(atom(Msg), 'duplicate spec in option ''~w''', [Name]),
        throw(error(domain_error(unique_functor, _),
                context(validate_opts_spec/1, Msg)))

    %invalid if unknown type
    ;   (   memberchk(type(Type), OptSpec),
            Type \== term,
            \+ clause(error:has_type(Type,_), _)
        )
    ->  format(atom(Msg), 'unknown type ''~w'' in option ''~w''', [Type, Name]),
        throw(error(type_error(flag_value, Type),
              context(validate_opts_spec/1, Msg)))

    %invalid if type does not match default
    %note1: reverse logic: we are trying to _in_validate OptSpec

    %note2: 'term' approves of any syntactically valid prolog term, since
    %if syntactically invalid, OptsSpec wouldn't have parsed

    %note3: the special placeholder '_' creates a new variable, so no typecheck
    ;    (memberchk(type(Type), OptSpec),
          Type \= term,
          memberchk(default(Default), OptSpec),
          Default \= '_'
    ->   \+ must_be(Type, Default))

    %invalidation failed, i.e., optspec is OK
    ; fail
    ).

duplicate_optspec(_, []) :- !, fail.
duplicate_optspec(OptSpec, [Func|Funcs]) :-
    functor(F, Func, 1),
    findall(F, member(F, OptSpec), Xs),
    (Xs = [_,_|_]
    -> true
    ; duplicate_optspec(OptSpec, Funcs)
    ).


%}}}


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARSE OPTIONS
% NOTE:
% -sbar could be interpreted in two ways: as short for -s bar, and
% as short ('clustered') for -s -b -a -r. Here, the former interpretation
% is chosen.
% Cf http://perldoc.perl.org/Getopt/Long.html (no clustering by default)


parse_options(OptsSpec, Args0, Options, PosArgs) :-
    append(Args0, [""], Args1),
    parse_args_(Args1, OptsSpec, Args2),
    partition_args_(Args2, Options, PosArgs).

%{{{ PARSE ARGS


%if arg is boolean flag given as --no-my-arg, expand to my-arg, false, re-call
parse_args_([Arg,Arg2|Args], OptsSpec, [opt(KID, false)|Result]) :-
    flag_name_long_neg(Dashed, NonDashed, Arg, []),
    flag_id_type(OptsSpec, NonDashed, KID, boolean),
    !,
    parse_args_([Dashed, "false", Arg2|Args], OptsSpec, Result).

%if arg is ordinary boolean flag, fill in implicit true if arg absent; re-call
parse_args_([Arg,Arg2|Args], OptsSpec, Result) :-
    flag_name(K, Arg, []),
    flag_id_type(OptsSpec, K, _KID, boolean),
    \+ member(Arg2, ["true", "false"]),
    !,
    parse_args_([Arg, "true", Arg2 | Args], OptsSpec, Result).

% separate short or long flag run together with its value and parse
parse_args_([Arg|Args], OptsSpec, [opt(KID, Val)|Result]) :-
    flag_name_value(Arg1, Arg2, Arg, []),
    \+ short_flag_w_equals(Arg1, Arg2),
    flag_name(K, Arg1, []),
    !,
    parse_option(OptsSpec, K, Arg2, opt(KID, Val)),
    parse_args_(Args, OptsSpec, Result).

%from here, unparsed args have form
%  PosArg1,Flag1,Val1,PosArg2,PosArg3,Flag2,Val2, PosArg4...
%i.e., positional args may go anywhere except between FlagN and ValueN
%(of course, good programming style says they should go last, but it is poor
%programming style to assume that)

parse_args_([Arg1,Arg2|Args], OptsSpec, [opt(KID, Val)|Result]) :-
    flag_name(K, Arg1, []),
    !,
    parse_option(OptsSpec, K, Arg2, opt(KID, Val)),
    parse_args_(Args, OptsSpec, Result).

parse_args_([Arg1,Arg2|Args], OptsSpec, [pos(At)|Result]) :-
    \+ flag_name(_, Arg1, []),
    !,
    atom_codes(At, Arg1),
    parse_args_([Arg2|Args], OptsSpec, Result).

parse_args_([""], _, []) :- !.   %placeholder, but useful for error messages
parse_args_([], _, []) :- !.

short_flag_w_equals([0'-,_C], [0'=|_]) :-
    throw(error(syntax_error('disallowed: <shortflag>=<value>'),_)).



flag_id_type(OptsSpec, FlagCodes, ID, Type) :-
    atom_codes(Flag, FlagCodes),
    member(OptSpec, OptsSpec),
    flags(OptSpec, Flags),
    member(Flag, Flags),
    member(type(Type), OptSpec),
    member(opt(ID), OptSpec).

%{{{ FLAG DCG

%DCG non-terminals:
%  flag_name(NonDashed)                  %c, flag-name, x
%  flag_name_short(Dashed, NonDashed)    %c, x
%  flag_name_long(Dashed, NonDashed)     %flag-name
%  flag_name_long_neg(Dashed, NonDashed) %no-flag-name
%  flag_value(Val)                       %non-empty string
%  flag_value0(Val)                      %any string, also empty
%  flag_name_value(Dashed, Val)          %pair of flag_name, flag_value


flag_name(NonDashed) --> flag_name_long(_, NonDashed).
flag_name(NonDashed) --> flag_name_short(_, NonDashed).
flag_name(NonDashed) --> flag_name_long_neg(_, NonDashed).

flag_name_long_neg([0'-,0'-|Cs], Cs) --> "--no-", name_long(Cs).
flag_name_long([0'-,0'-|Cs], Cs) --> "--", name_long(Cs).
flag_name_short([0'-|C], C) --> "-", name_1st(C).

flag_value([C|Cs]) --> [C], flag_value0(Cs).
flag_value0([]) --> [].
flag_value0([C|Cs]) --> [C], flag_value0(Cs).
flag_name_value(Dashed, Val) --> flag_name_long(Dashed, _), "=", flag_value0(Val).
flag_name_value(Dashed, Val) --> flag_name_short(Dashed, _), flag_value(Val).

name_long([C|Cs]) --> name_1st([C]), name_rest(Cs).
name_1st([C]) --> [C], {name_1st(C)}.
name_rest([]) --> [].
name_rest([C|Cs]) --> [C], {name_char(C)}, name_rest(Cs).
name_1st(C) :- char_type(C, alpha).
name_char(C) :- char_type(C, alpha).
name_char( 0'_ ).
name_char( 0'- ). %}}}


%{{{ PARSE OPTION
parse_option(OptsSpec, Arg1, Arg2, opt(KID, Val)) :-
    (  flag_id_type(OptsSpec, Arg1, KID, Type)
    -> parse_val(Arg1, Type, Arg2, Val)
    ;  format(atom(Msg), '~s', [Arg1]),
     opt_help(OptsSpec, Help),        %unknown flag: dump usage on stderr
     nl(user_error),
     write(user_error, Help),
     throw(error(domain_error(flag_value, Msg),context(_, 'unknown flag')))
    ).


parse_val(Opt, Type, Cs, Val) :-
    catch(
    parse_loc(Type, Cs, Val),
    E,
    ( format('~nERROR: flag ''~s'': expected atom parsable as ~w, found ''~s'' ~n',
                             [Opt,                           Type,        Cs]),
      throw(E))
    ).

%parse_loc(+Type, +ListOfCodes, -Result).
parse_loc(Type, _LOC, _) :-
    var(Type), !, throw(error(instantiation_error, _)).
parse_loc(_Type, LOC, _) :-
    var(LOC), !, throw(error(instantiation_error, _)).
parse_loc(boolean, Cs, true) :- atom_codes(true, Cs), !.
parse_loc(boolean, Cs, false) :- atom_codes(false, Cs), !.
parse_loc(atom, Cs, Result) :- atom_codes(Result, Cs), !.
parse_loc(integer, Cs, Result) :-
    number_codes(Result, Cs),
    integer(Result),

    !.
parse_loc(float, Cs, Result)   :-
    number_codes(Result, Cs),
    float(Result),

    !.
parse_loc(term, Cs, Result) :-
    atom_codes(A, Cs),
    term_to_atom(Result, A),

    !.
parse_loc(Type, Cs, Result) :-
    parse_type(Type, Cs, Result),
    !.
parse_loc(Type, _Cs, _) :- %could not parse Cs as Type
    throw(error(type_error(flag_value, Type), _)),
    !. %}}}
%}}}

%%  parse_type(+Type, +Codes:list(code), -Result) is semidet.
%
%   Hook to parse option text Codes to an object of type Type.

partition_args_([], [], []).
partition_args_([opt(K,V)|Rest], [opt(K,V)|RestOpts], RestPos) :-
    !,
    partition_args_(Rest, RestOpts, RestPos).
partition_args_([pos(Arg)|Rest], RestOpts, [Arg|RestPos]) :-
    !,
    partition_args_(Rest, RestOpts, RestPos).




%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ADD DEFAULTS

add_default_opts([], Opts, Opts).
add_default_opts([OptSpec|OptsSpec], OptsIn, Result) :-
    memberchk(opt(OptName), OptSpec),
    (  memberchk(opt(OptName, _Val), OptsIn)
    -> Result = OptsOut                      %value given on cl, ignore default

    ;                                        %value not given on cl:
       memberchk(default('_'), OptSpec)      % no default in OptsSpec (or 'VAR'):
    -> Result = [opt(OptName, _) | OptsOut]  % create uninstantiated variable
    ;
       memberchk(default(Def), OptSpec),     % default given in OptsSpec
%       memberchk(type(Type), OptSpec),      % already typechecked
%       assertion(must_be(Type, Def)),
       Result = [opt(OptName, Def) | OptsOut]
    ),
    add_default_opts(OptsSpec, OptsIn, OptsOut).



%}}}


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REMOVE DUPLICATES
remove_duplicates(_, [], []) :- !.
remove_duplicates(keeplast, [opt(OptName, Val) | Opts], Result) :-
    !,
    (  memberchk(opt(OptName, _), Opts)
    -> Result = RestOpts
    ;  Result = [opt(OptName, Val) | RestOpts]
    ),
    remove_duplicates(keeplast, Opts, RestOpts).

remove_duplicates(keepfirst, OptsIn, OptsOut) :-
    !,
    reverse(OptsIn, OptsInRev),
    remove_duplicates(keeplast, OptsInRev, OptsOutRev),
    reverse(OptsOutRev, OptsOut).

remove_duplicates(keepall, OptsIn, OptsIn) :- !.
remove_duplicates(K, [_|_], _) :-
    !,
    throw(error(domain_error(keep_flag, K), _)). %}}}


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REFUNCTOR
refunctor_opts(Fnct, OptsIn, OptsOut) :-
    maplist(refunctor_opt(Fnct), OptsIn, OptsOut).

refunctor_opt('OPTION', opt(OptName, OptVal), Result) :-
    !,
    Result =.. [OptName, OptVal].

refunctor_opt(F, opt(OptName, OptVal), Result) :-
    Result =.. [F, OptName, OptVal]. %}}}


%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ACCESSORS

flags(OptSpec, Flags) :- memberchk(shortflags(Flags), OptSpec).
flags(OptSpec, Flags) :- memberchk(longflags(Flags), OptSpec). %}}}

%{{{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS
is_list_of_atoms([]).
is_list_of_atoms([X|Xs]) :- atom(X), is_list_of_atoms(Xs).
%}}}
