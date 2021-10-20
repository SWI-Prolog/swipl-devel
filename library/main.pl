/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2021, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(prolog_main,
          [ main/0,
            argv_options/3,             % +Argv, -RestArgv, -Options
            argv_options/4,             % +Argv, -RestArgv, -Options, +ParseOpts
            argv_usage/1,               % +Level
            cli_parse_debug_options/2,  % +OptionsIn, -Options
            cli_enable_development_system/0
          ]).
% use autoload/1 to avoid checking these files at load time.
:- autoload(library(debug)).
:- autoload(library(threadutil)).
% These are fine to be checked and loaded
:- autoload(library(apply), [maplist/3, partition/4]).
:- autoload(library(lists), [append/3]).
:- autoload(library(pairs), [pairs_keys/2, pairs_values/2]).
:- autoload(library(prolog_code), [pi_head/2]).
:- autoload(library(prolog_debug), [spy/1]).
:- autoload(library(dcg/high_order), [sequence/5]).
:- autoload(library(option), [option/2]).

:- meta_predicate
    argv_options(:, -, -),
    argv_options(:, -, -, +),
    argv_usage(:).

:- dynamic
    interactive/0.

/** <module> Provide entry point for scripts

This library is intended for supporting   PrologScript on Unix using the
``#!`` magic sequence for scripts using   commandline options. The entry
point main/0 calls the user-supplied predicate  main/1 passing a list of
commandline options. Below is a simle `echo` implementation in Prolog.

```
#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :-
    echo(Argv).

echo([]) :- nl.
echo([Last]) :- !,
    write(Last), nl.
echo([H|T]) :-
    write(H), write(' '),
    echo(T).
```

@see	library(prolog_stack) to force backtraces in case of an
	uncaught exception.
@see    XPCE users should have a look at library(pce_main), which
        starts the GUI and processes events until all windows have gone.
*/

:- module_transparent
    main/0.

%!  main
%
%   Call main/1 using the passed  command-line arguments. Before calling
%   main/1  this  predicate  installs  a  signal  handler  for  =SIGINT=
%   (Control-C) that terminates the process with status 1.

main :-
    context_module(M),
    set_signals,
    current_prolog_flag(argv, Av),
    catch_with_backtrace(M:main(Av), Error, throw(Error)),
    (   interactive
    ->  cli_enable_development_system
    ;   true
    ).

set_signals :-
    on_signal(int, _, interrupt).

%!  interrupt(+Signal)
%
%   We received an interrupt.  This handler is installed using
%   on_signal/3.

interrupt(_Sig) :-
    halt(1).

		 /*******************************
		 *            OPTIONS		*
		 *******************************/

%!  argv_options(:Argv, -Positional, -Options) is det.
%
%   Parse command line arguments. This  predicate   acts  in  one of two
%   modes.
%
%     - If the calling module defines opt_type/3, full featured parsing
%       with long and short options, type conversion and help is
%       provided.
%     - If opt_type/3 is not defined, only unguided transformation
%       using long options is supported. See argv_untyped_options/3
%       for details.
%
%   When __guided__, three predicates are called  in the calling module.
%   opt_type/3 __must__ be defined, the others need not. Note that these
%   three predicates _may_ be defined as   _multifile_ to allow multiple
%   modules contributing to the provided   commandline options. Defining
%   them as _discontiguous_ allows for creating   blocks that describe a
%   group of related options.
%
%     - opt_type(Opt, Name, Type)
%       Defines Opt to add an option Name(Value), where Value statisfies
%       Type.  Opt does not include the leading `-`.  A single character
%       implies a short option, multiple a long option.  Long options
%       use ``_`` as _word separator_, user options may use either ``_``
%       or ``-``.  Type is one of:
%
%       - boolean(Default)
%       - boolean
%         Boolean options are special.  They do not take a value except
%         for when using the long ``--opt=value`` notation. This
%         explicit value specification converts ``true``, ``True``,
%         ``TRUE``, ``on``, ``On``, ``ON``, ``1`` and the obvious
%         false equivalents to Prolog `true` or `false`.  If the
%         option is specified, Default is used.  If ``--no-opt`` or
%         ``--noopt`` is used, the inverse of Default is used.
%       - integer
%         Argument is converted to an integer
%       - float
%         Argument is converted to a float.  User may specify an integer
%       - nonneg
%         As `integer`.  Requires value >= 0.
%       - natural
%         As `integer`.  Requires value >= 1.
%       - between(Low, High)
%         If both one of Low and High is a float, convert as `float`,
%         else convert as `integer`.  Then check the range.
%       - atom
%         No conversion
%       - oneof(List)
%         As `atom`, but requires the value to be a member of List
%         (_enum_ type).
%       - string
%         Convert to a SWI-Prolog string
%       - file
%         Convert to a file name in Prolog canonical notation
%         using prolog_to_os_filename/2.
%       - file(Access)
%         As `file`, and check access using access_file/2.  A value `-`
%         is not checked for access, assuming the application handles
%         this as standard input or output.
%       - term
%         Parse option value to a Prolog term.  Variable names for
%         the _last_ term processed are available using var_property/2.
%
%     - opt_help(Name, HelpString)
%       Help string used by argv_usage/1.
%
%     - opt_meta(Name, Meta)
%       If a typed argument is required this defines the placeholder
%       in the help message.  The default is the uppercase version of
%       the type _functor name_. This produces the ``FILE`` in e.g. ``-f
%       FILE``.

argv_options(M:Argv, Positional, Options) :-
    in(M:opt_type(_,_,_)),
    !,
    argv_options(M:Argv, Positional, Options, [on_error(halt(1))]).
argv_options(_:Argv, Positional, Options) :-
    argv_untyped_options(Argv, Positional, Options).

%!  argv_options(:Argv, -Positional, -Options, +ParseOptions) is det.
%
%   As argv_options/3 in __guided__ mode,  Currently this version allows
%   parsing argument options throwing an   exception rather than calling
%   halt/1 by passing an empty list to ParseOptions. ParseOptions:
%
%     - on_error(+Goal)
%       If Goal is halt(Code), exit with Code.

argv_options(Argv, Positional, Options, POptions) :-
    option(on_error(halt(Code)), POptions),
    !,
    E = error(_,_),
    catch(opt_parse(Argv, Positional, Options), E,
          ( print_message(error, E),
            halt(Code)
          )).
argv_options(Argv, Positional, Options, _POptions) :-
    opt_parse(Argv, Positional, Options).

%!  argv_untyped_options(+Argv, -RestArgv, -Options) is det.
%
%   Generic transformation of long  commandline   arguments  to options.
%   Each ``--Name=Value`` is mapped to Name(Value).   Each plain name is
%   mapped to Name(true), unless Name starts with ``no-``, in which case
%   the option is mapped  to  Name(false).   Numeric  option  values are
%   mapped to Prolog numbers.

argv_untyped_options([], Pos, Opts) =>
    Pos = [], Opts = [].
argv_untyped_options([--|R], Pos, Ops) =>
    Pos = R, Ops = [].
argv_untyped_options([H0|T0], R, Ops), sub_atom(H0, 0, _, _, --) =>
    Ops = [H|T],
    (   sub_atom(H0, B, _, A, =)
    ->  B2 is B-2,
        sub_atom(H0, 2, B2, _, Name),
        sub_string(H0, _, A,  0, Value0),
        convert_option(Name, Value0, Value)
    ;   sub_atom(H0, 2, _, 0, Name0),
        (   sub_atom(Name0, 0, _, _, 'no-')
        ->  sub_atom(Name0, 3, _, 0, Name),
            Value = false
        ;   Name = Name0,
            Value = true
        )
    ),
    canonical_name(Name, PlName),
    H =.. [PlName,Value],
    argv_untyped_options(T0, R, T).
argv_untyped_options([H|T0], Ops, T) =>
    Ops = [H|R],
    argv_untyped_options(T0, R, T).

convert_option(password, String, String) :- !.
convert_option(_, String, Number) :-
    number_string(Number, String),
    !.
convert_option(_, String, Atom) :-
    atom_string(Atom, String).

canonical_name(Name, PlName) :-
    split_string(Name, "-_", "", Parts),
    atomic_list_concat(Parts, '_', PlName).

%!  opt_parse(:Argv, -Positional, -Options) is det.
%
%   Rules follow those of Python optparse:
%
%     - Short options must be boolean, except for the last.
%     - The value of a short option can be connected or the next
%       argument
%     - Long options can have "=value" or have the value in the
%       next argument.

opt_parse(M:Argv, _Positional, _Options) :-
    opt_needs_help(Argv),
    argv_usage(M:debug),
    halt(0).
opt_parse(M:Argv, Positional, Options) :-
    opt_parse(Argv, Positional, Options, M).

opt_needs_help(['-h']).
opt_needs_help(['-?']).
opt_needs_help(['--help']).

opt_parse([], [], [], _).
opt_parse([--|T], T, [], _) :-
    !.
opt_parse([H|T], Positional, Options, M) :-
    atom_concat(--, Long, H),
    !,
    take_long(Long, T, Positional, Options, M).
opt_parse([H|T], Positional, Options, M) :-
    H \== '-',
    string_concat(-, Opts, H),
    !,
    string_chars(Opts, Shorts),
    take_shorts(Shorts, T, Positional, Options, M).
opt_parse([H|T], [H|PT], Options, M) :-
    opt_parse(T, PT, Options, M).


take_long(Long, T, Positional, Options, M) :- % --long=Value
    sub_atom(Long, B, _, A, =),
    !,
    sub_atom(Long, 0, B, _, LName0),
    sub_atom(Long, _, A, 0, VAtom),
    canonical_name(LName0, LName),
    (   in(M:opt_type(LName, Name, Type))
    ->  opt_value(Type, Long, VAtom, Value),
        Opt =.. [Name,Value],
        Options = [Opt|OptionsT],
        opt_parse(T, Positional, OptionsT, M)
    ;   opt_error(unknown_option(LName0))
    ).
take_long(LName0, T, Positional, Options, M) :- % --long
    canonical_name(LName0, LName),
    take_long_(LName, T, Positional, Options, M).

take_long_(Long, T, Positional, Options, M) :- % --long
    opt_bool_type(Long, Name, Value, M),
    !,
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    opt_parse(T, Positional, OptionsT, M).
take_long_(Long, T, Positional, Options, M) :- % --no-long, --nolong
    (   atom_concat('no_', LName, Long)
    ;   atom_concat('no', LName, Long)
    ),
    opt_bool_type(LName, Name, Value0, M),
    !,
    negate(Value0, Value),
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    opt_parse(T, Positional, OptionsT, M).
take_long_(Long, T, Positional, Options, M) :- % --long
    in(M:opt_type(Long, Name, Type)),
    !,
    (   T = [VAtom|T1]
    ->  opt_value(Type, Long, VAtom, Value),
        Opt =.. [Name,Value],
        Options = [Opt|OptionsT],
        opt_parse(T1, Positional, OptionsT, M)
    ;   opt_error(missing_value(Long, Type))
    ).
take_long_(Long, _, _, _, _) :-
    opt_error(unknown_option(Long)).

take_shorts([], T, Positional, Options, M) :-
    opt_parse(T, Positional, Options, M).
take_shorts([H|T], Argv, Positional, Options, M) :-
    opt_bool_type(H, Name, Value, M),
    !,
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    take_shorts(T, Argv, Positional, OptionsT, M).
take_shorts([H|T], Argv, Positional, Options, M) :-
    in(M:opt_type(H, Name, Type)),
    !,
    (   T == []
    ->  (   Argv = [VAtom|ArgvT]
        ->  opt_value(Type, H, VAtom, Value),
            Opt =.. [Name,Value],
            Options = [Opt|OptionsT],
            take_shorts(T, ArgvT, Positional, OptionsT, M)
        ;   opt_error(missing_value(H, Type))
        )
    ;   atom_chars(VAtom, T),
        opt_value(Type, H, VAtom, Value),
        Opt =.. [Name,Value],
        Options = [Opt|OptionsT],
        take_shorts([], Argv, Positional, OptionsT, M)
    ).
take_shorts([H|_], _, _, _, _) :-
    opt_error(unknown_option(H)).

opt_bool_type(Opt, Name, Value, M) :-
    in(M:opt_type(Opt, Name, Type)),
    (   Type == boolean
    ->  Value = true
    ;   Type = boolean(Value)
    ).

negate(true, false).
negate(false, true).

%!  opt_value(+Type, +Opt, +VAtom, -Value) is det.
%
%   @error opt_error(Error)

opt_value(Type, _Opt, VAtom, Value) :-
    opt_convert(Type, VAtom, Value),
    !.
opt_value(Type, Opt, VAtom, _) :-
    opt_error(value_type(Opt, Type, VAtom)).

%!  opt_convert(+Type, +VAtom, -Value) is semidet.

opt_convert(boolean, Spec, Value) :-
    to_bool(Spec, Value).
opt_convert(boolean(_), Spec, Value) :-
    to_bool(Spec, Value).
opt_convert(integer, Spec, Value) :-
    atom_number(Spec, Value),
    integer(Value).
opt_convert(float, Spec, Value) :-
    atom_number(Spec, Value0),
    Value is float(Value0).
opt_convert(nonneg, Spec, Value) :-
    atom_number(Spec, Value),
    integer(Value),
    Value >= 0.
opt_convert(natural, Spec, Value) :-
    atom_number(Spec, Value),
    integer(Value),
    Value >= 1.
opt_convert(between(Low, High), Spec, Value) :-
    atom_number(Spec, Value0),
    (   ( float(Low) ; float(High) )
    ->  Value is float(Value0)
    ;   integer(Value0),
        Value = Value0
    ),
    Value >= Low, Value =< High.
opt_convert(atom, Value, Value).
opt_convert(oneof(List), Value, Value) :-
    memberchk(Value, List).
opt_convert(string, Value0, Value) :-
    atom_string(Value0, Value).
opt_convert(file, Spec, Value) :-
    prolog_to_os_filename(Value, Spec).
opt_convert(file(Access), Spec, Value) :-
    (   Spec == '-'
    ->  Value = '-'
    ;   prolog_to_os_filename(Value, Spec),
        (   access_file(Value, Access)
        ->  true
        ;   opt_error(access_file(Spec, Access))
        )
    ).
opt_convert(term, Spec, Value) :-
    term_string(Value, Spec, [variable_names(Bindings)]),
    b_setval('$variable_names', Bindings).

to_bool(true,    true).
to_bool('True',  true).
to_bool('TRUE',  true).
to_bool(on,      true).
to_bool('On',    true).
to_bool('1',     true).
to_bool(false,   false).
to_bool('False', false).
to_bool('FALSE', false).
to_bool(off,     false).
to_bool('Off',   false).
to_bool('0',     false).

%!  argv_usage(:Level) is det.
%
%   Use print_message/2 to print a usage message  at Level. To print the
%   message as plain text indefault color, use `debug`. Other meaningful
%   options are `informational` or `warning`.
%
%   The default initial usage line is as below, where <command> is an as
%   good as possible approximation for running the script.
%
%       Usage: <command> [options]
%
%   The usage line can  be  defined   using  opt_help/1,  as illustrated
%   below.
%
%       opt_help(usage, String).

argv_usage(M:Level) :-
    print_message(Level, opt_usage(M)).

:- multifile
    prolog:message//1.

prolog:message(opt_usage(M)) -->
    usage(M).

usage(M) -->
    usage_line(M),
    usage_options(M).

usage_line(M) -->
    [ ansi(comment, 'Usage: ', []) ],
    cmdline(M),
    (   {in(M:opt_help(usage, Help))}
    ->  [ ' ~w'-[Help] ]
    ;   [ ' [options]'-[] ]
    ),
    [ nl, nl ].

cmdline(_M) -->
    { current_prolog_flag(associated_file, AbsFile),
      file_base_name(AbsFile, Base),
      current_prolog_flag(os_argv, Argv),
      append(Pre, [File|_], Argv),
      file_base_name(File, Base),
      append(Pre, [File], Cmd),
      !
    },
    sequence(cmdarg, [' '-[]], Cmd).
cmdline(_M) -->
    { current_prolog_flag(saved_program, true),
      current_prolog_flag(os_argv, OsArgv),
      append(_, ['-x', State|_], OsArgv),
      !
    },
    cmdarg(State).
cmdline(_M) -->
    { current_prolog_flag(os_argv, [Argv0|_])
    },
    cmdarg(Argv0).

cmdarg(A) -->
    [ '~w'-[A] ].

%!  usage_options(+Module)//
%
%   Find the defined options and display   help on them. Uses opt_type/3
%   to find the options and their type,   opt_help/2  to find the option
%   help comment and opt_meta/2 for _meta types_.

usage_options(M) -->
    { findall(Opt, get_option(M, Opt), Opts),
      maplist(options_width, Opts, OptWidths),
      max_list(OptWidths, MaxOptWidth),
      catch(tty_size(_, Width), _, Width = 80),
      OptColW is min(MaxOptWidth, 30),
      HelpColW is Width-4-OptColW
    },
    [ ansi(comment, 'Options:', []), nl ],
    sequence(opt_usage(OptColW, HelpColW), [nl], Opts).

opt_usage(OptColW, HelpColW, opt(_Name, Type, Short, Long, Help, Meta)) -->
    options(Type, Short, Long, Meta),
    [ '~t~*:| '-[OptColW] ],
    help_text(Help, OptColW, HelpColW).

help_text([First|Lines], Indent, _Width) -->
    !,
    [ '~w'-[First], nl ],
    sequence(rest_line(Indent), [nl], Lines).
help_text(Text, _Indent, Width) -->
    { string_length(Text, Len),
      Len =< Width
    },
    !,
    [ '~w'-[Text] ].
help_text(Text, Indent, Width) -->
    { wrap_text(Width, Text, [First|Lines])
    },
    [ '~w'-[First], nl ],
    sequence(rest_line(Indent), [nl], Lines).

rest_line(Indent, Line) -->
    [ '~t~*| ~w'-[Indent, Line] ].

%!  wrap_text(+Width, +Text, -Wrapped)
%
%   Simple text wrapper. Breaks Text into   words and creates lines with
%   minimally one word and as many  additional   words  as fit in Width.
%   Wrapped is a list of strings.

wrap_text(Width, Text, Wrapped) :-
    split_string(Text, " \t\n", " \t\n", Words),
    wrap_lines(Words, Width, Wrapped).

wrap_lines([], _, []).
wrap_lines([H|T0], Width, [Line|Lines]) :-
    !,
    string_length(H, Len),
    take_line(T0, T1, Width, Len, LineWords),
    atomics_to_string([H|LineWords], " ", Line),
    wrap_lines(T1, Width, Lines).

take_line([H|T0], T, Width, Here, [H|Line]) :-
    string_length(H, Len),
    NewHere is Here+Len+1,
    NewHere =< Width,
    !,
    take_line(T0, T, Width, NewHere, Line).
take_line(T, T, _, _, []).

%!  options(+Type, +ShortOpt, +LongOpts, +Meta)//
%
%   Emit a line with options.

options(Type, ShortOpt, LongOpts, Meta) -->
    { append(ShortOpt, LongOpts, Opts) },
    sequence(option(Type, Meta), [', '-[]], Opts).

option(boolean, _, Opt) -->
    opt(Opt).
option(_, Meta, Opt) -->
    opt(Opt),
    (   { short_opt(Opt) }
    ->  [ ' '-[] ]
    ;   [ '='-[] ]
    ),
    [ ansi(var, '~w', [Meta]) ].

%!  options_width(+Opt, -Width) is det.
%
%   Compute the width of the column we need for the options.

options_width(opt(_Name, boolean, Short, Long, _Help, _Meta), W) =>
    length(Short, SCount),
    length(Long, LCount),
    maplist(atom_length, Long, LLens),
    sum_list(LLens, LLen),
    W is ((SCount+LCount)-1)*2 +               % ', ' seps
         SCount*2 +
         LCount*2 + LLen.
options_width(opt(_Name, _Type, Short, Long, _Help, Meta), W) =>
    length(Short, SCount),
    length(Long, LCount),
    atom_length(Meta, MLen),
    maplist(atom_length, Long, LLens),
    sum_list(LLens, LLen),
    W is ((SCount+LCount)-1)*2 +               % ', ' seps
         SCount*3 + SCount*MLen +
         LCount*3 + LLen + LCount*MLen.

%!  get_option(+Module, -Opt) is multi.
%
%   Get a description for a single option.  Opt is a term
%
%       opt(Name, Type, ShortFlags, Longflags, Help, Meta).

get_option(M, opt(help, boolean, [h,?], [help],
                  Help, -)) :-
    (   in(M:opt_help(help, Help))
    ->  true
    ;   Help = "Show this help message and exit"
    ).
get_option(M, opt(Name, Type, Short, Long, Help, Meta)) :-
    findall(Name, in(M:opt_type(_, Name, _)), Names),
    list_to_set(Names, UNames),
    member(Name, UNames),
    findall(Opt-Type,
            in(M:opt_type(Opt, Name, Type)),
            Pairs),
    option_type(Name, Pairs, TypeT),
    functor(TypeT, Type, _),
    pairs_keys(Pairs, Opts),
    partition(short_opt, Opts, Short, Long),
    (   in(M:opt_help(Name, Help))
    ->  true
    ;   Help = ''
    ),
    (   in(M:opt_meta(Name, Meta))
    ->  true
    ;   upcase_atom(Type, Meta)
    ).

option_type(Name, Pairs, Type) :-
    pairs_values(Pairs, Types),
    sort(Types, [Type|UTypes]),
    (   UTypes = []
    ->  true
    ;   print_message(warning,
                      error(opt_error(multiple_types(Name, [Type|UTypes])),_))
    ).

%!  in(:Goal)
%
%   As call/1, but  fails  silently  if   there  is  no  predicate  that
%   implements Goal.

in(Goal) :-
    pi_head(PI, Goal),
    current_predicate(PI),
    call(Goal).

short_opt(Opt) :-
    atom_length(Opt, 1).

		 /*******************************
		 *      OPT ERROR HANDLING	*
		 *******************************/

%!  opt_error(+Error)
%
%   @error opt_error(Term)

opt_error(Error) :-
    throw(error(opt_error(Error), _)).

:- multifile
    prolog:error_message//1.

prolog:error_message(opt_error(Error)) -->
    opt_error(Error).

opt_error(unknown_option(Opt)) -->
    [ 'Unknown option: '-[] ],
    opt(Opt),
    [ ' (-h for help)'-[] ].
opt_error(missing_value(Opt, Type)) -->
    [ 'Option '-[] ],
    opt(Opt),
    [ ' requires an argument (of type ~p)'-[Type] ].
opt_error(value_type(Opt, Type, Found)) -->
    [ 'Option '-[] ],
    opt(Opt),
    type(Type),
    [ ' (found '-[], ansi(code, '~w', [Found]), ')'-[] ].
opt_error(access_file(File, exist)) -->
    [ 'File '-[], ansi(code, '~w', [File]),
      ' does not exist'-[]
    ].
opt_error(access_file(File, Access)) -->
    { access_verb(Access, Verb) },
    [ 'Cannot access file '-[], ansi(code, '~w', [File]),
      ' for '-[], ansi(code, '~w', [Verb])
    ].

access_verb(read,    reading).
access_verb(write,   writing).
access_verb(append,  writing).
access_verb(execute, executing).


opt(Opt) -->
    { short_opt(Opt) },
    !,
    [ ansi(bold, '-~w', [Opt]) ].
opt(Opt) -->
    [ ansi(bold, '--~w', [Opt]) ].

type(oneof(List)) -->
    !,
    [ ' requires one of '-[] ],
    sequence(atom, [', '], List).
type(between(Low, High)) -->
    !,
    [ ' requires a number '-[],
      ansi(code, '~w', [Low]), '..', ansi(code, '~w', [High])
    ].
type(nonneg) -->
    [ ' requires a non-negative integer'-[] ].
type(natural) -->
    [ ' requires a positive integer (>= 1)'-[] ].
type(Type) -->
    [ ' requires an argument of type '-[], ansi(code, '~w', [Type]) ].

atom(A) -->
    [ ansi(code, '~w', [A]) ].


		 /*******************************
		 *         DEBUG SUPPORT	*
		 *******************************/

%!	cli_parse_debug_options(+OptionsIn, -Options) is det.
%
%       Parse certain commandline options for  debugging and development
%       purposes. Options processed are  below.   Note  that  the option
%       argument is an atom such that these  options may be activated as
%       e.g., ``--debug='http(_)'``.
%
%         - debug(Topic)
%           Call debug(Topic).  See debug/1 and debug/3.
%         - spy(Predicate)
%           Place a spy-point on Predicate.
%         - gspy(Predicate)
%           As spy using the graphical debugger.  See tspy/1.
%         - interactive(true)
%           Start the Prolog toplevel after main/1 completes.

cli_parse_debug_options([], []).
cli_parse_debug_options([H|T0], Opts) :-
    debug_option(H),
    !,
    cli_parse_debug_options(T0, Opts).
cli_parse_debug_options([H|T0], [H|T]) :-
    cli_parse_debug_options(T0, T).

debug_option(interactive(true)) :-
    asserta(interactive).
debug_option(debug(TopicS)) :-
    term_string(Topic, TopicS),
    debug(Topic).
debug_option(spy(Atom)) :-
    atom_pi(Atom, PI),
    spy(PI).
debug_option(gspy(Atom)) :-
    atom_pi(Atom, PI),
    tspy(PI).

atom_pi(Atom, Module:PI) :-
    split(Atom, :, Module, PiAtom),
    !,
    atom_pi(PiAtom, PI).
atom_pi(Atom, Name//Arity) :-
    split(Atom, //, Name, Arity),
    !.
atom_pi(Atom, Name/Arity) :-
    split(Atom, /, Name, Arity),
    !.
atom_pi(Atom, _) :-
    format(user_error, 'Invalid predicate indicator: "~w"~n', [Atom]),
    halt(1).

split(Atom, Sep, Before, After) :-
    sub_atom(Atom, BL, _, AL, Sep),
    !,
    sub_atom(Atom, 0, BL, _, Before),
    sub_atom(Atom, _, AL, 0, AfterAtom),
    (   atom_number(AfterAtom, After)
    ->  true
    ;   After = AfterAtom
    ).


%!  cli_enable_development_system
%
%   Re-enable the development environment. Currently  re-enables xpce if
%   this was loaded, but not  initialised   and  causes  the interactive
%   toplevel to be re-enabled.
%
%   This predicate may  be  called  from   main/1  to  enter  the Prolog
%   toplevel  rather  than  terminating  the  application  after  main/1
%   completes.

cli_enable_development_system :-
    on_signal(int, _, debug),
    set_prolog_flag(xpce_threaded, true),
    set_prolog_flag(message_ide, true),
    (   current_prolog_flag(xpce_version, _)
    ->  use_module(library(pce_dispatch)),
        memberchk(Goal, [pce_dispatch([])]),
        call(Goal)
    ;   true
    ),
    set_prolog_flag(toplevel_goal, prolog).


		 /*******************************
		 *          IDE SUPPORT		*
		 *******************************/

:- multifile
    prolog:called_by/2.

prolog:called_by(main, [main(_)]).
prolog:called_by(argv_options(_,_,_),
                 [ opt_type(_,_,_),
                   opt_help(_,_),
                   opt_meta(_,_)
                 ]).
