/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2025, University of Amsterdam
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
            cli_debug_opt_type/3,       % -Flag, -Option, -Type
            cli_debug_opt_help/2,       % -Option, -Message
            cli_debug_opt_meta/2,       % -Option, -Arg
	    cli_enable_development_system/0
          ]).
:- use_module(library(debug), [debug/1]).
:- autoload(library(apply), [maplist/2, maplist/3, partition/4]).
:- autoload(library(lists),
            [append/3, max_list/2, sum_list/2, list_to_set/2, member/2]).
:- autoload(library(pairs), [pairs_keys/2, pairs_values/2]).
:- autoload(library(prolog_code), [pi_head/2]).
:- autoload(library(prolog_debug), [spy/1]).
:- autoload(library(dcg/high_order), [sequence//3, sequence//2]).
:- autoload(library(option), [option/2, option/3]).
:- if(exists_source(library(doc_markdown))).
:- autoload(library(doc_markdown), [print_markdown/2]).
:- endif.

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
%
%   When main/0 is called interactively it  simply calls main/1 with the
%   arguments. This allows for debugging scripts as follows:
%
%   ```
%   $ swipl -l script.pl -- arg ...
%   ?- gspy(suspect/1).		% setup debugging
%   ?- main.			% run program
%   ```

main :-
    current_prolog_flag(break_level, _),
    !,
    current_prolog_flag(argv, Av),
    context_module(M),
    M:main(Av).
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
%       - A|B
%         Disjunctive type.  Disjunction can be used create long
%         options with optional values.   For example, using the type
%         ``nonneg|boolean``, for an option `http` handles ``--http``
%         as http(true), ``--no-http`` as http(false) and ``--http=3000``
%         as http(3000). Note that with an optional boolean a option is
%         considered boolean unless it has a value written as
%         ``--longopt=value``.
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
%       - number
%         Any number (integer, float, rational).
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
%       - directory
%         Convert to a file name in Prolog canonical notation
%         using prolog_to_os_filename/2.  No checking is done and
%         thus this type is the same as `file`
%       - file(Access)
%         As `file`, and check access using access_file/2.  A value `-`
%         is not checked for access, assuming the application handles
%         this as standard input or output.
%       - directory(Access)
%         As `directory`, and check access.  Access is one of `read`
%         `write` or `create`.  In the latter case the parent directory
%         must exist and have write access.
%       - term
%         Parse option value to a Prolog term.
%       - term(+Options)
%         As `term`, but passes Options to term_string/3. If the option
%         variable_names(Bindings) is given the option value is set to
%         the _pair_ `Term-Bindings`.
%
%     - opt_help(Name, HelpString)
%       Help string used by argv_usage/1.
%
%     - opt_meta(Name, Meta)
%       If a typed argument is required this defines the placeholder
%       in the help message.  The default is the uppercase version of
%       the type _functor name_. This produces the ``FILE`` in e.g. ``-f
%       FILE``.
%
%    By default, ``-h``, ``-?`` and  ``--help``   are  bound to help. If
%    opt_type(Opt, help, boolean) is true for   some  `Opt`, the default
%    help binding and help message  are   disabled  and  the normal user
%    rules apply. In particular, the user should also provide a rule for
%    opt_help(help, String).

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
%       If Goal is halt(Code), exit with Code.  Other goals are
%       currently not supported.
%     - options_after_arguments(+Boolean)
%       If `false` (default `true`), stop parsing after the first
%       positional argument, returning options that follow this
%       argument as positional arguments.  E.g, ``-x file -y``
%       results in positional arguments `[file, '-y']`
%     - unknown_option(+Mode)
%       One of `error` (default) or `pass`.  Using `pass`, the
%       option is passed in Positional.  Multi-flag short options
%       may be processed partially.  For example, if ``-v`` is defined
%       and `-iv` is in Argv, Positional receives `'-i'` and the
%       option defined with ``-v`` is added to Options.
%
%   @tbd When passing unknown options we may wish to process multi-flag
%   options as a whole or not at all rather than passing the unknown
%   flags.

argv_options(Argv, Positional, Options, POptions) :-
    option(on_error(halt(Code)), POptions),
    !,
    E = error(_,_),
    catch(opt_parse(Argv, Positional, Options, POptions), E,
	  ( print_message(error, E),
	    halt(Code)
	  )).
argv_options(Argv, Positional, Options, POptions) :-
    opt_parse(Argv, Positional, Options, POptions).

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

%!  opt_parse(:Argv, -Positional, -Options, +POptions) is det.
%
%   Rules follow those of Python optparse:
%
%     - Short options must be boolean, except for the last.
%     - The value of a short option can be connected or the next
%       argument
%     - Long options can have "=value" or have the value in the
%       next argument.

opt_parse(M:Argv, _Positional, _Options, _POptions) :-
    opt_needs_help(M:Argv),
    !,
    argv_usage(M:debug),
    halt(0).
opt_parse(M:Argv, Positional, Options, POptions) :-
    opt_parse(Argv, Positional, Options, M, POptions).

opt_needs_help(M:[Arg]) :-
    in(M:opt_type(_, help, boolean)),
    !,
    in(M:opt_type(Opt, help, boolean)),
    (   short_opt(Opt)
    ->  atom_concat(-, Opt, Arg)
    ;   atom_concat(--, Opt, Arg)
    ),
    !.
opt_needs_help(_:['-h']).
opt_needs_help(_:['-?']).
opt_needs_help(_:['--help']).

opt_parse([], Positional, Options, _, _) =>
    Positional = [],
    Options = [].
opt_parse([--|T], Positional, Options, _, _) =>
    Positional = T,
    Options = [].
opt_parse([H|T], Positional, Options, M, POptions), atom_concat(--, Long, H) =>
    take_long(Long, T, Positional, Options, M, POptions).
opt_parse([H|T], Positional, Options, M, POptions),
    H \== '-',
    string_concat(-, Opts, H) =>
    string_chars(Opts, Shorts),
    take_shorts(Shorts, T, Positional, Options, M, POptions).
opt_parse(Argv, Positional, Options, _M, POptions),
    option(options_after_arguments(false), POptions) =>
    Positional = Argv,
    Options = [].
opt_parse([H|T], Positional, Options, M, POptions) =>
    Positional = [H|PT],
    opt_parse(T, PT, Options, M, POptions).


%!  take_long(+LongOpt, +Argv, -Positional, -Option, +M, +POptions) is det.

take_long(Long, T, Positional, Options, M, POptions) :- % --long=Value
    sub_atom(Long, B, _, A, =),
    !,
    sub_atom(Long, 0, B, _, LName0),
    sub_atom(Long, _, A, 0, VAtom),
    canonical_name(LName0, LName),
    (   in(M:opt_type(LName, Name, Type))
    ->  opt_value(Type, Long, VAtom, Value),
	Opt =.. [Name,Value],
	Options = [Opt|OptionsT],
	opt_parse(T, Positional, OptionsT, M, POptions)
    ;   option(unknown_option(pass), POptions, error)
    ->  atom_concat(--, Long, Opt),
        Positional = [Opt|PositionalT],
        opt_parse(T, PositionalT, Options, M, POptions)
    ;   opt_error(unknown_option(M:LName0))
    ).
take_long(LName0, T, Positional, Options, M, POptions) :- % --long
    canonical_name(LName0, LName),
    take_long_(LName, T, Positional, Options, M, POptions).

take_long_(Long, T, Positional, Options, M, POptions) :- % --long
    opt_bool_type(Long, Name, Value, M),                 % only boolean
    !,
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    opt_parse(T, Positional, OptionsT, M, POptions).
take_long_(Long, T, Positional, Options, M, POptions) :- % --no-long, --nolong
    (   atom_concat('no_', LName, Long)
    ;   atom_concat('no', LName, Long)
    ),
    in(M:opt_type(LName, Name, Type)),
    type_optional_bool(Type, Value0),
    !,
    negate(Value0, Value),
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    opt_parse(T, Positional, OptionsT, M, POptions).
take_long_(Long, T, Positional, Options, M, POptions) :- % --long [value]
    in(M:opt_type(Long, Name, Type)),
    type_optional_bool(Type, Value),
    !,
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    opt_parse(T, Positional, OptionsT, M, POptions).
take_long_(Long, T, Positional, Options, M, POptions) :- % --long
    in(M:opt_type(Long, Name, Type)),
    !,
    (   T = [VAtom|T1]
    ->  opt_value(Type, Long, VAtom, Value),
	Opt =.. [Name,Value],
	Options = [Opt|OptionsT],
	opt_parse(T1, Positional, OptionsT, M, POptions)
    ;   opt_error(missing_value(Long, Type))
    ).
take_long_(Long,  T, Positional, Options, M, POptions) :-
    option(unknown_option(pass), POptions, error),
    !,
    atom_concat(--, Long, Opt),
    Positional = [Opt|PositionalT],
    opt_parse(T, PositionalT, Options, M, POptions).
take_long_(Long, _, _, _, M, _) :-
    opt_error(unknown_option(M:Long)).

%!  take_shorts(+OptChars, +Argv, -Positional, -Options, +M, +POptions)

take_shorts(OptChars, Argv, Positional, Options, M, POptions) :-
    take_shorts_(OptChars, OptLeft, Argv, Positional0, Options, M, POptions),
    (   OptLeft == []
    ->  Positional = Positional0
    ;   atom_chars(Pass, [-|OptLeft]),
        Positional = [Pass|Positional0]
    ).

take_shorts_([], [], T, Positional, Options, M, POptions) :-
    opt_parse(T, Positional, Options, M, POptions).
take_shorts_([H|T], Pass, Argv, Positional, Options, M, POptions) :-
    opt_bool_type(H, Name, Value, M),
    !,
    Opt =.. [Name,Value],
    Options = [Opt|OptionsT],
    take_shorts_(T, Pass, Argv, Positional, OptionsT, M, POptions).
take_shorts_([H|T], Pass, Argv, Positional, Options, M, POptions) :-
    in(M:opt_type(H, Name, Type)),
    !,
    (   T == []
    ->  (   Argv = [VAtom|ArgvT]
	->  opt_value(Type, H, VAtom, Value),
	    Opt =.. [Name,Value],
	    Options = [Opt|OptionsT],
	    take_shorts_(T, Pass, ArgvT, Positional, OptionsT, M, POptions)
	;   opt_error(missing_value(H, Type))
	)
    ;   atom_chars(VAtom, T),
	opt_value(Type, H, VAtom, Value),
	Opt =.. [Name,Value],
	Options = [Opt|OptionsT],
	take_shorts_([], Pass, Argv, Positional, OptionsT, M, POptions)
    ).
take_shorts_([H|T], [H|Pass], Argv, Positional, Options, M, POptions) :-
    option(unknown_option(pass), POptions, error), !,
    take_shorts_(T, Pass, Argv, Positional, Options, M, POptions).
take_shorts_([H|_], _, _, _, _, M, _) :-
    opt_error(unknown_option(M:H)).

opt_bool_type(Opt, Name, Value, M) :-
    in(M:opt_type(Opt, Name, Type)),
    type_bool(Type, Value).

type_bool(Type, Value) :-
    (   Type == boolean
    ->  Value = true
    ;   Type = boolean(Value)
    ).

type_optional_bool((A|B), Value) =>
    (   type_optional_bool(A, Value)
    ->  true
    ;   type_optional_bool(B, Value)
    ).
type_optional_bool(Type, Value) =>
    type_bool(Type, Value).

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

opt_convert(A|B, Spec, Value) :-
    (   opt_convert(A, Spec, Value)
    ->  true
    ;   opt_convert(B, Spec, Value)
    ).
opt_convert(boolean, Spec, Value) :-
    to_bool(Spec, Value).
opt_convert(boolean(_), Spec, Value) :-
    to_bool(Spec, Value).
opt_convert(number, Spec, Value) :-
    atom_number(Spec, Value).
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
opt_convert(directory, Spec, Value) :-
    prolog_to_os_filename(Value, Spec).
opt_convert(directory(Access), Spec, Value) :-
    prolog_to_os_filename(Value, Spec),
    access_directory(Value, Access).
opt_convert(term, Spec, Value) :-
    term_string(Value, Spec, []).
opt_convert(term(Options), Spec, Value) :-
    term_string(Term, Spec, Options),
    (   option(variable_names(Bindings), Options)
    ->  Value = Term-Bindings
    ;   Value = Term
    ).

access_directory(Dir, read) =>
    exists_directory(Dir),
    access_file(Dir, read).
access_directory(Dir, write) =>
    exists_directory(Dir),
    access_file(Dir, write).
access_directory(Dir, create) =>
    (   exists_directory(Dir)
    ->  access_file(Dir, write)
    ;   \+ exists_file(Dir),
        file_directory_name(Dir, Parent),
        exists_directory(Parent),
        access_file(Parent, write)
    ).

to_bool(true,    true).
to_bool('True',  true).
to_bool('TRUE',  true).
to_bool(on,      true).
to_bool('On',    true).
to_bool(yes,     true).
to_bool('Yes',   true).
to_bool('1',     true).
to_bool(false,   false).
to_bool('False', false).
to_bool('FALSE', false).
to_bool(off,     false).
to_bool('Off',   false).
to_bool(no,      false).
to_bool('No',    false).
to_bool('0',     false).

%!  argv_usage(:Level) is det.
%
%   Use print_message/2 to print a usage message  at Level. To print the
%   message as plain text indefault color, use `debug`. Other meaningful
%   options are `informational` or `warning`. The  help page consists of
%   four sections, two of which are optional:
%
%     1. The __header__ is created from opt_help(help(header), String).
%        It is optional.
%     2. The __usage__ is added by default.  The part behind
%        ``Usage: <command>`` is by default ``[options]`` and can be
%        overruled using opt_help(help(usage), String).
%     3. The actual option descriptions.  The options are presented
%        in the order they are defined in opt_type/3.  Subsequent
%        options for the same _destination_ (option name) are joined
%        with the first.
%     4. The _footer__ is created from opt_help(help(footer), String).
%        It is optional.
%
%   The help provided by help(header),  help(usage) and help(footer) are
%   either a simple  string  or  a  list   of  elements  as  defined  by
%   print_message_lines/3. In the latter case, the construct `\Callable`
%   can be used to call a DCG  rule   in  the module from which the user
%   calls argv_options/3.  For example, we can add a bold title using
%
%       opt_help(help(header), [ansi(bold, '~w', ['My title'])]).

argv_usage(M:Level) :-
    print_message(Level, opt_usage(M)).

:- multifile
    prolog:message//1.

prolog:message(opt_usage(M)) -->
    usage(M).

usage(M) -->
    usage_text(M:header),
    usage_line(M),
    usage_text(M:description),
    usage_options(M),
    usage_text(M:footer).

%!  usage_text(:Which)// is det.
%
%   Emit  a  user  element.  This  may    use  elements  as  defined  by
%   print_message_lines/3 or can be a simple string.

usage_text(M:Which) -->
    { in(M:opt_help(help(Which), Help))
    },
    !,
    (   {Which == header ; Which == description}
    ->  user_text(M:Help), [nl, nl]
    ;   [nl, nl], user_text(M:Help)
    ).
usage_text(_) -->
    [].

user_text(M:Entries) -->
    { is_list(Entries) },
    !,
    sequence(help_elem(M), Entries).
:- if(current_predicate(print_markdown/2)).
user_text(_:md(Help)) -->
    !,
    { with_output_to(string(String),
                     ( current_output(S),
                       set_stream(S, tty(true)),
                       print_markdown(Help, []))) },
    [ '~s'-[String] ].
:- else.
user_text(_:md(Help)) -->
    !,
    [ '~w'-[Help] ].
:- endif.
user_text(_:Help) -->
    [ '~w'-[Help] ].

help_elem(M, \Callable) -->
    { callable(Callable) },
    call(M:Callable),
    !.
help_elem(_M, Elem) -->
    [ Elem ].

usage_line(M) -->
    { findall(Help, in(M:opt_help(help(usage), Help)), HelpLines)
    },
    [ ansi(comment, 'Usage: ', []) ],
    (   {HelpLines == []}
    ->  cmdline(M), [ ' [options]'-[] ]
    ;   sequence(usage_line(M), [nl], HelpLines)
    ),
    [ nl, nl ].

usage_line(M, Help) -->
    [ '~t~8|'-[] ],
    cmdline(M),
    user_text(M:Help).

cmdline(_M) -->
    { current_prolog_flag(app_name, App),
      !,
      current_prolog_flag(os_argv, [Argv0|_])
    },
    cmdarg(Argv0), [' '-[], ansi(bold, '~w', [App])].
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
      tty_width(Width),
      OptColW is min(MaxOptWidth, 30),
      HelpColW is Width-4-OptColW
    },
    [ ansi(comment, 'Options:', []), nl ],
    sequence(opt_usage(OptColW, HelpColW), [nl], Opts).

% Just  catch/3  is   enough,   but    dependency   tracking   in  e.g.,
% list_undefined/0 still considers this a missing dependency.
:- if(current_predicate(tty_size/2)).
tty_width(Width) :-
     catch(tty_size(_, Width), _, Width = 80).
:- else.
tty_width(80).
:- endif.

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
    opt(Opt),
    !.
option(_Type, [Meta], Opt) -->
    \+ { short_opt(Opt) },
    !,
    opt(Opt),
    [ '[='-[], ansi(var, '~w', [Meta]), ']'-[] ].
option(_Type, Meta, Opt) -->
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
    (   Meta = [MName]
    ->  atom_length(MName, MLen0),
        MLen is MLen0+2
    ;   atom_length(Meta, MLen)
    ),
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
    \+ in(M:opt_type(_, help, boolean)),       % user defined help
    (   in(M:opt_help(help, Help))
    ->  true
    ;   Help = "Show this help message and exit"
    ).
get_option(M, opt(Name, TypeName, Short, Long, Help, Meta)) :-
    findall(Name, in(M:opt_type(_, Name, _)), Names),
    list_to_set(Names, UNames),
    member(Name, UNames),
    findall(Opt-Type,
	    in(M:opt_type(Opt, Name, Type)),
	    Pairs),
    option_type(Name, Pairs, TypeT),
    functor(TypeT, TypeName, _),
    pairs_keys(Pairs, Opts),
    partition(short_opt, Opts, Short, Long),
    (   in(M:opt_help(Name, Help))
    ->  true
    ;   Help = ''
    ),
    (   in(M:opt_meta(Name, Meta0))
    ->  true
    ;   type_name(TypeT, Meta0)
    ->  true
    ;   upcase_atom(TypeName, Meta0)
    ),
    (   \+ type_bool(TypeT, _),
        type_optional_bool(TypeT, _)
    ->  Meta = [Meta0]
    ;   Meta = Meta0
    ).

type_name(oneof(Values), Name) :-
    atomics_to_string(Values, ",", S0),
    format(atom(Name), '{~w}', [S0]).

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

opt_error(unknown_option(M:Opt)) -->
    [ 'Unknown option: '-[] ],
    opt(Opt),
    hint_help(M).
opt_error(missing_value(Opt, Type)) -->
    [ 'Option '-[] ],
    opt(Opt),
    [ ' requires an argument (of type ~p)'-[Type] ].
opt_error(value_type(Opt, Type, Found)) -->
    [ 'Option '-[] ],
    opt(Opt), [' requires'],
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

hint_help(M) -->
    { in(M:opt_type(Opt, help, boolean)) },
    !,
    [ ' (' ], opt(Opt), [' for help)'].
hint_help(_) -->
    [ ' (-h for help)'-[] ].

opt(Opt) -->
    { short_opt(Opt) },
    !,
    [ ansi(bold, '-~w', [Opt]) ].
opt(Opt) -->
    [ ansi(bold, '--~w', [Opt]) ].

type(A|B) -->
    type(A), [' or'],
    type(B).
type(oneof([One])) -->
    !,
    [ ' ' ],
    atom(One).
type(oneof(List)) -->
    !,
    [ ' one of '-[] ],
    sequence(atom, [', '], List).
type(between(Low, High)) -->
    !,
    [ ' a number '-[],
      ansi(code, '~w', [Low]), '..', ansi(code, '~w', [High])
    ].
type(nonneg) -->
    [ ' a non-negative integer'-[] ].
type(natural) -->
    [ ' a positive integer (>= 1)'-[] ].
type(file(Access)) -->
    [ ' a file with ~w access'-[Access] ].
type(Type) -->
    [ ' an argument of type '-[], ansi(code, '~w', [Type]) ].

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

%!  cli_debug_opt_type(-Flag, -Option, -Type).
%!  cli_debug_opt_help(-Option, -Message).
%!  cli_debug_opt_meta(-Option, -Arg).
%
%   Implements  opt_type/3,  opt_help/2   and    opt_meta/2   for  debug
%   arguments. Applications that wish to  use   these  features can call
%   these predicates from their own hook.  Fot example:
%
%   ```
%   opt_type(..., ..., ...).	% application types
%   opt_type(Flag, Opt, Type) :-
%       cli_debug_opt_type(Flag, Opt, Type).
%   % similar for opt_help/2 and opt_meta/2
%
%   main(Argv) :-
%       argv_options(Argv, Positional, Options0),
%       cli_parse_debug_options(Options0, Options),
%       ...
%   ```

cli_debug_opt_type(debug,       debug,       string).
cli_debug_opt_type(spy,         spy,         string).
cli_debug_opt_type(gspy,        gspy,        string).
cli_debug_opt_type(interactive, interactive, boolean).

cli_debug_opt_help(debug,
                   "Call debug(Topic).  See debug/1 and debug/3. \c
                    Multiple topics may be separated by : or ;").
cli_debug_opt_help(spy,
                   "Place a spy-point on Predicate. \c
                    Multiple topics may be separated by : or ;").
cli_debug_opt_help(gspy,
                   "As --spy using the graphical debugger.  See tspy/1 \c
                    Multiple topics may be separated by `;`").
cli_debug_opt_help(interactive,
                   "Start the Prolog toplevel after main/1 completes.").

cli_debug_opt_meta(debug, 'TOPICS').
cli_debug_opt_meta(spy,   'PREDICATES').
cli_debug_opt_meta(gspy,  'PREDICATES').

:- meta_predicate
    spy_from_string(1, +).

debug_option(interactive(true)) :-
    asserta(interactive).
debug_option(debug(Spec)) :-
    split_string(Spec, ";", "", Specs),
    maplist(debug_from_string, Specs).
debug_option(spy(Spec)) :-
    split_string(Spec, ";", "", Specs),
    maplist(spy_from_string(spy), Specs).
debug_option(gspy(Spec)) :-
    split_string(Spec, ";", "", Specs),
    maplist(spy_from_string(cli_gspy), Specs).

debug_from_string(TopicS) :-
    term_string(Topic, TopicS),
    debug(Topic).

spy_from_string(Pred, Spec) :-
    atom_pi(Spec, PI),
    call(Pred, PI).

cli_gspy(PI) :-
    (   exists_source(library(threadutil))
    ->  use_module(library(threadutil), [tspy/1]),
	Goal = tspy(PI)
    ;   exists_source(library(gui_tracer))
    ->  use_module(library(gui_tracer), [gspy/1]),
	Goal = gspy(PI)
    ;   Goal = spy(PI)
    ),
    call(Goal).

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
prolog:called_by(argv_options(_,_,_,_), Called) :-
    prolog:called_by(argv_options(_,_,_), Called).
