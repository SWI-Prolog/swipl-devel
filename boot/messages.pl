/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2018, University of Amsterdam
                              VU University Amsterdam
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

:- module('$messages',
          [ print_message/2,            % +Kind, +Term
            print_message_lines/3,      % +Stream, +Prefix, +Lines
            message_to_string/2         % +Term, -String
          ]).

:- multifile
    prolog:message//1,              % entire message
    prolog:error_message//1,        % 1-st argument of error term
    prolog:message_context//1,      % Context of error messages
    prolog:message_location//1,     % (File) location of error messages
    prolog:message_line_element/2.  % Extend printing
:- discontiguous
    prolog_message/3.

:- public
    translate_message//1.

:- create_prolog_flag(message_context, [thread], []).

%!  translate_message(+Term)// is det.
%
%   Translate a message Term into message lines. The produced lines
%   is a list of
%
%       * nl
%       Emit a newline
%       * Fmt-Args
%       Emit the result of format(Fmt, Args)
%       * Fmt
%       Emit the result of format(Fmt)
%       * flush
%       Used only as last element of the list.   Simply flush the
%       output instead of producing a final newline.
%       * at_same_line
%       Start the messages at the same line (instead of using ~N)

translate_message(Term) -->
    translate_message2(Term),
    !.
translate_message(Term) -->
    { Term = error(_, _) },
    [ 'Unknown exception: ~p'-[Term] ].
translate_message(Term) -->
    [ 'Unknown message: ~p'-[Term] ].

translate_message2(Term) -->
    {var(Term)},
    !,
    [ 'Unknown message: ~p'-[Term] ].
translate_message2(Term) -->
    prolog:message(Term).
translate_message2(Term) -->
    prolog_message(Term).
translate_message2(error(resource_error(stack), Context)) -->
    out_of_stack(Context).
translate_message2(error(resource_error(Missing), _)) -->
    [ 'Not enough resources: ~w'-[Missing] ].
translate_message2(error(ISO, SWI)) -->
    swi_location(SWI),
    term_message(ISO),
    swi_extra(SWI).
translate_message2('$aborted') -->
    [ 'Execution Aborted' ].
translate_message2(message_lines(Lines), L, T) :- % deal with old C-warning()
    make_message_lines(Lines, L, T).
translate_message2(format(Fmt, Args)) -->
    [ Fmt-Args ].

make_message_lines([], T, T) :- !.
make_message_lines([Last],  ['~w'-[Last]|T], T) :- !.
make_message_lines([L0|LT], ['~w'-[L0],nl|T0], T) :-
    make_message_lines(LT, T0, T).

term_message(Term) -->
    {var(Term)},
    !,
    [ 'Unknown error term: ~p'-[Term] ].
term_message(Term) -->
    prolog:error_message(Term).
term_message(Term) -->
    iso_message(Term).
term_message(Term) -->
    swi_message(Term).
term_message(Term) -->
    [ 'Unknown error term: ~p'-[Term] ].

iso_message(type_error(evaluable, Actual)) -->
    { callable(Actual) },
    [ 'Arithmetic: `~p'' is not a function'-[Actual] ].
iso_message(type_error(free_of_attvar, Actual)) -->
    [ 'Type error: `~W'' contains attributed variables'-
      [Actual,[portray(true), attributes(portray)]] ].
iso_message(type_error(Expected, Actual)) -->
    [ 'Type error: `~w'' expected, found `~p'''-[Expected, Actual] ],
    type_error_comment(Expected, Actual).
iso_message(domain_error(Domain, Actual)) -->
    [ 'Domain error: '-[] ], domain(Domain),
    [ ' expected, found `~p'''-[Actual] ].
iso_message(instantiation_error) -->
    [ 'Arguments are not sufficiently instantiated' ].
iso_message(uninstantiation_error(Var)) -->
    [ 'Uninstantiated argument expected, found ~p'-[Var] ].
iso_message(representation_error(What)) -->
    [ 'Cannot represent due to `~w'''-[What] ].
iso_message(permission_error(Action, Type, Object)) -->
    permission_error(Action, Type, Object).
iso_message(evaluation_error(Which)) -->
    [ 'Arithmetic: evaluation error: `~p'''-[Which] ].
iso_message(existence_error(procedure, Proc)) -->
    [ 'Undefined procedure: ~q'-[Proc] ],
    undefined_proc_msg(Proc).
iso_message(existence_error(answer_variable, Var)) -->
    [ '$~w was not bound by a previous query'-[Var] ].
iso_message(existence_error(Type, Object)) -->
    [ '~w `~p'' does not exist'-[Type, Object] ].
iso_message(existence_error(Type, Object, In)) --> % not ISO
    [ '~w `~p'' does not exist in ~p'-[Type, Object, In] ].
iso_message(busy(Type, Object)) -->
    [ '~w `~p'' is busy'-[Type, Object] ].
iso_message(syntax_error(swi_backslash_newline)) -->
    [ 'Deprecated ... \\<newline><white>*.  Use \\c' ].
iso_message(syntax_error(Id)) -->
    [ 'Syntax error: ' ],
    syntax_error(Id).
iso_message(occurs_check(Var, In)) -->
    [ 'Cannot unify ~p with ~p: would create an infinite tree'-[Var, In] ].

%!  permission_error(Action, Type, Object)//
%
%   Translate  permission  errors.  Most  follow    te  pattern  "No
%   permission to Action Type Object", but some are a bit different.

permission_error(Action, built_in_procedure, Pred) -->
    { user_predicate_indicator(Pred, PI)
    },
    [ 'No permission to ~w built-in predicate `~p'''-[Action, PI] ],
    (   {Action \== export}
    ->  [ nl,
          'Use :- redefine_system_predicate(+Head) if redefinition is intended'
        ]
    ;   []
    ).
permission_error(import_into(Dest), procedure, Pred) -->
    [ 'No permission to import ~p into ~w'-[Pred, Dest] ].
permission_error(Action, static_procedure, Proc) -->
    [ 'No permission to ~w static procedure `~p'''-[Action, Proc] ],
    defined_definition('Defined', Proc).
permission_error(input, stream, Stream) -->
    [ 'No permission to read from output stream `~p'''-[Stream] ].
permission_error(output, stream, Stream) -->
    [ 'No permission to write to input stream `~p'''-[Stream] ].
permission_error(input, text_stream, Stream) -->
    [ 'No permission to read bytes from TEXT stream `~p'''-[Stream] ].
permission_error(output, text_stream, Stream) -->
    [ 'No permission to write bytes to TEXT stream `~p'''-[Stream] ].
permission_error(input, binary_stream, Stream) -->
    [ 'No permission to read characters from binary stream `~p'''-[Stream] ].
permission_error(output, binary_stream, Stream) -->
    [ 'No permission to write characters to binary stream `~p'''-[Stream] ].
permission_error(open, source_sink, alias(Alias)) -->
    [ 'No permission to reuse alias "~p": already taken'-[Alias] ].
permission_error(Action, Type, Object) -->
    [ 'No permission to ~w ~w `~p'''-[Action, Type, Object] ].


undefined_proc_msg(_:(^)/2) -->
    !,
    undefined_proc_msg((^)/2).
undefined_proc_msg((^)/2) -->
    !,
    [nl, '  ^/2 can only appear as the 2nd argument of setof/3 and bagof/3'].
undefined_proc_msg((:-)/2) -->
    !,
    [nl, '  Rules must be loaded from a file'],
    faq('ToplevelMode').
undefined_proc_msg((:-)/1) -->
    !,
    [nl, '  Directives must be loaded from a file'],
    faq('ToplevelMode').
undefined_proc_msg((?-)/1) -->
    !,
    [nl, '  ?- is the Prolog prompt'],
    faq('ToplevelMode').
undefined_proc_msg(Proc) -->
    { dwim_predicates(Proc, Dwims) },
    (   {Dwims \== []}
    ->  [nl, '  However, there are definitions for:', nl],
        dwim_message(Dwims)
    ;   []
    ).

faq(Page) -->
    [nl, '  See FAQ at http://www.swi-prolog.org/FAQ/', Page, '.txt' ].

type_error_comment(_Expected, Actual) -->
    { type_of(Actual, Type),
      (   sub_atom(Type, 0, 1, _, First),
          memberchk(First, [a,e,i,o,u])
      ->  Article = an
      ;   Article = a
      )
    },
    [ ' (~w ~w)'-[Article, Type] ].

type_of(Term, Type) :-
    (   attvar(Term)      -> Type = attvar
    ;   var(Term)         -> Type = var
    ;   atom(Term)        -> Type = atom
    ;   integer(Term)     -> Type = integer
    ;   string(Term)      -> Type = string
    ;   Term == []        -> Type = empty_list
    ;   blob(Term, BlobT) -> blob_type(BlobT, Type)
    ;   rational(Term)    -> Type = rational
    ;   float(Term)       -> Type = float
    ;   is_stream(Term)   -> Type = stream
    ;   is_dict(Term)     -> Type = dict
    ;   is_list(Term)     -> Type = list
    ;   cyclic_term(Term) -> Type = cyclic
    ;   compound(Term)    -> Type = compound
    ;                        Type = unknown
    ).

blob_type(BlobT, Type) :-
    atom_concat(BlobT, '_reference', Type).

syntax_error(end_of_clause) -->
    [ 'Unexpected end of clause' ].
syntax_error(end_of_clause_expected) -->
    [ 'End of clause expected' ].
syntax_error(end_of_file) -->
    [ 'Unexpected end of file' ].
syntax_error(end_of_file_in_block_comment) -->
    [ 'End of file in /* ... */ comment' ].
syntax_error(end_of_file_in_quoted(Quote)) -->
    [ 'End of file in quoted ' ],
    quoted_type(Quote).
syntax_error(illegal_number) -->
    [ 'Illegal number' ].
syntax_error(long_atom) -->
    [ 'Atom too long (see style_check/1)' ].
syntax_error(long_string) -->
    [ 'String too long (see style_check/1)' ].
syntax_error(operator_clash) -->
    [ 'Operator priority clash' ].
syntax_error(operator_expected) -->
    [ 'Operator expected' ].
syntax_error(operator_balance) -->
    [ 'Unbalanced operator' ].
syntax_error(quoted_punctuation) -->
    [ 'Operand expected, unquoted comma or bar found' ].
syntax_error(list_rest) -->
    [ 'Unexpected comma or bar in rest of list' ].
syntax_error(cannot_start_term) -->
    [ 'Illegal start of term' ].
syntax_error(punct(Punct, End)) -->
    [ 'Unexpected `~w\' before `~w\''-[Punct, End] ].
syntax_error(undefined_char_escape(C)) -->
    [ 'Undefined character escape in quoted atom or string: `\\~w\''-[C] ].
syntax_error(void_not_allowed) -->
    [ 'Empty argument list "()"' ].
syntax_error(Message) -->
    [ '~w'-[Message] ].

quoted_type('\'') --> [atom].
quoted_type('\"') --> { current_prolog_flag(double_quotes, Type) }, [Type-[]].
quoted_type('\`') --> { current_prolog_flag(back_quotes, Type) }, [Type-[]].

domain(range(Low,High)) -->
    !,
    ['[~q..~q]'-[Low,High] ].
domain(Domain) -->
    ['`~w\''-[Domain] ].

dwim_predicates(Module:Name/_Arity, Dwims) :-
    !,
    findall(Dwim, dwim_predicate(Module:Name, Dwim), Dwims).
dwim_predicates(Name/_Arity, Dwims) :-
    findall(Dwim, dwim_predicate(user:Name, Dwim), Dwims).

dwim_message([]) --> [].
dwim_message([M:Head|T]) -->
    { hidden_module(M),
      !,
      functor(Head, Name, Arity)
    },
    [ '        ~q'-[Name/Arity], nl ],
    dwim_message(T).
dwim_message([Module:Head|T]) -->
    !,
    { functor(Head, Name, Arity)
    },
    [ '        ~q'-[Module:Name/Arity], nl],
    dwim_message(T).
dwim_message([Head|T]) -->
    {functor(Head, Name, Arity)},
    [ '        ~q'-[Name/Arity], nl],
    dwim_message(T).


swi_message(io_error(Op, Stream)) -->
    [ 'I/O error in ~w on stream ~p'-[Op, Stream] ].
swi_message(shell(execute, Cmd)) -->
    [ 'Could not execute `~w'''-[Cmd] ].
swi_message(shell(signal(Sig), Cmd)) -->
    [ 'Caught signal ~d on `~w'''-[Sig, Cmd] ].
swi_message(format(Fmt, Args)) -->
    [ Fmt-Args ].
swi_message(signal(Name, Num)) -->
    [ 'Caught signal ~d (~w)'-[Num, Name] ].
swi_message(limit_exceeded(Limit, MaxVal)) -->
    [ 'Exceeded ~w limit (~w)'-[Limit, MaxVal] ].
swi_message(goal_failed(Goal)) -->
    [ 'goal unexpectedly failed: ~p'-[Goal] ].
swi_message(shared_object(_Action, Message)) --> % Message = dlerror()
    [ '~w'-[Message] ].
swi_message(system_error(Error)) -->
    [ 'error in system call: ~w'-[Error]
    ].
swi_message(system_error) -->
    [ 'error in system call'
    ].
swi_message(failure_error(Goal)) -->
    [ 'Goal failed: ~p'-[Goal] ].
swi_message(timeout_error(Op, Stream)) -->
    [ 'Timeout in ~w from ~p'-[Op, Stream] ].
swi_message(not_implemented(Type, What)) -->
    [ '~w `~p\' is not implemented in this version'-[Type, What] ].
swi_message(context_error(nodirective, Goal)) -->
    { goal_to_predicate_indicator(Goal, PI) },
    [ 'Wrong context: ~p can only be used in a directive'-[PI] ].
swi_message(context_error(edit, no_default_file)) -->
    (   { current_prolog_flag(windows, true) }
    ->  [ 'Edit/0 can only be used after opening a \c
               Prolog file by double-clicking it' ]
    ;   [ 'Edit/0 can only be used with the "-s file" commandline option'
        ]
    ),
    [ nl, 'Use "?- edit(Topic)." or "?- emacs."' ].
swi_message(context_error(function, meta_arg(S))) -->
    [ 'Functions are not (yet) supported for meta-arguments of type ~q'-[S] ].
swi_message(format_argument_type(Fmt, Arg)) -->
    [ 'Illegal argument to format sequence ~~~w: ~p'-[Fmt, Arg] ].
swi_message(format(Msg)) -->
    [ 'Format error: ~w'-[Msg] ].
swi_message(conditional_compilation_error(unterminated, Where)) -->
    [ 'Unterminated conditional compilation from '-[] ],
    cond_location(Where).
swi_message(conditional_compilation_error(no_if, What)) -->
    [ ':- ~w without :- if'-[What] ].
swi_message(duplicate_key(Key)) -->
    [ 'Duplicate key: ~p'-[Key] ].
swi_message(initialization_error(failed, Goal, File:Line)) -->
    !,
    [ '~w:~w: ~p: false'-[File, Line, Goal] ].
swi_message(initialization_error(Error, Goal, File:Line)) -->
    [ '~w:~w: ~p '-[File, Line, Goal] ],
    translate_message(Error).
swi_message(qlf_format_error(File, Message)) -->
    [ '~w: Invalid QLF file: ~w'-[File, Message] ].

cond_location(File:Line) -->
    { file_base_name(File, Base) },
    [ '~w:~d'-[Base, Line] ].

swi_location(X) -->
    { var(X)
    },
    !,
    [].
swi_location(Context) -->
    prolog:message_location(Context),
    !.
swi_location(context(Caller, _Msg)) -->
    { ground(Caller)
    },
    !,
    caller(Caller).
swi_location(file(Path, Line, -1, _CharNo)) -->
    !,
    [ '~w:~d: '-[Path, Line] ].
swi_location(file(Path, Line, LinePos, _CharNo)) -->
    [ '~w:~d:~d: '-[Path, Line, LinePos] ].
swi_location(stream(Stream, Line, LinePos, CharNo)) -->
    (   { is_stream(Stream),
          stream_property(Stream, file_name(File))
        }
    ->  swi_location(file(File, Line, LinePos, CharNo))
    ;   [ 'Stream ~w:~d:~d '-[Stream, Line, LinePos] ]
    ).
swi_location(_) -->
    [].

caller(system:'$record_clause'/3) -->
    !,
    [].
caller(Module:Name/Arity) -->
    !,
    (   { \+ hidden_module(Module) }
    ->  [ '~q:~q/~w: '-[Module, Name, Arity] ]
    ;   [ '~q/~w: '-[Name, Arity] ]
    ).
caller(Name/Arity) -->
    [ '~q/~w: '-[Name, Arity] ].
caller(Caller) -->
    [ '~p: '-[Caller] ].


swi_extra(X) -->
    { var(X)
    },
    !,
    [].
swi_extra(Context) -->
    prolog:message_context(Context).
swi_extra(context(_, Msg)) -->
    { nonvar(Msg),
      Msg \== ''
    },
    !,
    swi_comment(Msg).
swi_extra(string(String, CharPos)) -->
    { sub_string(String, 0, CharPos, _, Before),
      sub_string(String, CharPos, _, 0, After)
    },
    [ nl, '~w'-[Before], nl, '** here **', nl, '~w'-[After] ].
swi_extra(_) -->
    [].

swi_comment(already_from(Module)) -->
    !,
    [ ' (already imported from ~q)'-[Module] ].
swi_comment(directory(_Dir)) -->
    !,
    [ ' (is a directory)' ].
swi_comment(not_a_directory(_Dir)) -->
    !,
    [ ' (is not a directory)' ].
swi_comment(Msg) -->
    [ ' (~w)'-[Msg] ].


thread_context -->
    { thread_self(Me), Me \== main, thread_property(Me, id(Id)) },
    !,
    ['[Thread ~w] '-[Id]].
thread_context -->
    [].

                 /*******************************
                 *        NORMAL MESSAGES       *
                 *******************************/

prolog_message(initialization_error(_, E, File:Line)) -->
    !,
    [ '~w:~d: '-[File, Line],
      'Initialization goal raised exception:', nl
    ],
    translate_message(E).
prolog_message(initialization_error(Goal, E, _)) -->
    [ 'Initialization goal ~p raised exception:'-[Goal], nl ],
    translate_message(E).
prolog_message(initialization_failure(_Goal, File:Line)) -->
    !,
    [ '~w:~d: '-[File, Line],
      'Initialization goal failed'-[]
    ].
prolog_message(initialization_failure(Goal, _)) -->
    [ 'Initialization goal failed: ~p'-[Goal]
    ].
prolog_message(initialization_exception(E)) -->
    [ 'Prolog initialisation failed:', nl ],
    translate_message(E).
prolog_message(init_goal_syntax(Error, Text)) -->
    !,
    [ '-g ~w: '-[Text] ],
    translate_message(Error).
prolog_message(init_goal_failed(failed, @(Goal,File:Line))) -->
    !,
    [ '~w:~w: ~p: false'-[File, Line, Goal] ].
prolog_message(init_goal_failed(Error, @(Goal,File:Line))) -->
    !,
    [ '~w:~w: ~p '-[File, Line, Goal] ],
    translate_message(Error).
prolog_message(init_goal_failed(failed, Text)) -->
    !,
    [ '-g ~w: false'-[Text] ].
prolog_message(init_goal_failed(Error, Text)) -->
    !,
    [ '-g ~w: '-[Text] ],
    translate_message(Error).
prolog_message(unhandled_exception(E)) -->
    [ 'Unhandled exception: ' ],
    (   translate_message2(E)
    ->  []
    ;   [ '~p'-[E] ]
    ).
prolog_message(goal_failed(Context, Goal)) -->
    [ 'Goal (~w) failed: ~p'-[Context, Goal] ].
prolog_message(no_current_module(Module)) -->
    [ '~w is not a current module (created)'-[Module] ].
prolog_message(commandline_arg_type(Flag, Arg)) -->
    [ 'Bad argument to commandline option -~w: ~w'-[Flag, Arg] ].
prolog_message(missing_feature(Name)) -->
    [ 'This version of SWI-Prolog does not support ~w'-[Name] ].
prolog_message(singletons(_Term, List)) -->
    [ 'Singleton variables: ~w'-[List] ].
prolog_message(multitons(_Term, List)) -->
    [ 'Singleton-marked variables appearing more than once: ~w'-[List] ].
prolog_message(profile_no_cpu_time) -->
    [ 'No CPU-time info.  Check the SWI-Prolog manual for details' ].
prolog_message(non_ascii(Text, Type)) -->
    [ 'Unquoted ~w with non-portable characters: ~w'-[Type, Text] ].
prolog_message(io_warning(Stream, Message)) -->
    { stream_property(Stream, position(Position)),
      !,
      stream_position_data(line_count, Position, LineNo),
      stream_position_data(line_position, Position, LinePos),
      (   stream_property(Stream, file_name(File))
      ->  Obj = File
      ;   Obj = Stream
      )
    },
    [ '~p:~d:~d: ~w'-[Obj, LineNo, LinePos, Message] ].
prolog_message(io_warning(Stream, Message)) -->
    [ 'stream ~p: ~w'-[Stream, Message] ].
prolog_message(option_usage(pldoc)) -->
    [ 'Usage: --pldoc[=port]' ].
prolog_message(interrupt(begin)) -->
    [ 'Action (h for help) ? ', flush ].
prolog_message(interrupt(end)) -->
    [ 'continue' ].
prolog_message(interrupt(trace)) -->
    [ 'continue (trace mode)' ].
prolog_message(unknown_in_module_user) -->
    [ 'Using a non-error value for unknown in the global module', nl,
      'causes most of the development environment to stop working.', nl,
      'Please use :- dynamic or limit usage of unknown to a module.', nl,
      'See http://www.swi-prolog.org/howto/database.html'
    ].
prolog_message(deprecated(What)) -->
    deprecated(What).


                 /*******************************
                 *         LOADING FILES        *
                 *******************************/

prolog_message(modify_active_procedure(Who, What)) -->
    [ '~p: modified active procedure ~p'-[Who, What] ].
prolog_message(load_file(failed(user:File))) -->
    [ 'Failed to load ~p'-[File] ].
prolog_message(load_file(failed(Module:File))) -->
    [ 'Failed to load ~p into module ~p'-[File, Module] ].
prolog_message(load_file(failed(File))) -->
    [ 'Failed to load ~p'-[File] ].
prolog_message(mixed_directive(Goal)) -->
    [ 'Cannot pre-compile mixed load/call directive: ~p'-[Goal] ].
prolog_message(cannot_redefine_comma) -->
    [ 'Full stop in clause-body?  Cannot redefine ,/2' ].
prolog_message(illegal_autoload_index(Dir, Term)) -->
    [ 'Illegal term in INDEX file of directory ~w: ~w'-[Dir, Term] ].
prolog_message(redefined_procedure(Type, Proc)) -->
    [ 'Redefined ~w procedure ~p'-[Type, Proc] ],
    defined_definition('Previously defined', Proc).
prolog_message(declare_module(Module, abolish(Predicates))) -->
    [ 'Loading module ~w abolished: ~p'-[Module, Predicates] ].
prolog_message(import_private(Module, Private)) -->
    [ 'import/1: ~p is not exported (still imported into ~q)'-
      [Private, Module]
    ].
prolog_message(ignored_weak_import(Into, From:PI)) -->
    [ 'Local definition of ~p overrides weak import from ~q'-
      [Into:PI, From]
    ].
prolog_message(undefined_export(Module, PI)) -->
    [ 'Exported procedure ~q:~q is not defined'-[Module, PI] ].
prolog_message(no_exported_op(Module, Op)) -->
    [ 'Operator ~q:~q is not exported (still defined)'-[Module, Op] ].
prolog_message(discontiguous((-)/2,_)) -->
    prolog_message(minus_in_identifier).
prolog_message(discontiguous(Proc,Current)) -->
    [ 'Clauses of ~p are not together in the source-file'-[Proc], nl ],
    current_definition(Proc, '  Earlier definition at '),
    [ '  Current predicate: ~p'-[Current], nl,
      '  Use :- discontiguous ~p. to suppress this message'-[Proc]
    ].
prolog_message(decl_no_effect(Goal)) -->
    [ 'Deprecated declaration has no effect: ~p'-[Goal] ].
prolog_message(load_file(start(Level, File))) -->
    [ '~|~t~*+Loading '-[Level] ],
    load_file(File),
    [ ' ...' ].
prolog_message(include_file(start(Level, File))) -->
    [ '~|~t~*+include '-[Level] ],
    load_file(File),
    [ ' ...' ].
prolog_message(include_file(done(Level, File))) -->
    [ '~|~t~*+included '-[Level] ],
    load_file(File).
prolog_message(load_file(done(Level, File, Action, Module, Time, Clauses))) -->
    [ '~|~t~*+'-[Level] ],
    load_file(File),
    [ ' ~w'-[Action] ],
    load_module(Module),
    [ ' ~2f sec, ~D clauses'-[Time, Clauses] ].
prolog_message(dwim_undefined(Goal, Alternatives)) -->
    { goal_to_predicate_indicator(Goal, Pred)
    },
    [ 'Undefined procedure: ~q'-[Pred], nl,
      '    However, there are definitions for:', nl
    ],
    dwim_message(Alternatives).
prolog_message(dwim_correct(Into)) -->
    [ 'Correct to: ~q? '-[Into], flush ].
prolog_message(error(loop_error(Spec), file_search(Used))) -->
    [ 'File search: too many levels of indirections on: ~p'-[Spec], nl,
      '    Used alias expansions:', nl
    ],
    used_search(Used).
prolog_message(minus_in_identifier) -->
    [ 'The "-" character should not be used to seperate words in an', nl,
      'identifier.  Check the SWI-Prolog FAQ for details.'
    ].
prolog_message(qlf(removed_after_error(File))) -->
    [ 'Removed incomplete QLF file ~w'-[File] ].
prolog_message(qlf(recompile(Spec,_Pl,_Qlf,Reason))) -->
    [ '~p: recompiling QLF file'-[Spec] ],
    qlf_recompile_reason(Reason).
prolog_message(qlf(can_not_recompile(Spec,QlfFile,_Reason))) -->
    [ '~p: can not recompile "~w" (access denied)'-[Spec, QlfFile], nl,
      '\tLoading from source'-[]
    ].
prolog_message(redefine_module(Module, OldFile, File)) -->
    [ 'Module "~q" already loaded from ~w.'-[Module, OldFile], nl,
      'Wipe and reload from ~w? '-[File], flush
    ].
prolog_message(redefine_module_reply) -->
    [ 'Please answer y(es), n(o) or a(bort)' ].
prolog_message(reloaded_in_module(Absolute, OldContext, LM)) -->
    [ '~w was previously loaded in module ~w'-[Absolute, OldContext], nl,
      '\tnow it is reloaded into module ~w'-[LM] ].
prolog_message(expected_layout(Expected, Pos)) -->
    [ 'Layout data: expected ~w, found: ~p'-[Expected, Pos] ].

defined_definition(Message, Spec) -->
    { strip_module(user:Spec, M, Name/Arity),
      functor(Head, Name, Arity),
      predicate_property(M:Head, file(File)),
      predicate_property(M:Head, line_count(Line))
    },
    !,
    [ nl, '~w at ~w:~d'-[Message, File,Line] ].
defined_definition(_, _) --> [].

used_search([]) -->
    [].
used_search([Alias=Expanded|T]) -->
    [ '        file_search_path(~p, ~p)'-[Alias, Expanded], nl ],
    used_search(T).

load_file(file(Spec, _Path)) -->
    (   {atomic(Spec)}
    ->  [ '~w'-[Spec] ]
    ;   [ '~p'-[Spec] ]
    ).
%load_file(file(_, Path)) -->
%       [ '~w'-[Path] ].

load_module(user) --> !.
load_module(system) --> !.
load_module(Module) -->
    [ ' into ~w'-[Module] ].

goal_to_predicate_indicator(Goal, PI) :-
    strip_module(Goal, Module, Head),
    callable_name_arity(Head, Name, Arity),
    user_predicate_indicator(Module:Name/Arity, PI).

callable_name_arity(Goal, Name, Arity) :-
    compound(Goal),
    !,
    compound_name_arity(Goal, Name, Arity).
callable_name_arity(Goal, Goal, 0) :-
    atom(Goal).

user_predicate_indicator(Module:PI, PI) :-
    hidden_module(Module),
    !.
user_predicate_indicator(PI, PI).

hidden_module(user) :- !.
hidden_module(system) :- !.
hidden_module(M) :-
    sub_atom(M, 0, _, _, $).

current_definition(Proc, Prefix) -->
    { pi_head(Proc, Head),
      predicate_property(Head, file(File)),
      predicate_property(Head, line_count(Line))
    },
    [ '~w'-[Prefix], '~w:~d'-[File,Line], nl ].
current_definition(_, _) --> [].

pi_head(Module:Name/Arity, Module:Head) :-
    !,
    atom(Module), atom(Name), integer(Arity),
    functor(Head, Name, Arity).
pi_head(Name/Arity, user:Head) :-
    atom(Name), integer(Arity),
    functor(Head, Name, Arity).

qlf_recompile_reason(old) -->
    !,
    [ ' (out of date)'-[] ].
qlf_recompile_reason(_) -->
    [ ' (incompatible with current Prolog version)'-[] ].

prolog_message(file_search(cache(Spec, _Cond), Path)) -->
    [ 'File search: ~p --> ~p (cache)'-[Spec, Path] ].
prolog_message(file_search(found(Spec, Cond), Path)) -->
    [ 'File search: ~p --> ~p OK ~p'-[Spec, Path, Cond] ].
prolog_message(file_search(tried(Spec, Cond), Path)) -->
    [ 'File search: ~p --> ~p NO ~p'-[Spec, Path, Cond] ].

                 /*******************************
                 *              GC              *
                 *******************************/

prolog_message(agc(start)) -->
    thread_context,
    [ 'AGC: ', flush ].
prolog_message(agc(done(Collected, Remaining, Time))) -->
    [ at_same_line,
      'reclaimed ~D atoms in ~3f sec. (remaining: ~D)'-
      [Collected, Time, Remaining]
    ].
prolog_message(cgc(start)) -->
    thread_context,
    [ 'CGC: ', flush ].
prolog_message(cgc(done(CollectedClauses, _CollectedBytes,
                        RemainingBytes, Time))) -->
    [ at_same_line,
      'reclaimed ~D clauses in ~3f sec. (pending: ~D bytes)'-
      [CollectedClauses, Time, RemainingBytes]
    ].

		 /*******************************
		 *        STACK OVERFLOW	*
		 *******************************/

out_of_stack(Context) -->
    { human_stack_size(Context.localused,   Local),
      human_stack_size(Context.globalused,  Global),
      human_stack_size(Context.trailused,   Trail),
      human_stack_size(Context.stack_limit, Limit),
      LCO is (100*(Context.depth - Context.environments))/Context.depth
    },
    [ 'Stack limit (~s) exceeded'-[Limit], nl,
      '  Stack sizes: local: ~s, global: ~s, trail: ~s'-[Local,Global,Trail], nl,
      '  Stack depth: ~D, last-call: ~0f%, Choice points: ~D'-
         [Context.depth, LCO, Context.choicepoints], nl
    ],
    overflow_reason(Context, Resolve),
    resolve_overflow(Resolve).

human_stack_size(Size, String) :-
    Size < 100,
    format(string(String), '~dKb', [Size]).
human_stack_size(Size, String) :-
    Size < 100 000,
    Value is Size / 1024,
    format(string(String), '~1fMb', [Value]).
human_stack_size(Size, String) :-
    Value is Size / (1024*1024),
    format(string(String), '~1fGb', [Value]).

overflow_reason(Context, fix) -->
    show_non_termination(Context),
    !.
overflow_reason(Context, enlarge) -->
    { Stack = Context.get(stack) },
    !,
    [ '  In:'-[], nl ],
    stack(Stack).
overflow_reason(_Context, enlarge) -->
    [ '  Insufficient global stack'-[] ].

show_non_termination(Context) -->
    (   { Stack = Context.get(cycle) }
    ->  [ '  Probable infinite recursion (cycle):'-[], nl ]
    ;   { Stack = Context.get(non_terminating) }
    ->  [ '  Possible non-terminating recursion:'-[], nl ]
    ),
    stack(Stack).

stack([]) --> [].
stack([frame(Depth, M:Goal, _)|T]) -->
    [ '    [~D] ~q:'-[Depth, M] ],
    stack_goal(Goal),
    [ nl ],
    stack(T).

stack_goal(Goal) -->
    { compound(Goal),
      !,
      compound_name_arity(Goal, Name, Arity)
    },
    [ '~q('-[Name] ],
    stack_goal_args(1, Arity, Goal),
    [ ')'-[] ].
stack_goal(Goal) -->
    [ '~q'-[Goal] ].

stack_goal_args(I, Arity, Goal) -->
    { I =< Arity,
      !,
      arg(I, Goal, A),
      I2 is I + 1
    },
    stack_goal_arg(A),
    (   { I2 =< Arity }
    ->  [ ', '-[] ],
        stack_goal_args(I2, Arity, Goal)
    ;   []
    ).
stack_goal_args(_, _, _) -->
    [].

stack_goal_arg(A) -->
    { nonvar(A),
      A = [Len|T],
      !
    },
    (   {Len == cyclic_term}
    ->  [ '[cyclic list]'-[] ]
    ;   {T == []}
    ->  [ '[length:~D]'-[Len] ]
    ;   [ '[length:~D|~p]'-[Len, T] ]
    ).
stack_goal_arg(A) -->
    { nonvar(A),
      A = _/_,
      !
    },
    [ '<compound ~p>'-[A] ].
stack_goal_arg(A) -->
    [ '~p'-[A] ].

resolve_overflow(fix) -->
    [].
resolve_overflow(enlarge) -->
    { current_prolog_flag(stack_limit, LimitBytes),
      NewLimit is LimitBytes * 2
    },
    [ nl,
      'Use the --stack_limit=size[KMG] command line option or'-[], nl,
      '?- set_prolog_flag(stack_limit, ~I). to double the limit.'-[NewLimit]
    ].


                 /*******************************
                 *        MAKE/AUTOLOAD         *
                 *******************************/

prolog_message(make(reload(Files))) -->
    { length(Files, N)
    },
    [ 'Make: reloading ~D files'-[N] ].
prolog_message(make(done(_Files))) -->
    [ 'Make: finished' ].
prolog_message(make(library_index(Dir))) -->
    [ 'Updating index for library ~w'-[Dir] ].
prolog_message(autoload(Pred, File)) -->
    thread_context,
    [ 'autoloading ~p from ~w'-[Pred, File] ].
prolog_message(autoload(read_index(Dir))) -->
    [ 'Loading autoload index for ~w'-[Dir] ].


                 /*******************************
                 *       COMPILER WARNINGS      *
                 *******************************/

% print warnings about dubious code raised by the compiler.
% TBD: pass in PC to produce exact error locations.

prolog_message(compiler_warnings(Clause, Warnings0)) -->
    {   print_goal_options(DefOptions),
        (   prolog_load_context(variable_names, VarNames)
        ->  warnings_with_named_vars(Warnings0, VarNames, Warnings),
            Options = [variable_names(VarNames)|DefOptions]
        ;   Options = DefOptions,
            Warnings = Warnings0
        )
    },
    compiler_warnings(Warnings, Clause, Options).

warnings_with_named_vars([], _, []).
warnings_with_named_vars([H|T0], VarNames, [H|T]) :-
    term_variables(H, Vars),
    '$member'(V1, Vars),
    '$member'(_=V2, VarNames),
    V1 == V2,
    !,
    warnings_with_named_vars(T0, VarNames, T).
warnings_with_named_vars([_|T0], VarNames, T) :-
    warnings_with_named_vars(T0, VarNames, T).


compiler_warnings([], _, _) --> [].
compiler_warnings([H|T], Clause, Options) -->
    (   compiler_warning(H, Clause, Options)
    ->  []
    ;   [ 'Unknown compiler warning: ~W'-[H,Options] ]
    ),
    (   {T==[]}
    ->  []
    ;   [nl]
    ),
    compiler_warnings(T, Clause, Options).

compiler_warning(eq_vv(A,B), _Clause, Options) -->
    (   { A == B }
    ->  [ 'Test is always true: ~W'-[A==B, Options] ]
    ;   [ 'Test is always false: ~W'-[A==B, Options] ]
    ).
compiler_warning(eq_singleton(A,B), _Clause, Options) -->
    [ 'Test is always false: ~W'-[A==B, Options] ].
compiler_warning(neq_vv(A,B), _Clause, Options) -->
    (   { A \== B }
    ->  [ 'Test is always true: ~W'-[A\==B, Options] ]
    ;   [ 'Test is always false: ~W'-[A\==B, Options] ]
    ).
compiler_warning(neq_singleton(A,B), _Clause, Options) -->
    [ 'Test is always true: ~W'-[A\==B, Options] ].
compiler_warning(unify_singleton(A,B), _Clause, Options) -->
    [ 'Unified variable is not used: ~W'-[A=B, Options] ].
compiler_warning(always(Bool, Pred, Arg), _Clause, Options) -->
    { Goal =.. [Pred,Arg] },
    [ 'Test is always ~w: ~W'-[Bool, Goal, Options] ].
compiler_warning(unbalanced_var(V), _Clause, Options) -->
    [ 'Variable not introduced in all branches: ~W'-[V, Options] ].
compiler_warning(branch_singleton(V), _Clause, Options) -->
    [ 'Singleton variable in branch: ~W'-[V, Options] ].
compiler_warning(negation_singleton(V), _Clause, Options) -->
    [ 'Singleton variable in \\+: ~W'-[V, Options] ].
compiler_warning(multiton(V), _Clause, Options) -->
    [ 'Singleton-marked variable appears more than once: ~W'-[V, Options] ].

print_goal_options(
    [ quoted(true),
      portray(true)
    ]).


                 /*******************************
                 *      TOPLEVEL MESSAGES       *
                 *******************************/

prolog_message(version) -->
    { current_prolog_flag(version_git, Version) },
    !,
    [ '~w'-[Version] ].
prolog_message(version) -->
    { current_prolog_flag(version_data, swi(Major,Minor,Patch,Options))
    },
    (   { memberchk(tag(Tag), Options) }
    ->  [ '~w.~w.~w-~w'-[Major, Minor, Patch, Tag] ]
    ;   [ '~w.~w.~w'-[Major, Minor, Patch] ]
    ).
prolog_message(address_bits) -->
    { current_prolog_flag(address_bits, Bits)
    },
    !,
    [ '~d bits, '-[Bits] ].
prolog_message(threads) -->
    { current_prolog_flag(threads, true)
    },
    !,
    [ 'threaded, ' ].
prolog_message(threads) -->
    [].
prolog_message(copyright) -->
    [ 'SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.', nl,
      'Please run ?- license. for legal details.'
    ].
prolog_message(user_versions) -->
    (   { findall(Msg, prolog:version_msg(Msg), Msgs),
          Msgs \== []
        }
    ->  [nl],
        user_version_messages(Msgs)
    ;   []
    ).
prolog_message(documentaton) -->
    [ 'For online help and background, visit http://www.swi-prolog.org', nl,
      'For built-in help, use ?- help(Topic). or ?- apropos(Word).'
    ].
prolog_message(welcome) -->
    [ 'Welcome to SWI-Prolog (' ],
    prolog_message(threads),
    prolog_message(address_bits),
    ['version ' ],
    prolog_message(version),
    [ ')', nl ],
    prolog_message(copyright),
    [ nl ],
    prolog_message(user_versions),
    [ nl ],
    prolog_message(documentaton),
    [ nl, nl ].
prolog_message(about) -->
    [ 'SWI-Prolog version (' ],
    prolog_message(threads),
    prolog_message(address_bits),
    ['version ' ],
    prolog_message(version),
    [ ')', nl ],
    prolog_message(copyright).
prolog_message(halt) -->
    [ 'halt' ].
prolog_message(break(begin, Level)) -->
    [ 'Break level ~d'-[Level] ].
prolog_message(break(end, Level)) -->
    [ 'Exit break level ~d'-[Level] ].
prolog_message(var_query(_)) -->
    [ '... 1,000,000 ............ 10,000,000 years later', nl, nl,
      '~t~8|>> 42 << (last release gives the question)'
    ].
prolog_message(close_on_abort(Stream)) -->
    [ 'Abort: closed stream ~p'-[Stream] ].
prolog_message(cancel_halt(Reason)) -->
    [ 'Halt cancelled: ~p'-[Reason] ].

prolog_message(query(QueryResult)) -->
    query_result(QueryResult).

query_result(no) -->            % failure
    [ ansi([bold,fg(red)], 'false.', []) ],
    extra_line.
query_result(yes([])) -->      % prompt_alternatives_on: groundness
    !,
    [ ansi(bold, 'true.', []) ],
    extra_line.
query_result(yes(Residuals)) -->
    result([], Residuals),
    extra_line.
query_result(done) -->          % user typed <CR>
    extra_line.
query_result(yes(Bindings, Residuals)) -->
    result(Bindings, Residuals),
    prompt(yes, Bindings, Residuals).
query_result(more(Bindings, Residuals)) -->
    result(Bindings, Residuals),
    prompt(more, Bindings, Residuals).
query_result(help) -->
    [ nl, 'Actions:'-[], nl, nl,
      '; (n, r, space, TAB): redo    t:          trace & redo'-[], nl,
      'b:                    break   c (a, RET): exit'-[], nl,
      'w:                    write   p           print'-[], nl,
      'h (?):                help'-[],
      nl, nl
    ].
query_result(action) -->
    [ 'Action? '-[], flush ].
query_result(confirm) -->
    [ 'Please answer \'y\' or \'n\'? '-[], flush ].
query_result(eof) -->
    [ nl ].
query_result(toplevel_open_line) -->
    [].

prompt(Answer, [], []-[]) -->
    !,
    prompt(Answer, empty).
prompt(Answer, _, _) -->
    !,
    prompt(Answer, non_empty).

prompt(yes, empty) -->
    !,
    [ ansi(bold, 'true.', []) ],
    extra_line.
prompt(yes, _) -->
    !,
    [ full_stop ],
    extra_line.
prompt(more, empty) -->
    !,
    [ ansi(bold, 'true ', []), flush ].
prompt(more, _) -->
    !,
    [ ' '-[], flush ].

result(Bindings, Residuals) -->
    { current_prolog_flag(answer_write_options, Options0),
      Options = [partial(true)|Options0]
    },
    bindings(Bindings, [priority(699)|Options]),
    bind_res_sep(Bindings, Residuals),
    residuals(Residuals, [priority(999)|Options]).

bindings([], _) -->
    [].
bindings([binding(Names,Skel,Subst)|T], Options) -->
    { '$last'(Names, Name) },
    var_names(Names), value(Name, Skel, Subst, Options),
    (   { T \== [] }
    ->  [ ','-[], nl ],
        bindings(T, Options)
    ;   []
    ).

var_names([Name]) -->
    !,
    [ '~w = '-[Name] ].
var_names([Name1,Name2|T]) -->
    !,
    [ '~w = ~w, '-[Name1, Name2] ],
    var_names([Name2|T]).


value(Name, Skel, Subst, Options) -->
    (   { var(Skel), Subst = [Skel=S] }
    ->  { Skel = '$VAR'(Name) },
        [ '~W'-[S, Options] ]
    ;   [ '~W'-[Skel, Options] ],
        substitution(Subst, Options)
    ).

substitution([], _) --> !.
substitution([N=V|T], Options) -->
    [ ', ', ansi(fg(green), '% where', []), nl,
      '    ~w = ~W'-[N,V,Options] ],
    substitutions(T, Options).

substitutions([], _) --> [].
substitutions([N=V|T], Options) -->
    [ ','-[], nl, '    ~w = ~W'-[N,V,Options] ],
    substitutions(T, Options).


residuals(Normal-Hidden, Options) -->
    residuals1(Normal, Options),
    bind_res_sep(Normal, Hidden),
    (   {Hidden == []}
    ->  []
    ;   [ansi(fg(green), '% with pending residual goals', []), nl]
    ),
    residuals1(Hidden, Options).

residuals1([], _) -->
    [].
residuals1([G|Gs], Options) -->
    (   { Gs \== [] }
    ->  [ '~W,'-[G, Options], nl ],
        residuals1(Gs, Options)
    ;   [ '~W'-[G, Options] ]
    ).

bind_res_sep(_, []) --> !.
bind_res_sep(_, []-[]) --> !.
bind_res_sep([], _) --> !.
bind_res_sep(_, _) --> [','-[], nl].

extra_line -->
    { current_prolog_flag(toplevel_extra_white_line, true) },
    !,
    ['~N'-[]].
extra_line -->
    [].

prolog_message(if_tty(Message)) -->
    (   {current_prolog_flag(tty_control, true)}
    ->  [ at_same_line | Message ]
    ;   []
    ).
prolog_message(halt(Reason)) -->
    [ '~w: halt'-[Reason] ].
prolog_message(no_action(Char)) -->
    [ 'Unknown action: ~c (h for help)'-[Char], nl ].

prolog_message(history(help(Show, Help))) -->
    [ 'History Commands:', nl,
      '    !!.              Repeat last query', nl,
      '    !nr.             Repeat query numbered <nr>', nl,
      '    !str.            Repeat last query starting with <str>', nl,
      '    !?str.           Repeat last query holding <str>', nl,
      '    ^old^new.        Substitute <old> into <new> of last query', nl,
      '    !nr^old^new.     Substitute in query numbered <nr>', nl,
      '    !str^old^new.    Substitute in query starting with <str>', nl,
      '    !?str^old^new.   Substitute in query holding <str>', nl,
      '    ~w.~21|Show history list'-[Show], nl,
      '    ~w.~21|Show this list'-[Help], nl, nl
    ].
prolog_message(history(no_event)) -->
    [ '! No such event' ].
prolog_message(history(bad_substitution)) -->
    [ '! Bad substitution' ].
prolog_message(history(expanded(Event))) -->
    [ '~w.'-[Event] ].
prolog_message(history(history(Events))) -->
    history_events(Events).

history_events([]) -->
    [].
history_events([Nr/Event|T]) -->
    [ '~t~w   ~8|~W~W'-[ Nr,
                         Event, [partial(true)],
                         '.', [partial(true)]
                       ],
      nl
    ],
    history_events(T).


user_version_messages([]) --> [].
user_version_messages([H|T]) -->
    user_version_message(H),
    user_version_messages(T).

%!  user_version_message(+Term)

user_version_message(Term) -->
    translate_message2(Term), !, [nl].
user_version_message(Atom) -->
    [ '~w'-[Atom], nl ].


                 /*******************************
                 *       DEBUGGER MESSAGES      *
                 *******************************/

prolog_message(spy(Head)) -->
    { goal_to_predicate_indicator(Head, Pred)
    },
    [ 'Spy point on ~p'-[Pred] ].
prolog_message(nospy(Head)) -->
    { goal_to_predicate_indicator(Head, Pred)
    },
    [ 'Spy point removed from ~p'-[Pred] ].
prolog_message(trace_mode(Bool)) -->
    [ 'Trace mode switched to ~w'-[Bool] ].
prolog_message(debug_mode(Bool)) -->
    [ 'Debug mode switched to ~w'-[Bool] ].
prolog_message(debugging(Bool)) -->
    [ 'Debug mode is ~w'-[Bool] ].
prolog_message(spying([])) -->
    !,
    [ 'No spy points' ].
prolog_message(spying(Heads)) -->
    [ 'Spy points (see spy/1) on:', nl ],
    predicate_list(Heads).
prolog_message(trace(Head, [])) -->
    !,
    { goal_to_predicate_indicator(Head, Pred)
    },
    [ '        ~p: Not tracing'-[Pred], nl].
prolog_message(trace(Head, Ports)) -->
    { goal_to_predicate_indicator(Head, Pred)
    },
    [ '        ~p: ~w'-[Pred, Ports], nl].
prolog_message(tracing([])) -->
    !,
    [ 'No traced predicates (see trace/1)' ].
prolog_message(tracing(Heads)) -->
    [ 'Trace points (see trace/1) on:', nl ],
    tracing_list(Heads).

predicate_list([]) -->                  % TBD: Share with dwim, etc.
    [].
predicate_list([H|T]) -->
    { goal_to_predicate_indicator(H, Pred)
    },
    [ '        ~p'-[Pred], nl],
    predicate_list(T).

tracing_list([]) -->
    [].
tracing_list([trace(Head, Ports)|T]) -->
    translate_message(trace(Head, Ports)),
    tracing_list(T).

prolog_message(frame(Frame, backtrace, _PC)) -->
    !,
    { prolog_frame_attribute(Frame, level, Level)
    },
    [ ansi(bold, '~t[~D] ~10|', [Level]) ],
    frame_context(Frame),
    frame_goal(Frame).
prolog_message(frame(Frame, choice, PC)) -->
    !,
    prolog_message(frame(Frame, backtrace, PC)).
prolog_message(frame(_, cut_call, _)) --> !, [].
prolog_message(frame(Frame, trace(Port), _PC)) -->
    !,
    [ ' T ' ],
    port(Port),
    frame_level(Frame),
    frame_context(Frame),
    frame_goal(Frame).
prolog_message(frame(Frame, Port, _PC)) -->
    frame_flags(Frame),
    port(Port),
    frame_level(Frame),
    frame_context(Frame),
    frame_depth_limit(Port, Frame),
    frame_goal(Frame),
    [ flush ].

frame_goal(Frame) -->
    { prolog_frame_attribute(Frame, goal, Goal0),
      clean_goal(Goal0, Goal),
      current_prolog_flag(debugger_write_options, Options)
    },
    [ '~W'-[Goal, Options] ].

frame_level(Frame) -->
    { prolog_frame_attribute(Frame, level, Level)
    },
    [ '(~D) '-[Level] ].

frame_context(Frame) -->
    (   { current_prolog_flag(debugger_show_context, true),
          prolog_frame_attribute(Frame, context_module, Context)
        }
    ->  [ '[~w] '-[Context] ]
    ;   []
    ).

frame_depth_limit(fail, Frame) -->
    { prolog_frame_attribute(Frame, depth_limit_exceeded, true)
    },
    !,
    [ '[depth-limit exceeded] ' ].
frame_depth_limit(_, _) -->
    [].

frame_flags(Frame) -->
    { prolog_frame_attribute(Frame, goal, Goal),
      (   predicate_property(Goal, transparent)
      ->  T = '^'
      ;   T = ' '
      ),
      (   predicate_property(Goal, spying)
      ->  S = '*'
      ;   S = ' '
      )
    },
    [ '~w~w '-[T, S] ].

port(Port) -->
    { port_name(Port, Colour, Name)
    },
    !,
    [ ansi([bold,fg(Colour)], '~w: ', [Name]) ].

port_name(call,      green,   'Call').
port_name(exit,      green,   'Exit').
port_name(fail,      red,     'Fail').
port_name(redo,      yellow,  'Redo').
port_name(unify,     blue,    'Unify').
port_name(exception, magenta, 'Exception').

clean_goal(M:Goal, Goal) :-
    hidden_module(M),
    !.
clean_goal(M:Goal, Goal) :-
    predicate_property(M:Goal, built_in),
    !.
clean_goal(Goal, Goal).


                 /*******************************
                 *        COMPATIBILITY         *
                 *******************************/

prolog_message(compatibility(renamed(Old, New))) -->
    [ 'The predicate ~p has been renamed to ~p.'-[Old, New], nl,
      'Please update your sources for compatibility with future versions.'
    ].


                 /*******************************
                 *            THREADS           *
                 *******************************/

prolog_message(abnormal_thread_completion(Goal, exception(Ex))) -->
    !,
    [ 'Thread running "~p" died on exception: '-[Goal] ],
    translate_message(Ex).
prolog_message(abnormal_thread_completion(Goal, fail)) -->
    [ 'Thread running "~p" died due to failure'-[Goal] ].
prolog_message(threads_not_died(Running)) -->
    [ 'The following threads wouldn\'t die: ~p'-[Running] ].


                 /*******************************
                 *             PACKS            *
                 *******************************/

prolog_message(pack(attached(Pack, BaseDir))) -->
    [ 'Attached package ~w at ~q'-[Pack, BaseDir] ].
prolog_message(pack(duplicate(Entry, OldDir, Dir))) -->
    [ 'Package ~w already attached at ~q.'-[Entry,OldDir], nl,
      '\tIgnoring version from ~q'- [Entry, OldDir, Dir]
    ].
prolog_message(pack(no_arch(Entry, Arch))) -->
    [ 'Package ~w: no binary for architecture ~w'-[Entry, Arch] ].

                 /*******************************
                 *             MISC             *
                 *******************************/

prolog_message(null_byte_in_path(Component)) -->
    [ '0-byte in PATH component: ~p (skipped directory)'-[Component] ].
prolog_message(invalid_tmp_dir(Dir, Reason)) -->
    [ 'Cannot use ~p as temporary file directory: ~w'-[Dir, Reason] ].
prolog_message(ambiguous_stream_pair(Pair)) -->
    [ 'Ambiguous operation on stream pair ~p'-[Pair] ].

env(Name) -->
    { current_prolog_flag(windows, true) },
    [ '%~w%'-[Name] ].
env(Name) -->
    [ '$~w'-[Name] ].

		 /*******************************
		 *          DEPRECATED		*
		 *******************************/

deprecated(set_prolog_stack(_Stack,limit)) -->
    [ 'set_prolog_stack/2: limit(Size) sets the combined limit.'-[], nl,
      'See http://www.swi-prolog.org/changes/stack-limit.html'
    ].


                 /*******************************
                 *      PRINTING MESSAGES       *
                 *******************************/

:- multifile
    user:message_hook/3,
    prolog:message_prefix_hook/2.
:- dynamic
    user:message_hook/3,
    prolog:message_prefix_hook/2.
:- thread_local
    user:thread_message_hook/3.

%!  print_message(+Kind, +Term)
%
%   Print an error message using a term as generated by the exception
%   system.

print_message(Level, Term) :-
    (   must_print(Level, Term)
    ->  (   translate_message(Term, Lines, [])
        ->  (   nonvar(Term),
                (   notrace(user:thread_message_hook(Term, Level, Lines))
                ->  true
                ;   notrace(user:message_hook(Term, Level, Lines))
                )
            ->  true
            ;   print_system_message(Term, Level, Lines)
            )
        )
    ;   true
    ).

%!  print_system_message(+Term, +Kind, +Lines)
%
%   Print the message if the user did not intecept the message.
%   The first is used for errors and warnings that can be related
%   to source-location.  Note that syntax errors have their own
%   source-location and should therefore not be handled this way.

print_system_message(_, silent, _) :- !.
print_system_message(_, informational, _) :-
    current_prolog_flag(verbose, silent),
    !.
print_system_message(_, banner, _) :-
    current_prolog_flag(verbose, silent),
    !.
print_system_message(_, _, []) :- !.
print_system_message(Term, Kind, Lines) :-
    catch(flush_output(user_output), _, true),      % may not exist
    source_location(File, Line),
    Term \= error(syntax_error(_), _),
    msg_property(Kind, location_prefix(File:Line, LocPrefix, LinePrefix)),
    !,
    insert_prefix(Lines, LinePrefix, Ctx, PrefixLines),
    '$append'([ begin(Kind, Ctx),
                LocPrefix,
                nl
              | PrefixLines
              ],
              [ end(Ctx)
              ],
              AllLines),
    msg_property(Kind, stream(Stream)),
    ignore(stream_property(Stream, position(Pos))),
    print_message_lines(Stream, AllLines),
    (   \+ stream_property(Stream, position(Pos)),
        msg_property(Kind, wait(Wait)),
        Wait > 0
    ->  sleep(Wait)
    ;   true
    ).
print_system_message(_, Kind, Lines) :-
    msg_property(Kind, stream(Stream)),
    print_message_lines(Stream, kind(Kind), Lines).

:- multifile
    user:message_property/2.

msg_property(Kind, Property) :-
    user:message_property(Kind, Property),
    !.
msg_property(Kind, prefix(Prefix)) :-
    msg_prefix(Kind, Prefix),
    !.
msg_property(_, prefix('~N')) :- !.
msg_property(query, stream(user_output)) :- !.
msg_property(_, stream(user_error)) :- !.
msg_property(error,
             location_prefix(File:Line,
                             '~NERROR: ~w:~d:'-[File,Line], '~N\t')) :- !.
msg_property(warning,
             location_prefix(File:Line,
                             '~NWarning: ~w:~d:'-[File,Line], '~N\t')) :- !.
msg_property(error,   wait(0.1)) :- !.

msg_prefix(debug(_), Prefix) :-
    msg_context('~N% ', Prefix).
msg_prefix(warning, Prefix) :-
    msg_context('~NWarning: ', Prefix).
msg_prefix(error, Prefix) :-
    msg_context('~NERROR: ', Prefix).
msg_prefix(informational, '~N% ').
msg_prefix(information,   '~N% ').

%!  msg_context(+Prefix0, -Prefix) is det.
%
%   Add contextual information to a message.   This uses the Prolog flag
%   `message_context`. Recognised context terms are:
%
%     - time
%     - time(Format)
%     - thread
%
%   In addition, the hook prolog:message_prefix_hook/2   is  called that
%   allows for additional context information.

msg_context(Prefix0, Prefix) :-
    current_prolog_flag(message_context, Context),
    is_list(Context),
    !,
    add_message_context(Context, Prefix0, Prefix).
msg_context(Prefix, Prefix).

add_message_context([], Prefix, Prefix).
add_message_context([H|T], Prefix0, Prefix) :-
    (   add_message_context1(H, Prefix0, Prefix1)
    ->  true
    ;   Prefix1 = Prefix0
    ),
    add_message_context(T, Prefix1, Prefix).

add_message_context1(Context, Prefix0, Prefix) :-
    prolog:message_prefix_hook(Context, Extra),
    atomics_to_string([Prefix0, Extra, ' '], Prefix).
add_message_context1(time, Prefix0, Prefix) :-
    get_time(Now),
    format_time(string(S), '%T.%3f ', Now),
    string_concat(Prefix0, S, Prefix).
add_message_context1(time(Format), Prefix0, Prefix) :-
    get_time(Now),
    format_time(string(S), Format, Now),
    atomics_to_string([Prefix0, S, ' '], Prefix).
add_message_context1(thread, Prefix0, Prefix) :-
    thread_self(Id0),
    Id0 \== main,
    !,
    (   atom(Id0)
    ->  Id = Id0
    ;   thread_property(Id0, id(Id))
    ),
    format(string(Prefix), '~w[Thread ~w] ', [Prefix0, Id]).

%!  print_message_lines(+Stream, +PrefixOrKind, +Lines)
%
%   Quintus compatibility predicate to print message lines using
%   a prefix.

print_message_lines(Stream, kind(Kind), Lines) :-
    !,
    msg_property(Kind, prefix(Prefix)),
    insert_prefix(Lines, Prefix, Ctx, PrefixLines),
    '$append'([ begin(Kind, Ctx)
              | PrefixLines
              ],
              [ end(Ctx)
              ],
              AllLines),
    print_message_lines(Stream, AllLines).
print_message_lines(Stream, Prefix, Lines) :-
    insert_prefix(Lines, Prefix, _, PrefixLines),
    print_message_lines(Stream, PrefixLines).

%!  insert_prefix(+Lines, +Prefix, +Ctx, -PrefixedLines)

insert_prefix([at_same_line|Lines0], Prefix, Ctx, Lines) :-
    !,
    prefix_nl(Lines0, Prefix, Ctx, Lines).
insert_prefix(Lines0, Prefix, Ctx, [prefix(Prefix)|Lines]) :-
    prefix_nl(Lines0, Prefix, Ctx, Lines).

prefix_nl([], _, _, [nl]).
prefix_nl([nl], _, _, [nl]) :- !.
prefix_nl([flush], _, _, [flush]) :- !.
prefix_nl([nl|T0], Prefix, Ctx, [nl, prefix(Prefix)|T]) :-
    !,
    prefix_nl(T0, Prefix, Ctx, T).
prefix_nl([ansi(Attrs,Fmt,Args)|T0], Prefix, Ctx,
          [ansi(Attrs,Fmt,Args,Ctx)|T]) :-
    !,
    prefix_nl(T0, Prefix, Ctx, T).
prefix_nl([H|T0], Prefix, Ctx, [H|T]) :-
    prefix_nl(T0, Prefix, Ctx, T).

%!  print_message_lines(+Stream, +Lines)

print_message_lines(Stream, Lines) :-
    with_output_to(
        Stream,
        notrace(print_message_lines_guarded(current_output, Lines))).

print_message_lines_guarded(_, []) :- !.
print_message_lines_guarded(S, [H|T]) :-
    line_element(S, H),
    print_message_lines_guarded(S, T).

line_element(S, E) :-
    prolog:message_line_element(S, E),
    !.
line_element(S, full_stop) :-
    !,
    '$put_token'(S, '.').           % insert space if needed.
line_element(S, nl) :-
    !,
    nl(S).
line_element(S, prefix(Fmt-Args)) :-
    !,
    safe_format(S, Fmt, Args).
line_element(S, prefix(Fmt)) :-
    !,
    safe_format(S, Fmt, []).
line_element(S, flush) :-
    !,
    flush_output(S).
line_element(S, Fmt-Args) :-
    !,
    safe_format(S, Fmt, Args).
line_element(S, ansi(_, Fmt, Args)) :-
    !,
    safe_format(S, Fmt, Args).
line_element(S, ansi(_, Fmt, Args, _Ctx)) :-
    !,
    safe_format(S, Fmt, Args).
line_element(_, begin(_Level, _Ctx)) :- !.
line_element(_, end(_Ctx)) :- !.
line_element(S, Fmt) :-
    safe_format(S, Fmt, []).

%!  safe_format(+Stream, +Format, +Args) is det.

safe_format(S, Fmt, Args) :-
    E = error(_,_),
    catch(format(S,Fmt,Args), E,
          format_failed(S,Fmt,Args,E)).

format_failed(S, _Fmt, _Args, E) :-
    E = error(io_error(_,S),_),
    !,
    throw(E).
format_failed(S, Fmt, Args, error(E,_)) :-
    format(S, '~N    [[ EXCEPTION while printing message ~q~n\c
                        ~7|with arguments ~W:~n\c
                        ~7|raised: ~W~n~4|]]~n',
           [ Fmt,
             Args, [quoted(true), max_depth(10)],
             E, [quoted(true), max_depth(10)]
           ]).

%!  message_to_string(+Term, -String)
%
%   Translate an error term into a string

message_to_string(Term, Str) :-
    translate_message(Term, Actions, []),
    !,
    actions_to_format(Actions, Fmt, Args),
    format(string(Str), Fmt, Args).

actions_to_format([], '', []) :- !.
actions_to_format([nl], '', []) :- !.
actions_to_format([Term, nl], Fmt, Args) :-
    !,
    actions_to_format([Term], Fmt, Args).
actions_to_format([nl|T], Fmt, Args) :-
    !,
    actions_to_format(T, Fmt0, Args),
    atom_concat('~n', Fmt0, Fmt).
actions_to_format([Skip|T], Fmt, Args) :-
    action_skip(Skip),
    !,
    actions_to_format(T, Fmt, Args).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :-
    !,
    actions_to_format(Tail, Fmt1, Args1),
    atom_concat(Fmt0, Fmt1, Fmt),
    append_args(Args0, Args1, Args).
actions_to_format([Term|Tail], Fmt, Args) :-
    atomic(Term),
    !,
    actions_to_format(Tail, Fmt1, Args),
    atom_concat(Term, Fmt1, Fmt).
actions_to_format([Term|Tail], Fmt, Args) :-
    actions_to_format(Tail, Fmt1, Args1),
    atom_concat('~w', Fmt1, Fmt),
    append_args([Term], Args1, Args).

action_skip(at_same_line).
action_skip(flush).
action_skip(ansi(_Attrs, _Fmt, _Args)).
action_skip(begin(_Level, _Ctx)).
action_skip(end(_Ctx)).

append_args(M:Args0, Args1, M:Args) :-
    !,
    strip_module(Args1, _, A1),
    '$append'(Args0, A1, Args).
append_args(Args0, Args1, Args) :-
    strip_module(Args1, _, A1),
    '$append'(Args0, A1, Args).


                 /*******************************
                 *    MESSAGES TO PRINT ONCE    *
                 *******************************/

:- dynamic
    printed/2.

%!  print_once(Message, Level)
%
%   True for messages that must be printed only once.

print_once(compatibility(_), _).
print_once(null_byte_in_path(_), _).
print_once(deprecated(_), _).

%!  must_print(+Level, +Message)
%
%   True if the message must be printed.

must_print(Level, Message) :-
    nonvar(Message),
    print_once(Message, Level),
    !,
    \+ printed(Message, Level),
    assert(printed(Message, Level)).
must_print(_, _).

