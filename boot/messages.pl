/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  1997-2026, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module('$messages',
          [ print_message/2,            % +Kind, +Term
            print_message_lines/3,      % +Stream, +Prefix, +Lines
            message_to_string/2         % +Term, -String
          ]).

:- multifile
    prolog:message//1,              % entire message
    prolog:error_message//1,        % 1-st argument of error term
    prolog:message_context//1,      % Context of error messages
    prolog:deprecated//1,	    % Deprecated features
    prolog:message_location//1,     % (File) location of error messages
    prolog:message_line_element/2,  % Extend printing
    prolog:message_action/2.        % Side effects (broadcast)
:- dynamic
    prolog:message_action/2.        % Allow overruling
:- '$hide'((
    prolog:message//1,
    prolog:error_message//1,
    prolog:message_context//1,
    prolog:deprecated//1,
    prolog:message_location//1,
    prolog:message_line_element/2)).
% Lang, Term versions
:- multifile
    prolog:message//2,              % entire message
    prolog:error_message//2,        % 1-st argument of error term
    prolog:message_context//2,      % Context of error messages
    prolog:message_location//2,	    % (File) location of error messages
    prolog:deprecated//2.	    % Deprecated features
:- '$hide'((
    prolog:message//2,
    prolog:error_message//2,
    prolog:message_context//2,
    prolog:deprecated//2,
    prolog:message_location//2)).

:- discontiguous
    prolog_message/3.

:- public
    translate_message//1,           % +Message (deprecated)
    prolog:translate_message//1.    % +Message

:- create_prolog_flag(message_context, [thread], []).
:- create_prolog_flag(debugger_goal_links, auto,
                      [ type(oneof([false,true,auto])),
                        keep(true)
                      ]).

%!  translate_message(+Term)// is det.
%
%   Translate a message Term into message lines. The produced lines
%   is a list of
%
%       - nl
%         Emit a newline
%       - Fmt-Args
%         Emit the result of format(Fmt, Args)
%       - Fmt
%         Emit the result of format(Fmt)
%       - ansi(Class, Fmt, Args)
%         Use ansi_format/3 for color output.
%       - url(Location)
%         Emit a source location as a hyperlink.  Location is
%         File:Line:Column, File:Line, File or a URL.
%       - url(Location, Label)
%         As above, but print Label rather than Location.  Label is
%         plain text, Fmt-Args or ansi(Class, Fmt, Args), the latter
%         combining a hyperlink with a style class.
%       - flush
%         Used only as last element of the list.   Simply flush the
%         output instead of producing a final newline.
%       - at_same_line
%         Start the messages at the same line (instead of using ~N)
%       - eol
%         End the decorated part of the last line.  See
%         print_message_lines/3.
%
%   The elements begin(Class, Ctx) and end(Ctx) that decorate the
%   message as a whole are added by print_message_lines/3.
%
%   Use predicate_reference//1,2 to refer to a predicate rather than
%   formatting the predicate indicator by hand.
%
%   @deprecated  Use  code  for   message    translation   should   call
%   prolog:translate_message//1.

prolog:translate_message(Term) -->
    translate_message(Term).

%!  translate_message(+Term)// is det.
%
%   Translate a message term into  message   lines.  This version may be
%   called from user and library definitions for message translation.

translate_message(Term) -->
    { nonvar(Term) },
    (   { message_lang(Lang) },
        prolog:message(Lang, Term)
    ;   prolog:message(Term)
    ),
    !.
translate_message(Term) -->
    { nonvar(Term) },
    translate_message2(Term),
    !.
translate_message(Term) -->
    { nonvar(Term),
      Term = error(_, _)
    },
    [ 'Unknown exception: ~p'-[Term] ].
translate_message(Term) -->
    [ 'Unknown message: ~p'-[Term] ].

translate_message2(Term) -->
    prolog_message(Term).
translate_message2(error(resource_error(stack), Context)) -->
    !,
    out_of_stack(Context).
translate_message2(error(resource_error(tripwire(Wire, Context)), _)) -->
    !,
    tripwire_message(Wire, Context).
translate_message2(error(existence_error(reset, Ball), SWI)) -->
    swi_location(SWI),
    tabling_existence_error(Ball, SWI).
translate_message2(error(ISO, SWI)) -->
    swi_location(SWI),
    term_message(ISO),
    swi_extra(SWI).
translate_message2(unwind(Term)) -->
    unwind_message(Term).
translate_message2(message_lines(Lines), L, T) :- % deal with old C-warning()
    make_message_lines(Lines, L, T).
translate_message2(format(Fmt, Args)) -->
    [ Fmt-Args ].

make_message_lines([], T, T) :- !.
make_message_lines([Last],  ['~w'-[Last]|T], T) :- !.
make_message_lines([L0|LT], ['~w'-[L0],nl|T0], T) :-
    make_message_lines(LT, T0, T).

%!  term_message(+Term)//
%
%   Deal  with  the  formal  argument    of  error(Format,  ImplDefined)
%   exception  terms.  The  `ImplDefined`   argument    is   handled  by
%   swi_location//2.

:- public term_message//1.
term_message(Term) -->
    {var(Term)},
    !,
    [ 'Unknown error term: ~p'-[Term] ].
term_message(Term) -->
    { message_lang(Lang) },
    prolog:error_message(Lang, Term),
    !.
term_message(Term) -->
    prolog:error_message(Term),
    !.
term_message(Term) -->
    iso_message(Term).
term_message(Term) -->
    swi_message(Term).
term_message(Term) -->
    [ 'Unknown error term: ~p'-[Term] ].

iso_message(resource_error(c_stack)) -->
    out_of_c_stack.
iso_message(resource_error(Missing)) -->
    [ 'Not enough resources: ~w'-[Missing] ].
iso_message(type_error(Var, Actual)) -->
    { var(Var) },
    [ 'Type error: unbound (var) type expected, found `~p'''-[Actual] ].
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
    [ 'Unknown procedure: ' ],
    predicate_reference(Proc),
    unknown_proc_msg(Proc).
iso_message(existence_error(answer_variable, Var)) -->
    [ '$~w was not bound by a previous query'-[Var] ].
iso_message(existence_error(matching_rule, Goal)) -->
    [ 'No rule matches ~p'-[Goal] ].
iso_message(existence_error(Type, Object)) -->
    [ '~w `~p'' does not exist'-[Type, Object] ].
iso_message(existence_error(export, PI, module(M))) --> % not ISO
    [ 'Module ', ansi(code, '~q', [M]), ' does not export ' ],
    predicate_reference(M:PI, [module(hide)]).
iso_message(existence_error(Type, Object, In)) --> % not ISO
    [ '~w `~p'' does not exist in ~p'-[Type, Object, In] ].
iso_message(busy(Type, Object)) -->
    [ '~w `~p'' is busy'-[Type, Object] ].
iso_message(syntax_error(swi_backslash_newline)) -->
    [ 'Deprecated: ... \\<newline><white>*.  Use \\c' ].
iso_message(syntax_error(warning_var_tag)) -->
    [ 'Deprecated: dict with unbound tag (_{...}).  Mapped to #{...}.' ].
iso_message(syntax_error(var_tag)) -->
    [ 'Syntax error: dict syntax with unbound tag (_{...}).' ].
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
    [ 'No permission to ~w built-in predicate '-[Action] ],
    predicate_reference(Pred),
    (   {Action \== export}
    ->  [ nl,
          'Use :- redefine_system_predicate(+Head) if redefinition is intended'
        ]
    ;   []
    ).
permission_error(import_into(Dest), procedure, Pred) -->
    [ 'No permission to import ' ],
    predicate_reference(Pred),
    [ ' into ~w'-[Dest] ].
permission_error(Action, static_procedure, Proc) -->
    [ 'No permission to ~w static procedure '-[Action] ],
    predicate_reference(Proc),
    predicate_definition(Proc, 'Defined').
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
permission_error(tnot, non_tabled_procedure, Pred) -->
    [ 'The argument of ' ], predicate_reference(tnot/1),
    [ ' is not tabled: ' ], predicate_reference(Pred).
permission_error(assert, procedure, Pred) -->
    { predicate_head(Pred, Head),
      predicate_property(Head, ssu)
    },
    predicate_reference(Pred),
    [ ': an SSU (Head => Body) predicate cannot have normal Prolog clauses' ].
permission_error(Action, Type, Object) -->
    [ 'No permission to ~w ~w `~p'''-[Action, Type, Object] ].


unknown_proc_msg(_:(^)/2) -->
    !,
    unknown_proc_msg((^)/2).
unknown_proc_msg((^)/2) -->
    !,
    [nl, '  ^/2 can only appear as the 2nd argument of setof/3 and bagof/3'].
unknown_proc_msg((:-)/2) -->
    !,
    [nl, '  Rules must be loaded from a file'],
    faq('ToplevelMode').
unknown_proc_msg((=>)/2) -->
    !,
    [nl, '  Rules must be loaded from a file'],
    faq('ToplevelMode').
unknown_proc_msg((:-)/1) -->
    !,
    [nl, '  Directives must be loaded from a file'],
    faq('ToplevelMode').
unknown_proc_msg((?-)/1) -->
    !,
    [nl, '  ?- is the Prolog prompt'],
    faq('ToplevelMode').
unknown_proc_msg(Proc) -->
    { dwim_predicates(Proc, Dwims) },
    (   {Dwims \== []}
    ->  [nl, '  However, there are definitions for:', nl],
        dwim_alternatives(Dwims)
    ;   []
    ).

dependency_error(shared(Shared), private(Private)) -->
    [ 'Shared table for ' ], predicate_reference(Shared),
    [ ' may not depend on private ' ], predicate_reference(Private).
dependency_error(Dep, monotonic(On)) -->
    [ 'Dependent ' ], predicate_reference(Dep),
    [ ' on monotonic predicate ' ], predicate_reference(On),
    [ ' is not monotonic or incremental' ].

faq(Page) -->
    [nl, '  See FAQ at https://www.swi-prolog.org/FAQ/', Page, '.html' ].

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
    ;   Term = [_|_]      -> list_like(Term, Type)
    ;   cyclic_term(Term) -> Type = cyclic
    ;   compound(Term)    -> Type = compound
    ;                        Type = unknown
    ).

list_like(Term, Type) :-
    '$skip_list'(_, Term, Tail),
    (   var(Tail)
    ->  Type = partial_list
    ;   Type = invalid_list                      % TBD: Better name?
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
    [ 'Unknown character escape in quoted atom or string: `\\~w\''-[C] ].
syntax_error(void_not_allowed) -->
    [ 'Empty argument list "()"' ].
syntax_error(Term) -->
    { compound(Term),
      compound_name_arguments(Term, Syntax, [Text])
    }, !,
    [ '~w expected, found '-[Syntax], ansi(code, '"~w"', [Text]) ].
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

%!  tabling_existence_error(+Ball, +Context)//
%
%   Called on invalid shift/1  calls.  Track   those  that  result  from
%   tabling errors.

tabling_existence_error(Ball, Context) -->
    { table_shift_ball(Ball) },
    [ 'Tabling dependency error' ],
    swi_extra(Context).

table_shift_ball(dependency(_Head)).
table_shift_ball(dependency(_Skeleton, _Trie, _Mono)).
table_shift_ball(call_info(_Skeleton, _Status)).
table_shift_ball(call_info(_GenSkeleton, _Skeleton, _Status)).

%!  dwim_predicates(+PI, -Dwims)
%
%   Find related predicate indicators.

dwim_predicates(Module:Name/_Arity, Dwims) :-
    !,
    findall(Dwim, dwim_predicate(Module:Name, Dwim), Dwims).
dwim_predicates(Name/_Arity, Dwims) :-
    findall(Dwim, dwim_predicate(user:Name, Dwim), Dwims).

dwim_alternatives([]) --> [].
dwim_alternatives([H|T]) -->
    [ '        ' ],
    predicate_reference(H, [tag(true)]),
    [ nl ],
    dwim_alternatives(T).

swi_message(io_error(Op, Stream)) -->
    [ 'I/O error in ~w on stream ~p'-[Op, Stream] ].
swi_message(thread_error(TID, false)) -->
    [ 'Thread ~p died due to failure:'-[TID] ].
swi_message(thread_error(TID, exception(Error))) -->
    [ 'Thread ~p died abnormally:'-[TID], nl ],
    translate_message(Error).
swi_message(dependency_error(Tabled, DependsOn)) -->
    dependency_error(Tabled, DependsOn).
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
    [ 'Wrong context: ' ], predicate_reference(Goal),
    [ ' can only be used in a directive' ].
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
swi_message(conditional_compilation_error(unterminated, File:Line)) -->
    [ 'Unterminated conditional compilation from '-[], url(File:Line) ].
swi_message(conditional_compilation_error(no_if, What)) -->
    [ ':- ~w without :- if'-[What] ].
swi_message(duplicate_key(Key)) -->
    [ 'Duplicate key: ~p'-[Key] ].
swi_message(determinism_error(PI, det, Found, property)) -->
    (   { predicate_head(PI, Head),
          predicate_property(Head, det)
        }
    ->  [ 'Deterministic procedure ' ], predicate_reference(PI)
    ;   [ 'Procedure ' ], predicate_reference(PI),
        [ ' called from a deterministic procedure' ]
    ),
    det_error(Found).
swi_message(determinism_error(PI, det, fail, guard)) -->
    [ 'Procedure ' ], predicate_reference(PI),
    [ ' failed after $-guard' ].
swi_message(determinism_error(PI, det, fail, guard_in_caller)) -->
    [ 'Procedure ' ], predicate_reference(PI),
    [ ' failed after $-guard in caller' ].
swi_message(determinism_error(Goal, det, fail, goal)) -->
    [ 'Goal ~p failed'-[Goal] ].
swi_message(determinism_error(Goal, det, nondet, goal)) -->
    [ 'Goal ~p succeeded with a choice point'-[Goal] ].
swi_message(qlf_format_error(File, Message)) -->
    [ '~w: Invalid QLF file: ~w'-[File, Message] ].
swi_message(goal_expansion_error(bound, Term)) -->
    [ 'Goal expansion bound a variable to ~p'-[Term] ].

det_error(nondet) -->
    [ ' succeeded with a choicepoint'- [] ].
det_error(fail) -->
    [ ' failed'- [] ].


%!  swi_location(+Term)// is det.
%
%   Print location information for error(Formal,   ImplDefined) from the
%   ImplDefined term.

:- public swi_location//1.
swi_location(X) -->
    { var(X) },
    !.
swi_location(Context) -->
    { message_lang(Lang) },
    prolog:message_location(Lang, Context),
    !.
swi_location(Context) -->
    prolog:message_location(Context),
    !.
swi_location(context(Caller, _Msg)) -->
    { ground(Caller) },
    !,
    caller(Caller).
swi_location(file(Path, Line, -1, _CharNo)) -->
    !,
    [ url(Path:Line), ': ' ].
swi_location(file(Path, Line, LinePos, _CharNo)) -->
    { Column is LinePos+1 },                    % line_position is 0-based
    [ url(Path:Line:Column), ': ' ].
swi_location(stream(Stream, Line, LinePos, CharNo)) -->
    (   { is_stream(Stream),
          stream_property(Stream, file_name(File))
        }
    ->  swi_location(file(File, Line, LinePos, CharNo))
    ;   { Column is LinePos+1 },
        [ 'Stream ~w:~d:~d '-[Stream, Line, Column] ]
    ).
swi_location(autoload(File:Line)) -->
    [ url(File:Line), ': ' ].
swi_location(_) -->
    [].

caller(system:'$record_clause'/3) -->
    !,
    [].
caller(Caller) -->
    { predicate_indicator(Caller, _) },
    !,
    predicate_reference(Caller, [link(false)]),
    [ ': ' ].
caller(Caller) -->
    [ '~p: '-[Caller] ].


%!  swi_extra(+Term)// is det.
%
%   Extract information from the  second   argument  of an error(Formal,
%   ImplDefined) that is printed _after_ the core of the message.
%
%   @see swi_location//1 uses the same term   to insert context _before_
%   the core of the message.

swi_extra(X) -->
    { var(X) },
    !,
    [].
swi_extra(Context) -->
    { message_lang(Lang) },
    prolog:message_context(Lang, Context),
    !.
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
    { \+ current_prolog_flag(toplevel_thread, true),
      thread_self(Id)
    },
    !,
    ['[Thread ~w] '-[Id]].
thread_context -->
    [].

		 /*******************************
		 *        UNWIND MESSAGES	*
		 *******************************/

unwind_message(Var) -->
    { var(Var) }, !,
    [ 'Unknown unwind message: ~p'-[Var] ].
unwind_message(abort) -->
    [ 'Execution Aborted' ].
unwind_message(halt(_)) -->
    [].
unwind_message(thread_exit(Term)) -->
    [ 'Invalid thread_exit/1.  Payload: ~p'-[Term] ].
unwind_message(Term) -->
    [ 'Unknown "unwind" exception: ~p'-[Term] ].


                 /*******************************
                 *        NORMAL MESSAGES       *
                 *******************************/

:- dynamic prolog:version_msg/1.
:- multifile prolog:version_msg/1.

prolog_message(welcome) -->
    [ 'Welcome to SWI-Prolog (' ],
    prolog_message(threads),
    prolog_message(address_bits),
    ['version ' ],
    prolog_message(version),
    [ ')', nl ],
    prolog_message(copyright),
    [ nl ],
    translate_message(user_versions),
    [ nl ],
    prolog_message(documentaton),
    [ nl, nl ].
prolog_message(user_versions) -->
    (   { findall(Msg, prolog:version_msg(Msg), Msgs),
          Msgs \== []
        }
    ->  [nl],
        user_version_messages(Msgs)
    ;   []
    ).
prolog_message(deprecated(Term)) -->
    { nonvar(Term) },
    (   { message_lang(Lang) },
        prolog:deprecated(Lang, Term)
    ->  []
    ;   prolog:deprecated(Term)
    ->  []
    ;   deprecated(Term)
    ).
prolog_message(unhandled_exception(E)) -->
    { nonvar(E) },
    [ 'Unhandled exception: ' ],
    (   translate_message(E)
    ->  []
    ;   [ '~p'-[E] ]
    ).

%!  prolog_message(+Term)//

prolog_message(initialization_error(_, E, File:Line)) -->
    !,
    [ url(File:Line),
      ': Initialization goal raised exception:', nl
    ],
    translate_message(E).
prolog_message(initialization_error(Goal, E, _)) -->
    [ 'Initialization goal ~p raised exception:'-[Goal], nl ],
    translate_message(E).
prolog_message(initialization_failure(_Goal, File:Line)) -->
    !,
    [ url(File:Line),
      ': Initialization goal failed'-[]
    ].
prolog_message(initialization_failure(Goal, _)) -->
    [ 'Initialization goal failed: ~p'-[Goal]
    ].
prolog_message(initialization_exception(E)) -->
    [ 'Prolog initialisation failed:', nl ],
    translate_message(E).
prolog_message(initialization(halt(Status), Goal, File:Line)) -->
    [ url(File:Line), ': '], goal(Goal), [nl,
      '  Initialization goal called ', ansi(code, '~p', [halt(Status)]),
      '.', nl,
      '  The program entry point should be called using ',
      ansi(code, 'initialization/2', []), '.', nl,
      '  Consider using ', ansi(code, 'library(main)', []), '.'
    ].
prolog_message(init_goal_syntax(Error, Text)) -->
    !,
    [ '-g ~w: '-[Text] ],
    translate_message(Error).
prolog_message(init_goal_failed(failed, @(Goal,File:Line))) -->
    !,
    [ url(File:Line), ': ~p: false'-[Goal] ].
prolog_message(init_goal_failed(Error, @(Goal,File:Line))) -->
    !,
    [ url(File:Line), ': ~p '-[Goal] ],
    translate_message(Error).
prolog_message(init_goal_failed(failed, Text)) -->
    !,
    [ '-g ~w: false'-[Text] ].
prolog_message(init_goal_failed(Error, Text)) -->
    !,
    [ '-g ~w: '-[Text] ],
    translate_message(Error).
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
      Column is LinePos+1                       % line_position is 0-based
    },
    (   { stream_property(Stream, file_name(File)) }
    ->  [ url(File:LineNo:Column) ]
    ;   [ '~p:~d:~d'-[Stream, LineNo, Column] ]
    ),
    [ ': ~w'-[Message] ].
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
      'See https://www.swi-prolog.org/howto/database.html'
    ].
prolog_message(untable(PI)) -->
    [ 'Reconsult: removed tabling for ' ], predicate_reference(PI).
prolog_message(unknown_option(Set, Opt)) -->
    [ 'Unknown ~w option: ~p'-[Set, Opt] ].


                 /*******************************
                 *         LOADING FILES        *
                 *******************************/

prolog_message(modify_active_procedure(Who, What)) -->
    predicate_reference(Who),
    [ ': modified active procedure ' ],
    predicate_reference(What).
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
    [ 'Redefined ~w procedure '-[Type] ],
    predicate_reference(Proc),
    predicate_definition(Proc, 'Previously defined').
prolog_message(declare_module(Module, abolish(Predicates))) -->
    [ 'Loading module ~w abolished:'-[Module], nl ],
    predicate_list(Predicates).
prolog_message(import_private(Module, Private)) -->
    [ 'import/1: ' ], predicate_reference(Private),
    [ ' is not exported (still imported into ~q)'-[Module] ].
prolog_message(ignored_weak_import(Into, From:PI)) -->
    [ 'Local definition of ' ],
    predicate_reference(Into:PI, [module(show)]),
    [ ' overrides weak import from ~q'-[From] ].
prolog_message(undefined_export(Module, PI)) -->
    [ 'Exported procedure ' ],
    predicate_reference(Module:PI, [module(show)]),
    [ ' is not defined' ].
prolog_message(no_exported_op(Module, Op)) -->
    [ 'Operator ~q:~q is not exported (still defined)'-[Module, Op] ].
prolog_message(discontiguous((-)/2,_)) -->
    prolog_message(minus_in_identifier).
prolog_message(discontiguous(Proc,Current)) -->
    [ 'Clauses of ' ], predicate_reference(Proc),
    [ ' are not together in the source-file' ],
    predicate_definition(Proc, 'Earlier definition'),
    [ nl, 'Current predicate: ' ], predicate_reference(Current),
    [ nl, 'Use ', ansi(code, ':- discontiguous ~p.', [Proc]),
      ' to suppress this message'
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
    [ 'Unknown procedure: ' ],
    predicate_reference(Goal),
    [ nl, '    However, there are definitions for:', nl ],
    dwim_alternatives(Alternatives).
prolog_message(dwim_correct(Into)) -->
    [ ansi(warning, 'Correct to: ', []), ansi(code, '~q', [Into]),
      ansi(warning, '? ', []), flush
    ].
prolog_message(error(loop_error(Spec), file_search(Used))) -->
    [ 'File search: too many levels of indirections on: ~p'-[Spec], nl,
      '    Used alias expansions:', nl
    ],
    used_search(Used).
prolog_message(minus_in_identifier) -->
    [ 'The "-" character should not be used to separate words in an', nl,
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
prolog_message(qlf(system_lib_out_of_date(Spec,QlfFile))) -->
    [ '~p: can not recompile "~w" (access denied)'-[Spec, QlfFile], nl,
      '\tLoading QlfFile'-[]
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

%!  user_predicate_indicator(+QPI, -PI) is det.
%
%   Remove the module qualification from  QPI   if  it does not add
%   information for the user.  This is the single module hiding policy of
%   this file.  See also predicate_reference//2.

user_predicate_indicator(Module:PI, PI) :-
    hidden_module(Module),
    !.
user_predicate_indicator(PI, PI).

hidden_module(user) :- !.
hidden_module(system) :- !.
hidden_module(M) :-
    sub_atom(M, 0, _, _, $).

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

%!  out_of_c_stack
%
%   The thread's C-stack limit was exceeded. Give  some advice on how to
%   resolve this.

out_of_c_stack -->
    { statistics(c_stack, Limit), Limit > 0 },
    !,
    [ 'C-stack limit (~D bytes) exceeded.'-[Limit], nl ],
    resolve_c_stack_overflow(Limit).
out_of_c_stack -->
    { statistics(c_stack, Limit), Limit > 0 },
    [ 'C-stack limit exceeded.'-[Limit], nl ],
    resolve_c_stack_overflow(Limit).

resolve_c_stack_overflow(_Limit) -->
    { thread_self(main) },
    [ 'Use the shell command ' ], code('~w', 'ulimit -s size'),
    [ ' to enlarge the limit.' ].
resolve_c_stack_overflow(_Limit) -->
    [ 'Use the ' ], code('~w', 'c_stack(KBytes)'),
    [ ' option of '], code(thread_create/3), [' to enlarge the limit.' ].


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
    [ 'autoloading ' ], predicate_reference(Pred, [link(false)]),
    [ ' from ~w'-[File] ].
prolog_message(autoload(read_index(Dir))) -->
    [ 'Loading autoload index for ~w'-[Dir] ].
prolog_message(autoload(disabled(Loaded))) -->
    [ 'Disabled autoloading (loaded ~D files)'-[Loaded] ].
prolog_message(autoload(already_defined(PI, From))) -->
    predicate_reference(PI),
    (   { predicate_head(PI, Head),
          predicate_property(Head, built_in)
        }
    ->  [' is a built-in predicate']
    ;   [ ' is already imported from module ' ],
        code(From)
    ).

swi_message(autoload(Msg)) -->
    [ nl, '  ' ],
    autoload_message(Msg).

autoload_message(not_exported(PI, Spec, _FullFile, _Exports)) -->
    [ ansi(code, '~w', [Spec]),
      ' does not export '
    ],
    predicate_reference(PI, [link(false)]).
autoload_message(no_file(Spec)) -->
    [ ansi(code, '~p', [Spec]), ': No such file' ].


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
    (   { '$option'(tag(Tag), Options) }
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
      'Please run ', ansi(code, '?- license.', []), ' for legal details.'
    ].
prolog_message(documentaton) -->
    [ 'For online help and background, visit ', url('https://www.swi-prolog.org') ],
    (   { exists_source(library(help)) }
    ->  [ nl,
          'For built-in help, use ', ansi(code, '?- help(Topic).', []),
          ' or ', ansi(code, '?- apropos(Word).', [])
        ]
    ;   []
    ).
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
prolog_message(on_error(halt(Status))) -->
    { statistics(errors, Errors),
      statistics(warnings, Warnings)
    },
    [ 'Halting with status ~w due to ~D errors and ~D warnings'-
      [Status, Errors, Warnings] ].

prolog_message(query(QueryResult)) -->
    query_result(QueryResult).

query_result(no) -->            % failure
    [ ansi(truth(false), 'false.', []) ],
    extra_line.
query_result(yes(true, [])) -->      % prompt_alternatives_on: groundness
    !,
    [ ansi(truth(true), 'true.', []) ],
    extra_line.
query_result(yes(Delays, Residuals)) -->
    result([], Delays, Residuals),
    extra_line.
query_result(done) -->          % user typed <CR>
    extra_line.
query_result(yes(Bindings, Delays, Residuals)) -->
    result(Bindings, Delays, Residuals),
    prompt(yes, Bindings, Delays, Residuals).
query_result(more(Bindings, Delays, Residuals)) -->
    result(Bindings, Delays, Residuals),
    prompt(more, Bindings, Delays, Residuals).
:- if(current_prolog_flag(emscripten, true)).
query_result(help) -->
    [ ansi(bold, '  Possible actions:', []), nl,
      '  ; (n,r,space): redo              | t:       trace&redo'-[], nl,
      '  *:             show choicepoint  | . (c,a): stop'-[], nl,
      '  w:             write             | p:       print'-[], nl,
      '  +:             max_depth*5       | -:       max_depth//5'-[], nl,
      '  h (?):         help'-[],
      nl, nl
    ].
:- else.
query_result(help) -->
    [ ansi(bold, '  Possible actions:', []), nl,
      '  ; (n,r,space,TAB): redo              | t:           trace&redo'-[], nl,
      '  *:                 show choicepoint  | . (c,a,RET): stop'-[], nl,
      '  w:                 write             | p:           print'-[], nl,
      '  +:                 max_depth*5       | -:           max_depth//5'-[], nl,
      '  b:                 break             | h (?):       help'-[],
      nl, nl
    ].
:- endif.
query_result(action) -->
    [ 'Action? '-[], flush ].
query_result(confirm) -->
    [ 'Please answer \'y\' or \'n\'? '-[], flush ].
query_result(eof) -->
    [ nl ].
query_result(toplevel_open_line) -->
    [].

prompt(Answer, [], true, []-[]) -->
    !,
    prompt(Answer, empty).
prompt(Answer, _, _, _) -->
    !,
    prompt(Answer, non_empty).

prompt(yes, empty) -->
    !,
    [ ansi(truth(true), 'true.', []) ],
    extra_line.
prompt(yes, _) -->
    !,
    [ full_stop ],
    extra_line.
prompt(more, empty) -->
    !,
    [ ansi(truth(true), 'true ', []), flush ].
prompt(more, _) -->
    !,
    [ ' '-[], flush ].

result(Bindings, Delays, Residuals) -->
    { current_prolog_flag(answer_write_options, Options0),
      Options = [partial(true)|Options0],
      GOptions = [priority(999)|Options0]
    },
    wfs_residual_program(Delays, GOptions),
    bindings(Bindings, [priority(699)|Options]),
    (   {Residuals == []-[]}
    ->  bind_delays_sep(Bindings, Delays),
        delays(Delays, GOptions)
    ;   bind_res_sep(Bindings, Residuals),
        residuals(Residuals, GOptions),
        (   {Delays == true}
        ->  []
        ;   [','-[], nl],
            delays(Delays, GOptions)
        )
    ).

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
    [ ansi(binding(name), '~w', [Name]), ' = '-[] ].
var_names([Name1,Name2|T]) -->
    !,
    [ ansi(binding(name), '~w', [Name1]), ' = '-[],
      ansi(binding(name), '~w', [Name2]), ', '-[]
    ],
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
    [ ', ', ansi(comment, '% where', []), nl,
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
    ;   [ansi(comment, '% with pending residual goals', []), nl]
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

wfs_residual_program(true, _Options) -->
    !.
wfs_residual_program(Goal, _Options) -->
    { current_prolog_flag(toplevel_list_wfs_residual_program, true),
      '$current_typein_module'(TypeIn),
      (   current_predicate(delays_residual_program/2)
      ->  true
      ;   use_module(library(wfs), [delays_residual_program/2])
      ),
      delays_residual_program(TypeIn:Goal, TypeIn:Program),
      Program \== []
    },
    !,
    [ ansi(comment, '% WFS residual program', []), nl ],
    [ ansi(wfs(residual_program), '~@', ['$messages':list_clauses(Program)]) ].
wfs_residual_program(_, _) --> [].

delays(true, _Options) -->
    !.
delays(Goal, Options) -->
    { current_prolog_flag(toplevel_list_wfs_residual_program, true)
    },
    !,
    [ ansi(truth(undefined), '~W', [Goal, Options]) ].
delays(_, _Options) -->
    [ ansi(truth(undefined), undefined, []) ].

:- public list_clauses/1.

list_clauses([]).
list_clauses([H|T]) :-
    (   system_undefined(H)
    ->  true
    ;   portray_clause(user_output, H, [indent(4)])
    ),
    list_clauses(T).

system_undefined((undefined :- tnot(undefined))).
system_undefined((answer_count_restraint :- tnot(answer_count_restraint))).
system_undefined((radial_restraint :- tnot(radial_restraint))).

bind_res_sep(_, []) --> !.
bind_res_sep(_, []-[]) --> !.
bind_res_sep([], _) --> !.
bind_res_sep(_, _) --> [','-[], nl].

bind_delays_sep([], _) --> !.
bind_delays_sep(_, true) --> !.
bind_delays_sep(_, _) --> [','-[], nl].

%!  extra_line// is det.
%
%   End the answer and, if  `toplevel_extra_white_line`   is true, add an
%   empty line.  The ``~N`` cannot be replaced by `nl` because the answer
%   is not always left at a  non-empty   line.  The `eol` element paints
%   the remainder of the line if the message has a background colour,
%   which ``~N`` cannot do as it is written using format/3.
%
%   Note that `eol` ends the  _last  line   of  the  answer_.  The empty
%   line that separates the answer from the  next query is not part of
%   the answer and keeps the default background.

extra_line -->
    { current_prolog_flag(toplevel_extra_white_line, true) },
    !,
    [eol, '~N'-[]].
extra_line -->
    [eol].

prolog_message(if_tty(Message)) -->
    (   {current_prolog_flag(tty_control, true)}
    ->  [ at_same_line ], list(Message)
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
prolog_message(history(no_history)) -->
    [ '! event history not supported in this version' ].

history_events([]) -->
    [].
history_events([Nr-Event|T]) -->
    [ ansi(comment, '%', []),
      ansi(bold, '~t~w ~6|', [Nr]),
      ansi(code, '~s', [Event]),
      nl
    ],
    history_events(T).


%!  user_version_messages(+Terms)//
%
%   Helper for the `welcome`  message   to  print information registered
%   using version/1.

user_version_messages([]) --> [].
user_version_messages([H|T]) -->
    user_version_message(H),
    user_version_messages(T).

%!  user_version_message(+Term)

user_version_message(Term) -->
    translate_message(Term), !, [nl].
user_version_message(Atom) -->
    [ '~w'-[Atom], nl ].


                 /*******************************
                 *       DEBUGGER MESSAGES      *
                 *******************************/

prolog_message(spy(Head)) -->
    [ 'New spy point on ' ],
    predicate_reference(Head).
prolog_message(already_spying(Head)) -->
    [ 'Already spying ' ],
    predicate_reference(Head).
prolog_message(nospy(Head)) -->
    [ 'Removed spy point from ' ],
    predicate_reference(Head).
prolog_message(trace_mode(OnOff)) -->
    [ 'Trace mode switched to ~w'-[OnOff] ].
prolog_message(debug_mode(OnOff)) -->
    [ 'Debug mode switched to ~w'-[OnOff] ].
prolog_message(debugging(OnOff, Threads)) -->
    [ 'Debug mode is ~w'-[OnOff] ],
    debugging_threads(Threads).
prolog_message(spying([])) -->
    !,
    [ 'No spy points' ].
prolog_message(spying(Heads)) -->
    [ 'Spy points (see spy/1) on:', nl ],
    predicate_list(Heads).
prolog_message(trace(Head, [])) -->
    !,
    [ '    ' ], predicate_reference(Head, [tag(true)]),
    [ ' Not tracing'-[], nl].
prolog_message(trace(Head, Ports)) -->
    { '$member'(Port, Ports), compound(Port),
      !,
      numbervars(Head+Ports, 0, _, [singletons(true)])
    },
    [ '    ~p: ~p'-[Head,Ports] ].
prolog_message(trace(Head, Ports)) -->
    [ '    ' ], predicate_reference(Head, [tag(true)]),
    [ ': ~w'-[Ports], nl].
prolog_message(tracing([])) -->
    !,
    [ 'No traced predicates (see trace/1,2)' ].
prolog_message(tracing(Heads)) -->
    [ 'Trace points (see trace/1,2) on:', nl ],
    tracing_list(Heads).

%!  predicate_list(+Specs)// is det.
%
%   Emit a list of predicates, one per  line, each tagged with its kind.
%   See predicate_reference//2.

predicate_list([]) -->
    [].
predicate_list([H|T]) -->
    [ '    ' ], predicate_reference(H, [tag(true)]), [nl],
    predicate_list(T).

tracing_list([]) -->
    [].
tracing_list([trace(Head, Ports)|T]) -->
    translate_message(trace(Head, Ports)),
    tracing_list(T).

debugging_threads([]) -->
    [].
debugging_threads(ThreadsByClass) -->
    [ nl, 'Threads in the following classes run in debug mode:', nl],
    list_threads_by_class(ThreadsByClass).

list_threads_by_class([]) -->
    [].
list_threads_by_class([H|T]) -->
    list_thread_class(H),
    list_threads_by_class(T).

list_thread_class(Class-Threads) -->
    { length(Threads, Count) },
    [ '    Class ', ansi(code, '~p', [Class]), ': ~D threads'-[Count] ].

% frame(+Frame, +Choice, +Port, +PC) - Print for the debugger.
prolog_message(frame(Frame, _Choice, backtrace, _PC)) -->
    !,
    { prolog_frame_attribute(Frame, level, Level)
    },
    [ ansi(frame(level), '~t[~D] ~10|', [Level]) ],
    frame_context(Frame),
    frame_goal(Frame, backtrace).
prolog_message(frame(Frame, Choice, choice, PC)) -->
    !,
    prolog_message(frame(Frame, Choice, backtrace, PC)).
prolog_message(frame(_, _Choice, cut_call(_PC), _)) --> !.
prolog_message(frame(Frame, _Choice, Port, _PC)) -->
    frame_flags(Frame),
    port(Port),
    frame_level(Frame),
    frame_context(Frame),
    frame_depth_limit(Port, Frame),
    frame_goal(Frame, Port),
    [ flush ].

% frame(:Goal, +Trace)		- Print for trace/2
prolog_message(frame(Goal, trace(Port))) -->
    !,
    thread_context,
    [ ' T ' ],
    port(Port),
    predicate_goal(Goal, Port).
prolog_message(frame(Goal, trace(Port, Id))) -->
    !,
    thread_context,
    [ ' T ' ],
    port(Port, Id),
    predicate_goal(Goal, Port).

%!  goal_style(+Port, -Style) is det.
%
%   Style is the colour class used for the  goal of a frame that is being
%   reported for Port.  It is a  term   goal(Port,  Parity), which allows
%   themes to decorate the goal depending on   the port, on the parity of
%   the step count (_striping_, which  separates   the  steps  of a trace
%   visually) or both.  See also '$answer_class'/1, which does the same
%   for the answers of the interactive toplevel.

goal_style(Port0, goal(Port, Parity)) :-
    functor(Port0, Port, _),
    trace_parity(Parity).

trace_parity(Parity) :-
    (   nb_current('$trace_step', C0)
    ->  true
    ;   C0 = 0
    ),
    C is C0+1,
    nb_setval('$trace_step', C),
    (   C mod 2 =:= 0
    ->  Parity = even
    ;   Parity = odd
    ).

frame_goal(Frame, Port) -->
    { prolog_frame_attribute(Frame, goal, Goal),
      goal_style(Port, Style)
    },
    (   { frame_location(Frame, Location) }
    ->  goal(Goal, Style, Location)
    ;   goal(Goal, Style)
    ).

%!  predicate_goal(+Goal, +Port)// is det.
%
%   Emit Goal, linking it to the definition   of  its predicate.  Used if
%   we have no frame, i.e., for the messages of library(prolog_trace).

predicate_goal(Goal, Port) -->
    { goal_style(Port, Style)
    },
    (   { goal_links,
          predicate_location(Goal, Location)
        }
    ->  goal(Goal, Style, Location)
    ;   goal(Goal, Style)
    ).

goal(Goal0, Style) -->
    { goal_format(Goal0, Goal, Options)
    },
    [ ansi(Style, '~W', [Goal, Options]) ].

goal(Goal0, Style, Location) -->
    { goal_format(Goal0, Goal, Options)
    },
    [ url(Location, ansi(Style, '~W', [Goal, Options])) ].

goal_format(Goal0, Goal, Options) :-
    clean_goal(Goal0, Goal),
    current_prolog_flag(debugger_write_options, Options).

%!  frame_location(+Frame, -Location) is semidet.
%
%   Location is `File:Line` for the _call site_ of Frame, i.e., the place
%   in the clause of the parent frame from  which Frame was called.  This
%   is the position the user is at while  tracing.  If we cannot find it,
%   fall back to the clause that runs in   Frame  and finally to the file
%   that defines the predicate.
%
%   Resolving the call site uses library(prolog_stack),  which has to run
%   the decompiler and read the source file.   This  is affordable for an
%   interactive tracer, but we only do it if the location can actually be
%   used, i.e., if hyperlinks are rendered.  See goal_links/0.

frame_location(Frame, Location) :-
    goal_links,
    catch(frame_location_(Frame, Location), _, fail).

frame_location_(Frame, File:Line) :-
    prolog_frame_attribute(Frame, pc, PC),
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, clause, Clause),
    prolog_stack_frame_property(frame(_,clause(Clause,PC),_),
                                location(File:Line)),
    !.
frame_location_(Frame, File:Line) :-
    prolog_frame_attribute(Frame, clause, Clause),
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    !.
frame_location_(Frame, Location) :-
    prolog_frame_attribute(Frame, goal, Goal),
    predicate_location(Goal, Location).

%!  goal_links is semidet.
%
%   True when goals printed by the  debugger   should  be  linked to their
%   source location.  Controlled by the  flag `debugger_goal_links`, which
%   is one of `true`, `false` or `auto`.   Using `auto` (default) we create
%   the links if the console can render them.

goal_links :-
    current_prolog_flag(debugger_goal_links, Links),
    goal_links(Links).

goal_links(true).                       % note: no clause for `false`
goal_links(auto) :-
    (   current_prolog_flag(hyperlink_term, true)
    ->  true
    ;   predicate_property(ansi_term:hyperlink(_,_), number_of_clauses(N)),
        N > 0
    ).

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

% trace/1 context handling
port(Port, Dict) -->
    { _{level:Level, start:Time} :< Dict
    },
    (   { Port \== call,
          get_time(Now),
          Passed is (Now - Time)*1000.0
        }
    ->  [ '[~d +~1fms] '-[Level, Passed] ]
    ;   [ '[~d] '-[Level] ]
    ),
    port(Port).
port(Port, _Id-Level) -->
    [ '[~d] '-[Level] ],
    port(Port).

port(PortTerm) -->
    { functor(PortTerm, Port, _),
      port_name(Port, Name)
    },
    !,
    [ ansi(port(Port), '~w: ', [Name]) ].

port_name(call,      'Call').
port_name(exit,      'Exit').
port_name(fail,      'Fail').
port_name(redo,      'Redo').
port_name(unify,     'Unify').
port_name(exception, 'Exception').

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
    [ 'The predicate ' ], predicate_reference(Old, [link(false)]),
    [ ' has been renamed to ' ], predicate_reference(New),
    [ '.', nl,
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
      '\tIgnoring version from ~q'- [Dir]
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
prolog_message(backcomp(init_file_moved(FoundFile))) -->
    { absolute_file_name(app_config('init.pl'), InitFile,
                         [ file_errors(fail)
                         ])
    },
    [ 'The location of the config file has moved'-[], nl,
      '  from "~w"'-[FoundFile], nl,
      '  to   "~w"'-[InitFile], nl,
      '  See https://www.swi-prolog.org/modified/config-files.html'-[]
    ].
prolog_message(not_accessed_flags(List)) -->
    [ 'The following Prolog flags have been set but not used:', nl ],
    flags(List).
prolog_message(prolog_flag_invalid_preset(Flag, Preset, _Type, New)) -->
    [ 'Prolog flag ', ansi(code, '~q', Flag), ' has been (re-)created with a type that is \c
       incompatible with its value.', nl,
      'Value updated from ', ansi(code, '~p', [Preset]), ' to default (',
      ansi(code, '~p', [New]), ')'
    ].


flags([H|T]) -->
    ['  ', ansi(code, '~q', [H])],
    (   {T == []}
    ->  []
    ;   [nl],
        flags(T)
    ).


		 /*******************************
		 *          DEPRECATED		*
		 *******************************/

deprecated(set_prolog_stack(_Stack,limit)) -->
    [ 'set_prolog_stack/2: limit(Size) sets the combined limit.'-[], nl,
      'See https://www.swi-prolog.org/changes/stack-limit.html'
    ].
deprecated(autoload(TargetModule, File, _M:PI, expansion)) -->
    !,
    [ 'Auto-loading ' ], predicate_reference(PI, [link(false)]),
    [ ' from ' ],
    load_file(File), [ ' into ' ],
    target_module(TargetModule),
    [ ' is deprecated due to term- or goal-expansion' ].
deprecated(source_search_working_directory(File, _FullFile)) -->
    [ 'Found file ', ansi(code, '~w', [File]),
      ' relative to the current working directory.', nl,
      'This behaviour is deprecated but still supported by', nl,
      'the Prolog flag ',
      ansi(code, source_search_working_directory, []), '.', nl
    ].
deprecated(moved_library(Old, New)) -->
    [ 'Library was moved: ~q --> ~q'-[Old, New] ].

load_file(File) -->
    { file_base_name(File, Base),
      absolute_file_name(library(Base), File, [access(read), file_errors(fail)]),
      file_name_extension(Clean, pl, Base)
    },
    !,
    [ ansi(code, '~p', [library(Clean)]) ].
load_file(File) -->
    [ url(File) ].

target_module(Module) -->
    { module_property(Module, file(File)) },
    !,
    load_file(File).
target_module(Module) -->
    [ 'module ', ansi(code, '~p', [Module]) ].



		 /*******************************
		 *           TRIPWIRES		*
		 *******************************/

tripwire_message(max_integer_size, Bytes) -->
    !,
    [ 'Trapped tripwire max_integer_size: big integers and \c
       rationals are limited to ~D bytes'-[Bytes] ].
tripwire_message(Wire, Context) -->
    [ 'Trapped tripwire ~w for '-[Wire] ],
    tripwire_context(Wire, Context).

tripwire_context(_, ATrie) -->
    { '$is_answer_trie'(ATrie, _),
      !,
      '$tabling':atrie_goal(ATrie, QGoal),
      clean_goal(QGoal, Goal)          % a goal, not a predicate indicator
    },
    [ '~p'-[Goal] ].
tripwire_context(_, Ctx) -->
    [ '~p'-[Ctx] ].


		 /*******************************
		 *     INTERNATIONALIZATION	*
		 *******************************/

:- create_prolog_flag(message_language, default, []).

%!  message_lang(-Lang) is multi.
%
%   True when Lang is a language id  preferred for messages. Starts with
%   the most specific language (e.g., `nl_BE`) and ends with `en`.

message_lang(Lang) :-
    current_message_lang(Lang0),
    (   Lang0 == en
    ->  Lang = en
    ;   sub_atom(Lang0, 0, _, _, en_)
    ->  longest_id(Lang0, Lang)
    ;   (   longest_id(Lang0, Lang)
        ;   Lang = en
        )
    ).

longest_id(Lang, Id) :-
    split_string(Lang, "_-", "", [H|Components]),
    longest_prefix(Components, Taken),
    atomic_list_concat([H|Taken], '_', Id).

longest_prefix([H|T0], [H|T]) :-
    longest_prefix(T0, T).
longest_prefix(_, []).

%!  current_message_lang(-Lang) is det.
%
%   Get the current language for messages.

current_message_lang(Lang) :-
    (   current_prolog_flag(message_language, Lang0),
        Lang0 \== default
    ->  Lang = Lang0
    ;   os_user_lang(Lang0)
    ->  clean_encoding(Lang0, Lang1),
        set_prolog_flag(message_language, Lang1),
        Lang = Lang1
    ;   Lang = en
    ).

os_user_lang(Lang) :-
    current_prolog_flag(windows, true),
    win_get_user_preferred_ui_languages(name, [Lang|_]).
os_user_lang(Lang) :-
    catch(setlocale(messages, _, ''), _, fail),
    setlocale(messages, Lang, Lang).
os_user_lang(Lang) :-
    getenv('LANG', Lang).


clean_encoding(Lang0, Lang) :-
    (   sub_atom(Lang0, A, _, _, '.')
    ->  sub_atom(Lang0, 0, A, _, Lang)
    ;   Lang = Lang0
    ).

		 /*******************************
		 *          PRIMITIVES		*
		 *******************************/

code(Term) -->
    code('~p', Term).

code(Format, Term) -->
    [ ansi(code, Format, [Term]) ].

list([]) --> [].
list([H|T]) --> [H], list(T).


		 /*******************************
		 *     PREDICATE REFERENCES	*
		 *******************************/

%!  predicate_indicator(+Spec, -QPI) is semidet.
%
%   QPI is the fully qualified predicate  indicator ``Module:Name/Arity``
%   or, for a non-terminal, ``Module:Name//Arity``  for Spec. Spec is one
%   of
%
%     - A callable term (a _head_), optionally module qualified
%     - A predicate indicator, optionally module qualified
%
%   The module is _kept_ here.  Whether  or   not  it  is printed is left
%   to predicate_reference//2, which uses user_predicate_indicator/2.
%
%   @see pi_head/2 of library(prolog_code) for the general version.  This
%   one is in the boot files and thus cannot use it.

:- public
    predicate_indicator/2.

predicate_indicator(Spec, QPI) :-
    strip_module(user:Spec, Module, Spec1),
    (   is_predicate_indicator(Spec1)
    ->  dcg_indicator(Module, Spec1, QPI)
    ;   callable(Spec1),
        '$pi_head'(Module:PI, Module:Spec1),
        dcg_indicator(Module, PI, QPI)
    ).

%!  dcg_indicator(+Module, +PI, -QPI) is det.
%
%   Qualify PI with Module and use  the ``//`` notation if the predicate
%   is a non-terminal.

dcg_indicator(Module, Name//DCGArity, Module:Name//DCGArity) :-
    !.
dcg_indicator(Module, Name/Arity, QPI) :-
    (   Arity >= 2,
        current_predicate(Module:Name/Arity),
        functor(Head, Name, Arity),
        predicate_property(Module:Head, non_terminal)
    ->  DCGArity is Arity-2,
        QPI = Module:Name//DCGArity
    ;   QPI = Module:Name/Arity
    ).

%!  predicate_head(+Spec, -QHead) is semidet.
%
%   QHead is the module qualified _head_   for Spec, accepting the same
%   input as predicate_indicator/2.  Unqualified specs are qualified
%   using `user`.

predicate_head(Spec, QHead) :-
    strip_module(user:Spec, Module, Spec1),
    (   is_predicate_indicator(Spec1)
    ->  '$pi_head'(Module:Spec1, QHead)
    ;   callable(Spec1),
        QHead = Module:Spec1
    ).

is_predicate_indicator(Name/Arity) :-
    atomic(Name), integer(Arity).
is_predicate_indicator(Name//Arity) :-
    atomic(Name), integer(Arity).

%!  predicate_reference(+Spec)// is det.
%!  predicate_reference(+Spec, +Options)// is det.
%
%   Emit a reference to a predicate.  Spec  is a (possibly qualified)
%   head or predicate indicator.  The   reference  is  printed using the
%   style class `code` and, if the location of the predicate is known and
%   the output is a terminal that supports  it, it is a hyperlink to the
%   definition.  Options:
%
%     - module(+Which)
%       One of `auto` (default), `hide` or `show`.  Using `auto`, the
%       module qualification is removed if hidden_module/1 holds for it.
%     - link(+Bool)
%       If `false`, do not try to create a hyperlink.  Default `true`.
%     - style(+Class)
%       Style class for the reference.  Default `code`.
%     - tag(+Bool)
%       If `true`, add a tag that indicates the _kind_ of predicate.
%       See predicate_kind/2.  Default `false`.  Only sensible if the
%       reference is the only thing on the line.
%
%   If Spec cannot be interpreted as a predicate  it is printed using the
%   `code` class and ``~p``, i.e., we never fail on a malformed message.

:- public
    predicate_reference//1,
    predicate_reference//2.

predicate_reference(Spec) -->
    predicate_reference(Spec, []).

predicate_reference(Spec, Options) -->
    { predicate_indicator(Spec, QPI) },
    !,
    { pref_option(style(Style), Options, code),
      pref_option(module(Mode), Options, auto),
      reference_pi(Mode, QPI, PI)
    },
    predicate_link(QPI, ansi(Style, '~q', [PI]), Options),
    predicate_reference_tag(QPI, Options).
predicate_reference(Spec, _Options) -->
    [ ansi(code, '~p', [Spec]) ].

reference_pi(auto, QPI, PI) :-
    !,
    user_predicate_indicator(QPI, PI).
reference_pi(hide, _:PI, PI) :- !.
reference_pi(_, QPI, QPI).

predicate_link(QPI, Label, Options) -->
    { pref_option(link(true), Options, true),
      predicate_location(QPI, Location)
    },
    !,
    [ url(Location, Label) ].
predicate_link(_, Label, _) -->
    [ Label ].

%!  pref_option(?Option, +Options, +Default) is semidet.
%
%   Get an option from the option list  of predicate_reference//2.  Fails
%   if Options holds a value for Option that does not unify.  Note that
%   this deliberately does not use library(option): boot/messages.pl must
%   be able to print a message before the libraries are available.

pref_option(Option, Options, Default) :-
    functor(Option, Name, 1),
    functor(General, Name, 1),
    (   memberchk(General, Options)
    ->  General = Option
    ;   arg(1, Option, Default)
    ).

%!  predicate_location(+Spec, -Location) is semidet.
%
%   Location is `File:Line` for the definition of the predicate Spec.
%   Also deals with predicates defined in C.  Note that predicates that
%   are not loaded but can be autoloaded  are located from the autoload
%   index, i.e., printing a message never loads a library.

:- public
    predicate_location/2.

predicate_location(Spec, Location) :-
    predicate_head(Spec, Head),
    current_predicate(_, Head),                 % do not (auto)load
    '$predicate_source_location'(Head, Location).

%!  predicate_definition(+Spec, +Message)// is det.
%
%   Emit "Message at File:Line" on a new  line if the location of Spec is
%   known and nothing at all if it is not.

:- public
    predicate_definition//2.

predicate_definition(Spec, Message) -->
    { predicate_location(Spec, Location) },
    !,
    [ nl, '~w at '-[Message], url(Location) ].
predicate_definition(_, _) -->
    [].

%!  predicate_kind(+Spec, -Kind) is semidet.
%
%   Classify a predicate for the benefit of  the user who has to pick one
%   from a list of candidates.  Fails if  Spec is not a predicate.  Kind
%   is one of
%
%     - iso
%     - built_in
%     - foreign
%     - library(Name)
%     - module(Module)
%     - user
%     - undefined

:- public
    predicate_kind/2.

predicate_kind(Spec, Kind) :-
    predicate_head(Spec, Head),
    (   current_predicate(_, Head)              % do not autoload
    ->  defined_predicate_kind(Head, Kind)
    ;   predicate_property(Head, autoload(File))
    ->  library_name(File, Name),
        Kind = library(Name)
    ;   Kind = undefined
    ).

defined_predicate_kind(Head, Kind) :-
    (   predicate_property(Head, iso)
    ->  Kind = iso
    ;   predicate_property(Head, built_in)
    ->  (   predicate_property(Head, foreign)
        ->  Kind = foreign
        ;   Kind = built_in
        )
    ;   predicate_property(Head, imported_from(Module))
    ->  module_kind(Module, Kind)
    ;   predicate_property(Head, file(File)),
        library_file(File)
    ->  library_name(File, Name),
        Kind = library(Name)
    ;   Kind = user
    ).

module_kind(Module, Kind) :-
    (   hidden_module(Module)
    ->  Kind = user
    ;   module_property(Module, file(File)),
        library_file(File)
    ->  library_name(File, Name),
        Kind = library(Name)
    ;   Kind = module(Module)
    ).

%!  library_file(+File) is semidet.
%!  library_name(+File, -Name) is det.
%
%   True if File is in one of the  library directories and, if so, Name
%   is how the file is referred to as ``library(Name)``.

library_file(File) :-
    absolute_file_name(library(.), LibDir,
                       [ file_type(directory),
                         solutions(all),
                         file_errors(fail)
                       ]),
    sub_atom(File, 0, _, _, LibDir),
    !.

library_name(File, Name) :-
    (   file_name_extension(Base, Ext, File),
        Ext \== ''
    ->  true
    ;   Base = File
    ),
    file_base_name(Base, Name).

%!  predicate_reference_tag(+QPI, +Options)// is det.
%!  predicate_kind_tag(+Kind)// is det.
%
%   Emit the kind of the predicate as a short tag.

predicate_reference_tag(QPI, Options) -->
    { pref_option(tag(true), Options, false),
      predicate_kind(QPI, Kind)
    },
    !,
    predicate_kind_tag(Kind).
predicate_reference_tag(_, _) -->
    [].

predicate_kind_tag(Kind) -->
    { predicate_kind_label(Kind, Label) },
    [ ansi(predicate(Kind), ' [~w]', [Label]) ].

predicate_kind_label(iso,          'ISO').
predicate_kind_label(built_in,     'built-in').
predicate_kind_label(foreign,      'built-in').
predicate_kind_label(user,         'user').
predicate_kind_label(undefined,    'undefined').
predicate_kind_label(library(Name), Label) :-
    format(atom(Label), 'library(~w)', [Name]).
predicate_kind_label(module(Name), Name).


		 /*******************************
		 *        DEFAULT THEME		*
		 *******************************/

:- public default_theme/2.

default_theme(var,                    [fg(red)]).
default_theme(code,                   [fg(blue)]).
default_theme(comment,                [fg(green)]).
default_theme(warning,                [fg(red)]).
default_theme(error,                  [bold, fg(red)]).
default_theme(truth(false),           [bold, fg(red)]).
default_theme(truth(true),            [bold]).
default_theme(truth(undefined),       [bold, fg(cyan)]).
default_theme(wfs(residual_program),  [fg(cyan)]).
default_theme(frame(level),           [bold]).
default_theme(goal(_,_),              []).
default_theme(port(call),             [bold, fg(green)]).
default_theme(port(exit),             [bold, fg(green)]).
default_theme(port(fail),             [bold, fg(red)]).
default_theme(port(redo),             [bold, fg(yellow)]).
default_theme(port(unify),            [bold, fg(blue)]).
default_theme(port(exception),        [bold, fg(magenta)]).
default_theme(prompt,                 [bold]).
default_theme(input,                  []).
default_theme(answer(_),              []).
default_theme(binding(name),          [bold]).
default_theme(predicate(iso),         [italic, fg(cyan)]).
default_theme(predicate(built_in),    [italic, fg(cyan)]).
default_theme(predicate(foreign),     [italic, fg(cyan)]).
default_theme(predicate(library(_)),  [italic, fg(green)]).
default_theme(predicate(module(_)),   [italic, fg(green)]).
default_theme(predicate(user),        [italic, fg(default)]).
default_theme(predicate(undefined),   [italic, fg(red)]).
default_theme(message(informational), [fg(green)]).
default_theme(message(information),   [fg(green)]).
default_theme(message(debug(_)),      [fg(blue)]).
default_theme(message(Level),         Attrs) :-
    nonvar(Level),
    default_theme(Level, Attrs).


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
:- '$notransact'((user:message_hook/3,
                  prolog:message_prefix_hook/2,
                  user:thread_message_hook/3)).

%!  print_message(+Kind, +Term)
%
%   Print an error message using a term as generated by the exception
%   system.

print_message(Level, _Term) :-
    msg_property(Level, stream(S)),
    stream_property(S, error(true)),
    !.
print_message(Level, Term) :-
    setup_call_cleanup(
        notrace(push_msg(Term, Stack)),
        ignore(print_message_guarded(Level, Term)),
        notrace(pop_msg(Stack))),
    !.
print_message(Level, Term) :-
    (   Level \== silent
    ->  format(user_error, 'Recursive ~w message: ~q~n', [Level, Term]),
        autoload_call(backtrace(20))
    ;   true
    ).

push_msg(Term, Messages) :-
    nb_current('$inprint_message', Messages),
    !,
    \+ ( '$member'(Msg, Messages),
         Msg =@= Term
       ),
    Stack = [Term|Messages],
    b_setval('$inprint_message', Stack).
push_msg(Term, []) :-
    b_setval('$inprint_message', [Term]).

pop_msg(Stack) :-
    nb_delete('$inprint_message'),              % delete history
    b_setval('$inprint_message', Stack).

print_message_guarded(Level, Term) :-
    (   must_print(Level, Term)
    ->  (   prolog:message_action(Term, Level),
            fail                                % forall/2 is cleaner, but not yet
        ;   true                                % defined
        ),
        (   translate_message(Term, Lines, [])
        ->  (   nonvar(Term),
                (   notrace(user:thread_message_hook(Term, Level, Lines))
                ->  true
                ;   notrace(user:message_hook(Term, Level, Lines))
                )
            ->  true
            ;   '$inc_message_count'(Level),
                print_system_message(Term, Level, Lines),
                maybe_halt_on_error(Level)
            )
        )
    ;   true
    ).

maybe_halt_on_error(error) :-
    current_prolog_flag(on_error, halt),
    !,
    halt(1).
maybe_halt_on_error(warning) :-
    current_prolog_flag(on_warning, halt),
    !,
    halt(1).
maybe_halt_on_error(_).


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
    to_list(LocPrefix, LocPrefixL),
    insert_prefix(Lines, LinePrefix, Ctx, PrefixLines),
    '$append'([ [begin(Kind, Ctx)],
                LocPrefixL,
                [nl],
                PrefixLines,
                [end(Ctx)]
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

to_list(ListIn, List) :-
    is_list(ListIn),
    !,
    List = ListIn.
to_list(NonList, [NonList]).

:- multifile
    user:message_property/2.

msg_property(Kind, Property) :-
    notrace(user:message_property(Kind, Property)),
    !.
msg_property(Kind, prefix(Prefix)) :-
    msg_prefix(Kind, Prefix),
    !.
msg_property(_, prefix('~N')) :- !.
msg_property(query, color_class(Class)) :-
    !,
    '$answer_class'(Class).
msg_property(query, stream(user_output)) :- !.
msg_property(_, stream(user_error)) :- !.
msg_property(error, tag('ERROR')).
msg_property(warning, tag('Warning')).
msg_property(Level,
             location_prefix(File:Line,
                             ['~N~w: '-[Tag], url(File:Line), ':'],
                             '~N~w:    '-[Tag])) :-
    include_msg_location(Level),
    msg_property(Level, tag(Tag)).
msg_property(error,   wait(0.1)) :- !.

include_msg_location(warning).
include_msg_location(error).

msg_prefix(debug(_), Prefix) :-
    msg_context('~N% ', Prefix).
msg_prefix(Level, Prefix) :-
    msg_property(Level, tag(Tag)),
    atomics_to_string(['~N', Tag, ': '], Prefix0),
    msg_context(Prefix0, Prefix).
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
    \+ current_prolog_flag(toplevel_thread, true),
    thread_self(Id0),
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
%
%   If PrefixOrKind is kind(Kind), the  message   as  a  whole may be
%   decorated.  To this end the lines are wrapped in begin(Class, Ctx)
%   and end(Ctx), where Class is  derived   from  Kind using
%   msg_color_class/2.  `Ctx` is a variable   that  is bound by whoever
%   implements prolog:message_line_element/2 for begin/2 (normally
%   library(ansi_term)) and remains unbound  if   the decoration is not
%   available, e.g., because Stream is not a terminal.
%
%   The elements that need `Ctx` are rewritten by prefix_nl/4 to carry
%   it.  See there for the details.

print_message_lines(Stream, kind(Kind), Lines) :-
    !,
    msg_property(Kind, prefix(Prefix)),
    msg_color_class(Kind, Class),
    insert_prefix(Lines, Prefix, Ctx, PrefixLines),
    '$append'([ begin(Class, Ctx)
              | PrefixLines
              ],
              [ end(Ctx)
              ],
              AllLines),
    print_message_lines(Stream, AllLines).
print_message_lines(Stream, Prefix, Lines) :-
    insert_prefix(Lines, Prefix, _, PrefixLines),
    print_message_lines(Stream, PrefixLines).

%!  msg_color_class(+Kind, -Class) is det.
%
%   Colour class used to decorate an entire message of the given Kind.
%   Defaults to Kind itself, which is  mapped   to  `message(Kind)` (see
%   ansi_term:level_attrs/2).

msg_color_class(Kind, Class) :-
    msg_property(Kind, color_class(Class0)),
    !,
    Class = Class0.
msg_color_class(Kind, Kind).

%!  insert_prefix(+Lines, +Prefix, ?Ctx, -PrefixedLines) is det.
%
%   Add Prefix to the start of each line of Lines.  If the first element
%   is `at_same_line` the message continues  the   line  and  no initial
%   prefix is added.  Ctx is the  message   context;  see  prefix_nl/4 and
%   print_message_lines/3.

insert_prefix([at_same_line|Lines0], Prefix, Ctx, Lines) :-
    !,
    prefix_nl(Lines0, Prefix, Ctx, Lines).
insert_prefix(Lines0, Prefix, Ctx, [prefix(Prefix)|Lines]) :-
    prefix_nl(Lines0, Prefix, Ctx, Lines).

%!  prefix_nl(+Lines, +Prefix, ?Ctx, -Lines) is det.
%
%   Insert Prefix after each `nl` and  make   the  message context Ctx
%   available to the elements that need it:
%
%     - nl, flush and eol become nl(Ctx), flush(Ctx) and eol(Ctx).
%       Their handler writes the sequence that paints the remainder of
%       the line before ending it if the message has a background
%       colour.
%     - ansi(Attrs, Fmt, Args) becomes ansi(Attrs, Fmt, Args, Ctx).
%       Its handler re-installs the decoration of the message as a
%       whole after writing the element, as the element ends with a
%       full reset.  The same applies to an ansi/3 element used as the
%       _label_ of an url/2 element.
%
%   The last line of a message is  ended   implicitly:  if Lines does not
%   end in `nl` or `flush` an `nl` is added.  This one does not paint:
%   what follows the message is not part of it.  A message that wants
%   its last line painted ends it using `eol`.

prefix_nl([], _, _, [nl]).
prefix_nl([nl], _, Ctx, [nl(Ctx)]) :- !.
prefix_nl([flush], _, Ctx, [flush(Ctx)]) :- !.
prefix_nl([nl|T0], Prefix, Ctx, [nl(Ctx), prefix(Prefix)|T]) :-
    !,
    prefix_nl(T0, Prefix, Ctx, T).
prefix_nl([flush|T0], Prefix, Ctx, [flush(Ctx)|T]) :-
    !,
    prefix_nl(T0, Prefix, Ctx, T).
prefix_nl([eol|T0], Prefix, Ctx, [eol(Ctx)|T]) :-
    !,
    prefix_nl(T0, Prefix, Ctx, T).
prefix_nl([ansi(Attrs,Fmt,Args)|T0], Prefix, Ctx,
          [ansi(Attrs,Fmt,Args,Ctx)|T]) :-
    !,
    prefix_nl(T0, Prefix, Ctx, T).
prefix_nl([url(URL,ansi(Attrs,Fmt,Args))|T0], Prefix, Ctx,
          [url(URL,ansi(Attrs,Fmt,Args,Ctx))|T]) :-
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
line_element(S, nl(_Ctx)) :-
    !,
    nl(S).
line_element(S, flush(_Ctx)) :-
    !,
    flush_output(S).
line_element(_, eol(_Ctx)) :- !.
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
line_element(S, url(URL)) :-
    !,
    print_link(S, URL).
line_element(S, url(_URL, Label)) :-
    !,
    link_label(Label, Fmt, Args),
    safe_format(S, Fmt, Args).
line_element(_, begin(_Level, _Ctx)) :- !.
line_element(_, end(_Ctx)) :- !.
line_element(S, Fmt) :-
    safe_format(S, Fmt, []).

print_link(S, File:Line:Column) :-
    !,
    safe_format(S, '~w:~d:~d', [File, Line, Column]).
print_link(S, File:Line) :-
    !,
    safe_format(S, '~w:~d', [File, Line]).
print_link(S, File) :-
    safe_format(S, '~w', [File]).

%!  link_label(+Label, -Format, -Args) is det.
%
%   Decompose the _label_ of an url/2  message   element.  See  url/2 in
%   print_message_lines/3.  Note that a plain  label is _text_ rather than
%   a format: it typically holds a file name, which may contain ``~``.

:- public
    link_label/3.

link_label(Fmt-Args, Fmt, Args) :-
    atom(Fmt),
    is_list(Args),
    !.
link_label(ansi(_Class, Fmt, Args), Fmt, Args) :- !.
link_label(ansi(_Class, Fmt, Args, _Ctx), Fmt, Args) :- !.
link_label(Text, '~w', [Text]).

%!  safe_format(+Stream, +Format, +Args) is det.

safe_format(S, Fmt, Args) :-
    E = error(_,_),
    catch(format(S,Fmt,Args), E,
          format_failed(S,Fmt,Args,E)).

format_failed(S, _Fmt, _Args, E) :-
    stream_property(S, error(true)),
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
actions_to_format([nl(_)|T], Fmt, Args) :-      % see prefix_nl/4
    !,
    actions_to_format([nl|T], Fmt, Args).
actions_to_format([nl], '', []) :- !.
actions_to_format([Term, nl], Fmt, Args) :-
    !,
    actions_to_format([Term], Fmt, Args).
actions_to_format([nl|T], Fmt, Args) :-
    !,
    actions_to_format(T, Fmt0, Args),
    atom_concat('~n', Fmt0, Fmt).
actions_to_format([ansi(_Attrs, Fmt0, Args0)|Tail], Fmt, Args) :-
    !,
    actions_to_format(Tail, Fmt1, Args1),
    atom_concat(Fmt0, Fmt1, Fmt),
    append_args(Args0, Args1, Args).
actions_to_format([url(Pos)|Tail], Fmt, Args) :-
    !,
    actions_to_format(Tail, Fmt1, Args1),
    url_actions_to_format(url(Pos), Fmt1, Args1, Fmt, Args).
actions_to_format([url(URL, Label)|Tail], Fmt, Args) :-
    !,
    actions_to_format(Tail, Fmt1, Args1),
    url_actions_to_format(url(URL, Label), Fmt1, Args1, Fmt, Args).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :-
    !,
    actions_to_format(Tail, Fmt1, Args1),
    atom_concat(Fmt0, Fmt1, Fmt),
    append_args(Args0, Args1, Args).
actions_to_format([Skip|T], Fmt, Args) :-
    action_skip(Skip),
    !,
    actions_to_format(T, Fmt, Args).
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
action_skip(flush(_Ctx)).
action_skip(eol).
action_skip(eol(_Ctx)).
action_skip(begin(_Level, _Ctx)).
action_skip(end(_Ctx)).

url_actions_to_format(url(File:Line:Column), Fmt1, Args1, Fmt, Args) :-
    !,
    atom_concat('~w:~d:~d', Fmt1, Fmt),
    append_args([File,Line,Column], Args1, Args).
url_actions_to_format(url(File:Line), Fmt1, Args1, Fmt, Args) :-
    !,
    atom_concat('~w:~d', Fmt1, Fmt),
    append_args([File,Line], Args1, Args).
url_actions_to_format(url(File), Fmt1, Args1, Fmt, Args) :-
    !,
    atom_concat('~w', Fmt1, Fmt),
    append_args([File], Args1, Args).
url_actions_to_format(url(_URL, Label), Fmt1, Args1, Fmt, Args) :-
    !,
    link_label(Label, Fmt0, Args0),
    atom_concat(Fmt0, Fmt1, Fmt),
    append_args(Args0, Args1, Args).


append_args(M:Args0, Args1, M:Args) :-
    !,
    strip_module(Args1, _, A1),
    to_list(Args0, Args01),
    '$append'(Args01, A1, Args).
append_args(Args0, Args1, Args) :-
    strip_module(Args1, _, A1),
    to_list(Args0, Args01),
    '$append'(Args01, A1, Args).

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

