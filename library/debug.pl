/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2023, University of Amsterdam
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

:- module(prolog_debug,
          [ debug/3,                    % +Topic, +Format, :Args
            debug/1,                    % +Topic
            nodebug/1,                  % +Topic
            debugging/1,                % ?Topic
            debugging/2,                % ?Topic, ?Bool
            list_debug_topics/0,
            list_debug_topics/1,        % +Options
            debug_message_context/1,    % (+|-)What

            assertion/1                 % :Goal
          ]).
:- autoload(library(lists),[append/3,delete/3,selectchk/3,member/2]).
:- autoload(library(prolog_stack),[backtrace/1]).
:- autoload(library(option), [option/3, option/2]).

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    assertion(0),
    debug(+,+,:).

:- multifile prolog:assertion_failed/2.
:- dynamic   prolog:assertion_failed/2.

/*:- use_module(library(prolog_stack)).*/ % We use the autoloader if needed

%:- set_prolog_flag(generate_debug_info, false).

:- dynamic
    debugging/3.                    % Topic, Enabled, To

/** <module> Print debug messages and test assertions

This library is a replacement for  format/3 for printing debug messages.
Messages are assigned a _topic_. By   dynamically  enabling or disabling
topics the user can  select  desired   messages.  Calls  to  debug/3 and
assertion/1 are removed when  the  code   is  compiled  for optimization
unless the Prolog flag `optimise_debug` is set to `true`.

Using the predicate assertion/1 you  can   make  assumptions  about your
program explicit, trapping the debugger if the condition does not hold.

Output and actions by these predicates   can be configured using _hooks_
to fit your environment. With XPCE, you can  use the call below to start
a graphical monitoring tool.

    ?- prolog_ide(debug_monitor).
*/

%!  debugging(+Topic) is semidet.
%!  debugging(-Topic) is nondet.
%!  debugging(?Topic, ?Bool) is nondet.
%
%   Examine debug topics. The form debugging(+Topic)  may be used to
%   perform more complex debugging tasks.   A typical usage skeleton
%   is:
%
%     ```
%           (   debugging(mytopic)
%           ->  <perform debugging actions>
%           ;   true
%           ),
%           ...
%     ```
%
%   The other two calls are intended to examine existing and enabled
%   debugging tokens and are typically not used in user programs.

debugging(Topic) :-
    debugging(Topic, true, _To).

debugging(Topic, Bool) :-
    debugging(Topic, Bool, _To).

%!  debug(+Topic) is det.
%!  nodebug(+Topic) is det.
%
%   Add/remove a topic  from  being   printed.  nodebug(_)  removes  all
%   topics. Gives a warning if the  topic   is  not defined unless it is
%   used from a directive. The latter allows placing debug topics at the
%   start of a (load-)file without warnings.
%
%   For debug/1, Topic can be  a  term   `Topic  >  Out`, where `Out` is
%   either a stream or  stream-alias  or   a  filename  (an  atom). This
%   redirects debug information on this topic   to  the given output. On
%   Linux systems redirection can be used   to  make the message appear,
%   even if the `user_error` stream is redefined using
%
%       ?- debug(Topic > '/proc/self/fd/2').
%
%   A platform independent way to  get   debug  messages  in the current
%   console (for example, a `swipl-win` window,  or login using `ssh` to
%   Prolog running an SSH server from the `libssh` pack) is to use:
%
%       ?- stream_property(S, alias(user_error)),
%          debug(Topic > S).
%
%   Do not forget to  disable  the   debugging  using  nodebug/1  before
%   quitting the console if Prolog must remain running.

debug(Topic) :-
    with_mutex(prolog_debug, debug(Topic, true)).
nodebug(Topic) :-
    with_mutex(prolog_debug, debug(Topic, false)).

debug(Spec, Val) :-
    debug_target(Spec, Topic, Out),
    (   (   retract(debugging(Topic, Enabled0, To0))
        *-> update_debug(Enabled0, To0, Val, Out, Enabled, To),
            assert(debugging(Topic, Enabled, To)),
            fail
        ;   (   prolog_load_context(file, _)
            ->  true
            ;   print_message(warning, debug_no_topic(Topic))
            ),
            update_debug(false, [], Val, Out, Enabled, To),
            assert(debugging(Topic, Enabled, To))
        )
    ->  true
    ;   true
    ).

debug_target(Spec, Topic, To) :-
    nonvar(Spec),
    Spec = (Topic > To),
    !.
debug_target(Topic, Topic, -).

update_debug(_, To0, true, -, true, To) :-
    !,
    ensure_output(To0, To).
update_debug(true, To0, true, Out, true, Output) :-
    !,
    (   memberchk(Out, To0)
    ->  Output = To0
    ;   append(To0, [Out], Output)
    ).
update_debug(false, _, true, Out, true, [Out]) :- !.
update_debug(_, _, false, -, false, []) :- !.
update_debug(true, [Out], false, Out, false, []) :- !.
update_debug(true, To0, false, Out, true, Output) :-
    !,
    delete(To0, Out, Output).

ensure_output([], [user_error]) :- !.
ensure_output(List, List).

%!  debug_topic(+Topic) is det.
%
%   Declare a topic for debugging.  This can be used to find all
%   topics available for debugging.

debug_topic(Topic) :-
    (   debugging(Registered, _, _),
        Registered =@= Topic
    ->  true
    ;   assert(debugging(Topic, false, []))
    ).

%!  list_debug_topics is det.
%!  list_debug_topics(+Options) is det.
%
%   List currently known topics for debug/3   and their setting. Options
%   is  either  an  atom  or   string,    which   is   a  shorthand  for
%   `[search(String)]` or a normal option list. Defined options are:
%
%     - search(String)
%       Only show topics that match String.  Match is case insensitive
%       on the printed representation of the term.
%     - active(+Boolean)
%       Only print topics that are active (`true`) or inactive
%       (`false`).
%     - output(+To)
%       Only print topics whose target location matches To.  This option
%       implicitly restricts the output to active topics.

list_debug_topics :-
    list_debug_topics([]).

list_debug_topics(Options) :-
    (   atom(Options)
    ;   string(Options)
    ),
    !,
    list_debug_topics([search(Options)]).
list_debug_topics(Options) :-
    print_message(information, debug_topics(header)),
    option(active(Value), Options, _),
    (   debugging(Topic, Value, To),
        (   option(output(Stream), Options)
        ->  memberchk(Stream, To)
        ;   true
        ),
        numbervars(Topic, 0, _, [singletons(true)]),
        term_string(Topic, String, [quoted(true), numbervars(true)]),
        (   option(search(Search), Options)
        ->  sub_atom_icasechk(String, _, Search)
        ;   true
        ),
        print_message(information, debug_topic(Topic, String, Value, To)),
        fail
    ;   true
    ).

:- multifile
    prolog_debug_tools:debugging_hook/0.

prolog_debug_tools:debugging_hook :-
    (   debugging(_, true)
    ->  list_debug_topics([active(true)])
    ).


%!  debug_message_context(+What) is det.
%
%   Specify additional context for debug messages.
%
%   @deprecated New code should use   the Prolog flag `message_context`.
%   This predicates adds or deletes topics from this list.

debug_message_context(+Topic) :-
    current_prolog_flag(message_context, List),
    (   memberchk(Topic, List)
    ->  true
    ;   append(List, [Topic], List2),
        set_prolog_flag(message_context, List2)
    ).
debug_message_context(-Topic) :-
    current_prolog_flag(message_context, List),
    (   selectchk(Topic, List, Rest)
    ->  set_prolog_flag(message_context, Rest)
    ;   true
    ).

%!  debug(+Topic, +Format, :Args) is det.
%
%   Format a message if debug topic  is enabled. Similar to format/3
%   to =user_error=, but only prints if   Topic is activated through
%   debug/1. Args is a  meta-argument  to   deal  with  goal for the
%   @-command.   Output   is   first    handed     to    the    hook
%   prolog:debug_print_hook/3.  If  this  fails,    Format+Args   is
%   translated  to  text   using    the   message-translation   (see
%   print_message/2) for the  term  debug(Format,   Args)  and  then
%   printed to every matching destination   (controlled  by debug/1)
%   using print_message_lines/3.
%
%   The message is preceded by '% ' and terminated with a newline.
%
%   @see    format/3.

debug(Topic, Format, Args) :-
    debugging(Topic, true, To),
    !,
    print_debug(Topic, To, Format, Args).
debug(_, _, _).


%!  prolog:debug_print_hook(+Topic, +Format, +Args) is semidet.
%
%   Hook called by debug/3.  This  hook   is  used  by the graphical
%   frontend that can be activated using prolog_ide/1:
%
%     ==
%     ?- prolog_ide(debug_monitor).
%     ==

:- multifile
    prolog:debug_print_hook/3.

print_debug(_Topic, _To, _Format, _Args) :-
    nb_current(prolog_debug_printing, true),
    !.
print_debug(Topic, To, Format, Args) :-
    setup_call_cleanup(
        nb_setval(prolog_debug_printing, true),
        print_debug_guarded(Topic, To, Format, Args),
        nb_delete(prolog_debug_printing)).

print_debug_guarded(Topic, _To, Format, Args) :-
    prolog:debug_print_hook(Topic, Format, Args),
    !.
print_debug_guarded(_, [], _, _) :- !.
print_debug_guarded(Topic, To, Format, Args) :-
    phrase('$messages':translate_message(debug(Format, Args)), Lines),
    (   member(T, To),
        debug_output(T, Stream),
        with_output_to(
            Stream,
            print_message_lines(current_output, kind(debug(Topic)), Lines)),
        fail
    ;   true
    ).


debug_output(user, user_error) :- !.
debug_output(Stream, Stream) :-
    is_stream(Stream),
    !.
debug_output(File, Stream) :-
    open(File, append, Stream,
         [ close_on_abort(false),
           alias(File),
           buffer(line)
         ]).


                 /*******************************
                 *           ASSERTION          *
                 *******************************/

%!  assertion(:Goal) is det.
%
%   Acts similar to C assert()  macro.  It   has  no  effect if Goal
%   succeeds. If Goal fails or throws    an exception, the following
%   steps are taken:
%
%     * call prolog:assertion_failed/2.  If prolog:assertion_failed/2
%       fails, then:
%
%       - If this is an interactive toplevel thread, print a
%         message, the stack-trace, and finally trap the debugger.
%       - Otherwise, throw error(assertion_error(Reason, G),_) where
%         Reason is one of =fail= or the exception raised.

assertion(G) :-
    \+ \+ catch(G,
                Error,
                assertion_failed(Error, G)),

    !.
assertion(G) :-
    assertion_failed(fail, G),
    assertion_failed.               % prevent last call optimization.

assertion_failed(Reason, G) :-
    prolog:assertion_failed(Reason, G),
    !.
assertion_failed(Reason, _) :-
    assertion_rethrow(Reason),
    !,
    throw(Reason).
assertion_failed(Reason, G) :-
    print_message(error, assertion_failed(Reason, G)),
    backtrace(10),
    (   current_prolog_flag(break_level, _) % interactive thread
    ->  trace
    ;   throw(error(assertion_error(Reason, G), _))
    ).

assertion_failed.

assertion_rethrow(time_limit_exceeded).
assertion_rethrow('$aborted').


                 /*******************************
                 *           EXPANSION          *
                 *******************************/

%       The optimise_debug flag  defines whether  Prolog  optimizes
%       away assertions and  debug/3 statements.  Values are =true=
%       (debug is optimized away),  =false= (debug is retained) and
%       =default= (debug optimization is dependent on the optimise
%       flag).

optimise_debug :-
    (   current_prolog_flag(optimise_debug, true)
    ->  true
    ;   current_prolog_flag(optimise_debug, default),
        current_prolog_flag(optimise, true)
    ->  true
    ).

:- multifile
    system:goal_expansion/2.

system:goal_expansion(debug(Topic,_,_), true) :-
    (   optimise_debug
    ->  true
    ;   debug_topic(Topic),
        fail
    ).
system:goal_expansion(debugging(Topic), fail) :-
    (   optimise_debug
    ->  true
    ;   debug_topic(Topic),
        fail
    ).
system:goal_expansion(assertion(_), true) :-
    optimise_debug.
system:goal_expansion(assume(_), true) :-
    print_message(informational,
                  compatibility(renamed(assume/1, assertion/1))),
    optimise_debug.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(assertion_failed(_, G)) -->
    [ 'Assertion failed: ~q'-[G] ].
prolog:message(debug(Fmt, Args)) -->
    [ Fmt-Args ].
prolog:message(debug_no_topic(Topic)) -->
    [ '~q: no matching debug topic (yet)'-[Topic] ].
prolog:message(debug_topics(header)) -->
    [ ansi(bold, '~w~t ~w~35| ~w~n', ['Debug Topic', 'Activated', 'To']),
      '~`\u2015t~48|'
    ].
prolog:message(debug_topic(_, TopicString, true, [user_error])) -->
    [ ansi(bold, '~s~t \u2714~35|', [TopicString]) ].
prolog:message(debug_topic(_, TopicString, true, To)) -->
    [ ansi(bold, '~s~t \u2714~35| ~q', [TopicString, To]) ].
prolog:message(debug_topic(_, TopicString, false, _To)) -->
    [ '~s~t -~35|'-[TopicString] ].


                 /*******************************
                 *             HOOKS            *
                 *******************************/

%!  prolog:assertion_failed(+Reason, +Goal) is semidet.
%
%   This hook is called if the Goal  of assertion/1 fails. Reason is
%   unified with either =fail= if Goal simply failed or an exception
%   call otherwise. If this hook  fails,   the  default behaviour is
%   activated.  If  the  hooks  throws  an   exception  it  will  be
%   propagated into the caller of assertion/1.


                 /*******************************
                 *            SANDBOX           *
                 *******************************/

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(prolog_debug:assertion(X), [X]).
