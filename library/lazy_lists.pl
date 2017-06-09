/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
                         CWI Amsterdam
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

:- module(lazy_lists,
          [ lazy_list/2,                        % :Next, -List
            lazy_list/3,                        % :Next, +State0, -List
                                                % Utilities
            lazy_list_materialize/1,            % ?List
            lazy_list_length/2,                 % +List, -Len

            lazy_findall/3,                     % ?Templ, :Goal, -List
            lazy_findall/4,                     % +ChunkSize, ?Templ, :Goal, -List
                                                % Interators
            lazy_get_codes/4,                   % +Stream, +N, -List, -Tail
            lazy_read_terms/4,                  % +Stream, +Options, -List, -Tail
            lazy_read_lines/4,                  % +Stream, +Options, -List, -Tail

            lazy_message_queue/4,               % +Queue, +Options, -List, -Tail
            lazy_engine_next/4,                 % +Engine, +N, -List, -Tail

            lazy_list_iterator/4                % +Iterator, -Next, :GetNext,
                                                % :TestEnd
          ]).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(error)).

:- meta_predicate
    lazy_list(2, -),
    lazy_list(3, +, -),
    lazy_findall(?, 0, -),
    lazy_findall(+, ?, 0, -).

/** <module> Lazy list handling

This module builds a lazy list from   a predicate that fetches a _slice_
of this list. In addition it provides _interactors_ (slice constructors)
for several common use cases for lazy  lists, such as reading objects of
several sizes from files (characters,   lines,  terms), reading messages
from message queues and reading answers from _engines_.

Lazy lists are lists that  end  in   a  constraint.  Trying to unify the
constraint forces the next slice of the list  to be fetched and added to
the list.

The typical use case for lazy lists is to   run a DCG grammar on it. For
example, an _agent_ may be listening on a socket and turn the line-based
message protocol into a list using the fragment below.

```
        ...,
        tcp_open(Socket, Read, Write),
        lazy_list(lazy_read_lines(Read, [as(codes)]), List),
        phrase(action, List).
```

Typically, the iterator works on a globally allocated object that is not
always subject to garbage collection.  In such cases, the skeleton usage
follows the pattern below:

```
        setup_call_cleanup(
            <open resource>(R),
            (  lazy_list(<iterator>(R), List),
               process_list(List)
            ),
            <close resource>(R))
```

This is rather unfortunately, but there is no way we can act on the fact
that `List` is no further accessed. In  some cases, e.g., message queues
or engines, the resource is subject to (atom) garbage collection.
*/

:- predicate_options(lazy_read_terms/4, 2,
                     [ chunk(positive_integer),
                       pass_to(read_term/3, 3)
                     ]).
:- predicate_options(lazy_read_lines/4, 2,
                     [ chunk(positive_integer),
                       as(oneof([atom,string,codes,chars]))
                     ]).
:- predicate_options(lazy_message_queue/4, 2,
                     [ chunk(positive_integer),
                       pass_to(thread_get_message/3, 3)
                     ]).

%!  lazy_list(:Next, -List)
%
%   Create a lazy list from a callback. Next is called repeatedly to
%   extend the list. It is called   as call(Next, List, Tail), where
%   the _difference list_ List\Tail produces the   next slice of the
%   list. If the end of  the  input   is  reached,  `List` must be a
%   proper list and `Tail` must be `[]`.
%
%   @bug The content returned  by  the   iterator  is  duplicated in
%   nb_setarg/3. This is  needed  by  avoid   the  risk  of  trailed
%   assignments in the structure. Avoiding   this  duplication would
%   significantly reduce the overhead.

lazy_list(Next, List) :-
    put_attr(List, lazy_lists, lazy_list(Next, _)).

attr_unify_hook(State, Value) :-
    State = lazy_list(Next, Read),
    (   var(Read)
    ->  call(Next, NewList, Tail),
        (   Tail == []
        ->  nb_setarg(2, State, NewList)
        ;   lazy_list(Next, Tail),
            nb_setarg(2, State, NewList)
        ),
        arg(2, State, Value)
    ;   Value = Read
    ).

attribute_goals(X) -->
    { get_attr(X, lazy_lists, lazy_list(Next, _)) },
    [lazy_list(Next, X)].

%!  lazy_list(:Next, +State0, -List)
%
%   Create a lazy list where the next element is defined by
%
%       call(Next, State0, State1, Head)
%
%   The example below uses this  predicate   to  define  a lazy list
%   holding the Fibonacci numbers. Our state  keeps the two previous
%   Fibonacci numbers.
%
%     ```
%     fibonacci_numbers(L) :-
%         lazy_list(fib, state(-,-), L).
%
%     fib(state(-,-), state(0,-), 0) :- !.
%     fib(state(0,-), state(1,0), 1) :- !.
%     fib(state(P,Q), state(F,P), F) :-
%         F is P+Q.
%     ```
%
%   The above can be used to retrieve   the Nth Fibonacci number. As
%   fib/2 provides no access  to  the   complete  list  of Fibonacci
%   numbers, this can be used to generate large Fibonacci numbers.
%
%     ```
%     fib(N, F) :-
%         fibonacci_numbers(L),
%         nth1(N, L, F).
%     ```

lazy_list(Next, State0, List) :-
    lazy_list(lazy_state(Next, s(State0)), List).

lazy_state(Pred, LState, [H|T], T) :-
    LState = s(State0),
    call(Pred, State0, State1, H),
    !,
    nb_setarg(1, LState, State1).
lazy_state(_, _, [], []).


                 /*******************************
                 *   OPERATIONS ON LAZY LISTS   *
                 *******************************/

%!  lazy_list_materialize(?List) is det.
%
%   Materialize the lazy list.

lazy_list_materialize(List) :-
    '$skip_list'(_, List, Tail),
    (   var(Tail),
        Tail = [_|T2]
    ->  lazy_list_materialize(T2)
    ;   Tail = []
    ->  true
    ;   type_error(list, Tail)
    ).

%!  lazy_list_length(+List, -Len) is det.
%
%   True if Len is the length of   the  materialized lazy list. Note
%   that length/2 reports the length   of the currently materialized
%   part and on backtracking longer lists.

lazy_list_length(List, Len) :-
    lazy_list_length(List, 0, Len).

lazy_list_length(List, L0, L) :-
    !,
    '$skip_list'(N, List, Tail),
    (   var(Tail),
        Tail = [_|T2]
    ->  L1 is L0+N+1,
        lazy_list_length(T2, L1, L)
    ;   Tail = []
    ->  L is L0+N
    ;   type_error(list, Tail)
    ).


                 /*******************************
                 *          INTERATORS          *
                 *******************************/

lazy_list_expand_handler(
    lazy_list_iterator(Handler, Next, Get1, TestEnd),
    Clauses) :-
    negate(TestEnd, NotTestEnd),
    extend_goal(Handler, [N, List, Tail], Head),
    extend_goal(Handler, [N2,T,Tail], Recurse),
    general_goal(Handler, Handler2),
    extend_goal(Handler2, [_, Tail,Tail], Head2),
    Clauses = [ (Head :-
                    succ(N2, N), !,
                    (   Get1,
                        NotTestEnd
                    ->  List = [Next|T],
                        Recurse
                    ;   List = [],
                        Tail = []
                    )),
                (Head2)
              ].

negate(A==B, A\==B) :- !.
negate(fail, true) :- !.
negate(false, true) :- !.
negate(Goal, \+ Goal).

extend_goal(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
extend_goal(M:G, Args, M:GX) :-
    !,
    extend_goal(G, Args, GX).
extend_goal(Name, Args, GX) :-
    atom(Name),
    !,
    compound_name_arguments(GX, Name, Args).
extend_goal(G, XArgs, GX) :-
    compound_name_arguments(G, Name, Args0),
    append(Args0, XArgs, Args),
    compound_name_arguments(GX, Name, Args).

general_goal(Var, Var) :-
    var(Var),
    !.
general_goal(M:G, M:GG) :-
    !,
    general_goal(G, GG).
general_goal(Atom, Atom) :-
    atom(Atom),
    !.
general_goal(G, GG) :-
    !,
    compound_name_arity(G, Name, Arity),
    compound_name_arity(GG, Name, Arity).

:- multifile
    system:term_expansion/2.

system:term_expansion((:- lazy_list_iterator(It, One, GetNext, TestEnd)),
                      Expanded) :-
    lazy_list_expand_handler(
        lazy_list_iterator(It, One, GetNext, TestEnd),
        Expanded).

%!  lazy_list_iterator(+Iterator, -Next, :GetNext, :TestEnd)
%
%   Directive to create a lazy list  iterator from a predicate that
%   gets a single next value.

lazy_list_iterator(Iterator, Next, GetNext, TestEnd) :-
    throw(error(context_error(nodirective,
                              lazy_list_iterator(Iterator, Next,
                                                  GetNext, TestEnd)),
                _)).

%!  lazy_get_codes(+Stream, +N, -List, -Tail)
%
%   Lazy list iterator to get character   codes  from a stream.
%
%   @see library(pure_input) The predicate lazy_get_codes/4 provides
%   similar functionality to what   stream_to_lazy_list/2 does while
%   in addition library(pure_input) is faster due to the use of more
%   low-level primitives and supports fetching   the location in the
%   stream.

:- lazy_list_iterator(lazy_get_codes(Stream), Code,
                      get_code(Stream, Code),
                      Code == -1).

%!  lazy_read_terms(+Stream, +Options, -List, -Tail)
%
%   Turn a stream into a lazy list of Prolog terms.  Options are
%   passed to read_term/3, except for:
%
%     - chunk(ChunkSize)
%     Determines the read chunk size.  Default is 10.

lazy_read_terms(Stream, Options, List, Tail) :-
    select_option(chunk(N), Options, ReadOptions, 10),
    lazy_read_terms_(Stream, ReadOptions, N, List, Tail).

:- lazy_list_iterator(lazy_read_terms_(Stream, Options), Term,
                      read_term(Stream, Term, Options),
                      Term == end_of_file).

%!  lazy_read_lines(+Stream, +Options, -List, -Tail) is det.
%
%   Lazy list iterator to read lines from Stream.  Options include:
%
%     - chunk(ChunkSize)
%     Determines the read chunk size.  Default is 10.
%     - as(+Type)
%     Determine the output type for each line.  Valid values are
%     `atom`, `string`, `codes` or `chars`.  Default is `string`.

lazy_read_lines(Stream, Options, List, Tail) :-
    option(chunk(ChunkSize), Options, 10),
    option(as(Type), Options, string),
    must_be(positive_integer, ChunkSize),
    must_be(oneof([atom,string,codes,chars]), Type),
    lazy_read_lines(Type, Stream, ChunkSize, List, Tail).

lazy_read_lines(string, Stream, ChunkSize, List, Tail) :-
    lazy_read_string_lines(Stream, ChunkSize, List, Tail).
lazy_read_lines(atom, Stream, ChunkSize, List, Tail) :-
    lazy_read_atom_lines(Stream, ChunkSize, List, Tail).
lazy_read_lines(codes, Stream, ChunkSize, List, Tail) :-
    lazy_read_codes_lines(Stream, ChunkSize, List, Tail).
lazy_read_lines(chars, Stream, ChunkSize, List, Tail) :-
    lazy_read_chars_lines(Stream, ChunkSize, List, Tail).

:- lazy_list_iterator(lazy_read_string_lines(Stream), Line,
                      read_line_to_string(Stream, Line),
                      Line == end_of_file).
:- lazy_list_iterator(lazy_read_codes_lines(Stream), Line,
                      read_line_to_codes(Stream, Line),
                      Line == end_of_file).
:- lazy_list_iterator(lazy_read_chars_lines(Stream), Line,
                      read_line_to_chars(Stream, Line),
                      Line == end_of_file).
:- lazy_list_iterator(lazy_read_atom_lines(Stream), Line,
                      read_line_to_atom(Stream, Line),
                      Line == -1).

read_line_to_chars(Stream, Chars) :-
    read_line_to_string(Stream, String),
    (   String == end_of_file
    ->  Chars = String
    ;   string_chars(String, Chars)
    ).

read_line_to_atom(Stream, Atom) :-
    read_line_to_string(Stream, String),
    (   String == end_of_file
    ->  Atom = -1
    ;   atom_string(Atom, String)
    ).

%!  lazy_message_queue(+Queue, +Options, -List, -Tail) is det.
%
%   Lazy list iterator for message  queues.   Options  are passed to
%   thread_get_message/3. In addition,  the   following  options are
%   processed:
%
%     - chunk(ChunkSize)
%     Determines the read chunk size.  Default is 1.
%
%   A thread can listen to its own message queue using
%
%   ```
%           thread_self(Me),
%           lazy_list(lazy_message_queue(Me, []), List),
%           phrase(action(List)).
%   ```

lazy_message_queue(Queue, Options, List, Tail) :-
    select_option(chunk(ChunkSize), Options, QueueOptions, 1),
    lazy_message_queue_(Queue, QueueOptions, ChunkSize, List, Tail).

:- lazy_list_iterator(lazy_message_queue_(Queue, Options), Message,
                      thread_get_message(Queue, Message, Options),
                      fail).


%!  lazy_engine_next(+Engine, +N, -List, -Tail)
%
%   Lazy list iterator for  engines.  This   is  used  to  implement
%   lazy_findall/3,4.

:- lazy_list_iterator(lazy_engine_next(Engine), Answer,
                      engine_next(Engine, Answer),
                      fail).

%!  lazy_findall(?Templ, :Goal, -List) is det.
%!  lazy_findall(+ChunkSize, ?Templ, :Goal, -List) is det.
%
%   True when List is a lazy  list containing the instantiations for
%   Template for each  answer  of  Goal.   Goal  is  executed  in an
%   _engine_ (see engine_create/3).
%
%   @bug    Engines are reclaimed by atom garbage collection.  As
%           they can be quite expensive, a large amount of resources
%           may be waiting for collection.  If the list is fully
%           materialized only the dead engine remains, which is
%           fairly cheap.

lazy_findall(Templ, Goal, List) :-
    lazy_findall(1, Templ, Goal, List).
lazy_findall(Chunk, Templ, Goal, List) :-
    engine_create(Templ, Goal, Engine),
    lazy_list(lazy_engine_next(Engine, Chunk), List).


                 /*******************************
                 *            SANDBOX           *
                 *******************************/

:- multifile
    sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(lazy_lists:lazy_findall/3).
sandbox:safe_meta_predicate(lazy_lists:lazy_findall/4).
sandbox:safe_meta_predicate(lazy_lists:lazy_list/2).
sandbox:safe_meta_predicate(lazy_lists:lazy_list/3).
