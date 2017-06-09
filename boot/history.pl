/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2016, University of Amsterdam
                              VU University Amsterdam
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

:- module('$history',
          [ read_history/6,
            '$save_history_line'/1,             % +Line
            '$clean_history'/0,
            '$load_history'/0,
            '$save_history_event'/1
          ]).

%%  read_history(+History, +Help, +DontStore, +Prompt, -Term, -Bindings)
%
%   Give a prompt using Prompt. The   sequence  '%w' is substituted with
%   the current event number. Then read a term from the input stream and
%   perform the history expansion. Return  the   expanded  term  and the
%   bindings of the variables as with  read/2. entering the term History
%   makes read_history/5 print the  history.   Help  specifies  the help
%   command. DontStore is a list of events that need not be stored.

%   When read_history reads a term of   the  form $silent(Goal), it will
%   call Goal and pretend it has not seen anything. This hook is used by
%   the GNU-Emacs interface to for   communication between GNU-EMACS and
%   SWI-Prolog.

read_history(History, Help, DontStore, Prompt, Term, Bindings) :-
    repeat,
        prompt_history(Prompt),
        '$toplevel':read_query_line(user_input, Raw),
        read_history_(History, Help, DontStore, Raw, Term, Bindings),
    !.

read_history_(History, _, _, History, _, _) :-
    list_history,
    !,
    fail.
read_history_(Show, Help, _, Help, _, _) :-
    print_message(help, history(help(Show, Help))),
    !,
    fail.
read_history_(History, Help, DontStore, Raw, Term, Bindings) :-
    expand_history(Raw, Expanded, Changed),
    '$save_history_line'(Expanded),
    '$current_typein_module'(TypeIn),
    catch(read_term_from_atom(Expanded, Term0,
                              [ variable_names(Bindings0),
                                module(TypeIn)
                              ]),
          E,
          (   print_message(error, E),
              fail
          )),
    (   var(Term0)
    ->  Term = Term0,
        Bindings = Bindings0
    ;   Term0 = '$silent'(Goal)
    ->  user:ignore(Goal),
        read_history(History, Help, DontStore, '', Term, Bindings)
    ;   save_event(DontStore, Expanded),
        (   Changed == true
        ->  print_message(query, history(expanded(Expanded)))
        ;   true
        ),
        Term = Term0,
        Bindings = Bindings0
    ).


%   list_history
%   Write history events to the current output stream.

list_history :-
    (   '$history'(Last, _)
    ->  true
    ;   Last = 0
    ),
    history_depth_(Depth),
    plus(First, Depth, Last),
    findall(Nr/Event,
            (   between(First, Last, Nr),
                '$history'(Nr, Event)
            ),
            Events),
    print_message(query, history(history(Events))).

'$clean_history' :-
    retractall('$history'(_,_)).

%!  '$load_history' is det.
%
%   Load persistent history using a hook

'$load_history' :-
    '$clean_history',
    current_prolog_flag(history, Depth),
    Depth > 0,
    catch(prolog:history(current_input, load), _, true), !.
'$load_history'.


%%   prompt_history(+Prompt)
%
%    Give prompt, substituting '~!' by the event number.

prompt_history('') :-
    !,
    ttyflush.
prompt_history(Prompt) :-
    (   '$history'(Last, _)
    ->  This is Last + 1
    ;   This = 1
    ),
    atom_codes(Prompt, SP),
    atom_codes(This, ST),
    (   atom_codes('~!', Repl),
        substitute(Repl, ST, SP, String)
    ->  prompt1(String)
    ;   prompt1(Prompt)
    ),
    ttyflush.

%   substitute(+Old, +New, +String, -Substituted)
%   substitute first occurence of Old in String by New

substitute(Old, New, String, Substituted) :-
    '$append'(Head, OldAndTail, String),
    '$append'(Old, Tail, OldAndTail),
    !,
    '$append'(Head, New, HeadAndNew),
    '$append'(HeadAndNew, Tail, Substituted),
    !.

%!  '$save_history_line'(+Line)
%
%   Add Line to the command line editing history.

:- multifile
    prolog:history_line/2.

'$save_history_line'(end_of_file) :- !.
'$save_history_line'(Line) :-
    format(string(CompleteLine), '~W~W',
           [ Line, [partial(true)],
             '.',  [partial(true)]
           ]),
    catch(prolog:history(user_input, add(CompleteLine)), _, fail),
    !.
'$save_history_line'(_).

%!  save_event(+DoNotSave, +Event)
%
%   Save Event into the  history  system  if   it  is  not  a  member of
%   DoNotSave.

save_event(Dont, Event) :-
    memberchk(Event, Dont),
    !.
save_event(_, Event) :-
    '$save_history_event'(Event).

%!  '$save_history_event'(+Event) is det.
%
%   Save an input line as text into the !- based history. Event is one
%   of
%
%     * a *string*.  The event is added with a next number at the end.
%     * a *pair*.  The event is added with the given sequence number.

:- thread_local
    '$history'/2.

'$save_history_event'(Num-String) :-
    integer(Num), string(String),
    !,
    asserta('$history'(Num, String)),
    truncate_history(Num).
'$save_history_event'(Event) :-
    to_string(Event, Event1),
    !,
    last_event(Num, String),
    (   Event1 == String
    ->  true
    ;   New is Num + 1,
        asserta('$history'(New, Event1)),
        truncate_history(New)
    ).
'$save_history_event'(Event) :-
    '$type_error'(history_event, Event).

last_event(Num, String) :-
    '$history'(Num, String),
    !.
last_event(0, "").

to_string(String, String) :-
    string(String),
    !.
to_string(Atom, String) :-
    atom_string(Atom, String).

truncate_history(New) :-
    history_depth_(Depth),
    remove_history(New, Depth).

remove_history(New, Depth) :-
    New - Depth =< 0,
    !.
remove_history(New, Depth) :-
    Remove is New - Depth,
    retract('$history'(Remove, _)),
    !.
remove_history(_, _).

%    history_depth_(-Depth)
%    Define the depth to which to keep the history.

history_depth_(N) :-
    current_prolog_flag(history, N),
    integer(N),
    N > 0,
    !.
history_depth_(25).

%    expand_history(+Raw, -Expanded)
%    Expand Raw using the available history list. Expandations performed
%    are:
%
%       !match          % Last event starting <match>
%       !n              % Event nr. <n>
%       !!              % last event
%
%    Note: the first character after a '!' should be a letter or number to
%    avoid problems with the cut.

expand_history(Raw, Expanded, Changed) :-
    atom_chars(Raw, RawString),
    expand_history2(RawString, ExpandedString, Changed),
    atom_chars(Expanded, ExpandedString),
    !.

expand_history2([!], [!], false) :- !.
expand_history2([!, C|Rest], [!|Expanded], Changed) :-
    not_event_char(C),
    !,
    expand_history2([C|Rest], Expanded, Changed).
expand_history2([!|Rest], Expanded, true) :-
    !,
    match_event(Rest, Event, NewRest),
    '$append'(Event, RestExpanded, Expanded),
    !,
    expand_history2(NewRest, RestExpanded, _).
expand_history2(['\''|In], ['\''|Out], Changed) :-
    !,
    skip_quoted(In, '\'', Out, Tin, Tout),
    expand_history2(Tin, Tout, Changed).
expand_history2(['"'|In], ['"'|Out], Changed) :-
    !,
    skip_quoted(In, '"', Out, Tin, Tout),
    expand_history2(Tin, Tout, Changed).
expand_history2([H|T], [H|R], Changed) :-
    !,
    expand_history2(T, R, Changed).
expand_history2([], [], false).

skip_quoted([Q|T],Q,[Q|R], T, R) :- !.
skip_quoted([\,Q|T0],Q,[\,Q|T], In, Out) :-
    !,
    skip_quoted(T0, Q, T, In, Out).
skip_quoted([Q,Q|T0],Q,[Q,Q|T], In, Out) :-
    !,
    skip_quoted(T0, Q, T, In, Out).
skip_quoted([C|T0],Q,[C|T], In, Out) :-
    !,
    skip_quoted(T0, Q, T, In, Out).
skip_quoted([], _, [], [], []).

%   get_last_event(-String)
%   return last event typed as a string

get_last_event(Event) :-
    '$history'(_, Atom),
    atom_chars(Atom, Event),
    !.
get_last_event(_) :-
    print_message(query, history(no_event)),
    fail.

%   match_event(+Spec, -Event, -Rest)
%   Use Spec as a specification of and event and return the event as Event
%   and what is left of Spec as Rest.

match_event(Spec, Event, Rest) :-
    find_event(Spec, Event, Rest),
    !.
match_event(_, _, _) :-
    print_message(query, history(no_event)),
    fail.

not_event_char(C) :- code_type(C, csym), !, fail.
not_event_char(!) :- !, fail.
not_event_char(_).

find_event([!|Left], Event, Left) :-
    !,
    get_last_event(Event).
find_event([N|Rest], Event, Left) :-
    code_type(N, digit),
    !,
    take_number([N|Rest], String, Left),
    number_codes(Number, String),
    '$history'(Number, Atom),
    atom_chars(Atom, Event).
find_event(Spec, Event, Left) :-
    take_string(Spec, String, Left),
    matching_event(String, Event).

take_string([C|Rest], [C|String], Left) :-
    code_type(C, csym),
    !,
    take_string(Rest, String, Left).
take_string([C|Rest], [], [C|Rest]) :- !.
take_string([], [], []).

take_number([C|Rest], [C|String], Left) :-
    code_type(C, digit),
    !,
    take_string(Rest, String, Left).
take_number([C|Rest], [], [C|Rest]) :- !.
take_number([], [], []).

%   matching_event(+String, -Event)
%
%   Return first event with prefix String as a Prolog string.

matching_event(String, Event) :-
    '$history'(_, AtomEvent),
    atom_chars(AtomEvent, Event),
    '$append'(String, _, Event),
    !.
