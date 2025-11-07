/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
                              CWI Amsterdam
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

:- module('$history',
          [ read_term_with_history/2           % -Term, +Line
          ]).

:- multifile
    prolog:history/2.

%!  read_term_with_history(-Term, +Options)
%
%   Read a term guide by Options and  maintain a history similar to most
%   Unix shells.

read_term_with_history(Term, Options) :-
    '$option'(prompt(Prompt), Options, '~! ?-'),
    '$option'(input(Input), Options, user_input),
    repeat,
        prompt_history(Prompt),
        '$toplevel':read_query_line(Input, Raw),
        read_history_(Raw, Term, Options),
    !.

read_history_(Raw, _Term, Options) :-
    '$option'(show(Raw), Options, history),
    list_history,
    !,
    fail.
read_history_(Raw, _Term, Options) :-
    '$option'(help(Raw), Options, '!help'),
    '$option'(show(Show), Options, '!history'),
    print_message(help, history(help(Show, Raw))),
    !,
    fail.
read_history_(Raw, Term, Options) :-
    expand_history(Raw, Expanded, Changed),
    add_to_history(Expanded, Options),
    '$option'(module(Module), Options, Var),
    (   Module == Var
    ->  '$current_typein_module'(Module)
    ;   true
    ),
    '$option'(variable_names(Bindings), Options, Bindings0),
    catch(read_term_from_atom(Expanded, Term0,
                              [ module(Module),
                                variable_names(Bindings0)
                              ]),
          E,
          (   print_message(error, E),
              fail
          )),
    (   var(Term0)
    ->  Term = Term0,
        Bindings = Bindings0
    ;   (   Changed == true
        ->  print_message(query, history(expanded(Expanded)))
        ;   true
        ),
        Term = Term0,
        Bindings = Bindings0
    ).

%!  list_history
%
%   Write recorded history events using print_message/2.

list_history :-
    prolog:history(current_input, events(Events0)),
    !,
    '$reverse'(Events0, Events),
    print_message(query, history(history(Events))).
list_history :-
    print_message(query, history(no_history)).

%!   prompt_history(+Prompt)
%
%    Set the prompt using prompt1/1,  substituting   '~!'  by  the event
%    number.

prompt_history('') :-
    !,
    ttyflush.
prompt_history(Prompt) :-
    (   prolog:history(current_input, curr(Curr, _))
    ->  This is Curr + 1
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

%!  substitute(+Old, +New, +String, -Substituted) is semidet.
%
%   substitute first occurence of Old in String by New

substitute(Old, New, String, Substituted) :-
    '$append'(Head, OldAndTail, String),
    '$append'(Old, Tail, OldAndTail),
    !,
    '$append'(Head, New, HeadAndNew),
    '$append'(HeadAndNew, Tail, Substituted).

%!  add_to_history(+Line:atom, +Options) is det.
%
%   Add Line to the command line editing history. Line contains the
%   query as an atom without the Prolog full stop.

add_to_history(end_of_file, _) :- !.
add_to_history(Line, Options) :-
    '$option'(no_save(NoSave), Options),
    catch(term_string(Query, Line), error(_,_), fail),
    nonvar(Query),
    memberchk(Query, NoSave),
    !.
add_to_history(Line, _Options) :-
    format(string(CompleteLine), '~W~W',
           [ Line, [partial(true)],
             '.',  [partial(true)]
           ]),
    catch(prolog:history(user_input, add(CompleteLine)), _, fail),
    !.
add_to_history(_, _).

%!   expand_history(+Raw, -Expanded)
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

%!  get_last_event(-Chars) is semidet.
%
%   return last event typed as a list of characters without the
%   Prolog full stop.

get_last_event(Event) :-
    prolog:history(current_input, first(_Num, String)),
    string_chars(String, Event0),
    remove_full_stop(Event0, Event).
    !.
get_last_event(_) :-
    print_message(query, history(no_event)),
    fail.

remove_full_stop(In, Out) :-
    phrase(remove_full_stop(Out), In).

remove_full_stop([]) -->
    spaces, ['.'], spaces, eos,
    !.
remove_full_stop([H|T]) -->
    [H], !,
    remove_full_stop(T).
remove_full_stop([]) -->
    [].

spaces --> space, !, spaces.
spaces --> [].

space -->
    [C],
    { char_type(C, space) }.

eos([], []).

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
    take_number([N|Rest], NumCodes, Left),
    number_codes(Number, NumCodes),
    prolog:history(current_input, event(Number, String)),
    string_chars(String, Event0),
    remove_full_stop(Event0, Event).
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

%!  matching_event(+String, -Chars) is semidet.
%
%   Return first event with prefix String as a list of Prolog chars
%   without trailing full stop.

matching_event(String, Chars) :-
    prolog:history(current_input, prev_str(String, _Num, String)),
    string_chars(String, Chars0),
    remove_full_stop(Chars0, Chars).
