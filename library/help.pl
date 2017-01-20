/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(online_help,
          [ help/1,
            help/0,
            apropos/1
          ]).
:- use_module(lists, [append/3, member/2]).

:- if(exists_source(library(helpidx))).
:- use_module(library(helpidx)).
no_help :-
    fail.
:- else.
no_help :-
    print_message(warning, no_help_files).
function(_,_,_).                        % make check silent
predicate(_,_,_,_,_).
section(_,_,_,_).
:- endif.

:- multifile
    prolog:help_hook/1,             % Generic help hook.
    prolog:show_help_hook/2.        % +Title, +TmpFile

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module  defines the  online  help  facility of   SWI-Prolog.   It
assumes  (Prolog) index file  at library(help_index)   and  the actual
manual  at library(online_manual).   Output  is piped through  a  user
defined pager, which defaults to `more'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%       help/0

help :-
    no_help,
    !.
help :-
    prolog:help_hook(help),
    !.
help :-
    help(help/1).

%!  help(+Subject)
%
%   Display online help on specified subject.

help(_) :-
    no_help,
    !.
help(What) :-
    prolog:help_hook(help(What)),
    !.
help(What) :-
    give_help(What).

%!  apropos(Pattern)
%   Give a list of subjects that might be appropriate.

apropos(_) :-
    no_help,
    !.
apropos(What) :-
    prolog:help_hook(apropos(What)),
    !.
apropos(What) :-
    give_apropos(What).

give_help(Name/Arity) :-
    !,
    predicate(Name, Arity, _, From, To),
    !,
    show_help(Name/Arity, [From-To]).
give_help(Section) :-
    user_index(Index, Section),
    !,
    section(Index, _, From, To),
    show_help(Section, [From-To]).
give_help(Function) :-
    atom(Function),
    atom_concat('PL_', _, Function),
    function(Function, From, To),
    !,
    show_help(Function, [From-To]).
give_help(Name) :-
    findall(From-To, predicate(Name, _, _, From, To), Ranges),
    Ranges \== [],
    !,
    show_help(Name, Ranges).
give_help(What) :-
    format('No help available for ~w~n', [What]).

%!  show_help(+ListOfRanges)
%   Pipe specified ranges of the manual through the user defined pager

:- dynamic asserted_help_tmp_file/1.

help_tmp_file(X) :-
    asserted_help_tmp_file(X),
    !.
help_tmp_file(X) :-
    tmp_file(manual, X),
    asserta(asserted_help_tmp_file(X)).

write_ranges_to_file(Ranges, Outfile) :-
    online_manual_stream(Manual),
    help_tmp_file(Outfile),
    open(Outfile, write, Output),
    show_ranges(Ranges, Manual, Output),
    close(Manual),
    close(Output).

show_help(Title, Ranges) :-
    predicate_property(prolog:show_help_hook(_,_), number_of_clauses(N)),
    N > 0,
    write_ranges_to_file(Ranges, TmpFile),
    prolog:show_help_hook(Title, TmpFile).
show_help(_, Ranges) :-
    current_prolog_flag(pipe, true),
    !,
    online_manual_stream(Manual),
    pager_stream(Pager),
    catch(show_ranges(Ranges, Manual, Pager), _, true),
    close(Manual),
    catch(close(Pager), _, true).
show_help(_, Ranges) :-
    online_manual_stream(Manual),
    show_ranges(Ranges, Manual, user_output).

show_ranges([], _, _) :- !.
show_ranges([FromLine-ToLine|Rest], Manual, Pager) :-
    line_start(FromLine, From),
    line_start(ToLine, To),
    seek(Manual, From, bof, _),
    Range is To - From,
    copy_chars(Range, Manual, Pager),
    nl(Pager),
    show_ranges(Rest, Manual, Pager).

%!  copy_chars(+Count, +FromStream, +ToStream)
%
%   Note: stream is binary to deal with byte offsets. As the data is
%   ISO Latin-1 anyway, this is fine.

copy_chars(N, From, To) :-
    get0(From, C0),
    copy_chars(N, From, To, C0).

copy_chars(N, _, _, _) :-
    N =< 0,
    !.
copy_chars(N, _, To, _) :-
    0 =:= N mod 4096,
    flush_output(To),
    fail.
copy_chars(N, From, To, C) :-
    get_byte(From, C1),
    (   C1 == 8,                    % backspace
        \+ current_prolog_flag(write_help_with_overstrike, true)
    ->  get_byte(From, C2),
        NN is N - 2,
        copy_chars(NN, From, To, C2)
    ;   put_printable(To, C),
        NN is N - 1,
        copy_chars(NN, From, To, C1)
    ).

put_printable(_, 12) :- !.
put_printable(_, 13) :- !.
put_printable(_, -1) :- !.
put_printable(To, C) :-
    put_code(To, C).

online_manual_stream(Stream) :-
    find_manual(Manual),
    open(Manual, read, Stream, [type(binary)]).

pager_stream(Stream) :-
    find_pager(Pager),
    open(pipe(Pager), write, Stream).

find_manual(Path) :-
    absolute_file_name(library('MANUAL'), Path, [access(read)]).

find_pager(Pager) :-
    getenv('PAGER', Pager),
    !.
find_pager(more).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the write_help_with_overstrike feature.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

set_overstrike_feature :-
    current_prolog_flag(write_help_with_overstrike, _),
    !.
set_overstrike_feature :-
    (   getenv('TERM', xterm)
    ->  Flag = true
    ;   Flag = false
    ),
    create_prolog_flag(write_help_with_overstrike, Flag, []).

:- initialization set_overstrike_feature.

%!  line_start(Line, Start) is det.
%
%   True if Start is the byte position at which Line starts.

:- dynamic
    start_of_line/2.

line_start(Line, Start) :-
    start_of_line(Line, Start),
    !.
line_start(Line, Start) :-
    line_index,
    start_of_line(Line, Start).


%!  line_index
%
%   Create index holding the byte positions for the line starts

line_index :-
    start_of_line(_,_),
    !.
line_index :-
    online_manual_stream(Stream),
    set_stream(Stream, encoding(octet)),
    call_cleanup(line_index(Stream, 1), close(Stream)).

line_index(Stream, LineNo) :-
    byte_count(Stream, ByteNo),
    assert(start_of_line(LineNo, ByteNo)),
    (   at_end_of_stream(Stream)
    ->  true
    ;   LineNo2 is LineNo+1,
        skip(Stream, 10),
        line_index(Stream, LineNo2)
    ).


                 /*******************************
                 *             APROPOS          *
                 *******************************/

give_apropos(Atom) :-
    ignore(predicate_apropos(Atom)),
    ignore(function_apropos(Atom)),
    ignore(section_apropos(Atom)).

apropos_predicate(Pattern, Name, Arity, Summary) :-
    predicate(Name, Arity, Summary, _, _),
    (   apropos_match(Pattern, Name)
    ->  true
    ;   apropos_match(Pattern, Summary)
    ).

predicate_apropos(Pattern) :-
    findall(Name-Arity-Summary,
            apropos_predicate(Pattern, Name, Arity, Summary),
            Names),
    forall(member(Name-Arity-Summary, Names),
           format('~w/~w~t~30|~w~n', [Name, Arity, Summary])).

function_apropos(Pattern) :-
    findall(Name, (function(Name, _, _),
                   apropos_match(Pattern, Name)), Names),
    forall(member(Name, Names),
           format('Interface Function~t~30|~w()~n', Name)).

section_apropos(Pattern) :-
    findall(Index-Name, (section(Index, Name, _, _),
                   apropos_match(Pattern, Name)), Names),
    forall(member(Index-Name, Names),
           (user_index(Index, UserIndex),
            format('Section ~w~t~30|"~w"~n', [UserIndex, Name]))).

apropos_match(Needle, Haystack) :-
    sub_atom_icasechk(Haystack, _, Needle).

user_index(List, Index) :-
    is_list(List),
    !,
    to_user_index(List, S),
    name(Index, S).
user_index(List, Index) :-
    to_system_index(Index, List).

to_user_index([], []).
to_user_index([A], S) :-
    !,
    name(A, S).
to_user_index([A|B], S) :-
    name(A, S0),
    append(S0, [0'-], S1),
    append(S1, Rest, S),
    to_user_index(B, Rest).

to_system_index(A-B, I) :-
    !,
    to_system_index(A, C),
    integer(B),
    append(C, [B], I).
to_system_index(A, [A]) :-
    integer(A).

                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(no_help_files) -->
    [ 'The online help files (helpidx.pl, MANUAL) are not installed.', nl,
      'If you installed SWI-Prolog from GIT/CVS, please consult', nl,
      'README.doc and README.git in the toplevel of the sources.'
    ].


