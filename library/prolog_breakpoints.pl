/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/
    Copyright (c)  2011-2022, University of Amsterdam
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

:- module(prolog_breakpoints,
          [ set_breakpoint/4,           % +File, +Line, +CharPos, -Id
            set_breakpoint/5,           % +Owner, +File, +Line, +CharPos, -Id
            set_breakpoint/6,           % +Owner, +File, +Line, +CharPos, :Cond, -Id
            delete_breakpoint/1,        % +Id
            breakpoint_property/2       % ?Id, ?Property
          ]).
:- autoload(library(debug), [debug/3]).
:- autoload(library(error), [existence_error/2]).
:- autoload(library(lists), [nth1/3, member/2]).
:- autoload(library(prolog_clause), [clause_info/4, clause_name/2]).


/** <module> Manage Prolog break-points

This module provides an  interface  for   development  tools  to set and
delete break-points, giving a location in  the source. Development tools
that want to track changes to   breakpoints must use user:message_hook/3
to intercept these message terms:

  * breakpoint(set, Id)
  * breakpoint(delete, Id)

Note that the hook must fail  after   creating  its side-effects to give
other hooks the opportunity to react.
*/

:- meta_predicate set_breakpoint(+, +, +, +, 0, -).

%!  set_breakpoint(+File, +Line, +Char, -Id) is det.
%!  set_breakpoint(+Owner, +File, +Line, +Char, -Id) is det.
%!  set_breakpoint(+Owner, +File, +Line, +Char, :Cond, -Id) is det.
%
%   Put a breakpoint at the indicated source-location. File is a current
%   sourcefile (as reported by source_file/1). Line  is the 1-based line
%   in which Char is. Char is the position of the break.
%
%   Cond is a goal that will be invoked whenever the newly set
%   breakpoint is triggered. If goal fails, the breakpoint is skipped
%   and execution commences normally.  In the context of Cond,
%   break_context_frame/1 is made available for obtaining the
%   execution frame identifier in which the breakpoint was triggered.
%
%   First, '$clause_from_source'/4 uses  the   SWI-Prolog  clause-source
%   information  to  find  the  last    clause   starting  before  Line.
%   '$break_pc'  generates  (on  backtracking),  a    list  of  possible
%   breakpoints.
%
%   Note that in addition to setting the breakpoint, the system must be
%   in debug mode for the  breakpoint   to  take  effect. With threading
%   enabled, there are various different  ways   this  may  be done. See
%   debug/0, tdebug/0 and tdebug/1. Therefore, this predicate does *not*
%   enable debug mode.
%
%   @arg Owner denotes the file that _owns_ the clause. set_breakpoint/5
%   is used to set breakpoints in an included file in the context of the
%   Owner main file. See source_file_property/2.

set_breakpoint(File, Line, Char, Id) :-
    set_breakpoint(File, File, Line, Char, Id).

set_breakpoint(Owner, File, Line, Char, Id) :-
    set_breakpoint(Owner, File, Line, Char, true, Id).

set_breakpoint(Owner, File, Line, Char, Cond, Id) :-
    debug(break, 'break_at(~q, ~d, ~d).', [File, Line, Char]),
    '$clause_from_source'(Owner, File, Line, ClauseRefs),
    member(ClauseRef, ClauseRefs),
    clause_info(ClauseRef, InfoFile, TermPos, _NameOffset),
    (   InfoFile == File
    ->  '$break_pc'(ClauseRef, PC, NextPC),
        debug(break, 'Clause ~p, PC=~p NextPC=~p', [ClauseRef, PC, NextPC]),
        '$clause_term_position'(ClauseRef, NextPC, List),
        debug(break, 'Location = ~w', [List]),
        range(List, TermPos, SubPos),
        arg(1, SubPos, A),
        arg(2, SubPos, Z),
        debug(break, 'Term from ~w-~w', [A, Z]),
        Z >= Char, !,
        Len is Z - A,
        b_setval('$breakpoint', file_location(File, Line, A, Len)-Cond)
    ;   print_message(warning, breakpoint(no_source(ClauseRef, File, Line))),
        '$break_pc'(ClauseRef, PC, _), !,
        nb_delete('$breakpoint')
    ),
    debug(break, 'Break at clause ~w, PC=~w', [ClauseRef, PC]),
    '$break_at'(ClauseRef, PC, true),
    nb_delete('$breakpoint'),
    known_breakpoint(ClauseRef, PC, _Location, _Cond, Id).

range(_,  Pos, _), var(Pos) =>
    fail.
range(List, parentheses_term_position(_,_,Pos), SubPos) =>
    range(List, Pos, SubPos).
range([], Pos, SubPos) =>
    SubPos = Pos.
range([H|T], term_position(_, _, _, _, PosL), SubPos) =>
    nth1(H, PosL, Pos),
    range(T, Pos, SubPos).
range(exit, Pos, SubPos) =>
    arg(2, Pos, End),
    Dot is End,
    EndDot is Dot+1,
    SubPos = Dot-EndDot.

:- dynamic
    known_breakpoint/5,           % ClauseRef, PC, Location, Cond, Id
    break_id/1.

next_break_id(Id) :-
    retract(break_id(Id0)),
    !,
    Id is Id0+1,
    asserta(break_id(Id)).
next_break_id(1) :-
    asserta(break_id(1)).

%!  delete_breakpoint(+Id) is det.
%
%   Delete   breakpoint   with    given     Id.    If    successful,
%   print_message(breakpoint(delete, Id)) is called.   Message hooks
%   working on this message may still call breakpoint_property/2.
%
%   @error existence_error(breakpoint, Id).

delete_breakpoint(Id) :-
    integer(Id),
    known_breakpoint(ClauseRef, PC, _Location, _Cond, Id),
    !,
    '$break_at'(ClauseRef, PC, false).
delete_breakpoint(Id) :-
    existence_error(breakpoint, Id).

%!  breakpoint_property(?Id, ?Property) is nondet.
%
%   True when Property is a property of the breakpoint Id.  Defined
%   properties are:
%
%       * file(File)
%       Provided if the breakpoint is in a clause associated to a
%       file.  May not be known.
%       * line_count(Line)
%       Line of the breakpoint.  May not be known.
%       * character_range(Start, Len)
%       One-based character offset of the break-point.  May not be
%       known.
%       * clause(Reference)
%       Reference of the clause in which the breakpoint resides.

breakpoint_property(Id, file(File)) :-
    known_breakpoint(ClauseRef,_,_,_,Id),
    clause_property(ClauseRef, file(File)).
breakpoint_property(Id, line_count(Line)) :-
    known_breakpoint(_,_,Location,_,Id),
    location_line(Location, Line).
breakpoint_property(Id, character_range(Start, Len)) :-
    known_breakpoint(ClauseRef,PC,Location,_,Id),
    (   Location = file_location(_File, _Line, Start, Len)
    ->  true
    ;   break_location(ClauseRef, PC, _File, SubPos),
        compound(SubPos),
        arg(1, SubPos, Start),
        arg(2, Start, End),
        nonvar(Start), nonvar(End),
        Len is End+1-Start
    ).
breakpoint_property(Id, clause(Reference)) :-
    known_breakpoint(Reference,_,_,_,Id).
breakpoint_property(Id, condition(Cond)) :-
    known_breakpoint(_,_,_,Cond,Id).

location_line(file_location(_File, Line, _Start, _Len), Line).
location_line(file_character_range(File, Start, _Len), Line) :-
    file_line(File, Start, Line).
location_line(file_line(_File, Line), Line).


%!  file_line(+File, +StartIndex, -Line) is det.
%
%   True when Line is the  1-based  line   offset  in  which we find
%   character StartIndex.

file_line(File, Start, Line) :-
    setup_call_cleanup(
        prolog_clause:try_open_source(File, In),
        stream_line(In, Start, 1, Line),
        close(In)).

stream_line(In, _, Line0, Line) :-
    at_end_of_stream(In),
    !,
    Line = Line0.
stream_line(In, Index, Line0, Line) :-
    skip(In, 0'\n),
    character_count(In, At),
    (   At > Index
    ->  Line = Line0
    ;   Line1 is Line0+1,
        stream_line(In, Index, Line1, Line)
    ).


                 /*******************************
                 *            FEEDBACK          *
                 *******************************/

:- initialization
    prolog_unlisten(break, onbreak),
    prolog_listen(break, onbreak).

onbreak(exist, ClauseRef, PC) :-
    known_breakpoint(ClauseRef, PC, _Location, _Cond, Id),
    !,
    break_message(breakpoint(exist, Id)).
onbreak(true, ClauseRef, PC) :-
    !,
    debug(break, 'Trap in Clause ~p, PC ~d', [ClauseRef, PC]),
    with_mutex('$break', next_break_id(Id)),
    (   nb_current('$breakpoint', Location-Cond)
    ->  true
    ;   Cond = true,
        (   break_location(ClauseRef, PC, File, A-Z)
        ->  Len is Z+1-A,
            Location = file_character_range(File, A, Len)
        ;   clause_property(ClauseRef, file(File)),
            clause_property(ClauseRef, line_count(Line))
            ->  Location = file_line(File, Line)
        ;   Location = unknown
        )
    ),
    asserta(known_breakpoint(ClauseRef, PC, Location, Cond, Id)),
    break_message(breakpoint(set, Id)).
onbreak(false, ClauseRef, PC) :-
    debug(break, 'Remove breakpoint from ~p, PC ~d', [ClauseRef, PC]),
    delete_breakpoint(ClauseRef, PC).
onbreak(retract, ClauseRef, PC) :-
    debug(break, 'Remove breakpoint from ~p, PC ~d (due to retract)',
          [ClauseRef, PC]),
    delete_breakpoint(ClauseRef, PC).

delete_breakpoint(ClauseRef, PC) :-
    clause(known_breakpoint(ClauseRef, PC, _Location, Id), true, Ref),
    call_cleanup(break_message(breakpoint(delete, Id)), erase(Ref)).

break_message(Message) :-
    print_message(informational, Message).

%!  break_location(+ClauseRef, +PC, -File, -Pos) is det.
%
%   True when File and Pos represent the  file and subterm position term
%   for the goal called at PC in ClauseRef.
%
%   @arg Pos is a subterm position  term (see `subterm_positions` option
%   of read_term/3, where positions are character positions in File. The
%   first two argument always hold the start and end of the term.

break_location(ClauseRef, PC, File, SubPos) :-
    clause_info(ClauseRef, File, TermPos, _NameOffset),
    '$fetch_vm'(ClauseRef, PC, NPC, _VMI),
    '$clause_term_position'(ClauseRef, NPC, List),
    debug(break, 'ClausePos = ~w', [List]),
    range(List, TermPos, SubPos),
    debug(break, 'Subgoal at: ~p', [SubPos]).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(breakpoint(no_source(ClauseRef, _File, Line))) -->
    [ 'Failed to find line ~d in body of clause ~p.  Breaking at start of body.'-
      [Line, ClauseRef]
    ].
prolog:message(breakpoint(SetClear, Id)) -->
    setclear(SetClear),
    breakpoint(Id).

setclear(set) -->
    ['Breakpoint '].
setclear(exist) -->
    ['Existing breakpoint '].
setclear(delete) -->
    ['Deleted breakpoint '].

breakpoint(Id) -->
    breakpoint_name(Id),
    (   { breakpoint_property(Id, file(File)),
          breakpoint_property(Id, line_count(Line))
        }
    ->  [ ' at ', url(File:Line) ]
    ;   []
    ).

breakpoint_name(Id) -->
    { breakpoint_property(Id, clause(ClauseRef)) },
    (   { clause_property(ClauseRef, erased) }
    ->  ['~w'-[Id]]
    ;   { clause_name(ClauseRef, Name) },
        ['~w in ~w'-[Id, Name]]
    ).


:- thread_local break_context_frame/1.
:- multifile prolog:break_hook/7.

prolog:break_hook(Clause, PC, Frame, _Choice, _Goal, true, Action) :-
    known_breakpoint(Clause, PC, _, Cond, _Id),
    setup_call_cleanup(asserta(break_context_frame(Frame), Ref),
                       (   Cond
                       ->  Action = trace
                       ;   Action = continue
                       ),
                       erase(Ref)).

