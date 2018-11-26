/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2015, University of Amsterdam
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

:- module(quintus,
        [ unix/1,
%         file_exists/1,

          abs/2,
          sin/2,
          cos/2,
          tan/2,
          log/2,
          log10/2,
          pow/3,
          ceiling/2,
          floor/2,
          round/2,
          acos/2,
          asin/2,
          atan/2,
          atan2/3,
          sign/2,
          sqrt/2,

          genarg/3,

          (mode)/1,
          no_style_check/1,
          otherwise/0,
          simple/1,
%         statistics/2,                 % Please access as quintus:statistics/2
          prolog_flag/2,

          date/1,                       % -date(Year, Month, Day)

          current_stream/3,             % ?File, ?Mode, ?Stream
          stream_position/3,            % +Stream, -Old, +New
          skip_line/0,
          skip_line/1,                  % +Stream

          compile/1,                    % +File(s)

          atom_char/2,
          midstring/3,                  % ABC, B, AC
          midstring/4,                  % ABC, B, AC, LenA
          midstring/5,                  % ABC, B, AC, LenA, LenB
          midstring/6,                  % ABC, B, AC, LenA, LenB, LenC

          raise_exception/1,            % +Exception
          on_exception/3                % +Ball, :Goal, :Recover
        ]).
:- use_module(library(lists), [member/2]).

/** <module> Quintus compatibility

This  module  defines  several  predicates    from  the  Quintus  Prolog
libraries. Note that our library structure is totally different. If this
library were complete, Prolog  code  could   be  ported  by removing the
use_module/1 declarations, relying on the SWI-Prolog autoloader.

Bluffers guide to porting:

        * Remove =|use_module(library(...))|=
        * Run =|?- list_undefined.|=
        * Fix problems

Of course, this library is incomplete ...
*/

                /********************************
                *      SYSTEM INTERACTION       *
                *********************************/

%!  unix(+Action)
%   interface to  Unix.

unix(system(Command)) :-
    shell(Command).
unix(shell(Command)) :-
    shell(Command).
unix(shell) :-
    shell.
unix(access(File, 0)) :-
    access_file(File, read).
unix(cd) :-
    expand_file_name(~, [Home]),
    working_directory(_, Home).
unix(cd(Dir)) :-
    working_directory(_, Dir).
unix(args(L)) :-
    current_prolog_flag(os_argv, L).
unix(argv(L)) :-
    current_prolog_flag(os_argv, S),
    maplist(to_prolog, S, L).

to_prolog(S, A) :-
    name(S, L),
    name(A, L).


                /********************************
                *        META PREDICATES        *
                *********************************/

%!  otherwise
%
%   For (A -> B ; otherwise -> C)

otherwise.


                /********************************
                *          ARITHMETIC           *
                *********************************/

%!  abs(+Number, -Absolute)
%   Unify `Absolute' with the absolute value of `Number'.

abs(Number, Absolute) :-
    Absolute is abs(Number).

%!  sin(+Angle, -Sine) is det.
%!  cos(+Angle, -Cosine) is det.
%!  tan(+Angle, -Tangent) is det.
%!  log(+X, -NatLog) is det.
%!  log10(+X, -Log) is det.
%!  pow(+X, +Y, -Pow) is det.
%!  ceiling(+X, -Value) is det.
%!  floor(+X, -Value) is det.
%!  round(+X, -Value) is det.
%!  sqrt(+X, -Value) is det.
%!  acos(+X, -Value) is det.
%!  asin(+X, -Value) is det.
%!  atan(+X, -Value) is det.
%!  atan2(+Y, +X, -Value) is det.
%!  sign(+X, -Value) is det.
%
%   Math library predicates. SWI-Prolog (and   ISO) support these as
%   functions under is/2, etc.
%
%   @compat Quintus Prolog.
%   @deprecated Do not use these predicates except for compatibility
%   reasons.

sin(A, V) :-      V is sin(A).
cos(A, V) :-      V is cos(A).
tan(A, V) :-      V is tan(A).
log(A, V) :-      V is log(A).
log10(X, V) :-    V is log10(X).
pow(X,Y,V) :-     V is X**Y.
ceiling(X, V) :-  V is ceil(X).
floor(X, V) :-    V is floor(X).
round(X, V) :-    V is round(X).
sqrt(X, V) :-     V is sqrt(X).
acos(X, V) :-     V is acos(X).
asin(X, V) :-     V is asin(X).
atan(X, V) :-     V is atan(X).
atan2(Y, X, V) :- V is atan(Y, X).
sign(X, V) :-     V is sign(X).


                 /*******************************
                 *      TERM MANIPULATION       *
                 *******************************/

%!  genarg(?Index, +Term, ?Arg) is nondet.
%
%   Generalised version of ISO arg/3.  SWI-Prolog's arg/3 is already
%   genarg/3.

genarg(N, T, A) :-
    arg(N, T, A).


                 /*******************************
                 *            FLAGS             *
                 *******************************/

%!  prolog_flag(?Flag, ?Value) is nondet.
%
%   Same as ISO current_prolog_flag/2.  Maps =version=.
%
%   @bug    Should map relevant Quintus flag identifiers.

prolog_flag(version, Version) :-
    !,
    current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
    current_prolog_flag(arch, Arch),
    current_prolog_flag(compiled_at, Compiled),
    atomic_list_concat(['SWI-Prolog ',
                 Major, '.', Minor, '.', Patch,
                 ' (', Arch, '): ', Compiled], Version).
prolog_flag(Flag, Value) :-
    current_prolog_flag(Flag, Value).


                 /*******************************
                 *          STATISTICS          *
                 *******************************/

%       Here used to be a definition of Quintus statistics/2 in traditional
%       SWI-Prolog statistics/2.  The current built-in emulates Quintus
%       almost completely.


                 /*******************************
                 *           DATE/TIME          *
                 *******************************/

%!  date(-Date) is det.
%
%   Get current date as date(Y,M,D)

date(Date) :-
    get_time(T),
    stamp_date_time(T, DaTime, local),
    date_time_value(date, DaTime, Date).


                /********************************
                *          STYLE CHECK          *
                *********************************/

%!  no_style_check(Style) is det.
%
%   Same as SWI-Prolog =|style_check(-Style)|=.   The Quintus option
%   =single_var= is mapped to =singleton=.
%
%   @see style_check/1.

q_style_option(single_var, singleton) :- !.
q_style_option(Option, Option).

no_style_check(QOption) :-
    q_style_option(QOption, SWIOption),
    style_check(-SWIOption).


                /********************************
                *         DIRECTIVES            *
                *********************************/

%!  mode(+ModeDecl) is det.
%
%   Ignore a DEC10/Quintus `:-   mode(Head)`  declaration. Typically
%   these declarations are written in   operator  form. The operator
%   declaration is not part of the   Quintus  emulation library. The
%   following declaration is compatible with Quintus:
%
%     ==
%     :- op(1150, fx, [(mode)]).
%     ==

mode(_).


                 /*******************************
                 *            TYPES             *
                 *******************************/

%!  simple(@Term) is semidet.
%
%   Term is atomic or a variable.

simple(X) :-
    (   atomic(X)
    ->  true
    ;   var(X)
    ).


                 /*******************************
                 *            STREAMS           *
                 *******************************/

%!  current_stream(?Object, ?Mode, ?Stream)
%
%   SICStus/Quintus and backward compatible predicate.  New code should
%   be using the ISO compatible stream_property/2.

current_stream(Object, Mode, Stream) :-
    stream_property(Stream, mode(FullMode)),
    stream_mode(FullMode, Mode),
    (   stream_property(Stream, file_name(Object0))
    ->  true
    ;   stream_property(Stream, file_no(Object0))
    ->  true
    ;   Object0 = []
    ),
    Object = Object0.

stream_mode(read,   read).
stream_mode(write,  write).
stream_mode(append, write).
stream_mode(update, write).

%!  stream_position(+Stream, -Old, +New)
%
%   True when Old is the current position   in Stream and the stream
%   has been repositioned to New.
%
%   @deprecated New code should use the ISO predicates
%   stream_property/2 and set_stream_position/2.

stream_position(Stream, Old, New) :-
    stream_property(Stream, position(Old)),
    set_stream_position(Stream, New).


%!  skip_line is det.
%!  skip_line(Stream) is det.
%
%   Skip  the  rest  of  the  current  line  (on  Stream).  Same  as
%   =|skip(0'\n)|=.

skip_line :-
    skip(10).
skip_line(Stream) :-
    skip(Stream, 10).


                 /*******************************
                 *         COMPILATION          *
                 *******************************/

%!  compile(+Files) is det.
%
%   Compile   files.   SWI-Prolog   doesn't    distinguish   between
%   compilation and consult.
%
%   @see load_files/2.

:- meta_predicate
    compile(:).

compile(Files) :-
    consult(Files).

                 /*******************************
                 *         ATOM-HANDLING        *
                 *******************************/

%!  atom_char(+Char, -Code) is det.
%!  atom_char(-Char, +Code) is det.
%
%   Same as ISO char_code/2.

atom_char(Char, Code) :-
    char_code(Char, Code).

%!  midstring(?ABC, ?B, ?AC) is nondet.
%!  midstring(?ABC, ?B, ?AC, LenA) is nondet.
%!  midstring(?ABC, ?B, ?AC, LenA, LenB) is nondet.
%!  midstring(?ABC, ?B, ?AC, LenA, LenB, LenC) is nondet.
%
%   Too difficult to explain.  See the Quintus docs.  As far as I
%   understand them the code below emulates this function just fine.

midstring(ABC, B, AC) :-
    midstring(ABC, B, AC, _, _, _).
midstring(ABC, B, AC, LenA) :-
    midstring(ABC, B, AC, LenA, _, _).
midstring(ABC, B, AC, LenA, LenB) :-
    midstring(ABC, B, AC, LenA, LenB, _).
midstring(ABC, B, AC, LenA, LenB, LenC) :-      % -ABC, +B, +AC
    var(ABC),
    !,
    atom_length(AC, LenAC),
    (   nonvar(LenA) ; nonvar(LenC)
    ->  plus(LenA, LenC, LenAC)
    ;   true
    ),
    sub_atom(AC, 0, LenA, _, A),
    LenC is LenAC - LenA,
    sub_atom(AC, _, LenC, 0, C),
    atom_length(B, LenB),
    atomic_list_concat([A,B,C], ABC).
midstring(ABC, B, AC, LenA, LenB, LenC) :-
    sub_atom(ABC, LenA, LenB, LenC, B),
    sub_atom(ABC, 0, LenA, _, A),
    sub_atom(ABC, _, LenC, 0, C),
    atom_concat(A, C, AC).


                 /*******************************
                 *           EXCEPTIONS         *
                 *******************************/

%!  raise_exception(+Term)
%
%   Quintus compatible exception handling

raise_exception(Term) :-
    throw(Term).

%!  on_exception(+Template, :Goal, :Recover)

:- meta_predicate
    on_exception(+, 0, 0).

on_exception(Except, Goal, Recover) :-
    catch(Goal, Except, Recover).
