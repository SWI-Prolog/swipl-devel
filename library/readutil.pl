/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2013, University of Amsterdam
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

:- module(read_util,
          [ read_line_to_codes/2,       % +Stream, -Codes (without trailing \n)
            read_line_to_codes/3,       % +Stream, -Codes, ?Tail
            read_stream_to_codes/2,     % +Stream, -Codes
            read_stream_to_codes/3,     % +Stream, -Codes, ?Tail
            read_file_to_codes/3,       % +File, -Codes, +Options

            read_line_to_string/2,      % +Stream, -Line (without trailing \n)
            read_file_to_string/3,      % +File, -Codes, +Options

            read_file_to_terms/3        % +File, -Terms, +Options
          ]).
:- use_module(library(shlib)).
:- use_module(library(error)).
:- use_module(library(option)).

/** <module> Read utilities

This library provides some commonly used   reading  predicates. As these
predicates have proven to be time-critical in some applications we moved
them to C. For compatibility as well  as to reduce system dependency, we
link  the  foreign  code  at  runtime    and   fallback  to  the  Prolog
implementation if the shared object cannot be found.

@see library(pure_input) allows for processing files with DCGs.
*/

:- predicate_options(read_file_to_codes/3, 3,
                     [ tail(list_or_partial_list),
                       pass_to(system:open/4, 4)
                     ]).
:- predicate_options(read_file_to_string/3, 3,
                     [ pass_to(system:open/4, 4)
                     ]).
:- predicate_options(read_file_to_terms/3, 3,
                     [ tail(list_or_partial_list),
                       pass_to(read_stream_to_terms/4, 4),
                       pass_to(system:absolute_file_name/3, 3),
                       pass_to(system:open/4, 4)
                     ]).

:- volatile
    read_line_to_codes/2,
    read_line_to_codes/3,
    read_stream_to_codes/2,
    read_stream_to_codes/3.

link_foreign :-
    catch(load_foreign_library(foreign(readutil)), _, fail),
    !.
link_foreign :-
    assertz((read_line_to_codes(Stream, Line) :-
            pl_read_line_to_codes(Stream, Line))),
    assertz((read_line_to_codes(Stream, Line, Tail) :-
            pl_read_line_to_codes(Stream, Line, Tail))),
    assertz((read_stream_to_codes(Stream, Content) :-
            pl_read_stream_to_codes(Stream, Content))),
    assertz((read_stream_to_codes(Stream, Content, Tail) :-
            pl_read_stream_to_codes(Stream, Content, Tail))),
    compile_predicates([ read_line_to_codes/2,
                         read_line_to_codes/3,
                         read_stream_to_codes/2,
                         read_stream_to_codes/3
                       ]).

:- initialization(link_foreign, now).


                 /*******************************
                 *             LINES            *
                 *******************************/

%!  read_line_to_codes(+In:stream, -Line:codes) is det.
%
%   Read a line of input from  In   into  a list of character codes.
%   Trailing newline and  or  return   are  deleted.  Upon  reaching
%   end-of-file Line is unified to the atom =end_of_file=.

pl_read_line_to_codes(Stream, Codes) :-
    get_code(Stream, C0),
    (   C0 == -1
    ->  Codes0 = end_of_file
    ;   read_1line_to_codes(C0, Stream, Codes0)
    ),
    Codes = Codes0.

read_1line_to_codes(-1, _, []) :- !.
read_1line_to_codes(10, _, []) :- !.
read_1line_to_codes(13, Stream, L) :-
    !,
    get_code(Stream, C2),
    read_1line_to_codes(C2, Stream, L).
read_1line_to_codes(C, Stream, [C|T]) :-
    get_code(Stream, C2),
    read_1line_to_codes(C2, Stream, T).

%!  read_line_to_codes(+Stream, -Line, ?Tail) is det.
%
%   Read a line of input as a   difference list. This should be used
%   to read multiple lines  efficiently.   On  reaching end-of-file,
%   Tail is bound to the empty list.

pl_read_line_to_codes(Stream, Codes, Tail) :-
    get_code(Stream, C0),
    read_line_to_codes(C0, Stream, Codes0, Tail),
    Codes = Codes0.

read_line_to_codes(-1, _, Tail, Tail) :-
    !,
    Tail = [].
read_line_to_codes(10, _, [10|Tail], Tail) :- !.
read_line_to_codes(C, Stream, [C|T], Tail) :-
    get_code(Stream, C2),
    read_line_to_codes(C2, Stream, T, Tail).


%!  read_line_to_string(+Stream, -String) is det.
%
%   Read the next line from  Stream   into  String.  String does not
%   contain the line terminator. String is   unified with the _atom_
%   end_of_file if the end of the file is reached.
%
%   @see    read_string/5 can be used to read lines with separated
%           records without creating intermediate strings.

read_line_to_string(Stream, String) :-
    read_string(Stream, '\n', '\r', Sep, String0),
    (   Sep \== -1
    ->  String = String0
    ;   String0 == ""
    ->  String = end_of_file
    ;   String = String0
    ).


                 /*******************************
                 *     STREAM (ENTIRE INPUT)    *
                 *******************************/

%!  read_stream_to_codes(+Stream, -Codes) is det.
%!  read_stream_to_codes(+Stream, -Codes, ?Tail) is det.
%
%   Read input from Stream to a list of character codes. The version
%   read_stream_to_codes/3 creates a difference-list.

pl_read_stream_to_codes(Stream, Codes) :-
    pl_read_stream_to_codes(Stream, Codes, []).
pl_read_stream_to_codes(Stream, Codes, Tail) :-
    get_code(Stream, C0),
    read_stream_to_codes(C0, Stream, Codes0, Tail),
    Codes = Codes0.

read_stream_to_codes(-1, _, Tail, Tail) :- !.
read_stream_to_codes(C, Stream, [C|T], Tail) :-
    get_code(Stream, C2),
    read_stream_to_codes(C2, Stream, T, Tail).


%!  read_stream_to_terms(+Stream, -Terms, ?Tail, +Options) is det.

read_stream_to_terms(Stream, Terms, Tail, Options) :-
    read_term(Stream, C0, Options),
    read_stream_to_terms(C0, Stream, Terms0, Tail, Options),
    Terms = Terms0.

read_stream_to_terms(end_of_file, _, Tail, Tail, _) :- !.
read_stream_to_terms(C, Stream, [C|T], Tail, Options) :-
    read_term(Stream, C2, Options),
    read_stream_to_terms(C2, Stream, T, Tail, Options).


                 /*******************************
                 *      FILE (ENTIRE INPUT)     *
                 *******************************/

%!  read_file_to_codes(+Spec, -Codes, +Options) is det.
%
%   Read the file Spec into a list   of Codes. Options is split into
%   options for absolute_file_name/3 and open/4.   In  addition, the
%   following option is provided:
%
%     * tail(?Tail)
%     Read the data into a _difference list_ Codes\Tail.
%
%   @see phrase_from_file/3 and read_file_to_string/3.

read_file_to_codes(Spec, Codes, Options) :-
    must_be(list, Options),
    option(tail(Tail), Options, []),
    absolute_file_name(Spec,
                       [ access(read)
                       | Options
                       ],
                       Path),
    setup_call_cleanup(
        open(Path, read, Stream, Options),
        read_stream_to_codes(Stream, Codes, Tail),
        close(Stream)).

%!  read_file_to_string(+Spec, -String, +Options) is det.
%
%   Read the file Spec into a the   string  String. Options is split
%   into options for absolute_file_name/3 and open/4.
%
%   @see phrase_from_file/3 and read_file_to_codes/3.

read_file_to_string(Spec, Codes, Options) :-
    must_be(list, Options),
    absolute_file_name(Spec,
                       [ access(read)
                       | Options
                       ],
                       Path),
    setup_call_cleanup(
        open(Path, read, Stream, Options),
        read_string(Stream, _Len, Codes),
        close(Stream)).

%!  read_file_to_terms(+Spec, -Terms, +Options) is det.
%
%   Read the file Spec into a list   of terms. Options is split over
%   absolute_file_name/3, open/4 and  read_term/3.  In addition, the
%   following option is processed:
%
%     * tail(?Tail)
%     If present, Terms\Tail forms a _difference list_.
%
%   Note  that  the  `output'  options    of  read_term/3,  such  as
%   =variable_names=    or    =subterm_positions=      will    cause
%   read_file_to_terms/3 to fail if  Spec   contains  multiple terms
%   because the values for the different terms will not unify.

read_file_to_terms(Spec, Terms, Options) :-
    must_be(list, Options),
    option(tail(Tail), Options, []),
    absolute_file_name(Spec,
                       [ access(read)
                       | Options
                       ],
                       Path),
    setup_call_cleanup(
        open(Path, read, Stream, Options),
        read_stream_to_terms(Stream, Terms, Tail, Options),
        close(Stream)).
