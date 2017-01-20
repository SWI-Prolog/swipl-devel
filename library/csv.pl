/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2016, VU University Amsterdam
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

:- module(csv,
          [ csv//1,                     % +Rows
            csv//2,                     % +Rows, +Options
            csv_read_file/2,            % +File, -Data
            csv_read_file/3,            % +File, -Data, +Options
            csv_read_file_row/3,        % +File, -Row, +Options
            csv_write_file/2,           % +File, +Data
            csv_write_file/3,           % +File, +Data, +Options
            csv_write_stream/3          % +Stream, +Data, +Options
          ]).
:- use_module(library(record)).
:- use_module(library(error)).
:- use_module(library(pure_input)).
:- use_module(library(debug)).
:- use_module(library(option)).

/** <module> Process CSV (Comma-Separated Values) data

This library parses and generates CSV data.   CSV data is represented in
Prolog as a list of rows. Each row   is  a compound term, where all rows
have the same name and arity.

@tbd    Implement immediate assert of the data to avoid possible stack
        overflows.
@tbd    Writing creates an intermediate code-list, possibly overflowing
        resources.  This waits for pure output!
@see RFC 4180
*/

:- predicate_options(csv//2, 2,
                     [ separator(nonneg),       % mustv be code
                       strip(boolean),
                       ignore_quotes(boolean),
                       convert(boolean),
                       functor(atom),
                       arity(-nonneg),          % actually ?nonneg
                       match_arity(boolean)
                     ]).
:- predicate_options(csv_read_file/3, 3,
                     [ pass_to(csv//2, 2),
                       pass_to(phrase_from_file/3, 3)
                     ]).
:- predicate_options(csv_read_file_row/3, 3,
                     [ pass_to(csv//2, 2),
                       pass_to(open/4, 4)
                     ]).
:- predicate_options(csv_write_file/3, 3,
                     [ pass_to(csv//2, 2),
                       pass_to(open/4, 4)
                     ]).
:- predicate_options(csv_write_stream/3, 3,
                     [ pass_to(csv//2, 2)
                     ]).


:- record
    csv_options(separator:integer=0',,
                strip:boolean=false,
                ignore_quotes:boolean=false,
                convert:boolean=true,
                functor:atom=row,
                arity:integer,
                match_arity:boolean=true).


%!  csv_read_file(+File, -Rows) is det.
%!  csv_read_file(+File, -Rows, +Options) is det.
%
%   Read a CSV file into a list of   rows. Each row is a Prolog term
%   with the same arity. Options  is   handed  to  csv//2. Remaining
%   options  are  processed  by    phrase_from_file/3.  The  default
%   separator depends on the file name   extension and is =|\t|= for
%   =|.tsv|= files and =|,|= otherwise.
%
%   Suppose we want to create a predicate   table/6  from a CSV file
%   that we know contains 6 fields  per   record.  This  can be done
%   using the code below. Without the   option  arity(6), this would
%   generate a predicate table/N, where N   is  the number of fields
%   per record in the data.
%
%       ==
%       ?- csv_read_file(File, Rows, [functor(table), arity(6)]),
%          maplist(assert, Rows).
%       ==


csv_read_file(File, Rows) :-
    csv_read_file(File, Rows, []).

csv_read_file(File, Rows, Options) :-
    default_separator(File, Options, Options1),
    make_csv_options(Options1, Record, RestOptions),
    phrase_from_file(csv_roptions(Rows, Record), File, RestOptions).


default_separator(File, Options0, Options) :-
    (   option(separator(_), Options0)
    ->  Options = Options0
    ;   file_name_extension(_, Ext0, File),
        downcase_atom(Ext0, Ext),
        ext_separator(Ext, Sep)
    ->  Options = [separator(Sep)|Options0]
    ;   Options = Options0
    ).

ext_separator(csv, 0',).
ext_separator(tsv, 0'\t).


%!  csv(?Rows)// is det.
%!  csv(?Rows, +Options)// is det.
%
%   Prolog DCG to `read/write' CSV data.  Options:
%
%       * separator(+Code)
%       The comma-separator.  Must be a character code.  Default is
%       (of course) the comma. Character codes can be specified
%       using the 0' notion. E.g., using =|separator(0';)|= parses
%       a semicolon separated file.
%
%       * ignore_quotes(+Boolean)
%       If =true= (default false), threat double quotes as a normal
%       character.
%
%       * strip(+Boolean)
%       If =true= (default =false=), strip leading and trailing
%       blank space.  RFC4180 says that blank space is part of the
%       data.
%
%       * convert(+Boolean)
%       If =true= (default), use name/2 on the field data.  This
%       translates the field into a number if possible.
%
%       * functor(+Atom)
%       Functor to use for creating row terms.  Default is =row=.
%
%       * arity(?Arity)
%       Number of fields in each row.  This predicate raises
%       a domain_error(row_arity(Expected), Found) if a row is
%       found with different arity.
%
%       * match_arity(+Boolean)
%       If =false= (default =true=), do not reject CSV files where
%       lines provide a varying number of fields (columns).  This
%       can be a work-around to use some incorrect CSV files.

csv(Rows) -->
    csv(Rows, []).

csv(Rows, Options) -->
    { make_csv_options(Options, Record, _) },
    csv_roptions(Rows, Record).

csv_roptions(Rows, Record) -->
    { ground(Rows) },
    !,
    emit_csv(Rows, Record).
csv_roptions(Rows, Record) -->
    csv_data(Rows, Record).

csv_data([], _) -->
    eof,
    !.
csv_data([Row|More], Options) -->
    row(Row, Options),
    !,
    { debug(csv, 'Row: ~p', [Row]) },
    csv_data(More, Options).

eof([], []).

row(Row, Options) -->
    fields(Fields, Options),
    { csv_options_functor(Options, Functor),
      Row =.. [Functor|Fields],
      functor(Row, _, Arity),
      check_arity(Options, Arity)
    }.

check_arity(Options, Arity) :-
    csv_options_arity(Options, Arity),
    !.
check_arity(Options, _) :-
    csv_options_match_arity(Options, false),
    !.
check_arity(Options, Arity) :-
    csv_options_arity(Options, Expected),
    domain_error(row_arity(Expected), Arity).

fields([F|T], Options) -->
    field(F, Options),
    (   separator(Options)
    ->  fields(T, Options)
    ;   end_of_record
    ->  { T = [] }
    ).

field(Value, Options) -->
    "\"",
    { csv_options_ignore_quotes(Options, false) },
    !,
    string_codes(Codes),
    { make_value(Codes, Value, Options) }.
field(Value, Options) -->
    { csv_options_strip(Options, true) },
    !,
    stripped_field(Value, Options).
field(Value, Options) -->
    { csv_options_separator(Options, Sep) },
    field_codes(Codes, Sep),
    { make_value(Codes, Value, Options) }.


stripped_field(Value, Options) -->
    ws,
    (   "\"",
        { csv_options_strip(Options, false) }
    ->  string_codes(Codes),
        ws
    ;   { csv_options_separator(Options, Sep) },
        field_codes(Codes0, Sep),
        { strip_trailing_ws(Codes0, Codes) }
    ),
    { make_value(Codes, Value, Options) }.

ws --> " ", !, ws.
ws --> "\t", !, ws.
ws --> "".

strip_trailing_ws(List, Stripped) :-
    append(Stripped, WS, List),
    all_ws(WS).

all_ws([]).
all_ws([32|T]) :- all_ws(T).
all_ws([9|T]) :- all_ws(T).


%!  string_codes(-Codes)
%
%   Process a double-quotes string where  the   quote  is escaped by
%   doubling it. Eats the terminating double-quote.

string_codes(List) -->
    [H],
    (   { H == 0'" }
    ->  (   "\""
        ->  { List = [H|T] },
            string_codes(T)
        ;   { List = [] }
        )
    ;   { List = [H|T] },
        string_codes(T)
    ).

field_codes([], Sep), [Sep] --> [Sep], !.
field_codes([], _), "\n" --> "\r\n", !.
field_codes([], _), "\n" --> "\n", !.
field_codes([H|T], Sep) --> [H], !, field_codes(T, Sep).
field_codes([], _) --> [].              % unterminated last record

make_value(Codes, Value, Options) :-
    csv_options_convert(Options, true),
    !,
    name(Value, Codes).
make_value(Codes, Value, _) :-
    atom_codes(Value, Codes).

separator(Options) -->
    { csv_options_separator(Options, Sep) },
    [Sep].

end_of_record --> "\n".
end_of_record --> "\r\n".
end_of_record --> eof.                  % unterminated last record


%!  csv_read_file_row(+File, -Row, +Options) is nondet.
%
%   True when Row is a row in File.  First unifies Row with the first
%   row in File. Backtracking  yields  the   second,  ...  row.  This
%   interface  is  an  alternative  to  csv_read_file/3  that  avoids
%   loading all rows in memory.  Note   that  this interface does not
%   guarantee that all rows in File have the same arity.
%
%   In addition to the  options   of  csv_read_file/3, this predicate
%   processes the option:
%
%     * line(-Line)
%     Line is unified with the 1-based line-number from which Row is
%     read.  Note that Line is not the physical line, but rather the
%     _logical_ record number.
%
%   @tbd    Input is read line by line.  If a record separator is
%           embedded in a quoted field, parsing the record fails and
%           another line is added to the input.  This does not nicely
%           deal with other reasons why parsing the row may fail.

csv_read_file_row(File, Row, Options) :-
    default_separator(File, Options, Options1),
    make_csv_options(Options1, RecordOptions, Options2),
    select_option(line(Line), Options2, RestOptions, _),
    setup_call_cleanup(
        open(File, read, Stream, RestOptions),
        csv_read_stream_row(Stream, Row, Line, RecordOptions),
        close(Stream)).

csv_read_stream_row(Stream, _Row, _Line, _Options) :-
    at_end_of_stream(Stream),
    !,
    fail.
csv_read_stream_row(Stream, Row, Line, Options) :-
    between(1, infinite, Line),
    read_row(Stream, Row, Options),
    (   at_end_of_stream(Stream)            % make reading the last row
    ->  !                                   % deterministic.
    ;   true
    ).

read_row(Stream, Row, Options) :-
    read_lines_to_codes(Stream, Codes),
    phrase(row(Row0, Options), Codes),
    !,
    Row = Row0.

read_lines_to_codes(Stream, Codes) :-
    read_line_to_codes(Stream, Codes, Tail),
    (   Tail == []
    ->  true
    ;   Tail = []
    ;   read_lines_to_codes(Stream, Tail)
    ).


                /*******************************
                *             OUTPUT           *
                *******************************/

%!  csv_write_file(+File, +Data) is det.
%!  csv_write_file(+File, +Data, +Options) is det.
%
%   Write a list of Prolog terms to a CSV file.  Options are given
%   to csv//2.  Remaining options are given to open/4.  The  default
%   separator depends on the file name   extension and is =|\t|= for
%   =|.tsv|= files and =|,|= otherwise.

csv_write_file(File, Data) :-
    csv_write_file(File, Data, []).

csv_write_file(File, Data, Options) :-
    must_be(list, Data),
    default_separator(File, Options, Options1),
    make_csv_options(Options1, Record, RestOptions),
    phrase(emit_csv(Data, Record), String),
    setup_call_cleanup(
        open(File, write, Out, RestOptions),
        format(Out, '~s', [String]),
        close(Out)).


emit_csv([], _) --> [].
emit_csv([H|T], Options) -->
    emit_row(H, Options), "\r\n",   % RFC 4180 demands \r\n
    emit_csv(T, Options).

emit_row(Row, Options) -->
    { Row =.. [_|Fields] },
    emit_fields(Fields, Options).

emit_fields([H|T], Options) -->
    emit_field(H, Options),
    (   { T == [] }
        ->  []
        ;   { csv_options_separator(Options, Sep) },
        [Sep],
        emit_fields(T, Options)
    ).

emit_field(H, Options) -->
    { (   atom(H)
      ->  atom_codes(H, Codes)
      ;   string(H)
      ->  string_codes(H, Codes)
      )
    },
    !,
    (   { needs_quotes(H, Options) }
    ->  "\"", emit_string(Codes), "\""
    ;   emit_codes(Codes)
    ).
emit_field([], _) -->
    !,
    { atom_codes('[]', Codes) },
    emit_codes(Codes).
emit_field(H, _) -->
    { number_codes(H,Codes) },
    emit_codes(Codes).

needs_quotes(Atom, _) :-
    sub_atom(Atom, _, _, _, '"'),
    !.
needs_quotes(Atom, _) :-
    sub_atom(Atom, _, _, _, '\n'),
    !.
needs_quotes(Atom, _) :-
    sub_atom(Atom, _, _, _, '\r'),
    !.
needs_quotes(Atom, Options) :-
    csv_options_separator(Options, Sep),
    char_code(Char, Sep),
    sub_atom(Atom, _, _, _, Char),
    !.

emit_string([]) --> "".
emit_string([0'"|T]) --> !, "\"\"", emit_string(T).
emit_string([H|T]) --> [H], emit_string(T).

emit_codes([]) --> "".
emit_codes([0'"|T]) --> !, "\"\"", emit_codes(T).
emit_codes([H|T]) --> [H], emit_codes(T).


%%     csv_write_stream(+Stream, +Data, +Options) is det.
%
%      Write  the  rows  in  Data  to    Stream.   This  is  similar  to
%      csv_write_file/3,  but  can  deal  with  data  that  is  produced
%      incrementally. The example  below  saves   all  answers  from the
%      predicate data/3 to File.
%
%        ==
%        save_data(File) :-
%           setup_call_cleanup(
%               open(File, write, Out),
%               forall(data(C1,C2,C3),
%                      csv_write_stream(Out, [row(C1,C2,C3)], [])),
%               close(Out)),
%        ==

csv_write_stream(Stream, Data, Options) :-
    must_be(list, Data),
    make_csv_options(Options, Record, _),
    phrase(emit_csv(Data, Record), String),
    format(Stream, '~s', [String]).
