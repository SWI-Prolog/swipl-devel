/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2018, VU University Amsterdam
                              CWI, Amsterdam
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
            csv_read_stream/3,          % +Stream, -Data, +Options

            csv_read_file_row/3,        % +File, -Row, +Options
            csv_read_row/3,		% +Stream, -Row, +CompiledOptions
            csv_options/2,		% -Compiled, +Options

            csv_write_file/2,           % +File, +Data
            csv_write_file/3,           % +File, +Data, +Options
            csv_write_stream/3          % +Stream, +Data, +Options
          ]).
:- use_module(library(record)).
:- use_module(library(error)).
:- use_module(library(pure_input)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

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
                       case(oneof([down,preserve,up])),
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
                case:oneof([down,preserve,up])=preserve,
                functor:atom=row,
                arity:integer,
                match_arity:boolean=true,
                skip_header:atom).


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


%!  csv_read_stream(+Stream, -Rows, +Options) is det.
%
%   Read CSV data from Stream.  See also csv_read_row/3.

csv_read_stream(Stream, Rows, Options) :-
    make_csv_options(Options, Record, _),
    phrase_from_stream(csv_roptions(Rows, Record), Stream).


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
%       * skip_header(+CommentLead)
%       Skip leading lines that start with CommentLead.  There is
%       no standard for comments in CSV files, but some CSV files
%       have a header where each line starts with `#`.  After
%       skipping comment lines this option causes csv//2 to skip empty
%       lines.  Note that an empty line may not contain white space
%       characters (space or tab) as these may provide valid data.
%
%       * convert(+Boolean)
%       If =true= (default), use name/2 on the field data.  This
%       translates the field into a number if possible.
%
%       * case(+Action)
%       If =down=, downcase atomic values.  If =up=, upcase them
%       and if =preserve= (default), do not change the case.
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
    skip_header(Record),
    csv_data(Rows, Record).

skip_header(Options) -->
    { csv_options_skip_header(Options, CommentStart),
      nonvar(CommentStart),
      atom_codes(CommentStart, Codes)
    },
    !,
    skip_header_lines(Codes),
    skip_blank_lines.
skip_header(_) -->
    [].

skip_header_lines(CommentStart) -->
    string(CommentStart),
    !,
    (   string(_Comment),
        end_of_record
    ->  skip_header_lines(CommentStart)
    ).
skip_header_lines(_) -->
    [].

skip_blank_lines -->
    eos,
    !.
skip_blank_lines -->
    end_of_record,
    !,
    skip_blank_lines.
skip_blank_lines -->
    [].

csv_data([], _) -->
    eos,
    !.
csv_data([Row|More], Options) -->
    row(Row, Options),
    !,
    { debug(csv, 'Row: ~p', [Row]) },
    csv_data(More, Options).


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
field_codes([], _), "\n" --> "\r", !.
field_codes([H|T], Sep) --> [H], !, field_codes(T, Sep).
field_codes([], _) --> [].              % unterminated last record

%!  make_value(+Codes, -Value, +Options) is det.
%
%   Convert a list of character codes to the actual value, depending
%   on Options.

make_value(Codes, Value, Options) :-
    csv_options_convert(Options, Convert),
    csv_options_case(Options, Case),
    make_value(Convert, Case, Codes, Value).

make_value(true, preserve, Codes, Value) :-
    !,
    name(Value, Codes).
make_value(true, Case, Codes, Value) :-
    !,
    (   number_string(Value, Codes)
    ->  true
    ;   make_value(false, Case, Codes, Value)
    ).
make_value(false, preserve, Codes, Value) :-
    !,
    atom_codes(Value, Codes).
make_value(false, down, Codes, Value) :-
    !,
    string_codes(String, Codes),
    downcase_atom(String, Value).
make_value(false, up, Codes, Value) :-
    string_codes(String, Codes),
    upcase_atom(String, Value).

separator(Options) -->
    { csv_options_separator(Options, Sep) },
    [Sep].

end_of_record --> "\n".			% Unix files
end_of_record --> "\r\n".               % DOS files
end_of_record --> "\r".                 % MacOS files
end_of_record --> eos.                  % unterminated last record


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

csv_read_stream_row(Stream, Row, Line, Options) :-
    between(1, infinite, Line),
    (   csv_read_row(Stream, Row0, Options),
        Row0 \== end_of_file
    ->  Row = Row0
    ;   !,
        fail
    ).


%!  csv_read_row(+Stream, -Row, +CompiledOptions) is det.
%
%   Read the next CSV record from Stream  and unify the result with Row.
%   CompiledOptions is created from  options   defined  for csv//2 using
%   csv_options/2. Row is unified with   `end_of_file` upon reaching the
%   end of the input.

csv_read_row(Stream, Row, _Record) :-
    at_end_of_stream(Stream),
    !,
    Row = end_of_file.
csv_read_row(Stream, Row, Record) :-
    read_lines_to_codes(Stream, Codes, Record, even),
    phrase(row(Row0, Record), Codes),
    !,
    Row = Row0.

read_lines_to_codes(Stream, Codes, Options, QuoteQuantity) :-
    read_line_to_codes(Stream, Codes0),
    Codes0 \== end_of_file,
    (   (   csv_options_ignore_quotes(Options, true)
        ;   check_quotes(Codes0, QuoteQuantity, even)
        )
    ->  Codes = Codes0
    ;   append(Codes0, [0'\n|Tail], Codes),
        read_lines_to_codes(Stream, Tail, Options, odd)
    ).

check_quotes([], QuoteQuantity, QuoteQuantity) :-
    !.
check_quotes([0'"|T], odd, Result) :-
    !,
    check_quotes(T, even, Result).
check_quotes([0'"|T], even, Result) :-
    !,
    check_quotes(T, odd, Result).
check_quotes([_|T], QuoteQuantity, Result) :-
    check_quotes(T, QuoteQuantity, Result).


%!  csv_options(-Compiled, +Options) is det.
%
%   Compiled is the  compiled  representation   of  the  CSV  processing
%   options as they may be passed into   csv//2,  etc. This predicate is
%   used in combination with csv_read_row/3 to avoid repeated processing
%   of the options.

csv_options(Compiled, Options) :-
    make_csv_options(Options, Compiled, _Ignored).


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
    make_csv_options(Options1, OptionsRecord, RestOptions),
    setup_call_cleanup(
        open(File, write, Out, RestOptions),
        maplist(csv_write_row(Out, OptionsRecord), Data),
        close(Out)).

csv_write_row(Out, OptionsRecord, Row) :-
    phrase(emit_row(Row, OptionsRecord), String),
    format(Out, '~s', [String]).

emit_csv([], _) --> [].
emit_csv([H|T], Options) -->
    emit_row(H, Options),
    emit_csv(T, Options).

emit_row(Row, Options) -->
    { Row =.. [_|Fields] },
    emit_fields(Fields, Options),
    "\r\n".                                     % RFC 4180 demands \r\n

emit_fields([], _) -->
    "".
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
    make_csv_options(Options, OptionsRecord, _),
    maplist(csv_write_row(Stream, OptionsRecord), Data).
