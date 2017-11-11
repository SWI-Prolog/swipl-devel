/*  Part of SWI-Prolog

    Author:        Leonid Mokrushin
    E-mail:        likelion@gmail.com

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

:- module(test_csv, [test_csv/0]).
:- use_module(library(plunit)).

test_csv :-
	run_tests([ csv_read_file_row ]).

:- begin_tests(csv_read_file_row, []).
:- use_module(library(csv)).

csv_secret_sauce.

csv_file(Name, File) :-
  once(predicate_property(_:csv_secret_sauce, file(Here))),
	file_directory_name(Here, Dir),
	atomic_list_concat([Dir, /, 'csv/', Name], File).

test(normal) :-
  csv_file('normal.csv', File),
	findall(Row, csv_read_file_row(File, Row, []), Rows),
  Rows = [ row(a1,a2,a3),
           row(b1,b2,b3)
         ].

test(emptyline) :-
  csv_file('emptyline.csv', File),
  findall(Row, csv_read_file_row(File, Row, []), Rows),
  Rows = [ row(a1,a2,a3),
           row(''),
           row(b1,b2,b3)
         ].

test(quoted) :-
  csv_file('quoted.csv', File),
  findall(Row, csv_read_file_row(File, Row, []), Rows),
  Rows = [ row(a1,a2,a3),
           row(b1,b2,b3),
           row('c1_1"c1_2','"c2','c3"')
         ].

test(quoted_ignored) :-
  csv_file('quoted.csv', File),
  findall(Row, csv_read_file_row(File, Row, [ignore_quotes(true)]), Rows),
  Rows = [ row(a1,'"a2"',a3),
           row(b1,b2,b3),
           row('"c1_1""c1_2"','"""c2"','"c3"""')
         ].

test(quoted_lf) :-
  csv_file('quoted_lf.csv', File),
  findall(Row, csv_read_file_row(File, Row, []), Rows),
  Rows = [ row(a1,a2,'a3_1\na3_2'),
           row(b1,b2,b3),
           row(c1,'c2_1\n\nc2_2',c3)
         ].

test(quoted_lf_ignored) :-
  csv_file('quoted_lf.csv', File),
  findall(Row, csv_read_file_row(File, Row, [ignore_quotes(true)]), Rows),
  Rows = [ row(a1,a2,'"a3_1'),
           row('a3_2"'),
           row(b1,b2,b3),
           row(c1,'"c2_1'),
           row(''),
           row('c2_2"',c3)
         ].

test(quoted_crlf) :-
  csv_file('quoted_crlf.csv', File),
  findall(Row, csv_read_file_row(File, Row, []), Rows),
  Rows = [ row(a1,a2,'a3_1\na3_2'),
           row(b1,b2,b3),
           row(c1,'c2_1\n\nc2_2',c3)
         ].

test(quoted_crlf_ignored) :-
  csv_file('quoted_crlf.csv', File),
  findall(Row, csv_read_file_row(File, Row, [ignore_quotes(true)]), Rows),
  Rows = [ row(a1,a2,'"a3_1'),
           row('a3_2"'),
           row(b1,b2,b3),
           row(c1,'"c2_1'),
           row(''),
           row('c2_2"',c3)
         ].

:- end_tests(csv_read_file_row).