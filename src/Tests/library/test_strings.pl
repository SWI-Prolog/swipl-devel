/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v.
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

:- module(test_strings,
          [ test_strings/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(strings)).

test_strings :-
    run_tests([strings]).

:- begin_tests(strings).

test(plain, A == "hello world") :-
    A = {|string(To)||hello world|}.
test(interpolate, A == "hello world") :-
    To = world,
    A = {|string(To)||hello {To}|}.
test(interpolate, error(existence_error(template_var, 'ToX'))) :-
    To = world,
    _ = {|string(To)||hello {ToX}|}.
test(interpolate, A == "hello world") :-
    To = no_world,
    A = {|string(To)||hello {ToX,world}|}.
test(dedent, A == "hello\n  world\n") :-
    A = {|string||
	 | hello
         |   world
         |}.
test(dedent, A == "hello\n  world\n") :-
    A = {|string||
	 hello
           world
         |}.
test(dedent, A == "hello\n  world") :-
    A = {|string||
	 | hello
         |   world|}.

% Tests adapted from https://github.com/python/cpython/blob/master/Lib/test/string_tests.py

% These tests were made by changed "\r\n" to "\n" and other "\r"s to "\n".
% Tests with keepends=True were removed.
test(splitlines, Lines == ["abc", "def", "", "ghi"]) :- string_lines("abc\ndef\n\nghi", Lines).
test(splitlines, Lines == ["abc", "def", "", "ghi"]) :- string_lines("abc\ndef\n\nghi", Lines).
test(splitlines, Lines == ["abc", "def", "ghi"]) :- string_lines("abc\ndef\nghi", Lines).
test(splitlines, Lines == ["abc", "def", "ghi"]) :- string_lines("abc\ndef\nghi\n", Lines).
% Removed "\r" tests

% The tests for string_lines/2 cover the three cases of String and Line
% being instantiated (both String and Line uninstantiated is an error).
test(string_lines) :- string_lines("ab",     ["ab"]).
test(string_lines) :- string_lines("ab\n",   ["ab"]).
test(string_lines) :- string_lines("ab\n",   ["ab", ""]).
test(string_lines) :- string_lines("ab\n\n", ["ab", ""]).
test(string_lines) :- string_lines("ab\n\n", ["ab", "", ""]).
test(string_lines) :- string_lines("\nab",   ["", "ab"]).
test(string_lines) :- string_lines("\nab\n", ["", "ab"]).
test(string_lines) :- string_lines("\nab\n", ["", "ab", ""]).
test(string_lines, Lines == ["ab"])     :- string_lines("ab",     Lines).
test(string_lines, Lines == ["ab"])     :- string_lines("ab\n",   Lines).
test(string_lines, Lines == ["ab", ""]) :- string_lines("ab\n\n", Lines).
test(string_lines, Lines == ["", "ab"]) :- string_lines("\nab",   Lines).
test(string_lines, Lines == ["", "ab"]) :- string_lines("\nab\n", Lines).
test(string_lines, String == "ab\n")   :- string_lines(String, ["ab"]).
test(string_lines, String == "ab\n\n")   :- string_lines(String, ["ab", ""]).
test(string_lines, String == "ab\n\n\n") :- string_lines(String, ["ab", "", ""]).
test(string_lines, String == "\nab\n")   :- string_lines(String, ["", "ab"]).
test(string_lines, String == "\nab\n\n") :- string_lines(String, ["", "ab", ""]).
test(string_lines, String == "\nab\n3\n\n") :- string_lines(String, ['', "ab", 3, '']).


% Tests adapted from https://github.com/python/cpython/blob/master/Lib/test/test_textwrap.py

dedent_lines_default(In, Out) :-
    dedent_lines(In, Out, []).

% Test dedent_lines

test(dedent_nomargin, Text == Dedent) :- % No lines indented.
    Text = "Hello there.\nHow are you?\nOh good, I'm glad.",
    dedent_lines_default(Text, Dedent).

test(dedent_nomargin, Text == Dedent) :- % Similar, with a blank line.
    Text = "Hello there.\n\nBoo!",
    dedent_lines_default(Text, Dedent).

test(dedent_nomargin, Text == Dedent) :- % Some lines indented, but overall margin is still zero.
    Text = "Hello there.\n  This is indented.",
    dedent_lines_default(Text, Dedent).

test(dedent_nomargin, Text == Dedent) :- % Again, add a blank line.
    Text = "Hello there.\n\n  Boo!\n",
    dedent_lines_default(Text, Dedent).

test(dedent_even, Dedent == Expect) :- % All lines indented by two spaces.
    Text = "  Hello there.\n  How are ya?\n  Oh good.",
    Expect = "Hello there.\nHow are ya?\nOh good.",
    dedent_lines_default(Text, Dedent).
test(dedent_even, Dedent == Expect) :- % Same, with blank lines.
    Text = "  Hello there.\n\n  How are ya?\n  Oh good.\n",
    Expect = "Hello there.\n\nHow are ya?\nOh good.\n",
    dedent_lines_default(Text, Dedent).
test(dedent_even, Dedent == Expect) :- % Now indent one of the blank lines.
    Text = "  Hello there.\n  \n  How are ya?\n  Oh good.\n",
    Expect = "Hello there.\n\nHow are ya?\nOh good.\n",
    dedent_lines_default(Text, Dedent).

test(dedent_uneven, Dedent == Expect) :- % Lines indented unevenly.
        Text = "
        def foo():
            while 1:
                return foo
        ",
        Expect = "
def foo():
    while 1:
        return foo
",
        dedent_lines_default(Text, Dedent).
test(dedent_uneven, Dedent == Expect) :- % Uneven indentation with a blank line.
    Text = "  Foo\n    Bar\n\n   Baz\n",
    Expect = "Foo\n  Bar\n\n Baz\n",
    dedent_lines_default(Text, Dedent).
test(dedent_uneven,  Dedent == Expect) :- % Uneven indentation with a whitespace-only line.
    Text = "  Foo\n    Bar\n \n   Baz\n",
    Expect = "Foo\n  Bar\n\n Baz\n",
    dedent_lines_default(Text, Dedent).

test(dedent_declining, Dedent == Expect) :- % Uneven indentation with declining indent level.
    Text = "     Foo\n    Bar\n",  % 5 spaces, then 4
    Expect = " Foo\nBar\n",
    dedent_lines_default(Text, Dedent).
test(dedent_declining, Dedent == Expect) :-   %  Declining indent level with blank line.
    Text = "     Foo\n\n    Bar\n",  % 5 spaces, blank, then 4
    Expect = " Foo\n\nBar\n",
    dedent_lines_default(Text, Dedent).
test(dedent_declining, Dedent == Expect) :- %  Declining indent level with whitespace only line.
    Text = "     Foo\n    \n    Bar\n", % 5 spaces, then 4, then 4
    Expect = " Foo\n\nBar\n",
    dedent_lines_default(Text, Dedent).

% dedent() should not mangle internal tabs
test(dedent_preserve_internal_tabs, [Dedent,Dedent2] == [Expect,Expect]) :-
    Text = "  hello\tthere\n  how are\tyou?",
    Expect = "hello\tthere\nhow are\tyou?",
    dedent_lines_default(Text, Dedent),
    % make sure that it preserves tabs when it's not making any
    % changes at all
    dedent_lines_default(Expect, Dedent2).

% dedent() should not mangle tabs in the margin (i.e.
% tabs and spaces both count as margin, but are *not*
% considered equivalent)
test(dedent_preserve_margin_tabs, Text == Dedent) :-
    Text = "  hello there\n\thow are you?",
    dedent_lines_default(Text, Dedent).

test(dedent_preserve_margin_tabs, Text == Dedent) :-
    % same effect even if we have 8 spaces
    Text = "        hello there\n\thow are you?",
    dedent_lines_default(Text, Dedent).

test(dedent_preserve_margin_tabs, [Dedent1, Dedent2, Dedent3, Dedent4, Dedent5] == [Expect1, Expect2, Expect3, Expect4, Expect5]) :-
    % dedent() only removes whitespace that can be uniformly removed!
    Text1 = "\thello there\n\thow are you?",
    Expect1 = "hello there\nhow are you?",
    dedent_lines_default(Text1, Dedent1),

    Text2 = "  \thello there\n  \thow are you?",
    Expect2 = Expect1,
    dedent_lines_default(Text2, Dedent2),

    Text3 = "  \t  hello there\n  \t  how are you?",
    Expect3 = Expect1,
    dedent_lines_default(Text3, Dedent3),

    Text4 = "  \thello there\n  \t  how are you?",
    Expect4 = "hello there\n  how are you?",
    dedent_lines_default(Text4, Dedent4),

    % test margin is smaller than smallest indent
    Text5 = "  \thello there\n   \thow are you?\n \tI'm fine, thanks",
    Expect5 = " \thello there\n  \thow are you?\n\tI'm fine, thanks",
    dedent_lines_default(Text5, Dedent5).

% Test indent_lines
% The examples used for tests. If any of these change, the expected
% results in the various test cases must also be updated.
% The Python tests with "\r" were removed, as SWI-Prolog uses a
% different convention (I/O handles conversion of all line-ends to
% "\n" internally).

roundtrip_cases([
       % Basic test case
       "Hi.\nThis is a test.\nTesting.",
       % Include a blank line
       "Hi.\nThis is a test.\n\nTesting.",
       % Include leading and trailing blank lines
       "\nHi.\nThis is a test.\nTesting.\n",
       % Windows line ending ("\r\n") test cases have been removed.
       % Pathological case
       "\nHi.\n\nThis is a test.\n\n\nTesting.\n\n\n",
       % Additional sanity tests
       "\n",
       "\n\n",
       "",
       "T1\n"
      ]).

cases(Cases) :-
    roundtrip_cases(RoundtripCases),
    append(RoundtripCases, [
       " ",
       "  ",
       "  \n",
       "T2",
       "\n ",
       "\n \n",
       " \n"
       ], Cases).

true_(_).                       % Pred for indent - always succeeds
fail_(_) :- fail.               % Pred for indent - always fails.

test(indent_nomargin_default, Cases == Outs) :- % indent should do nothing if 'prefix' is empty.
    cases(Cases),
    maplist(indent_lines(""), Cases, Outs).
% Skip test(indent_nomargin_explicit_default) - not applicable
test(indent_nomargin_all_lines, Cases == Outs) :- % The same as indent_nomargin, but
                                                  % using the optional predicate argument
    cases(Cases),
    maplist(indent_lines(true_, ""), Cases, Outs).
test(indent_no_lines, Cases == Outs) :- % Explicitly skip indenting any lines
    cases(Cases),
    maplist(indent_lines(fail_, "    "), Cases, Outs).
test(roundtrip_spaces, Cases == Cases2) :- % A whitespace prefix should roundtrip with dedent.
    roundtrip_cases(Cases),
    maplist(indent_lines("    "), Cases, IndentedCases),
    maplist(dedent_lines_default, IndentedCases, Cases2).
test(roundtrip_tabs, Cases == Cases2) :- % A whitespace prefix should roundtrip with dedent
    roundtrip_cases(Cases),
    maplist(indent_lines("\t\t"), Cases, IndentedCases),
    maplist(dedent_lines_default, IndentedCases, Cases2).
test(roundtrip_mixed, Cases == Cases2) :- % A whitespace prefix should roundtrip with dedent
    roundtrip_cases(Cases),
    maplist(indent_lines(" \t  \t "), Cases, IndentedCases),
    maplist(dedent_lines_default, IndentedCases, Cases2).
test(indent_default, Results == Expected) :- % Test default indenting of lines that are not whitespace only
    cases(Cases),
    Prefix = "  ",
    Expected = [
                % Basic test case
                "  Hi.\n  This is a test.\n  Testing.",
                % Include a blank line
                "  Hi.\n  This is a test.\n\n  Testing.",
                % Include leading and trailing blank lines
                "\n  Hi.\n  This is a test.\n  Testing.\n",
                % Windows line ending ("\r\n") test cases have been removed.
                % Pathological case
                "\n  Hi.\n\n  This is a test.\n\n\n  Testing.\n\n\n",
                % Additional sanity tests
                "\n",
                "\n\n",
                "",
                "  T1\n",
                " ",
                "  ",
                "  \n",
                "  T2",
                "\n ",
                "\n \n",
                " \n"
               ],
    maplist(indent_lines(Prefix), Cases, Results).
% Skip test(indent_explicit_default) - not applicable
test(indent_all_lines, Results == Expected) :- % Add 'prefix' to all lines, including whitespace-only ones.
    cases(Cases),
    Prefix = "  ",
    Expected = [
                % Basic test case
                "  Hi.\n  This is a test.\n  Testing.",
                % Include a blank line
                "  Hi.\n  This is a test.\n  \n  Testing.",
                % Include leading and trailing blank lines
                "  \n  Hi.\n  This is a test.\n  Testing.\n",
                % Pathological case
                "  \n  Hi.\n  \n  This is a test.\n  \n  \n  Testing.\n  \n  \n",
                % Additional sanity tests
                "  \n",
                "  \n  \n",
                "",
                "  T1\n",
                "   ",
                "    ",
                "    \n",
                "  T2",
                "  \n   ",
                "  \n   \n",
                "   \n"
                ],
    maplist(indent_lines(true_, Prefix), Cases, Results).
test(indent_empty_lines, Results == Expected) :- % Add 'prefix' solely to whitespace-only lines.
    cases(Cases),
    Prefix = "  ",
    Expected = [
                % Basic test case
                "Hi.\nThis is a test.\nTesting.",
                % Include a blank line
                "Hi.\nThis is a test.\n  \nTesting.",
                % Include leading and trailing blank lines
                "  \nHi.\nThis is a test.\nTesting.\n",
                % Pathological case
                "  \nHi.\n  \nThis is a test.\n  \n  \nTesting.\n  \n  \n",
                % Additional sanity tests
                "  \n",
                "  \n  \n",
                "",
                "T1\n",
                "   ",
                "    ",
                "    \n",
                "T2",
                "  \n   ",
                "  \n   \n",
                "   \n"
               ],
    maplist(indent_lines(whitespace_only_, Prefix), Cases, Results).

whitespace_only_(Str) :-
  split_string(Str, "", " \t", [""]).


:- end_tests(strings).
