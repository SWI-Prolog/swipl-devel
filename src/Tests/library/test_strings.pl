/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
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

splitlines_check(Result, Str) :-
    split_lines(Str, Lines),
    Lines == Result.

% These tests were made by changed "\r\n" to "\n" and other "\r" to "\n":
% TODO: uncomment the cases where Python and swipl semantics differ
test(splitlines) :- splitlines_check(["abc", "def", "", "ghi"], "abc\ndef\n\nghi").
test(splitlines) :- splitlines_check(["abc", "def", "", "ghi"], "abc\ndef\n\nghi").
test(splitlines) :- splitlines_check(["abc", "def", "ghi"], "abc\ndef\nghi").
% test(splitlines) :- splitlines_check(["abc", "def", "ghi"], "abc\ndef\nghi\n").
% test(splitlines) :- splitlines_check(["abc", "def", "ghi", ""], "abc\ndef\nghi\n\r").
% test(splitlines) :- splitlines_check(["", "abc", "def", "ghi", ""], "\nabc\ndef\nghi\n\r").

% TODO: enable DOS linends.
% test(splitlines) :- splitlines_check(["abc", "def", "", "ghi"], "abc\ndef\n\rghi").
% test(splitlines) :- splitlines_check(["abc", "def", "", "ghi"], "abc\ndef\n\r\nghi").
% test(splitlines) :- splitlines_check(["abc", "def", "ghi"], "abc\ndef\r\nghi").
% test(splitlines) :- splitlines_check(["abc", "def", "ghi"], "abc\ndef\r\nghi\n").
% test(splitlines) :- splitlines_check(["abc", "def", "ghi", ""], "abc\ndef\r\nghi\n\r").
% test(splitlines) :- splitlines_check(["", "abc", "def", "ghi", ""], "\nabc\ndef\r\nghi\n\r").

% TODO: handle keepends (string_split_with_ends/2)
    %     splitlines_check(["", "abc", "def", "ghi", ""],
    %                     "\nabc\ndef\r\nghi\n\r", "splitlines", False)
    %     splitlines_check(["\n", "abc\n", "def\r\n", "ghi\n", "\r"],
    %                     "\nabc\ndef\r\nghi\n\r", "splitlines", True)
    %     splitlines_check(["", "abc", "def", "ghi", ""], "\nabc\ndef\r\nghi\n\r",
    %                     "splitlines", keepends=False)
    %     splitlines_check(["\n", "abc\n", "def\r\n", "ghi\n", "\r"],
    %                     "\nabc\ndef\r\nghi\n\r", "splitlines", keepends=True)
    %     self.checkraises(TypeError, "abc", "splitlines", 42, 42)


% Tests adapted from https://github.com/python/cpython/blob/master/Lib/test/test_textwrap.py
% TODO: add the Windows test cases, when "\r\n" separators are handled.

dedent_string_no_options(In, Out) :-
    dedent_string(In, Out, []).

dedent_string_expect(In, Expect) :-
    dedent_string(In, Out, []),
    Out == Expect.

% Test dedent_string

assert_unchanged(Text) :- % assert that dedent() has no effect on 'text'
    dedent_string_expect(Text, Text).

test(dedent_nomargin) :-
    % No lines indented.
    assert_unchanged("Hello there.\nHow are you?\nOh good, I'm glad."),

    % Similar, with a blank line.
    assert_unchanged("Hello there.\n\nBoo!"),

    % Some lines indented, but overall margin is still zero.
    assert_unchanged("Hello there.\n  This is indented."),

    % Again, add a blank line.
    assert_unchanged("Hello there.\n\n  Boo!\n").

test(dedent_even) :-            % All lines indented by two spaces.
    Text = "  Hello there.\n  How are ya?\n  Oh good.",
    Expect = "Hello there.\nHow are ya?\nOh good.",
    dedent_string_expect(Text, Expect).
test(dedent_even) :-            % Same, with blank lines.
    Text = "  Hello there.\n\n  How are ya?\n  Oh good.\n",
    Expect = "Hello there.\n\nHow are ya?\nOh good.\n",
    dedent_string_expect(Text, Expect).
test(dedent_even) :- % Now indent one of the blank lines.
    Text = "  Hello there.\n  \n  How are ya?\n  Oh good.\n",
    Expect = "Hello there.\n\nHow are ya?\nOh good.\n",
    dedent_string_expect(Text, Expect).

test(dedent_uneven) :- % Lines indented unevenly.
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
        dedent_string_expect(Text, Expect).
test(dedent_uneven) :-         % Uneven indentation with a blank line.
    Text = "  Foo\n    Bar\n\n   Baz\n",
    Expect = "Foo\n  Bar\n\n Baz\n",
    dedent_string_expect(Text, Expect).
test(dedent_uneven) :- % Uneven indentation with a whitespace-only line.
    Text = "  Foo\n    Bar\n \n   Baz\n",
    % Expect = "Foo\n  Bar\n\n Baz\n",  % TODO: this is what Python does
    Expect = "Foo\n  Bar\n \n Baz\n",
    dedent_string_expect(Text, Expect).

test(dedent_declining) :- % Uneven indentation with declining indent level.
    Text = "     Foo\n    Bar\n",  % 5 spaces, then 4
    Expect = " Foo\nBar\n",
    dedent_string_expect(Text, Expect).
test(dedent_declining) :-   %  Declining indent level with blank line.
    Text = "     Foo\n\n    Bar\n",  % 5 spaces, blank, then 4
    Expect = " Foo\n\nBar\n",
    dedent_string_expect(Text, Expect).
test(dedent_declining) :- %  Declining indent level with whitespace only line.
    Text = "     Foo\n    \n    Bar\n", % 5 spaces, then 4, then 4
    Expect = " Foo\n\nBar\n",
    dedent_string_expect(Text, Expect).

% dedent() should not mangle internal tabs
test(dedent_preserve_internal_tabs) :-
    Text = "  hello\tthere\n  how are\tyou?",
    Expect = "hello\tthere\nhow are\tyou?",
    dedent_string_expect(Text, Expect),
    % make sure that it preserves tabs when it's not making any
    % changes at all
    dedent_string_expect(Expect, Expect).

% dedent() should not mangle tabs in the margin (i.e.
% tabs and spaces both count as margin, but are *not*
% considered equivalent)
test(dedent_preserve_margin_tabs) :-
    assert_unchanged("  hello there\n\thow are you?"),

    % same effect even if we have 8 spaces
    assert_unchanged("        hello there\n\thow are you?").

test(dedent_preserve_margin_tabs) :-
    % dedent() only removes whitespace that can be uniformly removed!
    Text1 = "\thello there\n\thow are you?",
    Expect = "hello there\nhow are you?",
    dedent_string_expect(Text1, Expect),

    Text2 = "  \thello there\n  \thow are you?",
    dedent_string_expect(Text2, Expect),

    Text3 = "  \t  hello there\n  \t  how are you?",
    dedent_string_expect(Text3, Expect),

    Text4 = "  \thello there\n  \t  how are you?",
    Expect4 = "hello there\n  how are you?",
    dedent_string_expect(Text4, Expect4),

    % test margin is smaller than smallest indent
    Text5 = "  \thello there\n   \thow are you?\n \tI'm fine, thanks",
    Expect5 = " \thello there\n  \thow are you?\n\tI'm fine, thanks",
    dedent_string_expect(Text5, Expect5).

% Test indent_string
% The examples used for tests. If any of these change, the expected
% results in the various test cases must also be updated.
% The roundtrip cases are separate, because textwrap.dedent doesn't
% handle Windows line endings

roundtrip_cases([
                 % Basic test case
                 "Hi.\nThis is a test.\nTesting.",
                 % Include a blank line
                 "Hi.\nThis is a test.\n\nTesting.",
                 % Include leading and trailing blank lines
                 "\nHi.\nThis is a test.\nTesting.\n"
                ]).
cases(Cases) :-
    roundtrip_cases(Cases).
    % TODO: append the following:
    % % Use Windows line endings
    % "Hi.\r\nThis is a test.\r\nTesting.\r\n",
    % % Pathological case
    % "\nHi.\r\nThis is a test.\n\r\nTesting.\r\n\n"

true_(_).                       % Pred for indent - always succeeds
fail_(_) :- fail.               % Pred for indent - always fails.

test(indent_nomargin_default) :- % indent should do nothing if 'prefix' is empty.
    cases(Cases),
    maplist(indent_string(""), Cases, Outs),
    Cases == Outs.
% Skip test(indent_nomargin_explicit_default) - not applicable
test(indent_nomargin_all_lines) :- % The same as indent_nomargin, but
                                   % using the optional predicate argument
    cases(Cases),
    maplist(indent_string(true_, ""), Cases, Outs),
    Cases == Outs.
test(indent_no_lines) :-        % Explicitly skip indenting any lines
    cases(Cases),
    maplist(indent_string(fail_, "    "), Cases, Outs),
    Cases == Outs.
test(roundtrip_spaces) :- % A whitespace prefix should roundtrip with dedent.
    roundtrip_cases(Cases),
    maplist(indent_string("    "), Cases, IndentedCases),
    maplist(dedent_string_no_options, IndentedCases, Cases2),
    Cases = Cases2.
% TODO: roundtrip_tabs when supported by dedent
% TODO: roundtrip_mixed when supported by dedent
test(indent_default) :-    % Test default indenting of lines that are not whitespace only
    cases(Cases),
    Prefix = "  ",
    Expected = [
                % Basic test case
                "  Hi.\n  This is a test.\n  Testing.",
                % Include a blank line
                "  Hi.\n  This is a test.\n\n  Testing.",
                % Include leading and trailing blank lines
                "\n  Hi.\n  This is a test.\n  Testing.\n"
                % TODO: % Use Windows line endings
                %       "  Hi.\r\n  This is a test.\r\n  Testing.\r\n",
                %       % Pathological case
                %       "\n  Hi.\r\n  This is a test.\n\r\n  Testing.\r\n\n"
               ],
    maplist(indent_string(Prefix), Cases, Results),
    Results == Expected.
% Skip test(indent_explicit_default) - not applicable
test(indent_all_lines) :- % Add 'prefix' to all lines, including whitespace-only ones.
    cases(Cases),
    Prefix = "  ",
    Expected = [
                % Basic test case
                "  Hi.\n  This is a test.\n  Testing.",
                % Include a blank line
                "  Hi.\n  This is a test.\n  \n  Testing.",
                
                % Include leading and trailing blank lines

                % TODO: This result is *different* from what Python
                %       textwrap.indent does -- Python's code uses
                %       splitlines(True) which preserves the
                %       end-of-line character; our implementation
                %       does a split on "\n"s.

                "  \n  Hi.\n  This is a test.\n  Testing.\n  "

                % TODO: % Use Windows line endings
                %       "  Hi.\r\n  This is a test.\r\n  Testing.\r\n",
                %       %  Pathological case
                %       "  \n  Hi.\r\n  This is a test.\n  \r\n  Testing.\r\n  \n"
                ],
    maplist(indent_string(true_, Prefix), Cases, Results),
    Results == Expected.
% TODO: indent_empty_lines test case:
/*
      def test_indent_empty_lines(self):
        # Add 'prefix' solely to whitespace-only lines.
        prefix = '  '
        expected = (
          # Basic test case
          "Hi.\nThis is a test.\nTesting.",
          # Include a blank line
          "Hi.\nThis is a test.\n  \nTesting.",
          # Include leading and trailing blank lines
          "  \nHi.\nThis is a test.\nTesting.\n",
          # Use Windows line endings
          "Hi.\r\nThis is a test.\r\nTesting.\r\n",
          # Pathological case
          "  \nHi.\r\nThis is a test.\n  \r\nTesting.\r\n  \n",
        )
        predicate = lambda line: not line.strip()
        for text, expect in zip(self.CASES, expected):
            self.assertEqual(indent(text, prefix, predicate), expect)
  */


:- end_tests(strings).
