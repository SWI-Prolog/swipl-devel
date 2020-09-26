/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v
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

:- module(strings,
          [ dedent_lines/3,             % +In,-Out,+Options
            indent_lines/3,             % +Prefix,+In,-Out
            indent_lines/4,             % :Pred,+Prefix,+In,-Out
            interpolate_string/4,       % +In,-Out,+Map,+Options
            string_lines/2,             % ?In,?Lines
            string/4                    % Quasi quotation support
          ]).
:- autoload(library(apply), [include/3, foldl/4, maplist/3, maplist/2]).
:- autoload(library(error), [existence_error/2, must_be/2]).
:- autoload(library(lists), [member/2, append/3]).
:- autoload(library(option), [option/3]).
:- autoload(library(quasi_quotations),
            [quasi_quotation_syntax/1, with_quasi_quotation_input/3]).
:- autoload(library(dcg/basics),
            [string/3, prolog_var_name/3, string_without/4, eos//0]).

:- meta_predicate
    interpolate_string(:, -, +, +),
    indent_lines(1, +, +, -).

:- quasi_quotation_syntax(string).

/** <module> String utilities

This module provides string handling   utilities,  currently notably for
dealing  with  multi-line  strings  and   _interpolation_.  The  library
provides a couple of primitives  as   well  definitions for the `string`
_quasi quotation_ syntax. The latter allows for constructing both single
line and multi-line long strings based  on template interpolation. Below
is a simple example using the quasi quotation syntax.


```
test(To) :-
    write({|string(To)||
           | Dear {To},
           |
           | I'm happy to announce a string interpolation quasi quoter.
           |}.
```

__Warning__

The general purpose string  interpolation   implemented  by this library
should __not__ be used to create strings   for a formal language such as
HTML, JavaScript, SQL, etc.  because  the   result  will  be  subject to
__injection attacks__, providing a serious   __security risc__. The core
idea of quasi quotation  is  to  know   about  the  target  language and
interpolate Prolog data into the template  __while respecting the syntax
of the target language__, notable to   __escape certain characters where
needed__. See also library(http/html_write)   and library(http/js_write)
which define quasi quotation rules for HTML and JavaScript.

@see format/3 can format to a string as well.  The library(lynx/format)
provides primitive to wrap long strings.
@see The core system provides many additional string processing
predicates.
@tbd There are probably many other high level string predicates that
belong in this library. For example, predicates similar to the
functions in https://docs.python.org/3/library/textwrap.html
*/

%!  string(+Content, +Args, +Binding, -DOM)
%
%   Implements  the  quasi  quotation  syntax  `string`.  If  the  first
%   character of the content is a  newline   (i.e.,  there  is a newline
%   _immediately_   after   the   ``||``   token)    this   first   uses
%   dedent_lines/3 to the remove common  white   space  prefix from the
%   lines. This is called with  the   option  chars("\s\t|"), i.e., also
%   removing ``|`` characters and tab(8).
%
%   If the quasi quotation syntax  carries arguments (e.g., string(To)),
%   the string is compiled into a function   that produces the result of
%   interpolating the arguments into the template. See user functions on
%   dict objects. If there are no arguments,   the  result is simply the
%   final string.
%
%   @see interpolate_string/4 for the interpolation syntax.
%   @see Section for examples and discussion.
%   @tbd Specify tab width and allow for {@Goal} templates.

string(Content, Args, Binding, DOM) :-
    must_be(list, Binding),
    include(qq_var(Args), Binding, QQDict),
    with_quasi_quotation_input(Content, Stream,
                               read_string(Stream, _, String)),
    (   string_concat("\n", String1, String)
    ->  dedent_lines(String1, String2, [tab(8), chars("\s\t|")])
    ;   String2 = String
    ),
    (   prolog_load_context(module, Module)
    ->  true
    ;   Module = user                   % typein?
    ),
    (   Args == []
    ->  DOM = String2
    ;   comp_interpolate(String2, Compiled, QQDict, [module(Module)]),
        DOM =.. ['.',strings{type:string},exec(Compiled, QQDict)]
    ).

qq_var(Vars, _=Var) :- member(V, Vars), V == Var, !.

_Dict.exec(Compiled, Map) := String :-
    exec_interpolate(Compiled, String, Map).

%!  interpolate_string(:In, -Out, +Map, +Options)
%
%   Establish a string from a template  by replacing patterns. Supported
%   patterns are:
%
%     - {Name}
%       If Map contains `Name=Value`, insert `Value` using write/1.
%       If `Name` does not appear in Map, raise an existence error.
%       `Name` must satisfy the rules for a Prolog variable.
%     - {Name,Default}
%       As above, but if `Name` does not appear in Map, use `Value`
%     - {@Goal}
%       Insert the output (to `current_output`) of `Goal` here.
%       For safety reasons only accepted if Options contains
%       `goals(true)`


interpolate_string(Module:In, Out, Map, Options) :-
    comp_interpolate(In, Compiled, Map, [module(Module)|Options]),
    exec_interpolate(Compiled, Out, Map).

comp_interpolate(In, Compiled, Map, Options) :-
    string_codes(In, Codes),
    phrase(interpolate(Compiled, [], Map, Options), Codes).

interpolate([PreS,Action|T0], T, Map, Options) -->
    string(Pre),
    "{", interpolate_pattern(Action, Options), "}",
    !,
    { string_codes(PreS, Pre) },
    interpolate(T0, T, Map, Options).
interpolate(T0, T, _Map, _Options) -->
    string(Pre),
    eos,
    (   { Pre == [] }
    ->  { T0 = T }
    ;   { string_codes(PreS, Pre),
          T0 = [PreS|T]
        }
    ).

interpolate_pattern(Pattern, _) -->
    prolog_var_name(Name),
    !,
    (   ","
    ->  default_value(Default),
        { Pattern = var(Name, Default) }
    ;   { Pattern = var(Name) }
    ).
interpolate_pattern(goal(Goal), Options) -->
    { option(goals(true), Options, false) },
    "@",
    !,
    goal(Goal, Options).

default_value(String) -->
    string_without("}", Codes),
    { string_codes(String, Codes) }.

goal(M:Goal, Options) -->
    string_without("}", Codes),
    { option(module(M), Options, user),
      string_codes(String, Codes),
      term_string(Goal, String)
    }.

exec_interpolate(Compiled, String, Map) :-
    maplist(exec_interpolate1(Map), Compiled, Parts),
    atomics_to_string(Parts, String).

exec_interpolate1(Map, var(Var), Out) :-
    !,
    (   memberchk(Var = Value, Map)
    ->  format(string(Out), '~w', Value)
    ;   existence_error(template_var, Var)
    ).
exec_interpolate1(Map, var(Var, Default), Out) :-
    !,
    (   memberchk(Var = Value, Map)
    ->  true
    ;   Value = Default
    ),
    format(string(Out), '~w', Value).
exec_interpolate1(_Map, goal(Goal), Out) :-
    !,
    format(string(Out), '~@', [Goal]).
exec_interpolate1(_, String, String).

%!  string_lines(?String, ?Lines) is det.
%
%   True when String represents Lines.  This   follows  the  normal text
%   convention that a  line  is  defined   as  a  possible  empty string
%   followed by a newline character ("\n").  E.g.
%
%   ```
%   ?- string_lines("a\nb\n", L).
%   L = ["a", "b"].
%   ?- string_lines(S, ["a", "b"]).
%   S = "a\nb\n".
%   ```
%
%   This predicate is  a  true  _relation_   if  both  arguments  are in
%   canonical form, i.e. all text  is   represented  as  strings and the
%   first argument ends with  a   newline.  The implementation tolerates
%   non-canonical input: other  types  than   strings  are  accepted and
%   String does not need to end with a newline.
%
%   @see split_string/4. Using split_string(String, "\n",  "", Lines) on
%   a string that ends in a  newline   adds  an  additional empty string
%   compared to string_lines/2.

string_lines(String, Lines) :-
    (   var(String)
    ->  must_be(list, Lines),
        append(Lines, [""], Lines1),
        atomics_to_string(Lines1, "\n", String)
    ;   split_string(String, "\n", "", Lines0),
        (   append(Lines, [""], Lines0)
        ->  true
        ;   Lines = Lines0
        )
    ).

%!  dedent_lines(+In, -Out, +Options)
%
%   Remove shared indentation for all lines in a string. Lines are separated
%   by "\n" -- conversion to and from  external forms  (such as "\r\n")  are
%   typically done by the I/O predicates.
%   A final "\n" is preserved.
%
%   Options:
%
%     - tab(N)
%       Assume tabs at columns of with N.  When omitted, tabs are
%       taken literally and only exact matches are removed.
%     - chars(CodesOrString)
%       Characters to remove.  This can notably be used to remove
%       additional characters such as `*` or `|`.  Default is
%       `" \t"`.

dedent_lines(In, Out, Options) :-
    option(tab(Tab), Options, 0),
    option(chars(Chars), Options, "\s\t"),
    string_codes(Sep, Chars),
    How = s(Tab,Sep),
    split_string(In, "\n", "", Lines),
    foldl(common_indent(How), Lines, _, Indent0),
    (   prepare_delete(Indent0, Indent)
    ->  maplist(dedent_line(Tab, Indent), Lines, Dedented),
        atomics_to_string(Dedented, "\n", Out)
    ;   length(Lines, NLines),
        NewLines is NLines - 1,
        length(Codes, NewLines),
        maplist(=(0'\n), Codes),
        string_codes(Out, Codes)
    ).

prepare_delete(Var, _) :-               % All blank lines
    var(Var),
    !,
    fail.
prepare_delete(Width, Width) :-
    integer(Width),
    !.
prepare_delete(Codes, String) :-
    string_codes(String, Codes).

common_indent(s(0,Sep), Line, Indent0, Indent) :-
    !,
    line_indent(Line, Indent1, Sep),
    join_indent(Indent0, Indent1, Indent).
common_indent(s(Tab,Sep), Line, Indent0, Indent) :-
    !,
    line_indent_width(Line, Indent1, Tab, Sep),
    join_indent_width(Indent0, Indent1, Indent).

%!  line_indent(+Line, -Indent, +Sep) is det.
%
%   Determine the indentation as a list of character codes.  If the
%   line only holds white space Indent is left unbound.

line_indent(Line, Indent, Sep) :-
    string_codes(Line, Codes),
    code_indent(Codes, Indent0, Sep),
    (   is_list(Indent0)
    ->  Indent = Indent0
    ;   true
    ).

code_indent([H|T0], [H|T], Sep) :-
    string_code(_, Sep, H),
    !,
    code_indent(T0, T, Sep).
code_indent([], _, _) :-
    !.
code_indent(_, [], _).

join_indent(Var, Indent, Indent) :-
    var(Var),
    !.
join_indent(Indent, Var, Indent) :-
    var(Var),
    !.
join_indent(Indent1, Indent2, Indent) :-
    shared_prefix(Indent1, Indent2, Indent).

shared_prefix(Var, Prefix, Prefix) :-
    var(Var),
    !.
shared_prefix(Prefix, Var, Prefix) :-
    var(Var),
    !.
shared_prefix([H|T0], [H|T1], [H|T]) :-
    !,
    shared_prefix(T0, T1, T).
shared_prefix(_, _, []).

%!  line_indent_width(+Line, -Indent, +Tab, +Sep) is det.
%
%   Determine the indentation as a  column,   compensating  for  the Tab
%   width.  This is used if the tab(Width) option is provided.

line_indent_width(Line, Indent, Tab, Sep) :-
    string_codes(Line, Codes),
    code_indent_width(Codes, 0, Indent, Tab, Sep).

code_indent_width([H|T], Indent0, Indent, Tab, Sep) :-
    string_code(_, Sep, H),
    !,
    update_pos(H, Indent0, Indent1, Tab),
    code_indent_width(T, Indent1, Indent, Tab, Sep).
code_indent_width([], _, _, _, _) :-
    !.
code_indent_width(_, Indent, Indent, _, _).

join_indent_width(Var, Indent, Indent) :-
    var(Var),
    !.
join_indent_width(Indent, Var, Indent) :-
    var(Var),
    !.
join_indent_width(Indent0, Indent1, Indent) :-
    Indent is min(Indent0, Indent1).

%!  dedent_line(+Tab, +Indent, +String, -Dedented)
%
%   Dedent a single line according to Tab   and Indent. Indent is either
%   an integer, deleting the  first  Indent   characters  or  a  string,
%   deleting the string literally.

dedent_line(_Tab, Indent, String, Dedented) :-
    string(Indent),
    !,
    (   string_concat(Indent, Dedented, String)
    ->  true
    ;   Dedented = ""               % or ""?
    ).
dedent_line(Tab, Indent, String, Dedented) :-
    string_codes(String, Codes),
    delete_width(0, Indent, Codes, Codes1, Tab),
    string_codes(Dedented, Codes1).

delete_width(Here, Indent, Codes, Codes, _) :-
    Here =:= Indent,
    !.
delete_width(Here, Indent, Codes0, Codes, _) :-
    Here > Indent,
    !,
    NSpaces is Here-Indent,
    length(Spaces, NSpaces),
    maplist(=(0'\s), Spaces),
    append(Spaces, Codes0, Codes).
delete_width(Here, Indent, [H|T0], T, Tab) :-
    !,
    update_pos(H, Here, Here1, Tab),
    delete_width(Here1, Indent, T0, T, Tab).
delete_width(_, _, [], [], _).

update_pos(0'\t, Here0, Here, Tab) :-
    !,
    Here is ((Here0+Tab)//Tab)*Tab.
update_pos(_, Here0, Here, _) :-
    Here is Here0 + 1.

%!  indent_lines(+Prefix, +In, -Out) is det.
%
%   Add Prefix to the beginning of lines   in In. Lines are separated by
%   "\n" -- conversion to and from external   forms (such as "\r\n") are
%   typically done by the I/O predicates. Lines that consist entirely of
%   whitespace are left as-is.

indent_lines(Prefix, In, Out) :-
    indent_lines(ignore_whitespace_line, Prefix, In, Out).

%!  indent_lines(:Filter, +Prefix, +In, -Out) is det.
%
%   Similar to indent_lines/3, but only adds   Prefix to lines for which
%   call(Filter, Line) succeeds.

indent_lines(Pred, Prefix, In, Out) :-
    % Use split_string/4 rather than string_lines/2, to preserve final "\n".
    split_string(In, "\n", "", Lines0),
    (   append(Lines, [""], Lines0)
    ->  maplist(concat_to_string(Pred, Prefix), Lines, IndentedLines0),
        append(IndentedLines0, [""], IndentedLines),
        atomics_to_string(IndentedLines, "\n", Out)
    ;   Lines = Lines0,
        maplist(concat_to_string(Pred, Prefix), Lines, IndentedLines),
        atomics_to_string(IndentedLines, "\n", Out)
    ).

ignore_whitespace_line(Str) :-
    \+ split_string(Str, "", " \t", [""]).

:- meta_predicate concat_to_string(:, +, +, -).

concat_to_string(Pred, Prefix, Line, Out) :-
    (   call(Pred, Line)
    ->  atomics_to_string([Prefix, Line], Out)
    ;   Out = Line
    ).

