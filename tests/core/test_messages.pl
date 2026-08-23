/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_messages,
          [ test_messages/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> Test the message infra structure

Tests the central handling of  _predicate  references_  in messages: the
predicate indicator is normalised, printed  using   the  `code` class and
turned into a hyperlink to its definition if the location is known.  See
predicate_reference//2 in `boot/messages.pl`.
*/

test_messages :-
    run_tests([ predicate_reference,
                predicate_kind,
                predicate_source_location,
                message_elements
              ]).

:- dynamic
    my_pred/2.

my_pred(a,b).

my_nt --> [].

%!  reference(+Spec, -Elements) is det.
%!  reference(+Spec, +Options, -Elements) is det.

reference(Spec, Elements) :-
    reference(Spec, [], Elements).

reference(Spec, Options, Elements) :-
    phrase('$messages':predicate_reference(Spec, Options), Elements).

%!  ref_pi(+Elements, -PI) is semidet.
%
%   PI is the predicate indicator in the (single) reference Elements.

ref_pi(Elements, PI) :-
    memberchk(url(_, ansi(code, '~q', [PI])), Elements),
    !.
ref_pi(Elements, PI) :-
    memberchk(ansi(code, '~q', [PI]), Elements).

%!  ref_location(+Elements, -Location) is semidet.

ref_location(Elements, Location) :-
    memberchk(url(Location, _), Elements).

%!  ref_tag(+Elements, -Kind) is semidet.

ref_tag(Elements, Kind) :-
    memberchk(ansi(predicate(Kind), _, _), Elements).


		 /*******************************
		 *   PREDICATE REFERENCES	*
		 *******************************/

:- begin_tests(predicate_reference).

% All input forms lead to the same reference.

test(from_pi, PI == append/3) :-
    reference(append/3, E),
    ref_pi(E, PI).
test(from_qualified_pi, PI == append/3) :-
    reference(user:append/3, E),
    ref_pi(E, PI).
test(from_head, PI == append/3) :-
    reference(append(_,_,_), E),
    ref_pi(E, PI).
test(from_qualified_head, PI == append/3) :-
    reference(user:append(_,_,_), E),
    ref_pi(E, PI).

% A non-terminal is printed using //

test(non_terminal, PI == test_messages:my_nt//0) :-
    reference(test_messages:my_nt(_,_), E),
    ref_pi(E, PI).

% The module is hidden if it does not add information and kept if it
% does.

test(hide_system, PI == atom/1) :-
    reference(system:atom/1, E),
    ref_pi(E, PI).
test(keep_module, PI == lists:append/3) :-
    reference(lists:append/3, E),
    ref_pi(E, PI).
test(module_hide, PI == append/3) :-
    reference(lists:append/3, [module(hide)], E),
    ref_pi(E, PI).
test(module_show, PI == user:my_pred/2) :-
    reference(my_pred/2, [module(show)], E),
    ref_pi(E, PI).

% A predicate we can locate becomes a link, one we cannot does not.

test(link_to_prolog_source, true) :-
    reference(append/3, E),
    ref_location(E, File:Line),
    assertion(integer(Line)),
    assertion(sub_atom(File, _, _, 0, 'lists.pl')).
test(undefined_has_no_link, Location == none) :-
    reference(no_such_predicate_at_all/7, E),
    (   ref_location(E, Location0)
    ->  Location = Location0
    ;   Location = none
    ).
test(link_false) :-
    reference(append/3, [link(false)], E),
    assertion(\+ ref_location(E, _)).

% Options that do not apply must not silently change the result.

test(style, E == [ansi(comment, '~q', [my_pred/2])]) :-
    reference(my_pred/2, [style(comment), link(false)], E).

% Anything that is not a predicate is printed rather than raising an
% error: a message must always print.

test(not_a_predicate, E == [ansi(code, '~p', ["hello"])]) :-
    reference("hello", E).

:- end_tests(predicate_reference).


		 /*******************************
		 *	 PREDICATE KINDS	*
		 *******************************/

:- begin_tests(predicate_kind).

test(iso, Kind == iso) :-
    '$messages':predicate_kind(atom/1, Kind).
test(built_in, Kind == built_in) :-
    '$messages':predicate_kind(system:forall/2, Kind).
test(foreign, Kind == foreign) :-
    '$messages':predicate_kind(system:tab/1, Kind).
test(library, Kind == library(lists)) :-
    '$messages':predicate_kind(append/3, Kind).
test(library_qualified, Kind == library(lists)) :-
    '$messages':predicate_kind(lists:append/3, Kind).
test(user, Kind == user) :-
    '$messages':predicate_kind(test_messages:my_pred/2, Kind).
test(undefined, Kind == undefined) :-
    '$messages':predicate_kind(no_such_predicate_at_all/7, Kind).

% The tag is only emitted if asked for and carries the kind as its
% style class.

test(no_tag_by_default) :-
    reference(append/3, E),
    assertion(\+ ref_tag(E, _)).
test(tag, Kind == library(lists)) :-
    reference(append/3, [tag(true)], E),
    ref_tag(E, Kind).

:- end_tests(predicate_kind).


		 /*******************************
		 *	  SOURCE LOCATION	*
		 *******************************/

:- begin_tests(predicate_source_location).

test(prolog_predicate, true) :-
    '$predicate_source_location'(lists:append(_,_,_), File:Line),
    assertion(sub_atom(File, _, _, 0, 'lists.pl')),
    assertion(integer(Line)).

% Imported predicates are traced back to the module that defines them.

test(follows_import, true) :-
    '$predicate_source_location'(user:append(_,_,_), File:Line),
    assertion(sub_atom(File, _, _, 0, 'lists.pl')),
    assertion(integer(Line)).

% Predicates defined in C.  This requires addr2line(1) or atos(1) _and_
% source line information for the C library, which is not available in
% every build.  If we get a location it must be a C source location.

test(foreign_predicate, true) :-
    (   '$predicate_source_location'(system:atom(_), File:Line)
    ->  assertion(sub_atom(File, _, _, 0, '.c')),
        assertion(integer(Line))
    ;   true                            % no line info for the C library
    ).

test(addr2line_location, File-Line == '/x/pl-prims.c'-42) :-
    '$addr2line_location'("pl_atom1_va() at /x/pl-prims.c:42", File, Line).
test(addr2line_discriminator, File-Line == '/x/pl-prims.c'-42) :-
    '$addr2line_location'("f() at /x/pl-prims.c:42 (discriminator 1)",
                          File, Line).
test(addr2line_no_location, fail) :-
    '$addr2line_location'("/usr/lib/libswipl.so(pl_atom1_va+0x20)", _, _).
% Without debug information addr2line(1) says `??:0` and atos(1) reports
% the bare symbol.  Neither is a source location.
test(addr2line_unknown, fail) :-
    '$addr2line_location'("??() at ??:0", _, _).
test(addr2line_symbol_only, fail) :-
    '$addr2line_location'("pl_atom1_va", _, _).

:- end_tests(predicate_source_location).


		 /*******************************
		 *	MESSAGE ELEMENTS	*
		 *******************************/

:- begin_tests(message_elements).

% The three forms of the label of an url/2 element all end up as plain
% text, both when printed and when the message is turned into a string.

test(url_label_text, S == "see there") :-
    elements_string(['see ', url('http://x.org', there)], S).
test(url_label_format, S == "see there") :-
    elements_string(['see ', url('http://x.org', '~w'-[there])], S).
test(url_label_ansi, S == "see there") :-
    elements_string(['see ', url('http://x.org', ansi(code, '~w', [there]))],
                    S).
test(url_label_ansi_string, S == "see there") :-
    elements_format(['see ', url('http://x.org', ansi(code, '~w', [there]))],
                    S).

% A location without a label prints as File:Line

test(url_location, S == "at /x/y.pl:42") :-
    elements_string(['at ', url('/x/y.pl':42)], S).

% A label is text, not a format: it typically holds a file name.

test(url_label_is_text, S == "~w.pl") :-
    elements_string([url('/x/~w.pl', '~w.pl')], S).
test(url_label_is_text_string, S == "~w.pl") :-
    elements_format([url('/x/~w.pl', '~w.pl')], S).

%!  elements_string(+Elements, -String) is det.
%
%   String is the result of print_message_lines/3 on Elements, without
%   the newline that ends the message.

elements_string(Elements, String) :-
    with_output_to(string(Raw),
                   print_message_lines(current_output, '', Elements)),
    split_string(Raw, "", "\n", [String]).

%!  elements_format(+Elements, -String) is det.
%
%   String is Elements as message_to_string/2 renders them.

elements_format(Elements, String) :-
    '$messages':actions_to_format(Elements, Fmt, Args),
    format(string(String), Fmt, Args).

:- end_tests(message_elements).
