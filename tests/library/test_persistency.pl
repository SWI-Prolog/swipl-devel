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


:- module(test_persistency,
	  [ test_persistency/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(persistency)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [append/3, last/2]).

/** <module> Test library(persistency)

Currently only tests recovery from a database file of which the last
term was only partially written, e.g., because the disk was full or the
process was killed while writing.
*/

test_persistency :-
    run_tests([ persistency_recover
	      ]).

:- persistent
	fact(name:atom, value:any).

facts([ aap-1,
	'noot€'-2,
	mies-f(a,"b",[c])
      ]).


		 /*******************************
		 *            HELPERS           *
		 *******************************/

%!  create_db(+File) is det.
%
%   Create File holding facts/1 and detach from it.

create_db(File) :-
    db_attach(File, [sync(close)]),
    facts(Facts),
    forall(member(Name-Value, Facts),
	   assert_fact(Name, Value)),
    db_detach.

%!  load_db(+File, -Loaded) is det.
%
%   Attach to File, collect the facts and detach again.

load_db(File, Loaded) :-
    db_attach(File, [sync(close)]),
    findall(Name-Value, fact(Name, Value), Loaded),
    db_detach.

%!  add_fact(+File, +Name, +Value) is det.
%
%   Append a fact to the database in File.

add_fact(File, Name, Value) :-
    db_attach(File, [sync(close)]),
    assert_fact(Name, Value),
    db_detach.

%!  db_bytes(+File, -Bytes) is det.
%!  db_bytes(-File, +Bytes) is det.
%
%   Read/write File as a string of bytes.

db_bytes(File, Bytes), var(Bytes) =>
    setup_call_cleanup(
	open(File, read, In, [type(binary)]),
	read_string(In, _, Bytes),
	close(In)).
db_bytes(File, Bytes) =>
    setup_call_cleanup(
	open(File, write, Out, [type(binary)]),
	write(Out, Bytes),
	close(Out)).

%!  reference_db(-Bytes, -LineEnds) is det.
%
%   Bytes is the content of a database holding facts/1.  LineEnds holds,
%   for each term in the file, the offset just after the `.` that ends
%   the term.

:- dynamic reference_db_cache/2.

reference_db(Bytes, LineEnds) :-
    reference_db_cache(Bytes, LineEnds),
    !.
reference_db(Bytes, LineEnds) :-
    tmp_db(File),
    setup_call_cleanup(
	create_db(File),
	db_bytes(File, Bytes),
	delete_file(File)),
    line_ends(Bytes, LineEnds),
    assertz(reference_db_cache(Bytes, LineEnds)).

line_ends(Bytes, Ends) :-
    split_string(Bytes, "\n", "", Parts),
    append(Lines, [""], Parts),		% the file ends in a newline
    !,
    line_ends(Lines, 0, Ends).

line_ends([], _, []).
line_ends([H|T], Offset0, [End|Ends]) :-
    string_length(H, Len),
    End is Offset0+Len,
    Offset is End+1,
    line_ends(T, Offset, Ends).

tmp_db(File) :-
    setup_call_cleanup(
	tmp_file_stream(File, Out, [extension(db)]),
	true,
	close(Out)).

%!  truncated_db(-Size, -Count, -Warn) is nondet.
%
%   Enumerate all databases that are shorter than the reference database
%   but still hold the complete leading created(Stamp) term.  Count is
%   the number of facts that must be loaded from such a database and
%   Warn is `true` if the file was truncated inside a term.

truncated_db(Size, Count, Warn) :-
    reference_db(Bytes, [First|Ends]),
    string_length(Bytes, Full),
    Max is Full-1,
    between(First, Max, Size),
    findall(E, (member(E, [First|Ends]), E =< Size), Complete),
    length(Complete, Terms),
    Count is Terms-1,
    last(Complete, LastEnd),
    (   (   Size =:= LastEnd		% no newline after the last term
	;   Size =:= LastEnd+1		% truncated at a term boundary
	)
    ->	Warn = false
    ;	Warn = true
    ).

%!  write_truncated_db(+File, +Size) is det.

write_truncated_db(File, Size) :-
    reference_db(Bytes, _),
    sub_string(Bytes, 0, Size, _, Truncated),
    db_bytes(File, Truncated).


		 /*******************************
		 *           MESSAGES           *
		 *******************************/

:- dynamic
	trapping/0,
	trapped/1.

:- multifile
	user:message_hook/3.

user:message_hook(persistency(Msg), Kind, _Lines) :-
    trapping,
    assertz(trapped(Kind-Msg)).

trap_messages :-
    retractall(trapped(_)),
    assertz(trapping).

trapped_messages(Msgs) :-
    retractall(trapping),
    findall(M, trapped(M), Msgs),
    retractall(trapped(_)).


		 /*******************************
		 *            TESTS             *
		 *******************************/

:- begin_tests(persistency_recover).

% Loading a database that was truncated anywhere after the leading
% created(Stamp) term must load all facts that were completely written,
% warn if the file was truncated inside a term and leave a file that (1)
% loads without a warning and (2) can be extended.

test(truncate, [ forall(truncated_db(Size, Count, Warn)),
		 setup(( tmp_db(File),
			 write_truncated_db(File, Size)
		       )),
		 cleanup(delete_file(File))
	       ]) :-
    facts(Facts),
    length(Prefix, Count),
    append(Prefix, _, Facts),
    trap_messages,
    load_db(File, Loaded),
    trapped_messages(Msgs),
    assertion(Loaded == Prefix),
    (	Warn == true
    ->	assertion(Msgs = [warning-truncated_db(File, _, _)])
    ;	assertion(Msgs == [])
    ),
    % The repaired file must load silently and give the same result
    trap_messages,
    load_db(File, Loaded2),
    trapped_messages(Msgs2),
    assertion(Loaded2 == Loaded),
    assertion(Msgs2 == []),
    % and it must be possible to append to it
    add_fact(File, zus, 4),
    trap_messages,
    load_db(File, Loaded3),
    trapped_messages(Msgs3),
    append(Loaded, [zus-4], Expected),
    assertion(Loaded3 == Expected),
    assertion(Msgs3 == []).

% A file that does not hold a single valid term is not a persistent
% database and must not be truncated.

test(no_db, [ setup(( tmp_db(File),
		      db_bytes(File, "not a Prolog term")
		    )),
	      cleanup(delete_file(File)),
	      throws(error(syntax_error(_), _))
	    ]) :-
    load_db(File, _).

:- end_tests(persistency_recover).
