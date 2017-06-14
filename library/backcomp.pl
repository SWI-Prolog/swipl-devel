/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
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

:- module(backward_compatibility,
          [ '$arch'/2,
            '$version'/1,
            '$home'/1,
            '$argv'/1,
            '$set_prompt'/1,
            '$strip_module'/3,
            '$declare_module'/3,
            '$module'/2,
            at_initialization/1,        % :Goal
            displayq/1,
            displayq/2,
            sformat/2,                  % -String, +Fmt
            sformat/3,                  % -String, +Fmt, +Args
            concat/3,
            concat_atom/2,              % +List, -Atom
            concat_atom/3,              % +List, +Sep, -Atom
            '$apropos_match'/2,         % +Needle, +Hashstack
            read_clause/1,              % -Term
            read_clause/2,              % +Stream, -Term
            read_variables/2,           % -Term, -VariableNames
            read_variables/3,           % +Stream, -Term, -VariableNames
            read_pending_input/3,       % +Stream, -List, ?Tail
            feature/2,
            set_feature/2,
            substring/4,
            string_to_list/2,           % ?String, ?Codes
            string_to_atom/2,           % ?String, ?Atom
            flush/0,
            write_ln/1,                 % +Term
            proper_list/1,              % @Term
            free_variables/2,           % +Term, -Variables
            subsumes_chk/2,             % @Generic, @Specific
            subsumes/2,                 % @Generic, @Specific
            hash_term/2,                % +Term, -Hash
            checklist/2,                % :Goal, +List
            sublist/3,                  % :Goal, +List, -Sublist
            sumlist/2,                  % +List, -Sum
            convert_time/2,             % +Stamp, -String
            convert_time/8,             % +String, -YMDmhs.ms
            'C'/3,                      % +List, -Head, -Tail
            current_thread/2,           % ?Thread, ?Status
            current_mutex/3,            % ?Mutex, ?Owner, ?Count
            message_queue_size/2,       % +Queue, -TermsWaiting
            lock_predicate/2,           % +Name, +Arity
            unlock_predicate/2,         % +Name, +Arity
            current_module/2,           % ?Module, ?File
            export_list/2,              % +Module, -Exports
            setup_and_call_cleanup/3,   % :Setup, :Goal, :Cleanup
            setup_and_call_cleanup/4,   % :Setup, :Goal, ?Catcher, :Cleanup
            merge/3,                    % +List1, +List2, -Union
            merge_set/3,                % +Set1, +Set2, -Union
            index/1,                    % :Head
            hash/1,                     % :PI
            set_base_module/1,          % :Base
            eval_license/0,
            trie_insert_new/3		% +Trie, +Term, -Node
          ]).
:- use_module(apply,  [maplist/2]).
:- use_module(system, [lock_predicate/1, unlock_predicate/1]).
:- use_module(lists,  [sum_list/2]).

:- meta_predicate
    at_initialization(0),
    setup_and_call_cleanup(0,0,0),
    setup_and_call_cleanup(0,0,?,0),
    checklist(1, +),
    sublist(1, +, ?),
    index(:),
    hash(:),
    set_base_module(:).

/** <module> Backward compatibility

This library defines predicates that used to exist in older version of
SWI-Prolog, but are considered obsolete as there functionality is neatly
covered by new features. Most often, these constructs are superceeded by
ISO-standard compliant predicates.

Please also note the existence of   quintus.pl and edinburgh.pl for more
compatibility predicates.

@see    gxref/0 can be used to find files that import from
        library(backcomp) and thus reply on deprecated features.
*/

%!  '$arch'(-Architecture, -Version) is det.
%
%   @deprecated use current_prolog_flag(arch, Architecture)

'$arch'(Arch, unknown) :-
    current_prolog_flag(arch, Arch).

%!  '$version'(Version:integer) is det.
%
%   @deprecated use current_prolog_flag(version, Version)

'$version'(Version) :-
    current_prolog_flag(version, Version).

%!  '$home'(-SWIPrologDir) is det.
%
%   @deprecated use current_prolog_flag(home, SWIPrologDir)
%   @see file_search_path/2, absolute_file_name/3,  The Prolog home
%        directory is available through the alias =swi=.

'$home'(Home) :-
    current_prolog_flag(home, Home).

%!  '$argv'(-Argv:list) is det.
%
%   @deprecated use current_prolog_flag(os_argv, Argv) or
%   current_prolog_flag(argv, Argv)

'$argv'(Argv) :-
    current_prolog_flag(os_argv, Argv).

%!  '$set_prompt'(+Prompt) is det.
%
%   Set the prompt for the toplevel
%
%   @deprecated use set_prolog_flag(toplevel_prompt, Prompt).

'$set_prompt'(Prompt) :-
    (   is_list(Prompt)
    ->  Prompt0 = Prompt
    ;   atom_codes(Prompt, Prompt0)
    ),
    maplist(percent_to_tilde, Prompt0, Prompt1),
    atom_codes(Atom, Prompt1),
    set_prolog_flag(toplevel_prompt, Atom).

percent_to_tilde(0'%, 0'~) :- !.
percent_to_tilde(X, X).


%!  displayq(@Term) is det.
%!  displayq(+Stream, @Term) is det.
%
%   Write term ignoring operators and quote atoms.
%
%   @deprecated Use write_term/3 or write_canonical/2.

displayq(Term) :-
    write_term(Term, [ignore_ops(true),quoted(true)]).
displayq(Stream, Term) :-
    write_term(Stream, Term, [ignore_ops(true),quoted(true)]).


%!  sformat(-String, +Format, +Args) is det.
%!  sformat(-String, +Format) is det.
%
%   @deprecated Use format/3 as =|format(string(String), ...)|=

:- module_transparent sformat/2, sformat/3.

sformat(String, Format) :-
    format(string(String), Format, []).
sformat(String, Format, Arguments) :-
    format(string(String), Format, Arguments).

%!  concat(+Atom1, +Atom2, -Atom) is det.
%
%   @deprecated Use ISO atom_concat/3

concat(A, B, C) :-
    atom_concat(A, B, C).

%!  concat_atom(+List, -Atom) is det.
%
%   Concatenate a list of atomic values to an atom.
%
%   @deprecated Use atomic_list_concat/2 as proposed by the prolog
%               commons initiative.

concat_atom([A, B], C) :-
    !,
    atom_concat(A, B, C).
concat_atom(L, Atom) :-
    atomic_list_concat(L, Atom).


%!  concat_atom(+List, +Seperator, -Atom) is det.
%
%   Concatenate a list of atomic values to an atom, inserting Seperator
%   between each consecutive elements.
%
%   @deprecated Use atomic_list_concat/3 as proposed by the prolog
%               commons initiative.

concat_atom(L, Sep, Atom) :-
    atomic_list_concat(L, Sep, Atom).

%!  '$apropos_match'(+Needle, +Haystack) is semidet.
%
%   True if Needle is a sub atom of Haystack.  Ignores the case
%   of Haystack.

'$apropos_match'(Needle, Haystack) :-
    sub_atom_icasechk(Haystack, _, Needle).

%!  read_clause(-Term) is det.
%
%   @deprecated Use read_clause/3 or read_term/3.

read_clause(Term) :-
    read_clause(current_input, Term).

%!  read_clause(+Stream, -Term) is det.
%
%   @deprecated Use read_clause/3 or read_term/3.

read_clause(Stream, Term) :-
    read_clause(Stream, Term, [process_comment(false)]).

%!  read_variables(-Term, -Bindings) is det.
%!  read_variables(+In:stream, -Term, -Bindings) is det.
%
%   @deprecated Use ISO read_term/2 or read_term/3.

read_variables(Term, Vars) :-
    read_term(Term, [variable_names(Vars)]).

read_variables(Stream, Term, Vars) :-
    read_term(Stream, Term, [variable_names(Vars)]).

%!  read_pending_input(+Stream, -Codes, ?Tail) is det.
%
%   @deprecated Use read_pending_codes/3.

read_pending_input(Stream, Codes, Tail) :-
    read_pending_codes(Stream, Codes, Tail).

%!  feature(?Key, ?Value) is nondet.
%!  set_feature(+Key, @Term) is det.
%
%   Control Prolog flags.
%
%   @deprecated Use ISO current_prolog_flag/2 and set_prolog_flag/2.

feature(Key, Value) :-
    current_prolog_flag(Key, Value).

set_feature(Key, Value) :-
    set_prolog_flag(Key, Value).

%!  substring(+String, +Offset, +Length, -Sub)
%
%   Predecessor of sub_string using 1-based Offset.
%
%   @deprecated Use sub_string/5.

substring(String, Offset, Length, Sub) :-
    Offset0 is Offset - 1,
    sub_string(String, Offset0, Length, _After, Sub).

%!  string_to_list(?String, ?Codes) is det.
%
%   Bi-directional conversion between a string and a list of
%   character codes.
%
%   @deprecated Use string_codes/2.

string_to_list(String, Codes) :-
    string_codes(String, Codes).

%!  string_to_atom(?String, ?Atom) is det.
%
%   Bi-directional conversion between string and atom.
%
%   @deprecated     Use atom_string/2. Note that the order of the
%                   arguments is reversed.

string_to_atom(Atom, String) :-
    atom_string(String, Atom).

%!  flush is det.
%
%   @deprecated use ISO flush_output/0.

flush :-
    flush_output.

%!  write_ln(X) is det
%
%   @deprecated Use writeln(X).

write_ln(X) :-
    writeln(X).

%!  proper_list(+List)
%
%   Old SWI-Prolog predicate to check for a list that really ends
%   in a [].  There is not much use for the quick is_list, as in
%   most cases you want to process the list element-by-element anyway.
%
%   @deprecated Use ISO is_list/1.

proper_list(List) :-
    is_list(List).

%!  free_variables(+Term, -Variables)
%
%   Return  a  list  of  unbound  variables    in   Term.  The  name
%   term_variables/2 is more widely used.
%
%   @deprecated Use term_variables/2.

free_variables(Term, Variables) :-
    term_variables(Term, Variables).

%!  subsumes_chk(@Generic, @Specific)
%
%   True if Generic can be made equivalent to Specific without
%   changing Specific.
%
%   @deprecated Replace by subsumes_term/2.

subsumes_chk(Generic, Specific) :-
    subsumes_term(Generic, Specific).

%!  subsumes(+Generic, @Specific)
%
%   True  if  Generic  is  unified   to  Specific  without  changing
%   Specific.
%
%   @deprecated It turns out that calls to this predicate almost
%   always should have used subsumes_term/2.  Also the name is
%   misleading.  In case this is really needed, one is adviced to
%   follow subsumes_term/2 with an explicit unification.

subsumes(Generic, Specific) :-
    subsumes_term(Generic, Specific),
    Generic = Specific.

%!  hash_term(+Term, -Hash) is det.
%
%   If Term is ground, Hash is unified to an integer representing
%   a hash for Term.  Otherwise Hash is left unbound.
%
%   @deprecated Use term_hash/2.

hash_term(Term, Hash) :-
    term_hash(Term, Hash).

%!  checklist(:Goal, +List)
%
%   @deprecated Use maplist/2


checklist(Goal, List) :-
    maplist(Goal, List).

%!  sublist(:Goal, +List1, ?List2)
%
%   Succeeds if List2 unifies with a list holding those terms for wich
%   call(Goal, Elem) succeeds.
%
%   @deprecated Use include/3 from library(apply)
%   @compat DEC10 library

sublist(_, [], []) :- !.
sublist(Goal, [H|T], Sub) :-
    call(Goal, H),
    !,
    Sub = [H|R],
    sublist(Goal, T, R).
sublist(Goal, [_|T], R) :-
    sublist(Goal, T, R).

%!  sumlist(+List, -Sum) is det.
%
%   True when Sum is the list of all numbers in List.
%
%   @deprecated Use sum_list/2

sumlist(List, Sum) :-
    sum_list(List, Sum).

%!  '$strip_module'(+Term, -Module, -Plain)
%
%   This used to be an internal predicate.  It was added to the XPCE
%   compatibility library without $ and  since   then  used  at many
%   places. From 5.4.1 onwards strip_module/3 is  built-in and the $
%   variation is added here for compatibility.
%
%   @deprecated Use strip_module/3.

:- module_transparent
    '$strip_module'/3.

'$strip_module'(Term, Module, Plain) :-
    strip_module(Term, Module, Plain).

%!  '$module'(-OldTypeIn, +NewTypeIn)

'$module'(OldTypeIn, NewTypeIn) :-
    '$current_typein_module'(OldTypeIn),
    '$set_typein_module'(NewTypeIn).

%!  '$declare_module'(Module, File, Line)
%
%   Used in triple20 particle library. Should use a public interface

'$declare_module'(Module, File, Line) :-
    '$declare_module'(Module, user, user, File, Line, false).


%!  at_initialization(:Goal) is det.
%
%   Register goal only to be run if a saved state is restored.
%
%   @deprecated Use initialization(Goal, restore)

at_initialization(Goal) :-
    initialization(Goal, restore).

%!  convert_time(+Stamp, -String)
%
%   Convert  a time-stamp as  obtained though get_time/1 into a  textual
%   representation  using the C-library function ctime().  The  value is
%   returned  as a  SWI-Prolog string object  (see section  4.23).   See
%   also convert_time/8.
%
%   @deprecated Use format_time/3.


convert_time(Stamp, String) :-
    format_time(string(String), '%+', Stamp).

%!  convert_time(+Stamp, -Y, -Mon, -Day, -Hour, -Min, -Sec, -MilliSec)
%
%   Convert   a  time  stamp,   provided  by   get_time/1,   time_file/2,
%   etc.   Year is  unified with the year,  Month with the month  number
%   (January  is 1), Day  with the day of  the month (starting with  1),
%   Hour  with  the hour  of the  day (0--23),  Minute  with the  minute
%   (0--59).   Second with the  second (0--59) and MilliSecond with  the
%   milliseconds  (0--999).  Note that the latter might not  be accurate
%   or  might always be 0, depending  on the timing capabilities of  the
%   system.  See also convert_time/2.
%
%   @deprecated Use stamp_date_time/3.

convert_time(Stamp, Y, Mon, Day, Hour, Min, Sec, MilliSec) :-
    stamp_date_time(Stamp,
                    date(Y, Mon, Day,
                         Hour, Min, FSec,
                         _, _, _),
                    local),
    Sec is integer(float_integer_part(FSec)),
    MilliSec is integer(float_fractional_part(FSec)*1000).

%!  'C'(?List, ?Head, ?Tail) is det.
%
%   Used to be generated by DCG.  Some people appear to be using in
%   in normal code too.
%
%   @deprecated Do not use in normal code; DCG no longer generates it.

'C'([H|T], H, T).


%!  current_thread(?Thread, ?Status) is nondet.
%
%   @deprecated Replaced by thread_property/2

current_thread(Thread, Status) :-
    nonvar(Thread),
    !,
    catch(thread_property(Thread, status(Status)),
          error(existence_error(thread, _), _),
          fail).
current_thread(Thread, Status) :-
    thread_property(Thread, status(Status)).

%!  current_mutex(?Mutex, ?Owner, ?Count) is nondet.
%
%   @deprecated Replaced by mutex_property/2

current_mutex(Mutex, Owner, Count) :-
    nonvar(Mutex),
    !,
    catch(mutex_property(Mutex, status(Status)),
          error(existence_error(mutex, _), _),
          fail),
    map_mutex_status(Status, Owner, Count).
current_mutex(Mutex, Owner, Count) :-
    mutex_property(Mutex, status(Status)),
    map_mutex_status(Status, Owner, Count).

map_mutex_status(unlocked, [], 0).
map_mutex_status(locked(Owner, Count), Owner, Count).


%!  message_queue_size(+Queue, -Size) is det.
%
%   True if Queue holds Size terms.
%
%   @deprecated Please use message_queue_property(Queue, Size)

message_queue_size(Queue, Size) :-
    message_queue_property(Queue, size(Size)).

%!  lock_predicate(+Name, +Arity) is det.
%!  unlock_predicate(+Name, +Arity) is det.
%
%   @deprecated see lock_predicate/1 and unlock_predicate/1.

:- module_transparent
    lock_predicate/2,
    unlock_predicate/2.

lock_predicate(Name, Arity) :-
    lock_predicate(Name/Arity).

unlock_predicate(Name, Arity) :-
    unlock_predicate(Name/Arity).

%!  current_module(?Module, ?File) is nondet.
%
%   True if Module is a module loaded from File.
%
%   @deprecated Use module_property(Module, file(File))

current_module(Module, File) :-
    module_property(Module, file(File)).

%!  export_list(+Module, -List) is det.
%
%   Module exports the predicates of List.
%
%   @deprecated Use module_property(Module, exports(List))

export_list(Module, List) :-
    module_property(Module, exports(List)).

%!  setup_and_call_cleanup(:Setup, :Goal, :Cleanup).
%
%   Call Cleanup once after Goal is finished.
%
%   @deprecated Use setup_call_cleanup/3.

setup_and_call_cleanup(Setup, Goal, Cleanup) :-
    setup_call_cleanup(Setup, Goal, Cleanup).

%!  setup_and_call_cleanup(:Setup, :Goal, Catcher, :Cleanup).
%
%   Call Cleanup once after Goal is finished, with Catcher
%   unified to the reason
%
%   @deprecated Use setup_call_cleanup/3.

setup_and_call_cleanup(Setup, Goal, Catcher, Cleanup) :-
    setup_call_catcher_cleanup(Setup, Goal, Catcher,Cleanup).

%!  merge_set(+Set1, +Set2, -Set3)
%
%   Merge the ordered sets Set1 and  Set2   into  a  new ordered set
%   without duplicates.
%
%   @deprecated     New code should use ord_union/3 from
%                   library(ordsets)

merge_set([], L, L) :- !.
merge_set(L, [], L) :- !.
merge_set([H1|T1], [H2|T2], [H1|R]) :- H1 @< H2, !, merge_set(T1, [H2|T2], R).
merge_set([H1|T1], [H2|T2], [H2|R]) :- H1 @> H2, !, merge_set([H1|T1], T2, R).
merge_set([H1|T1], [H2|T2], [H1|R]) :- H1 == H2,    merge_set(T1, T2, R).


%!  merge(+List1, +List2, -List3)
%
%   Merge the ordered sets List1 and List2 into a new ordered  list.
%   Duplicates are not removed and their order is maintained.
%
%   @deprecated     The name of this predicate is far too general for
%                   a rather specific function.

merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([H1|T1], [H2|T2], [H|R]) :-
    (   H1 @=< H2
    ->  H = H1,
        merge(T1, [H2|T2], R)
    ;   H = H2,
        merge([H1|T1], T2, R)
    ).

%!  index(:Head) is det.
%
%   Prepare the predicate  indicated  by   Head  for  multi-argument
%   indexing.
%
%   @deprecated     As of version 5.11.29, SWI-Prolog performs
%                   just-in-time indexing on all arguments.

index(Head) :-
    print_message(warning, decl_no_effect(index(Head))).

%!  hash(:PredInd) is det.
%
%   Demands PredInd to be  indexed  using   a  hash-table.  This  is
%   handled dynamically.

hash(PI) :-
    print_message(warning, decl_no_effect(hash(PI))).

%!  set_base_module(:Base) is det.
%
%   Set the default module from whic we inherit.
%
%   @deprecated Equivalent to set_module(base(Base)).

set_base_module(M:Base) :-
    set_module(M:base(Base)).

%!  eval_license is det.
%
%   @deprecated Equivalent to license/0

eval_license :-
    license.

%!  trie_insert_new(+Trie, +Term, -Handle) is semidet.
%
%   @deprecated use trie_insert/4.

trie_insert_new(Trie, Term, Handle) :-
    trie_insert(Trie, Term, [], Handle).
