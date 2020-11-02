/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module('$syspreds',
          [ leash/1,
            visible/1,
            style_check/1,
            (spy)/1,
            (nospy)/1,
            nospyall/0,
            debugging/0,
            flag/3,
            atom_prefix/2,
            dwim_match/2,
            source_file_property/2,
            source_file/1,
            source_file/2,
            unload_file/1,
            exists_source/1,                    % +Spec
            exists_source/2,                    % +Spec, -Path
            use_foreign_library/1,		% :FileSpec
            use_foreign_library/2,		% :FileSpec, +Install
            prolog_load_context/2,
            stream_position_data/3,
            current_predicate/2,
            '$defined_predicate'/1,
            predicate_property/2,
            '$predicate_property'/2,
            (dynamic)/2,                        % :Predicates, +Options
            clause_property/2,
            current_module/1,                   % ?Module
            module_property/2,                  % ?Module, ?Property
            module/1,                           % +Module
            current_trie/1,                     % ?Trie
            trie_property/2,                    % ?Trie, ?Property
            working_directory/2,                % -OldDir, +NewDir
            shell/1,                            % +Command
            on_signal/3,
            current_signal/3,
            open_shared_object/2,
            open_shared_object/3,
            format/1,
            garbage_collect/0,
            set_prolog_stack/2,
            prolog_stack_property/2,
            absolute_file_name/2,
            tmp_file_stream/3,                  % +Enc, -File, -Stream
            call_with_depth_limit/3,            % :Goal, +Limit, -Result
            call_with_inference_limit/3,        % :Goal, +Limit, -Result
            numbervars/3,                       % +Term, +Start, -End
            term_string/3,                      % ?Term, ?String, +Options
            nb_setval/2,                        % +Var, +Value
            thread_create/2,                    % :Goal, -Id
            thread_join/1,                      % +Id
            set_prolog_gc_thread/1,		% +Status

            '$wrap_predicate'/5                 % :Head, +Name, -Closure, -Wrapped, +Body
          ]).

:- meta_predicate
    dynamic(:, +),
    use_foreign_library(:),
    use_foreign_library(:, +).


                /********************************
                *           DEBUGGER            *
                *********************************/

%!  map_bits(:Pred, +Modify, +OldBits, -NewBits)

:- meta_predicate
    map_bits(2, +, +, -).

map_bits(_, Var, _, _) :-
    var(Var),
    !,
    '$instantiation_error'(Var).
map_bits(_, [], Bits, Bits) :- !.
map_bits(Pred, [H|T], Old, New) :-
    map_bits(Pred, H, Old, New0),
    map_bits(Pred, T, New0, New).
map_bits(Pred, +Name, Old, New) :-     % set a bit
    !,
    bit(Pred, Name, Bits),
    !,
    New is Old \/ Bits.
map_bits(Pred, -Name, Old, New) :-     % clear a bit
    !,
    bit(Pred, Name, Bits),
    !,
    New is Old /\ (\Bits).
map_bits(Pred, ?(Name), Old, Old) :-   % ask a bit
    !,
    bit(Pred, Name, Bits),
    Old /\ Bits > 0.
map_bits(_, Term, _, _) :-
    '$type_error'('+|-|?(Flag)', Term).

bit(Pred, Name, Bits) :-
    call(Pred, Name, Bits),
    !.
bit(_:Pred, Name, _) :-
    '$domain_error'(Pred, Name).

:- public port_name/2.                  % used by library(test_cover)

port_name(      call, 2'000000001).
port_name(      exit, 2'000000010).
port_name(      fail, 2'000000100).
port_name(      redo, 2'000001000).
port_name(     unify, 2'000010000).
port_name(     break, 2'000100000).
port_name(  cut_call, 2'001000000).
port_name(  cut_exit, 2'010000000).
port_name( exception, 2'100000000).
port_name(       cut, 2'011000000).
port_name(       all, 2'000111111).
port_name(      full, 2'000101111).
port_name(      half, 2'000101101).     % '

leash(Ports) :-
    '$leash'(Old, Old),
    map_bits(port_name, Ports, Old, New),
    '$leash'(_, New).

visible(Ports) :-
    '$visible'(Old, Old),
    map_bits(port_name, Ports, Old, New),
    '$visible'(_, New).

style_name(atom,            0x0001) :-
    print_message(warning, decl_no_effect(style_check(atom))).
style_name(singleton,       0x0042).            % semantic and syntactic
style_name(discontiguous,   0x0008).
style_name(charset,         0x0020).
style_name(no_effect,       0x0080).
style_name(var_branches,    0x0100).

%!  style_check(+Spec) is nondet.

style_check(Var) :-
    var(Var),
    !,
    '$instantiation_error'(Var).
style_check(?(Style)) :-
    !,
    (   var(Style)
    ->  enum_style_check(Style)
    ;   enum_style_check(Style)
    ->  true
    ).
style_check(Spec) :-
    '$style_check'(Old, Old),
    map_bits(style_name, Spec, Old, New),
    '$style_check'(_, New).

enum_style_check(Style) :-
    '$style_check'(Bits, Bits),
    style_name(Style, Bit),
    Bit /\ Bits =\= 0.


%!  prolog:debug_control_hook(+Action)
%
%   Allow user-hooks in the Prolog debugger interaction.  See the calls
%   below for the provided hooks.  We use a single predicate with action
%   argument to avoid an uncontrolled poliferation of hooks.

:- multifile
    prolog:debug_control_hook/1.    % +Action

:- meta_predicate
    spy(:),
    nospy(:).

%!  spy(:Spec) is det.
%!  nospy(:Spec) is det.
%!  nospyall is det.
%
%   Set/clear spy-points. A successfully set or cleared spy-point is
%   reported using print_message/2, level  =informational=, with one
%   of the following terms, where Spec is of the form M:Head.
%
%       - spy(Spec)
%       - nospy(Spec)
%
%   @see    spy/1 and nospy/1 call the hook prolog:debug_control_hook/1
%           to allow for alternative specifications of the thing to
%           debug.

spy(_:X) :-
    var(X),
    throw(error(instantiation_error, _)).
spy(_:[]) :- !.
spy(M:[H|T]) :-
    !,
    spy(M:H),
    spy(M:T).
spy(Spec) :-
    notrace(prolog:debug_control_hook(spy(Spec))),
    !.
spy(Spec) :-
    '$find_predicate'(Spec, Preds),
    '$member'(PI, Preds),
        pi_to_head(PI, Head),
        '$define_predicate'(Head),
        '$spy'(Head),
    fail.
spy(_).

nospy(_:X) :-
    var(X),
    throw(error(instantiation_error, _)).
nospy(_:[]) :- !.
nospy(M:[H|T]) :-
    !,
    nospy(M:H),
    nospy(M:T).
nospy(Spec) :-
    notrace(prolog:debug_control_hook(nospy(Spec))),
    !.
nospy(Spec) :-
    '$find_predicate'(Spec, Preds),
    '$member'(PI, Preds),
         pi_to_head(PI, Head),
        '$nospy'(Head),
    fail.
nospy(_).

nospyall :-
    notrace(prolog:debug_control_hook(nospyall)),
    fail.
nospyall :-
    spy_point(Head),
        '$nospy'(Head),
    fail.
nospyall.

pi_to_head(M:PI, M:Head) :-
    !,
    pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).

%!  debugging is det.
%
%   Report current status of the debugger.

debugging :-
    notrace(prolog:debug_control_hook(debugging)),
    !.
debugging :-
    current_prolog_flag(debug, true),
    !,
    print_message(informational, debugging(on)),
    findall(H, spy_point(H), SpyPoints),
    print_message(informational, spying(SpyPoints)).
debugging :-
    print_message(informational, debugging(off)).

spy_point(Module:Head) :-
    current_predicate(_, Module:Head),
    '$get_predicate_attribute'(Module:Head, spy, 1),
    \+ predicate_property(Module:Head, imported_from(_)).

%!  flag(+Name, -Old, +New) is det.
%
%   True when Old is the current value associated with the flag Name
%   and New has become the new value.

flag(Name, Old, New) :-
    Old == New,
    !,
    get_flag(Name, Old).
flag(Name, Old, New) :-
    with_mutex('$flag', update_flag(Name, Old, New)).

update_flag(Name, Old, New) :-
    get_flag(Name, Old),
    (   atom(New)
    ->  set_flag(Name, New)
    ;   Value is New,
        set_flag(Name, Value)
    ).


                /********************************
                *             ATOMS             *
                *********************************/

dwim_match(A1, A2) :-
    dwim_match(A1, A2, _).

atom_prefix(Atom, Prefix) :-
    sub_atom(Atom, 0, _, _, Prefix).


                /********************************
                *             SOURCE            *
                *********************************/

%!  source_file(-File) is nondet.
%!  source_file(+File) is semidet.
%
%   True if File is loaded into  Prolog.   If  File is unbound it is
%   bound to the canonical name for it. If File is bound it succeeds
%   if the canonical name  as   defined  by  absolute_file_name/2 is
%   known as a loaded filename.
%
%   Note that Time = 0.0 is used by  PlDoc and other code that needs
%   to create a file record without being interested in the time.

source_file(File) :-
    (   current_prolog_flag(access_level, user)
    ->  Level = user
    ;   true
    ),
    (   ground(File)
    ->  (   '$time_source_file'(File, Time, Level)
        ;   absolute_file_name(File, Abs),
            '$time_source_file'(Abs, Time, Level)
        ), !
    ;   '$time_source_file'(File, Time, Level)
    ),
    Time > 0.0.

%!  source_file(+Head, -File) is semidet.
%!  source_file(?Head, ?File) is nondet.
%
%   True when Head is a predicate owned by File.

:- meta_predicate source_file(:, ?).

source_file(M:Head, File) :-
    nonvar(M), nonvar(Head),
    !,
    (   '$c_current_predicate'(_, M:Head),
        predicate_property(M:Head, multifile)
    ->  multi_source_files(M:Head, Files),
        '$member'(File, Files)
    ;   '$source_file'(M:Head, File)
    ).
source_file(M:Head, File) :-
    (   nonvar(File)
    ->  true
    ;   source_file(File)
    ),
    '$source_file_predicates'(File, Predicates),
    '$member'(M:Head, Predicates).

:- thread_local found_src_file/1.

multi_source_files(Head, Files) :-
    call_cleanup(
        findall(File, multi_source_file(Head, File), Files),
        retractall(found_src_file(_))).

multi_source_file(Head, File) :-
    nth_clause(Head, _, Clause),
    clause_property(Clause, source(File)),
    \+ found_src_file(File),
    asserta(found_src_file(File)).


%!  source_file_property(?File, ?Property) is nondet.
%
%   True if Property is a property of the loaded source-file File.

source_file_property(File, P) :-
    nonvar(File),
    !,
    canonical_source_file(File, Path),
    property_source_file(P, Path).
source_file_property(File, P) :-
    property_source_file(P, File).

property_source_file(modified(Time), File) :-
    '$time_source_file'(File, Time, user).
property_source_file(source(Source), File) :-
    (   '$source_file_property'(File, from_state, true)
    ->  Source = state
    ;   '$source_file_property'(File, resource, true)
    ->  Source = resource
    ;   Source = file
    ).
property_source_file(module(M), File) :-
    (   nonvar(M)
    ->  '$current_module'(M, File)
    ;   nonvar(File)
    ->  '$current_module'(ML, File),
        (   atom(ML)
        ->  M = ML
        ;   '$member'(M, ML)
        )
    ;   '$current_module'(M, File)
    ).
property_source_file(load_context(Module, Location, Options), File) :-
    '$time_source_file'(File, _, user),
    clause(system:'$load_context_module'(File, Module, Options), true, Ref),
    (   clause_property(Ref, file(FromFile)),
        clause_property(Ref, line_count(FromLine))
    ->  Location = FromFile:FromLine
    ;   Location = user
    ).
property_source_file(includes(Master, Stamp), File) :-
    system:'$included'(File, _Line, Master, Stamp).
property_source_file(included_in(Master, Line), File) :-
    system:'$included'(Master, Line, File, _).
property_source_file(derived_from(DerivedFrom, Stamp), File) :-
    system:'$derived_source'(File, DerivedFrom, Stamp).
property_source_file(reloading, File) :-
    source_file(File),
    '$source_file_property'(File, reloading, true).
property_source_file(load_count(Count), File) :-
    source_file(File),
    '$source_file_property'(File, load_count, Count).
property_source_file(number_of_clauses(Count), File) :-
    source_file(File),
    '$source_file_property'(File, number_of_clauses, Count).


%!  canonical_source_file(+Spec, -File) is semidet.
%
%   File is the canonical representation of the source-file Spec.

canonical_source_file(Spec, File) :-
    atom(Spec),
    '$time_source_file'(Spec, _, _),
    !,
    File = Spec.
canonical_source_file(Spec, File) :-
    system:'$included'(_Master, _Line, Spec, _),
    !,
    File = Spec.
canonical_source_file(Spec, File) :-
    absolute_file_name(Spec,
                           [ file_type(prolog),
                             access(read),
                             file_errors(fail)
                           ],
                           File),
    source_file(File).


%!  exists_source(+Source) is semidet.
%!  exists_source(+Source, -Path) is semidet.
%
%   True if Source (a term  valid   for  load_files/2) exists. Fails
%   without error if this is not the case. The predicate is intended
%   to be used with  :-  if,  as   in  the  example  below. See also
%   source_exports/2.
%
%   ```
%   :- if(exists_source(library(error))).
%   :- use_module_library(error).
%   :- endif.
%   ```

exists_source(Source) :-
    exists_source(Source, _Path).

exists_source(Source, Path) :-
    absolute_file_name(Source, Path,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]).


%!  prolog_load_context(+Key, -Value)
%
%   Provides context information for  term_expansion and directives.
%   Note  that  only  the  line-number  info    is   valid  for  the
%   '$stream_position'. Largely Quintus compatible.

prolog_load_context(module, Module) :-
    '$current_source_module'(Module).
prolog_load_context(file, File) :-
    input_file(File).
prolog_load_context(source, F) :-       % SICStus compatibility
    input_file(F0),
    '$input_context'(Context),
    '$top_file'(Context, F0, F).
prolog_load_context(stream, S) :-
    (   system:'$load_input'(_, S0)
    ->  S = S0
    ).
prolog_load_context(directory, D) :-
    input_file(F),
    file_directory_name(F, D).
prolog_load_context(dialect, D) :-
    current_prolog_flag(emulated_dialect, D).
prolog_load_context(term_position, TermPos) :-
    source_location(_, L),
    (   nb_current('$term_position', Pos),
        compound(Pos),              % actually set
        stream_position_data(line_count, Pos, L)
    ->  TermPos = Pos
    ;   TermPos = '$stream_position'(0,L,0,0)
    ).
prolog_load_context(script, Bool) :-
    (   '$toplevel':loaded_init_file(script, Path),
        input_file(File),
        same_file(File, Path)
    ->  Bool = true
    ;   Bool = false
    ).
prolog_load_context(variable_names, Bindings) :-
    nb_current('$variable_names', Bindings).
prolog_load_context(term, Term) :-
    nb_current('$term', Term).
prolog_load_context(reloading, true) :-
    prolog_load_context(source, F),
    '$source_file_property'(F, reloading, true).

input_file(File) :-
    (   system:'$load_input'(_, Stream)
    ->  stream_property(Stream, file_name(File))
    ),
    !.
input_file(File) :-
    source_location(File, _).


%!  unload_file(+File) is det.
%
%   Remove all traces of loading file.

:- dynamic system:'$resolved_source_path'/2.

unload_file(File) :-
    (   canonical_source_file(File, Path)
    ->  '$unload_file'(Path),
        retractall(system:'$resolved_source_path'(_, Path))
    ;   true
    ).

		 /*******************************
		 *      FOREIGN LIBRARIES	*
		 *******************************/

%!  use_foreign_library(+FileSpec) is det.
%!  use_foreign_library(+FileSpec, +Entry:atom) is det.
%
%   Load and install a foreign   library as load_foreign_library/1,2
%   and register the installation using   initialization/2  with the
%   option =now=. This is similar to using:
%
%     ==
%     :- initialization(load_foreign_library(foreign(mylib))).
%     ==
%
%   but using the initialization/1 wrapper causes  the library to be
%   loaded _after_ loading of  the  file   in  which  it  appears is
%   completed,  while  use_foreign_library/1  loads    the   library
%   _immediately_. I.e. the  difference  is   only  relevant  if the
%   remainder of the file uses functionality of the C-library.

use_foreign_library(FileSpec) :-
    ensure_shlib,
    initialization(shlib:load_foreign_library(FileSpec), now).

use_foreign_library(FileSpec, Entry) :-
    ensure_shlib,
    initialization(shlib:load_foreign_library(FileSpec, Entry), now).

ensure_shlib :-
    '$get_predicate_attribute'(shlib:load_foreign_library(_), defined, 1),
    '$get_predicate_attribute'(shlib:load_foreign_library(_,_), defined, 1),
    !.
ensure_shlib :-
    use_module(library(shlib), []).


                 /*******************************
                 *            STREAMS           *
                 *******************************/

%!  stream_position_data(?Field, +Pos, ?Date)
%
%   Extract values from stream position objects. '$stream_position' is
%   of the format '$stream_position'(Byte, Char, Line, LinePos)

stream_position_data(Prop, Term, Value) :-
    nonvar(Prop),
    !,
    (   stream_position_field(Prop, Pos)
    ->  arg(Pos, Term, Value)
    ;   throw(error(domain_error(stream_position_data, Prop)))
    ).
stream_position_data(Prop, Term, Value) :-
    stream_position_field(Prop, Pos),
    arg(Pos, Term, Value).

stream_position_field(char_count,    1).
stream_position_field(line_count,    2).
stream_position_field(line_position, 3).
stream_position_field(byte_count,    4).


                 /*******************************
                 *            CONTROL           *
                 *******************************/

%!  call_with_depth_limit(:Goal, +DepthLimit, -Result)
%
%   Try to proof Goal, but fail on any branch exceeding the indicated
%   depth-limit.  Unify Result with the maximum-reached limit on success,
%   depth_limit_exceeded if the limit was exceeded and fails otherwise.

:- meta_predicate
    call_with_depth_limit(0, +, -).

call_with_depth_limit(G, Limit, Result) :-
    '$depth_limit'(Limit, OLimit, OReached),
    (   catch(G, E, '$depth_limit_except'(OLimit, OReached, E)),
        '$depth_limit_true'(Limit, OLimit, OReached, Result, Det),
        ( Det == ! -> ! ; true )
    ;   '$depth_limit_false'(OLimit, OReached, Result)
    ).

%!  call_with_inference_limit(:Goal, +InferenceLimit, -Result)
%
%   Equivalent to call(Goal),  but  poses  a   limit  on  the  number of
%   inferences. If this  limit  is  reached,   Result  is  unified  with
%   `inference_limit_exceeded`, otherwise Result is unified  with `!` if
%   Goal succeeded without a choicepoint and `true` otherwise.
%
%   Note that we perform calls in  system to avoid auto-importing, which
%   makes raiseInferenceLimitException() fail  to   recognise  that  the
%   exception happens in the overhead.

:- meta_predicate
    call_with_inference_limit(0, +, -).

call_with_inference_limit(G, Limit, Result) :-
    '$inference_limit'(Limit, OLimit),
    (   catch(G, Except,
              system:'$inference_limit_except'(OLimit, Except, Result0)),
        (   Result0 == inference_limit_exceeded
        ->  !
        ;   system:'$inference_limit_true'(Limit, OLimit, Result0),
            ( Result0 == ! -> ! ; true )
        ),
        Result = Result0
    ;   system:'$inference_limit_false'(OLimit)
    ).


                /********************************
                *           DATA BASE           *
                *********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The predicate current_predicate/2 is   a  difficult subject since  the
introduction  of defaulting     modules   and   dynamic     libraries.
current_predicate/2 is normally  called with instantiated arguments to
verify some  predicate can   be called without trapping   an undefined
predicate.  In this case we must  perform the search algorithm used by
the prolog system itself.

If the pattern is not fully specified, we only generate the predicates
actually available in this  module.   This seems the best for listing,
etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- meta_predicate
    current_predicate(?, :),
    '$defined_predicate'(:).

current_predicate(Name, Module:Head) :-
    (var(Module) ; var(Head)),
    !,
    generate_current_predicate(Name, Module, Head).
current_predicate(Name, Term) :-
    '$c_current_predicate'(Name, Term),
    '$defined_predicate'(Term),
    !.
current_predicate(Name, Module:Head) :-
    default_module(Module, DefModule),
    '$c_current_predicate'(Name, DefModule:Head),
    '$defined_predicate'(DefModule:Head),
    !.
current_predicate(Name, Module:Head) :-
    '$autoload':autoload_in(Module, general),
    \+ current_prolog_flag(Module:unknown, fail),
    (   compound(Head)
    ->  compound_name_arity(Head, Name, Arity)
    ;   Name = Head, Arity = 0
    ),
    '$find_library'(Module, Name, Arity, _LoadModule, _Library),
    !.

generate_current_predicate(Name, Module, Head) :-
    current_module(Module),
    QHead = Module:Head,
    '$c_current_predicate'(Name, QHead),
    '$get_predicate_attribute'(QHead, defined, 1).

'$defined_predicate'(Head) :-
    '$get_predicate_attribute'(Head, defined, 1),
    !.

%!  predicate_property(?Predicate, ?Property) is nondet.
%
%   True when Property is a property of Predicate.

:- meta_predicate
    predicate_property(:, ?).

:- multifile
    '$predicate_property'/2.

:- '$iso'(predicate_property/2).

predicate_property(Pred, Property) :-           % Mode ?,+
    nonvar(Property),
    !,
    property_predicate(Property, Pred).
predicate_property(Pred, Property) :-           % Mode +,-
    define_or_generate(Pred),
    '$predicate_property'(Property, Pred).

%!  property_predicate(+Property, ?Pred)
%
%   First handle the special  cases  that   are  not  about querying
%   normally  defined  predicates:   =undefined=,    =visible=   and
%   =autoload=, followed by the generic case.

property_predicate(undefined, Pred) :-
    !,
    Pred = Module:Head,
    current_module(Module),
    '$c_current_predicate'(_, Pred),
    \+ '$defined_predicate'(Pred),          % Speed up a bit
    \+ current_predicate(_, Pred),
    goal_name_arity(Head, Name, Arity),
    \+ system_undefined(Module:Name/Arity).
property_predicate(visible, Pred) :-
    !,
    visible_predicate(Pred).
property_predicate(autoload(File), Head) :-
    !,
    \+ current_prolog_flag(autoload, false),
    '$autoload':autoloadable(Head, File).
property_predicate(implementation_module(IM), M:Head) :-
    !,
    atom(M),
    (   default_module(M, DM),
        '$get_predicate_attribute'(DM:Head, defined, 1)
    ->  (   '$get_predicate_attribute'(DM:Head, imported, ImportM)
        ->  IM = ImportM
        ;   IM = M
        )
    ;   \+ current_prolog_flag(M:unknown, fail),
        goal_name_arity(Head, Name, Arity),
        '$find_library'(_, Name, Arity, LoadModule, _File)
    ->  IM = LoadModule
    ;   M = IM
    ).
property_predicate(iso, _:Head) :-
    callable(Head),
    !,
    goal_name_arity(Head, Name, Arity),
    current_predicate(system:Name/Arity),
    '$predicate_property'(iso, system:Head).
property_predicate(built_in, Module:Head) :-
    callable(Head),
    !,
    goal_name_arity(Head, Name, Arity),
    current_predicate(Module:Name/Arity),
    '$predicate_property'(built_in, Module:Head).
property_predicate(Property, Pred) :-
    define_or_generate(Pred),
    '$predicate_property'(Property, Pred).

goal_name_arity(Head, Name, Arity) :-
    compound(Head),
    !,
    compound_name_arity(Head, Name, Arity).
goal_name_arity(Head, Head, 0).


%!  define_or_generate(+Head) is semidet.
%!  define_or_generate(-Head) is nondet.
%
%   If the predicate is known, try to resolve it. Otherwise generate
%   the known predicate, but do not try to (auto)load the predicate.

define_or_generate(M:Head) :-
    callable(Head),
    atom(M),
    '$get_predicate_attribute'(M:Head, defined, 1),
    !.
define_or_generate(M:Head) :-
    callable(Head),
    nonvar(M), M \== system,
    !,
    '$define_predicate'(M:Head).
define_or_generate(Pred) :-
    current_predicate(_, Pred),
    '$define_predicate'(Pred).


'$predicate_property'(interpreted, Pred) :-
    '$get_predicate_attribute'(Pred, foreign, 0).
'$predicate_property'(visible, Pred) :-
    '$get_predicate_attribute'(Pred, defined, 1).
'$predicate_property'(built_in, Pred) :-
    '$get_predicate_attribute'(Pred, system, 1).
'$predicate_property'(exported, Pred) :-
    '$get_predicate_attribute'(Pred, exported, 1).
'$predicate_property'(public, Pred) :-
    '$get_predicate_attribute'(Pred, public, 1).
'$predicate_property'(non_terminal, Pred) :-
    '$get_predicate_attribute'(Pred, non_terminal, 1).
'$predicate_property'(foreign, Pred) :-
    '$get_predicate_attribute'(Pred, foreign, 1).
'$predicate_property'((dynamic), Pred) :-
    '$get_predicate_attribute'(Pred, (dynamic), 1).
'$predicate_property'((static), Pred) :-
    '$get_predicate_attribute'(Pred, (dynamic), 0).
'$predicate_property'((volatile), Pred) :-
    '$get_predicate_attribute'(Pred, (volatile), 1).
'$predicate_property'((thread_local), Pred) :-
    '$get_predicate_attribute'(Pred, (thread_local), 1).
'$predicate_property'((multifile), Pred) :-
    '$get_predicate_attribute'(Pred, (multifile), 1).
'$predicate_property'(imported_from(Module), Pred) :-
    '$get_predicate_attribute'(Pred, imported, Module).
'$predicate_property'(transparent, Pred) :-
    '$get_predicate_attribute'(Pred, transparent, 1).
'$predicate_property'(meta_predicate(Pattern), Pred) :-
    '$get_predicate_attribute'(Pred, meta_predicate, Pattern).
'$predicate_property'(file(File), Pred) :-
    '$get_predicate_attribute'(Pred, file, File).
'$predicate_property'(line_count(LineNumber), Pred) :-
    '$get_predicate_attribute'(Pred, line_count, LineNumber).
'$predicate_property'(notrace, Pred) :-
    '$get_predicate_attribute'(Pred, trace, 0).
'$predicate_property'(nodebug, Pred) :-
    '$get_predicate_attribute'(Pred, hide_childs, 1).
'$predicate_property'(spying, Pred) :-
    '$get_predicate_attribute'(Pred, spy, 1).
'$predicate_property'(number_of_clauses(N), Pred) :-
    '$get_predicate_attribute'(Pred, number_of_clauses, N).
'$predicate_property'(number_of_rules(N), Pred) :-
    '$get_predicate_attribute'(Pred, number_of_rules, N).
'$predicate_property'(last_modified_generation(Gen), Pred) :-
    '$get_predicate_attribute'(Pred, last_modified_generation, Gen).
'$predicate_property'(indexed(Indices), Pred) :-
    '$get_predicate_attribute'(Pred, indexed, Indices).
'$predicate_property'(noprofile, Pred) :-
    '$get_predicate_attribute'(Pred, noprofile, 1).
'$predicate_property'(iso, Pred) :-
    '$get_predicate_attribute'(Pred, iso, 1).
'$predicate_property'(quasi_quotation_syntax, Pred) :-
    '$get_predicate_attribute'(Pred, quasi_quotation_syntax, 1).
'$predicate_property'(defined, Pred) :-
    '$get_predicate_attribute'(Pred, defined, 1).
'$predicate_property'(tabled, Pred) :-
    '$get_predicate_attribute'(Pred, tabled, 1).
'$predicate_property'(tabled(Flag), Pred) :-
    '$get_predicate_attribute'(Pred, tabled, 1),
    table_flag(Flag, Pred).
'$predicate_property'(incremental, Pred) :-
    '$get_predicate_attribute'(Pred, incremental, 1).
'$predicate_property'(abstract(N), Pred) :-
    '$get_predicate_attribute'(Pred, abstract, N).
'$predicate_property'(size(Bytes), Pred) :-
    '$get_predicate_attribute'(Pred, size, Bytes).

system_undefined(user:prolog_trace_interception/4).
system_undefined(user:prolog_exception_hook/4).
system_undefined(system:'$c_call_prolog'/0).
system_undefined(system:window_title/2).

table_flag(variant, Pred) :-
    '$tbl_implementation'(Pred, M:Head),
    M:'$tabled'(Head, variant).
table_flag(subsumptive, Pred) :-
    '$tbl_implementation'(Pred, M:Head),
    M:'$tabled'(Head, subsumptive).
table_flag(shared, Pred) :-
    '$get_predicate_attribute'(Pred, tshared, 1).
table_flag(incremental, Pred) :-
    '$get_predicate_attribute'(Pred, incremental, 1).
table_flag(subgoal_abstract(N), Pred) :-
    '$get_predicate_attribute'(Pred, subgoal_abstract, N).
table_flag(answer_abstract(N), Pred) :-
    '$get_predicate_attribute'(Pred, subgoal_abstract, N).
table_flag(subgoal_abstract(N), Pred) :-
    '$get_predicate_attribute'(Pred, max_answers, N).


%!  visible_predicate(:Head) is nondet.
%
%   True when Head can be called without raising an existence error.
%   This implies it is defined,  can   be  inherited  from a default
%   module or can be autoloaded.

visible_predicate(Pred) :-
    Pred = M:Head,
    current_module(M),
    (   callable(Head)
    ->  (   '$get_predicate_attribute'(Pred, defined, 1)
        ->  true
        ;   \+ current_prolog_flag(M:unknown, fail),
            functor(Head, Name, Arity),
            '$find_library'(M, Name, Arity, _LoadModule, _Library)
        )
    ;   setof(PI, visible_in_module(M, PI), PIs),
        '$member'(Name/Arity, PIs),
        functor(Head, Name, Arity)
    ).

visible_in_module(M, Name/Arity) :-
    default_module(M, DefM),
    DefHead = DefM:Head,
    '$c_current_predicate'(_, DefHead),
    '$get_predicate_attribute'(DefHead, defined, 1),
    \+ hidden_system_predicate(Head),
    functor(Head, Name, Arity).
visible_in_module(_, Name/Arity) :-
    '$in_library'(Name, Arity, _).

hidden_system_predicate(Head) :-
    functor(Head, Name, _),
    atom(Name),                     % Avoid [].
    sub_atom(Name, 0, _, _, $),
    \+ current_prolog_flag(access_level, system).


%!  clause_property(+ClauseRef, ?Property) is nondet.
%
%   Provide information on individual clauses.  Defined properties
%   are:
%
%       * line_count(-Line)
%       Line from which the clause is loaded.
%       * file(-File)
%       File from which the clause is loaded.
%       * source(-File)
%       File that `owns' the clause: reloading this file wipes
%       the clause.
%       * fact
%       Clause has body =true=.
%       * erased
%       Clause was erased.
%       * predicate(:PI)
%       Predicate indicator of the predicate this clause belongs
%       to.  Can be used to find the predicate of erased clauses.
%       * module(-M)
%       Module context in which the clause was compiled.

clause_property(Clause, Property) :-
    '$clause_property'(Property, Clause).

'$clause_property'(line_count(LineNumber), Clause) :-
    '$get_clause_attribute'(Clause, line_count, LineNumber).
'$clause_property'(file(File), Clause) :-
    '$get_clause_attribute'(Clause, file, File).
'$clause_property'(source(File), Clause) :-
    '$get_clause_attribute'(Clause, owner, File).
'$clause_property'(size(Bytes), Clause) :-
    '$get_clause_attribute'(Clause, size, Bytes).
'$clause_property'(fact, Clause) :-
    '$get_clause_attribute'(Clause, fact, true).
'$clause_property'(erased, Clause) :-
    '$get_clause_attribute'(Clause, erased, true).
'$clause_property'(predicate(PI), Clause) :-
    '$get_clause_attribute'(Clause, predicate_indicator, PI).
'$clause_property'(module(M), Clause) :-
    '$get_clause_attribute'(Clause, module, M).

%!  dynamic(:Predicates, +Options) is det.
%
%   Define a predicate as dynamic with optionally additional properties.
%   Defined options are:
%
%     - incremental(+Bool)
%     - abstract(+Level)
%     - multifile(+Bool)
%     - discontiguous(+Bool)
%     - thread(+Mode)
%     - volatile(+Bool)

dynamic(M:Predicates, Options) :-
    '$must_be'(list, Predicates),
    options_properties(Options, Props),
    set_pprops(Predicates, M, [dynamic|Props]).

set_pprops([], _, _).
set_pprops([H|T], M, Props) :-
    set_pprops1(Props, M:H),
    strip_module(M:H, M2, P),
    '$pi_head'(M2:P, Pred),
    (   '$get_predicate_attribute'(Pred, incremental, 1)
    ->  '$wrap_incremental'(Pred)
    ;   '$unwrap_incremental'(Pred)
    ),
    set_pprops(T, M, Props).

set_pprops1([], _).
set_pprops1([H|T], P) :-
    (   atom(H)
    ->  '$set_predicate_attribute'(P, H, true)
    ;   H =.. [Name,Value]
    ->  '$set_predicate_attribute'(P, Name, Value)
    ),
    set_pprops1(T, P).

options_properties(Options, Props) :-
    G = opt_prop(_,_,_,_),
    findall(G, G, Spec),
    options_properties(Spec, Options, Props).

options_properties([], _, []).
options_properties([opt_prop(Name, Type, SetValue, Prop)|T],
                   Options, [Prop|PT]) :-
    Opt =.. [Name,V],
    '$option'(Opt, Options),
    '$must_be'(Type, V),
    V = SetValue,
    !,
    options_properties(T, Options, PT).
options_properties([_|T], Options, PT) :-
    options_properties(T, Options, PT).

opt_prop(incremental,   boolean,               Bool,  incremental(Bool)).
opt_prop(abstract,      between(0,0),          0,     abstract).
opt_prop(multifile,     boolean,               true,  multifile).
opt_prop(discontiguous, boolean,               true,  discontiguous).
opt_prop(volatile,      boolean,               true,  volatile).
opt_prop(thread,        oneof(atom, [local,shared],[local,shared]),
                                               local, thread_local).

                /********************************
                *            MODULES            *
                *********************************/

%!  current_module(?Module) is nondet.
%
%   True if Module is a currently defined module.

current_module(Module) :-
    '$current_module'(Module, _).

%!  module_property(?Module, ?Property) is nondet.
%
%   True if Property is a property of Module.  Defined properties
%   are:
%
%       * file(File)
%       Module is loaded from File.
%       * line_count(Count)
%       The module declaration is on line Count of File.
%       * exports(ListOfPredicateIndicators)
%       The module exports ListOfPredicateIndicators
%       * exported_operators(ListOfOp3)
%       The module exports the operators ListOfOp3.

module_property(Module, Property) :-
    nonvar(Module), nonvar(Property),
    !,
    property_module(Property, Module).
module_property(Module, Property) :-    % -, file(File)
    nonvar(Property), Property = file(File),
    !,
    (   nonvar(File)
    ->  '$current_module'(Modules, File),
        (   atom(Modules)
        ->  Module = Modules
        ;   '$member'(Module, Modules)
        )
    ;   '$current_module'(Module, File),
        File \== []
    ).
module_property(Module, Property) :-
    current_module(Module),
    property_module(Property, Module).

property_module(Property, Module) :-
    module_property(Property),
    (   Property = exported_operators(List)
    ->  '$exported_ops'(Module, List, [])
    ;   '$module_property'(Module, Property)
    ).

module_property(class(_)).
module_property(file(_)).
module_property(line_count(_)).
module_property(exports(_)).
module_property(exported_operators(_)).
module_property(size(_)).
module_property(program_size(_)).
module_property(program_space(_)).
module_property(last_modified_generation(_)).

%!  module(+Module) is det.
%
%   Set the module that is associated to the toplevel to Module.

module(Module) :-
    atom(Module),
    current_module(Module),
    !,
    '$set_typein_module'(Module).
module(Module) :-
    '$set_typein_module'(Module),
    print_message(warning, no_current_module(Module)).

%!  working_directory(-Old, +New)
%
%   True when Old is the current working directory and the working
%   directory has been updated to New.

working_directory(Old, New) :-
    '$cwd'(Old),
    (   Old == New
    ->  true
    ;   '$chdir'(New)
    ).


                 /*******************************
                 *            TRIES             *
                 *******************************/

%!  current_trie(?Trie) is nondet.
%
%   True if Trie is the handle of an existing trie.

current_trie(Trie) :-
    current_blob(Trie, trie),
    is_trie(Trie).

%!  trie_property(?Trie, ?Property)
%
%   True when Property is a property of Trie. Defined properties
%   are:
%
%     - value_count(Count)
%       Number of terms in the trie.
%     - node_count(Count)
%       Number of nodes in the trie.
%     - size(Bytes)
%       Number of bytes needed to store the trie.
%     - hashed(Count)
%       Number of hashed nodes.
%     - compiled_size(Bytes)
%       Size of the compiled representation (if the trie is compiled)
%     - lookup_count(Count)
%       Number of data lookups on the trie
%     - gen_call_count(Count)
%       Number of trie_gen/2 calls on this trie
%
%   Incremental tabling statistics:
%
%     - invalidated(Count)
%       Number of times the trie was inivalidated
%     - reevaluated(Count)
%       Number of times the trie was re-evaluated
%
%   Shared tabling statistics:
%
%     - deadlock(Count)
%       Number of times the table was involved in a deadlock
%     - wait(Count)
%       Number of times a thread had to wait for this table

trie_property(Trie, Property) :-
    current_trie(Trie),
    trie_property(Property),
    '$trie_property'(Trie, Property).

trie_property(node_count(_)).
trie_property(value_count(_)).
trie_property(size(_)).
trie_property(hashed(_)).
trie_property(compiled_size(_)).
                                                % below only when -DO_TRIE_STATS
trie_property(lookup_count(_)).                 % is enabled in pl-trie.h
trie_property(gen_call_count(_)).
trie_property(invalidated(_)).                  % IDG stats
trie_property(reevaluated(_)).
trie_property(deadlock(_)).                     % Shared tabling stats
trie_property(wait(_)).
trie_property(idg_affected_count(_)).
trie_property(idg_dependent_count(_)).
trie_property(idg_size(_)).


                /********************************
                *      SYSTEM INTERACTION       *
                *********************************/

shell(Command) :-
    shell(Command, 0).


                 /*******************************
                 *            SIGNALS           *
                 *******************************/

:- meta_predicate
    on_signal(+, :, :),
    current_signal(?, ?, :).

%!  on_signal(+Signal, -OldHandler, :NewHandler) is det.

on_signal(Signal, Old, New) :-
    atom(Signal),
    !,
    '$on_signal'(_Num, Signal, Old, New).
on_signal(Signal, Old, New) :-
    integer(Signal),
    !,
    '$on_signal'(Signal, _Name, Old, New).
on_signal(Signal, _Old, _New) :-
    '$type_error'(signal_name, Signal).

%!  current_signal(?Name, ?SignalNumber, :Handler) is nondet.

current_signal(Name, Id, Handler) :-
    between(1, 32, Id),
    '$on_signal'(Id, Name, Handler, Handler).

:- multifile
    prolog:called_by/2.

prolog:called_by(on_signal(_,_,New), [New+1]) :-
    (   new == throw
    ;   new == default
    ), !, fail.


                 /*******************************
                 *            DLOPEN            *
                 *******************************/

%!  open_shared_object(+File, -Handle) is det.
%!  open_shared_object(+File, -Handle, +Flags) is det.
%
%   Open a shared object or DLL file. Flags  is a list of flags. The
%   following flags are recognised. Note   however  that these flags
%   may have no affect on the target platform.
%
%       * =now=
%       Resolve all symbols in the file now instead of lazily.
%       * =global=
%       Make new symbols globally known.

open_shared_object(File, Handle) :-
    open_shared_object(File, Handle, []). % use pl-load.c defaults

open_shared_object(File, Handle, Flags) :-
    (   is_list(Flags)
    ->  true
    ;   throw(error(type_error(list, Flags), _))
    ),
    map_dlflags(Flags, Mask),
    '$open_shared_object'(File, Handle, Mask).

dlopen_flag(now,        2'01).          % see pl-load.c for these constants
dlopen_flag(global,     2'10).          % Solaris only

map_dlflags([], 0).
map_dlflags([F|T], M) :-
    map_dlflags(T, M0),
    (   dlopen_flag(F, I)
    ->  true
    ;   throw(error(domain_error(dlopen_flag, F), _))
    ),
    M is M0 \/ I.


                 /*******************************
                 *             I/O              *
                 *******************************/

format(Fmt) :-
    format(Fmt, []).

                 /*******************************
                 *            FILES             *
                 *******************************/

%!  absolute_file_name(+Term, -AbsoluteFile)

absolute_file_name(Name, Abs) :-
    atomic(Name),
    !,
    '$absolute_file_name'(Name, Abs).
absolute_file_name(Term, Abs) :-
    '$chk_file'(Term, [''], [access(read)], true, File),
    !,
    '$absolute_file_name'(File, Abs).
absolute_file_name(Term, Abs) :-
    '$chk_file'(Term, [''], [], true, File),
    !,
    '$absolute_file_name'(File, Abs).

%!  tmp_file_stream(-File, -Stream, +Options) is det.
%!  tmp_file_stream(+Encoding, -File, -Stream) is det.
%
%   Create a temporary file and open it   atomically. The second mode is
%   for compatibility reasons.

tmp_file_stream(Enc, File, Stream) :-
    atom(Enc), var(File), var(Stream),
    !,
    '$tmp_file_stream'('', Enc, File, Stream).
tmp_file_stream(File, Stream, Options) :-
    current_prolog_flag(encoding, DefEnc),
    '$option'(encoding(Enc), Options, DefEnc),
    '$option'(extension(Ext), Options, ''),
    '$tmp_file_stream'(Ext, Enc, File, Stream),
    set_stream(Stream, file_name(File)).


                /********************************
                *        MEMORY MANAGEMENT      *
                *********************************/

%!  garbage_collect is det.
%
%   Invoke the garbage collector.  The   argument  of the underlying
%   '$garbage_collect'/1  is  the  debugging  level  to  use  during
%   garbage collection. This only works if   the  system is compiled
%   with the -DODEBUG cpp flag. Only to simplify maintenance.

garbage_collect :-
    '$garbage_collect'(0).

%!  set_prolog_stack(+Name, +Option) is det.
%
%   Set a parameter for one of the Prolog stacks.

set_prolog_stack(Stack, Option) :-
    Option =.. [Name,Value0],
    Value is Value0,
    '$set_prolog_stack'(Stack, Name, _Old, Value).

%!  prolog_stack_property(?Stack, ?Property) is nondet.
%
%   Examine stack properties.

prolog_stack_property(Stack, Property) :-
    stack_property(P),
    stack_name(Stack),
    Property =.. [P,Value],
    '$set_prolog_stack'(Stack, P, Value, Value).

stack_name(local).
stack_name(global).
stack_name(trail).

stack_property(limit).
stack_property(spare).
stack_property(min_free).
stack_property(low).
stack_property(factor).


                 /*******************************
                 *             TERM             *
                 *******************************/

:- '$iso'((numbervars/3)).

%!  numbervars(+Term, +StartIndex, -EndIndex) is det.
%
%   Number all unbound variables in Term   using  '$VAR'(N), where the
%   first N is StartIndex and EndIndex is  unified to the index that
%   will be given to the next variable.

numbervars(Term, From, To) :-
    numbervars(Term, From, To, []).


                 /*******************************
                 *            STRING            *
                 *******************************/

%!  term_string(?Term, ?String, +Options)
%
%   Parse/write a term from/to a string using Options.

term_string(Term, String, Options) :-
    nonvar(String),
    !,
    read_term_from_atom(String, Term, Options).
term_string(Term, String, Options) :-
    (   '$option'(quoted(_), Options)
    ->  Options1 = Options
    ;   '$merge_options'(_{quoted:true}, Options, Options1)
    ),
    format(string(String), '~W', [Term, Options1]).


                 /*******************************
                 *             GVAR             *
                 *******************************/

%!  nb_setval(+Name, +Value) is det.
%
%   Bind the non-backtrackable variable Name with a copy of Value

nb_setval(Name, Value) :-
    duplicate_term(Value, Copy),
    nb_linkval(Name, Copy).


		 /*******************************
		 *            THREADS		*
		 *******************************/

:- meta_predicate
    thread_create(0, -).

%!  thread_create(:Goal, -Id)
%
%   Shorthand for thread_create(Goal, Id, []).

thread_create(Goal, Id) :-
    thread_create(Goal, Id, []).

%!  thread_join(+Id)
%
%   Join a thread and raise an error of the thread did not succeed.
%
%   @error  thread_error(Status),  where  Status  is    the   result  of
%   thread_join/2.

thread_join(Id) :-
    thread_join(Id, Status),
    (   Status == true
    ->  true
    ;   throw(error(thread_error(Id, Status), _))
    ).

%!  set_prolog_gc_thread(+Status)
%
%   Control the GC thread.  Status is one of
%
%     - false
%     Disable the separate GC thread, running atom and clause
%     garbage collection in the triggering thread.
%     - true
%     Enable the separate GC thread.  All implicit atom and clause
%     garbage collection is executed by the thread `gc`.
%     - stop
%     Stop the `gc` thread if it is running.  The thread is recreated
%     on the next implicit atom or clause garbage collection.  Used
%     by fork/1 to avoid forking a multi-threaded application.

set_prolog_gc_thread(Status) :-
    var(Status),
    !,
    '$instantiation_error'(Status).
set_prolog_gc_thread(false) :-
    !,
    set_prolog_flag(gc_thread, false),
    (   current_prolog_flag(threads, true)
    ->  (   '$gc_stop'
        ->  thread_join(gc)
        ;   true
        )
    ;   true
    ).
set_prolog_gc_thread(true) :-
    !,
    set_prolog_flag(gc_thread, true).
set_prolog_gc_thread(stop) :-
    !,
    (   current_prolog_flag(threads, true)
    ->  (   '$gc_stop'
        ->  thread_join(gc)
        ;   true
        )
    ;   true
    ).
set_prolog_gc_thread(Status) :-
    '$domain_error'(gc_thread, Status).

%!  '$wrap_predicate'(:Head, +Name, -Closure, -Wrapped, +Body) is det.
%
%   Would be nicer to have this   from library(prolog_wrap), but we need
%   it for tabling, so it must be a system predicate.

:- meta_predicate
    '$wrap_predicate'(:, +, -, -, +).

'$wrap_predicate'(M:Head, WName, Closure, call(Wrapped), Body) :-
    callable_name_arguments(Head, PName, Args),
    callable_name_arity(Head, PName, Arity),
    (   is_most_general_term(Head)
    ->  true
    ;   '$domain_error'(most_general_term, Head)
    ),
    atomic_list_concat(['$wrap$', PName], WrapName),
    volatile(M:WrapName/Arity),
    module_transparent(M:WrapName/Arity),
    WHead =.. [WrapName|Args],
    '$c_wrap_predicate'(M:Head, WName, Closure, Wrapped, M:(WHead :- Body)).

callable_name_arguments(Head, PName, Args) :-
    atom(Head),
    !,
    PName = Head,
    Args = [].
callable_name_arguments(Head, PName, Args) :-
    compound_name_arguments(Head, PName, Args).

callable_name_arity(Head, PName, Arity) :-
    atom(Head),
    !,
    PName = Head,
    Arity = 0.
callable_name_arity(Head, PName, Arity) :-
    compound_name_arity(Head, PName, Arity).
