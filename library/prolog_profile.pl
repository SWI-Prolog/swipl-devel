/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023-2025, SWI-Prolog Solutions b.v.
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

:- module(prolog_profile,
          [ profile/1,                  % :Goal
            profile/2,                  % :Goal, +Options
            show_profile/1,             % +Options
            profile_data/1,             % -Dict
            profile_procedure_data/2    % :PI, -Data
          ]).
:- autoload(library(error),[must_be/2]).
:- autoload(library(lists), [member/2, append/3]).
:- autoload(library(apply), [foldl/4]).
:- autoload(library(option), [option/3]).
:- autoload(library(pairs), [map_list_to_pairs/3, pairs_values/2]).
:- autoload(library(prolog_code), [predicate_sort_key/2, predicate_label/2]).

:- meta_predicate
    profile(0),
    profile(0, +),
    profile_procedure_data(:, -).

:- create_prolog_flag(profile_ports, true,
                      [ keep(true),
                        type(oneof([true,false,classic]))
                      ]).
:- create_prolog_flag(profile_exact, auto,
                      [ keep(true),
                        type(oneof([auto,true,false]))
                      ]).
:- create_prolog_flag(profile_sample_rate, 200.0,
                      [ keep(true),
                        type(float)
                      ]).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Execution profiler

This module provides a simple frontend on  the execution profiler with a
hook  to  the  GUI  visualiser   for    profiling   results  defined  in
library(swi/pce_profile).
*/

:- multifile
    prolog:show_profile_hook/1,
    prolog:profile_node_label/2.

%!  profile(:Goal).
%!  profile(:Goal, +Options).
%
%   Run once(Goal) under the execution profiler.   If  the (xpce) GUI is
%   enabled this predicate is  hooked   by  library(swi/pce_profile) and
%   results are presented in a gui that enables navigating the call tree
%   and jump to predicate implementations.  Without   the  GUI, a simple
%   textual report is generated. Defined options are:
%
%     - time(Which)
%       Profile `cpu` or `wall` time.  The default is CPU time.
%     - exact(Exact)
%       If `true`, do not sample, but read the clock each time
%       execution moves to another node of the call tree.  This gives
%       exact wall times, also for short running goals.  For CPU time,
%       the thread's CPU time is read once per sample period (see
%       sample_rate(Rate)) and after each long interval, where the
%       thread may have been off the CPU.  If `auto`, use exact
%       timing if reading the clock is cheap and, for CPU time, the
%       thread CPU clock is precise.  This is typically not the case
%       in virtual machines.  Otherwise use sampling.  Default is
%       defined by the Prolog flag `profile_exact`, which defaults to
%       `auto`.
%     - sample_rate(Rate)
%       Samples per second, any numeric value between 1 and 1000.
%       Default is defined by the Prolog flag `profile_sample_rate`,
%       which defaults to 200.
%     - ports(Bool)
%       Specifies ports counted - `true` (all ports), `false` (call
%       port only) or `classic` (all with some errors).
%       Accomodates space/accuracy tradeoff building call tree.
%       Default is defined by the Prolog flag `profile_ports`,
%       which defaults to `true`.
%     - top(N)
%       When generating a textual report, show the top N predicates.
%     - cumulative(Bool)
%       If `true` (default `false`), show cumulative output in
%       a textual report.
%
%   @tbd The textual input reflects only part of the information.
%   @see show_coverage/2 from library(test_cover).

profile(Goal) :-
    profile(Goal, []).

profile(Goal0, Options) :-
    current_prolog_flag(profile_ports, DefPorts),
    current_prolog_flag(profile_sample_rate, DefRate),
    option(time(Which), Options, cpu),
    time_name(Which, Time),
    current_prolog_flag(profile_exact, DefExact),
    option(exact(Exact0), Options, DefExact),
    must_be(oneof([auto,true,false]), Exact0),
    exact(Exact0, Time, Exact),
    exact_time(Exact, Time, How),
    option(ports(Ports), Options, DefPorts),
    must_be(oneof([true,false,classic]),Ports),
    option(sample_rate(Rate), Options, DefRate),
    must_be(between(1.0,1000), Rate),
    expand_goal(Goal0, Goal),
    call_cleanup('$profile'(Goal, How, Ports, Rate),
                 prolog_statistics:show_profile(Options)).

exact(auto, Time, Exact) :-
    !,
    (   time_clock(Time, Clock),
        '$prof_exact_ok'(Clock)
    ->  Exact = true
    ;   Exact = false
    ).
exact(Exact, _, Exact).

time_clock(cputime,  cpu).
time_clock(walltime, wall).

exact_time(false, Time, Time).
exact_time(true,  Time, How) :-
    exact_name(Time, How).

exact_name(cputime,  exact_cputime).
exact_name(walltime, exact_walltime).

time_name(cpu,      cputime)  :- !.
time_name(wall,     walltime) :- !.
time_name(cputime,  cputime)  :- !.
time_name(walltime, walltime) :- !.
time_name(Time, _) :-
    must_be(oneof([cpu,wall]), Time).

%!  show_profile(+Options)
%
%   Display last collected profiling data.  Options are
%
%     * top(N)
%     When generating a textual report, show the top N predicates.
%     * cumulative(Bool)
%     If =true= (default =false=), show cumulative output in
%     a textual report.

show_profile(N) :-
    integer(N),
    !,
    show_profile([top(N)]).
show_profile(Options) :-
    profiler(Old, false),
    show_profile_(Options),
    profiler(_, Old).

show_profile_(Options) :-
    prolog:show_profile_hook(Options),
    !.
show_profile_(Options) :-
    show_text_profile(Options).

%   Textual report on the last collected profile.

show_text_profile(Options) :-
    prof_statistics(Stat),
    NetTicks is Stat.ticks-Stat.accounting,
    percentage(NetTicks, Stat.ticks, NetPC),
    NetTime is NetPC*Stat.time/100,
    Ports = Stat.ports,
    findall(Node, procedure_data(_, Node), Nodes),
    (   option(cumulative(false), Options, false)
    ->  SortKey = ticks_self
    ;   SortKey = ticks
    ),
    sort_prof_nodes(SortKey, Nodes, Sorted),
    option(top(N), Options, 25),
    top(N, Sorted, Top),
    column_stops(Top, Ports, Stops),
    Stops = stops(CallStop, RedoStop, ExitStop),
    format_divider,
    (   Stat.exact == true
    ->  Mode = 'exact '
    ;   Mode = ''
    ),
    format('Number of nodes: ~w~t[~w~w time, ports(~w)]~60|~t\c
            Total time: ~3f seconds~101|~n',
           [Stat.nodes, Mode, Stat.clock, Ports, NetTime]),
    format('Predicate~tCalls +~*| Redos~t~*|~t \c
            Exits +~*| Fails~tTime:Self +~87| Time:Children~n',
           [CallStop, RedoStop, ExitStop]),
    format_divider,
    forall(member(Node, Top),
           show_plain(Node, (NetTicks,NetTime,Ports), Stops)),
    format_divider.

top(N, List, Top) :-
    length(List, Len),
    Len > N,
    !,
    length(Top, N),
    append(Top, _, List).
top(_, List, List).

%   column_stops(+Nodes, +Ports, -Stops) is det.
%
%   The columns of the counts are as wide as their widest count in
%   Nodes.  A column that is wider than the default moves the columns
%   left of it to the left, taking the space from the predicate labels.
%   Stops is stops(CallStop, RedoStop, ExitStop), the column where the
%   calls, redos and exits end.

column_stops(Nodes, Ports, stops(CallStop, RedoStop, ExitStop)) :-
    foldl(count_widths(Ports), Nodes, 0-0-0, RW-EW-FW),
    ExitStop is 58 - max(0, FW-10),
    RedoStop is ExitStop - 9 - max(0, EW-7),
    CallStop is RedoStop - 8 - max(0, RW-6).

count_widths(Ports, Node, R0-E0-F0, R-E-F) :-
    counts(Node, Ports, _Call, Redo, Exit, Fail),
    count_width(Redo, RW), R is max(R0, RW),
    count_width(Exit, EW), E is max(E0, EW),
    count_width(Fail, FW), F is max(F0, FW).

count_width(Count, Width) :-
    format(string(S), '~D', [Count]),
    string_length(S, Width).

counts(Node, Ports, Call, Redo, Exit, Fail) :-
    value(call, Node, Call),
    (   Ports == false
    ->  Redo = 0, Exit = 0, Fail = 0
    ;   value(redo, Node, Redo),
        value(exit, Node, Exit),
        Fail is Call+Redo-Exit
    ).

sort_prof_nodes(ticks, Nodes, Sorted) :-
    !,
    map_list_to_pairs(key_ticks, Nodes, Keyed),
    sort(1, >=, Keyed, KeySorted),
    pairs_values(KeySorted, Sorted).
sort_prof_nodes(Key, Nodes, Sorted) :-
    sort(Key, >=, Nodes, Sorted).

key_ticks(Node, Ticks) :-
	value(ticks,Node,Ticks).

show_plain(Node, (NetTicks,NetTime,Ports),
           stops(CallStop, RedoStop, ExitStop)) :-
    value(label, Node, Label),
    counts(Node, Ports, Call, Redo, Exit, Fail),
    count_width(Call, CallsWidth),
    Width is CallStop - CallsWidth - 3, % " " before and " +" after Calls
    elide(Label, Width, Pred),
    time_data(Node,NetTicks,NetTime,SelfPC,SelfTime,ChildrenPC,ChildrenTime),
    format('~w ~t~D +~*| ~D ~t~*|~t~D +~*| ~D ~t~2fs.(~78|~t~1f%) +~87|~t~2fs.(~95|~t~1f%)~102|~n',
           [ Pred, Call, CallStop, Redo, RedoStop, Exit, ExitStop, Fail,
             SelfTime, SelfPC, ChildrenTime, ChildrenPC
           ]).

format_divider :- format('~`=t~102|~n').

%!  elide(+Label, +Width, -Elided) is det.
%
%   If Label is wider than Width, replace its middle by an ellipsis, so
%   both the module or class and the name remain visible.

elide(Label, Width, Elided) :-
    string_length(Label, Len),
    Len > Width,
    !,
    ellipsis(Ellipsis),
    string_length(Ellipsis, ELen),
    Keep is max(0, Width-ELen),
    Pre is Keep//2,
    Post is Keep-Pre,
    sub_string(Label, 0, Pre, _, S1),
    sub_string(Label, _, Post, 0, S2),
    atomics_to_string([S1, Ellipsis, S2], Elided).
elide(Label, _, Label).

ellipsis("\u2026") :-
    stream_property(current_output, encoding(Enc)),
    unicode_encoding(Enc),
    !.
ellipsis("...").

unicode_encoding(utf8).
unicode_encoding(unicode_be).
unicode_encoding(unicode_le).
unicode_encoding(wchar_t).

time_data(Data,NetTicks,NetTime,SelfPC,SelfTime,ChildrenPC,ChildrenTime) :-
    value(ticks_self,Data,Ticks),
    percentage(Ticks, NetTicks, SelfPC),
    SelfTime is SelfPC*NetTime/100,
    value(ticks_siblings,Data,ChildrenTicks),
    percentage(ChildrenTicks, NetTicks, ChildrenPC),
    ChildrenTime is ChildrenPC*NetTime/100.

%   Percentage of Part in Total.  Total is 0 if no sample was taken.

percentage(_, Total, 0.0) :-
    Total =:= 0,
    !.
percentage(Part, Total, Percent) :-
    Percent is 100*Part/Total.


                 /*******************************
                 *         DATA GATHERING       *
                 *******************************/

%!  profile_data(-Data) is det.
%
%   Gather all relevant data from profiler. This predicate may be called
%   while profiling is active  in  which   case  it  is  suspended while
%   collecting the data. Data is a dict providing the following fields:
%
%     - summary:Dict
%       Overall statistics providing
%       - samples:Count:
%         Times the statistical profiler was called or, if `exact`
%         is `true`, the number of times execution moved to another
%         node of the call tree.
%       - ticks:Count
%         Nanoseconds sampled during profiling
%       - accounting:Count
%         Nanoseconds spent on accounting.  This is the profiler's
%         own overhead.  The exact modes measure it and exclude it
%         from the times of the predicates.
%       - time:Seconds
%         Total time sampled
%       - nodes:Count
%         Nodes in the call graph.
%       - sample_period: MicroSeconds
%         Same interval timer period in micro seconds
%       - ports: Ports
%         One of `true`, `false` or `classic`
%       - clock: Clock
%         One of `cpu` or `wall`, the time that was profiled.
%       - exact: Bool
%         Whether the data was collected using exact(true)
%     - nodes
%       List of nodes.  Each node provides:
%       - predicate:PredicateIndicator
%         or a blob that represents another profiled object, such as an
%         XPCE method.
%       - ticks_self:Count
%       - ticks_siblings:Count
%         Nanoseconds sampled in the predicate itself and in its
%         callees.
%       - call:Count
%       - redo:Count
%       - exit:Count
%       - callers:list_of(Relative)
%       - callees:list_of(Relative)
%
%    _Relative_ is a term of the shape below that represents a caller or
%    callee. Future versions are likely to use a dict instead.
%
%        node(PredicateIndicator, CycleID, Ticks, TicksSiblings,
%             Calls, Redos, Exits)

profile_data(Data) :-
    setup_call_cleanup(
        profiler(Old, false),
        profile_data_(Data),
        profiler(_, Old)).

profile_data_(profile{summary:Summary, nodes:Nodes}) :-
    prof_statistics(Summary),
    findall(Node, procedure_data(_, Node), Nodes).

%!  prof_statistics(-Node) is det.
%
%   Get overall statistics
%
%   @param Node     term of the format prof(Ticks, Account, Time, Nodes)

prof_statistics(summary{samples:Samples, ticks:Ticks,
                        accounting:Account, time:Time,
                        nodes:Nodes,
                        sample_period: Period,
                        ports: Ports,
                        clock: Clock,
                        exact: Exact
                       }) :-
    '$prof_statistics'(Samples, Ticks, Account, Time, Nodes, Period, Ports,
                       Mode),
    prof_mode(Mode, Clock, Exact).

prof_mode(cputime,        cpu,  false).
prof_mode(walltime,       wall, false).
prof_mode(exact_cputime,  cpu,  true).
prof_mode(exact_walltime, wall, true).
prof_mode(false,          cpu,  false).     % no data

%!  profile_procedure_data(?Pred, -Data:dict) is nondet.
%
%   Collect data for Pred. If Pred is   unbound  data for each predicate
%   that has profile data available is   returned.  Data is described in
%   profile_data/1 as an element of the `nodes` key.

profile_procedure_data(Pred, Node) :-
    procedure_data(Pred, Node).

%   procedure_data(?Pred, -Data:dict) is nondet.
%
%   As profile_procedure_data/2, but if Pred is unbound, also enumerate
%   the nodes of other profile types such as XPCE methods.  These are
%   not qualified.

procedure_data(Pred, Node) :-
    Node = node{predicate:Pred,
                ticks_self:TicksSelf, ticks_siblings:TicksSiblings,
                call:Call, redo:Redo, exit:Exit,
                callers:Parents, callees:Siblings},
    (   specified(Pred)
    ->  true
    ;   profiled_predicates(Preds),
        member(Pred, Preds)
    ),
    '$prof_procedure_data'(Pred,
                           TicksSelf, TicksSiblings,
                           Call, Redo, Exit,
                           Parents, Siblings).

specified(Module:Head) :-
    atom(Module),
    callable(Head).

profiled_predicates(Preds) :-
    setof(Pred, prof_impl(Pred), Preds).

prof_impl(Pred) :-
    prof_node_id(Node),
    node_id_pred(Node, Pred).

prof_node_id(N) :-
    prof_node_id_below(N, -).

prof_node_id_below(N, Root) :-
    '$prof_sibling_of'(N0, Root),
    (   N = N0
    ;   prof_node_id_below(N, N0)
    ).

node_id_pred(Node, Pred) :-
    '$prof_node'(Node, Pred, _Calls, _Redos, _Exits, _Recur,
                 _Ticks, _SiblingTicks).

%!  node_label(+Predicate, -Label) is det.
%
%   Label is the text that identifies the  predicate field of a node.
%   This is a predicate indicator or  an   object  of another profile
%   type, such as an XPCE method,  which   is  named by the multifile
%   hook prolog:profile_node_label/2.

node_label(Pred, Label) :-
    prolog:profile_node_label(Pred, Label),
    !.
node_label(Pred, Label) :-
    ground(Pred),
    Pred = _:_,
    !,
    predicate_label(Pred, Label).
node_label(Pred, Label) :-
    format(string(Label), '~p', [Pred]).

%!  value(+Key, +NodeData, -Value)
%
%   Obtain possible computed attributes from NodeData.

value(name, Data, Name) :-
    !,
    predicate_sort_key(Data.predicate, Name).
value(label, Data, Label) :-
    !,
    node_label(Data.predicate, Label).
value(ticks, Data, Ticks) :-
    !,
    Ticks is Data.ticks_self + Data.ticks_siblings.
value(time(Key, percentage, TotalTicks), Data, Percent) :-
    !,
    value(Key, Data, Ticks),
    (TotalTicks > 0
     -> Percent is 100 * (Ticks/TotalTicks)
     ;  Percent is 0.0
    ).
value(Name, Data, Value) :-
    Value = Data.Name.
