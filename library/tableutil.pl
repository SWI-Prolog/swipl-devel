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

:- module(prolog_table_utils,
          [ table_statistics/0,			% Statistics
            table_statistics/1,			% ?Variant
            table_statistics_by_predicate/0,
            table_statistics_by_predicate/1,	% +Options
            table_statistics/2,                 % ?Stat, -Value
            table_statistics/3,                 % ?Variant, ?Stat, -Value
            tstat/2,                            % ?Stat, ?Top
            tstat/3,                            % ?Variant, ?Stat, ?Top
            tdump/0,                            % Dump tables
            tdump/1,                            % :Goal
            tdump/2,                            % :Goal, +Options
            tidg/0,
            tidg/1,                             % :Goal
            summarize_idg/0,
            summarize_idg/1                     % +Top
          ]).
:- autoload(library(lists), [member/2]).
:- autoload(library(aggregate), [aggregate_all/3]).
:- autoload(library(ansi_term), [ansi_format/3]).
:- autoload(library(apply), [exclude/3, maplist/2]).
:- autoload(library(dif), [dif/2]).
:- autoload(library(error), [domain_error/2]).
:- autoload(library(option), [option/3, option/2]).
:- autoload(library(prolog_code), [pi_head/2]).
:- autoload(library(solution_sequences), [limit/2, order_by/2]).
:- autoload(library(varnumbers), [numbervars/1]).

:- meta_predicate
    tdump(:),
    tdump(:, +),
    idg(:),
    summarize_idg(:),
    table_statistics(:),
    table_statistics(:, ?, -),
    tstat(:, ?, ?).

/** <module> Table inspection and statistics utilities

This library provides tools  intended  to   be  used  at the interactive
toplevel for inspecting tables, table dependencies and table statistics.
*/

summary_table_width(55).

%!  table_statistics(?Stat, -Value) is nondet.
%!  table_statistics(?Variant, ?Stat, -Value) is nondet.
%
%   Give summary statistics for the tables  associated with all subgoals
%   of Variant. The table_statistics/2 version considers all tables.
%
%   The values for Stat are:
%
%     - tables
%       Total number of answer tries
%     - answers
%       Total number of answers in the combined tries
%     - duplicate_ratio
%       Ratio of generated (and thus ignored) duplicate answers.
%       `1` means no duplicates.  `2` means for every answer there
%       was (on everage) a duplicate generated.
%     - space_ratio
%       Number of nodes with a value divided by the total number
%       of nodes in a trie.  The maximum is `1`.  A low number
%       implies that a large amount of differently shaped data
%       is included in the answer tries.
%     - complete_call
%       Number of times answers are generated from a completed
%       table, i.e., times answers are _reused_.
%     - invalidated
%       Number of times an incremental table was invalidated.
%     - reevaluated
%       Number of times an invalidated table wa reevaluated.  If
%       lower than `invalidated` this implies that dependent nodes
%       of the IDG were reevaluated to the same answer set.
%     - space
%       Summed memory usage of the answer tries in bytes.
%     - compiled_space
%       Summed size for the compiled representation of completed
%       tables.

table_statistics(Stat, Value) :-
    table_statistics(_:_, Stat, Value).

table_statistics(Variant, Stat, Value) :-
    (   var(Stat)
    ->  table_statistics_(Variant, Stat, Value)
    ;   table_statistics_(Variant, Stat, Value)
    ->  true
    ).

table_statistics_(Variant, tables, NTables) :-
    aggregate_all(count, table(Variant, _), NTables).
table_statistics_(Variant, Stat, Total) :-
    variant_trie_stat(Stat, _What),
    \+ hidden_stat(Stat, Variant),
    (   avg(Stat)
    ->  aggregate_all(sum(Ratio)+count,
                      variant_stat(Stat, Variant, Ratio),
                      Sum+Count),
        Count > 0,
        Total is Sum/Count
    ;   aggregate_all(sum(Count), variant_stat(Stat, Variant, Count), Total)
    ).

hidden_stat(variables, _).
hidden_stat(lookup, _).
hidden_stat(invalidated, Variant) :-
    callable(Variant),
    \+ predicate_property(Variant, tabled(incremental)).
hidden_stat(reevaluated, Variant) :-
    callable(Variant),
    \+ predicate_property(Variant, tabled(incremental)).

avg(space_ratio).
avg(duplicate_ratio).

%!  table_statistics
%
%   Print a summary of statistics relevant to tabling.
%
%   @see table_statistics/2 for an explanation

table_statistics :-
    (   (   '$tbl_global_variant_table'(Table),
            call_table_properties(shared, Table)
        ;   '$tbl_local_variant_table'(Table),
            call_table_properties(private, Table)
        ),
        fail
    ;   true
    ),
    ansi_format([bold], 'Summary of answer trie statistics:', []),
    nl,
    table_statistics_(_:_, [tables(false)]).

%!  table_statistics(:Variant)
%
%   Print a summary for the statistics  of   all  tables for subgoals of
%   Variant. See table_statistics/2 for an explanation.

table_statistics(Variant) :-
    table_statistics_(Variant, []).

table_statistics_(Variant, Options) :-
    table_statistics_dict(Variant, Dict),
    print_table_statistics(Dict, Options).

table_statistics_dict(Variant, Dict) :-
    findall(Stat-Value, table_statistics(Variant, Stat, Value), Pairs),
    dict_create(Dict, table_stat, [variant-Variant|Pairs]).

print_table_statistics(Dict, Options) :-
    summary_table_width(DefWidth),
    option(width(Width), Options, DefWidth),
    (   option(tables(false), Options)
    ->  dif(Stat, tables)
    ;   true
    ),
    (   option(header(true), Options)
    ->  print_table_predicate_header(Dict.variant, [width(Width)|Options])
    ;   true
    ),
    (   variant_trie_stat0(Stat, What),
        Value = Dict.get(Stat),
        (   integer(Value)
        ->  format('  ~w ~`.t ~D~*|~n', [What, Value, Width])
        ;   format('  ~w ~`.t ~2f~*|~n', [What, Value, Width])
        ),
        fail
    ;   true
    ).

print_table_predicate_header(Pred, Options) :-
    option(width(Width), Options),
    Pred = M:Head,
    tflags(Pred, Flags),
    functor(Head, Name, Arity),
    format('~n~`\u2015t~*|~n', [Width]),
    format('~t~p~t~w~*|~n', [M:Name/Arity, Flags, Width]),
    format('~`\u2015t~*|~n', [Width]).

tflags(Pred, Flags) :-
    findall(F, tflag(Pred, F), List),
    atomic_list_concat(List, Flags).

tflag(Pred, Flag) :-
    predicate_property(Pred, tabled(How)),
    tflag_name(How, Flag).

tflag_name(variant,     'V').
tflag_name(subsumptive, 'S').
tflag_name(shared,      'G').
tflag_name(incremental, 'I').


variant_trie_stat0(tables, "Answer tables").
variant_trie_stat0(Stat, What) :-
    dif(Stat, tables),
    variant_trie_stat(Stat, What).

call_table_properties(Which, Trie) :-
    ansi_format([bold], 'Statistics for ~w call trie:', [Which]),
    nl,
    summary_table_width(Width),
    (   call_trie_property_name(P, Label, Value),
        atrie_prop(Trie, P),
        (   integer(Value)
        ->  format('  ~w ~`.t ~D~*|~n', [Label, Value, Width])
        ;   format('  ~w ~`.t ~1f~*|~n', [Label, Value, Width])
        ),
        fail
    ;   true
    ).

call_trie_property_name(value_count(N), 'Number of tables',     N).
call_trie_property_name(size(N),        'Memory for call trie', N).
call_trie_property_name(space_ratio(N), 'Space efficiency',     N).

%!  table_statistics_by_predicate is det.
%!  table_statistics_by_predicate(+Options) is det.
%
%   Print statistics on memory usage  and   lookups  per  predicate. The
%   version without options  dumps  all   predicates  without  ordering.
%   Options:
%
%     - order_by(+Key)
%       Order the predicates according to Key. Default is `tables`, the
%       number of answer tables.  See table_statistics/2 for a list
%       of values for Key.
%     - top(N)
%       Only show the top N predicates.
%     - module(Module)
%       Limit the results to predicates of the given module.

table_statistics_by_predicate :-
    Pred = _:_,
    summary_table_width(Width),
    (   tabled_predicate_with_tables(Pred),
        print_table_predicate_header(Pred, [width(Width)]),
        table_statistics(Pred),
        fail
    ;   true
    ).

table_statistics_by_predicate(Options) :-
    option(order_by(OrderBy), Options, tables),
    option(top(Top), Options, infinite),
    option(module(M), Options, _),
    Pred = (M:_),
    findall(Dict,
            (  tabled_predicate_with_tables(Pred),
               table_statistics_dict(Pred, Dict)
            ),
            Dicts),
    exclude(has_no_key(OrderBy), Dicts, Dicts1),
    (   integer(Top), Top < 0
    ->  Order = @=<,
        TopN is -Top
    ;   Order = @>=,
        TopN is Top
    ),
    sort(OrderBy, Order, Dicts1, Sorted),
    forall(limit(TopN, member(Dict, Sorted)),
           print_table_statistics(Dict, [header(true)|Options])).

has_no_key(Key, Dict) :-
    \+ _ = Dict.get(Key).

tabled_predicate_with_tables(Pred) :-
    Pred = _:_,
    predicate_property(Pred, tabled),
    \+ predicate_property(Pred, imported_from(_)),
    \+ \+ table(Pred, _).

%!  tstat(?Value, ?Top).
%!  tstat(?Variant, ?Value, ?Top).
%
%   Print the top-N (for positive Top)   or  bottom-N (for negative Top)
%   for `Stat` for  all  tabled  subgoals   of  Variant  (or  all tabled
%   subgoals for tstat/2).  Stat is one of
%
%     - answers
%     - duplicate_ratio
%     - space_ratio
%     - gen(call)
%     - space
%     - compiled_space
%       See table_statistics/2.
%     - deadlock
%       Times this table was involved in a deadlock (cycle of threads
%       waiting for each others table to complete)
%     - wait
%       Times a thread waited for this table.
%     - variables
%       The number of variables in the variant.  The tabling logic
%       adds a term ret(...) to the table for each answer, where each
%       variable is an argument of the ret(...) term.  The arguments
%       are placed in depth-first lef-right order they appear in the
%       variant.  Optimal behaviour of the trie is achieved if the
%       variance is as much as possible to the rightmost arguments.
%       Poor allocation shows up as a low `space_ratio` statistics.
%
%   Below are some examples

%     - Find the tables with poor space behaviour (bad)
%
%           ?- tstat(space_ratio, -10).
%
%     - Find the tables with a high number of duplicates (good, but
%       if the number of duplicates can easily be reduced a lot it
%       makes your code faster):
%
%           ?- tstat(duplicate_ratio, 10).

tstat(Stat, Top) :-
    tstat(_:_, Stat, Top).
tstat(Variant, Stat, Top) :-
    variant_trie_stat(Stat, What),
    top(Top, Count, Limit, Dir, Order),
    findall(Variant-Count,
            limit(Limit, order_by([Order], variant_stat(Stat, Variant, Count))),
            Pairs),
    write_variant_table('~w ~w count per variant'-[Dir, What], Pairs).

top(Top, Var, 10, "Top", desc(Var)) :-
    var(Top), !.
top(Top, Var, Top, "Top", desc(Var)) :-
    Top >= 0, !.
top(Top, Var, Limit, "Bottom", asc(Var)) :-
    Limit is -Top.

variant_stat(Stat, V, Count) :-
    variant_trie_stat(Stat, _, Count, Property),
    table(V, T),
    atrie_prop(T, Property).

atrie_prop(T, size(Bytes)) :-
    '$trie_property'(T, size(Bytes)).
atrie_prop(T, compiled_size(Bytes)) :-
    '$trie_property'(T, compiled_size(Bytes)).
atrie_prop(T, value_count(Count)) :-
    '$trie_property'(T, value_count(Count)).
atrie_prop(T, space_ratio(Values/Nodes)) :-
    '$trie_property'(T, value_count(Values)),
    Values > 0,
    '$trie_property'(T, node_count(Nodes)).
atrie_prop(T, lookup_count(Count)) :-
    '$trie_property'(T, lookup_count(Count)).
atrie_prop(T, duplicate_ratio(Ratio)) :-
    '$trie_property'(T, value_count(Values)),
    Values > 0,
    '$trie_property'(T, lookup_count(Lookup)),
    Ratio is (Lookup - Values)/Values.
atrie_prop(T, gen_call_count(Count)) :-
    '$trie_property'(T, gen_call_count(Count)).
atrie_prop(T, invalidated(Count)) :-
    '$trie_property'(T, invalidated(Count)).
atrie_prop(T, reevaluated(Count)) :-
    '$trie_property'(T, reevaluated(Count)).
atrie_prop(T, deadlock(Count)) :-
    '$trie_property'(T, deadlock(Count)),
    Count > 0.
atrie_prop(T, wait(Count)) :-
    '$trie_property'(T, wait(Count)),
    Count > 0.
atrie_prop(T, variables(Count)) :-
    '$tbl_table_status'(T, _Status, _Wrapper, Skeleton),
    functor(Skeleton, ret, Count).

variant_trie_stat(Stat, What) :-
    (   variant_trie_stat(Stat, What, _, _)
    *-> true
    ;   domain_error(tstat_key, Stat)
    ).

variant_trie_stat(answers,        "Number of answers",
                  Count, value_count(Count)).
variant_trie_stat(duplicate_ratio,"Duplicate answer ratio",
                  Ratio, duplicate_ratio(Ratio)).
variant_trie_stat(space_ratio,    "Space efficiency",
                  Ratio, space_ratio(Ratio)).
variant_trie_stat(complete_call,  "Calls to completed tables",
                  Count, gen_call_count(Count)).
variant_trie_stat(invalidated,    "Times the tables were invalidated",
                  Count, invalidated(Count)).
variant_trie_stat(reevaluated,    "Times the tables were reevaluated",
                  Count, reevaluated(Count)).
variant_trie_stat(space,          "Memory usage for answer tables",
                  Bytes, size(Bytes)).
variant_trie_stat(compiled_space, "Memory usage for compiled answer tables",
                  Bytes, compiled_size(Bytes)).
variant_trie_stat(variables,      "Number of variables in answer skeletons",
                  Count, variables(Count)).
variant_trie_stat(wait,		  "Times table was waited for",
                  Count, wait(Count)).
variant_trie_stat(deadlock,	  "Times table was involved in a deadlock",
                  Count, deadlock(Count)).

%!  write_variant_table(+Title, +Pairs)

write_variant_table(Format-Args, Pairs) :-
    format(string(Title), Format, Args),
    tty_size(_, Cols),
    W is Cols - 8,
    format('~`\u2015t~*|~n', [W]),
    format('~t~w~t~*|~n', [Title, W]),
    format('~`\u2015t~*|~n', [W]),
    maplist(write_variant_stat(W), Pairs).

write_variant_stat(W, V-Stat) :-
    \+ \+ ( numbervars(V, 0, _, [singletons(true)]),
            (   integer(Stat)
            ->  format('~p ~`.t ~D~*|~n', [V, Stat, W])
            ;   format('~p ~`.t ~2f~*|~n', [V, Stat, W])
            )
          ).

table(M:Variant, Trie) :-
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:Variant, Trie).


                /*******************************
                *         DUMP TABLES          *
                *******************************/

%!  tdump is det.
%!  tdump(:Goal) is det.
%!  tdump(:Goal, +Options) is det.
%
%   Dump all tables and their status that _unify_ with Goal.  Options:
%
%     - scope(Scope)
%       Limit displayed tables to `local` or `global`.
%     - limit(Count)
%       Limit the number of answers displayed to Count
%     - reset(Boolean)
%       If `true`, also show reset (fresh) global tables.  These
%       are tables that have been abolished.

tdump :-
    tdump(_:_).
tdump(M:Goal) :-
    tdump(M:Goal, []).

tdump(M:Goal, Options) :-
    option(scope(Scope), Options, _),
    option(limit(Limit), Options, 100),
    (   table(Scope, M:Goal, Trie),
	'$tbl_table_status'(Trie, Status, M:Variant, Skeleton),
        M:'$table_mode'(Head0, Variant, Moded),
        Head = M:Head0,
        (   option(reset(true), Options)
        ->  true
        ;   \+ (Scope == global, Status == fresh)
        ),
        ansi_format(comment, 'Trie for variant ', []),
        pflags(Variant, Flags),
        format('~s ', [Flags]),
        print_variant(Head),
        Answer = Head,
        '$tbl_trienode'(Reserved),
        (   Moded == Reserved
        ->  findall(Answer-Delay,
                    '$tbl_answer'(Trie, Skeleton, Delay), Pairs),
            ExtraProp = ''
        ;   findall(Answer-Delay,
                    '$tbl_answer'(Trie, Skeleton, Moded, Delay), Pairs),
            ExtraProp = 'moded, '
        ),
        sort(1, @<, Pairs, Sorted),
        length(Sorted, Count),
        status_color(Status, Color),
        ansi_format(comment, ' (~p,', [Scope]),
        ansi_format(Color,   ' ~p', [Status]),
        ansi_format(comment, ', ~w~D answers)~n', [ExtraProp, Count]),
        (   Count == 0
        ->  ansi_format(warning, '  (empty)~n', [])
        ;   forall(limit(Limit, member(Ans, Sorted)),
                   dump_answer(M, Ans))
        ),
        fail
    ;   true
    ).

status_color(invalid, warning) :- !.
status_color(_, comment).


table(local, Variant, Trie) :-
    '$tbl_local_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Variant0, Trie),
    subsumes_term(Variant, Variant0),
    Variant = Variant0.
table(global, Variant, Trie) :-
    '$tbl_global_variant_table'(VariantTrie),
    trie_gen(VariantTrie, Variant0, Trie),
    subsumes_term(Variant, Variant0),
    Variant = Variant0.

print_variant(Head) :-
    term_attvars(Head, []),
    !,
    \+ \+ ( numbervars(Head, 0, _),
            ansi_format(code,  '~p', [Head])
          ).
print_variant(Head) :-
    copy_term(Head, Copy, Constraints),
    numbervars(Copy+Constraints, 0, _),
    format('~p', [Copy]),
    forall(member(C, Constraints),
           ansi_format(fg(blue), ', ~p', [C])).

dump_answer(M, Answer0-true) :-
    !,
    unqualify(Answer0, M, Answer),
    \+ \+ print_answer(Answer).
dump_answer(M, Answer0-Condition) :-
    unqualify(Answer0, M, Answer),
    unqualify(Condition, M, SimpleCondition),
    \+ \+ ( numbervars(Answer+SimpleCondition, 0, _),
            format('  ~p', [Answer]),
            ansi_format(bold, ' :- ', []),
            ansi_format(fg(cyan), '~p~n', [SimpleCondition])
          ).

print_answer(Answer) :-
    term_attvars(Answer, []),
    !,
    numbervars(Answer, 0, _),
    format('  ~p~n', [Answer]).
print_answer(Answer) :-
    copy_term(Answer, Copy, Constraints),
    numbervars(Copy+Constraints, 0, _),
    format('  ~p', [Copy]),
    forall(member(C, Constraints),
           ansi_format(fg(blue), ', ~p', [C])),
    nl.

unqualify(Var, _M, Var) :-
    var(Var),
    !.
unqualify((A0,B0), M, (A,B)) :-
    !,
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify((A0;B0), M, (A;B)) :-
    !,
    unqualify(A0, M, A),
    unqualify(B0, M, B).
unqualify(tnot(A0), M, tnot(A)) :-
    !,
    unqualify(A0, M, A).
unqualify((M1:Variant)/ModeArgs, M, Goal) :-
    !,
    M1:'$table_mode'(G0, Variant, ModeArgs),
    unqualify(M1:G0, M, Goal).
unqualify(M:G, M, G) :-
    !.
unqualify(G, _, G).

%!  tidg is det.
%!  tidg(:Goal) is det.
%
%   Dump the incremental dependency graph. idg/1  dumps the graph around
%   a given node

tidg :-
    ansi_format(comment,
                '% Node1 [falsecount] (affects -->) Node1 [falsecount]~n', []),
    forall(idg(t(_:From,FFC,FAC), affected, t(_:To,TFC,TAC)),
           \+ \+ ( numbervars(From),
                   numbervars(To),
                   print_edge(From, FFC,FAC, To, TFC,TAC)
                 )).

tidg(M:Node) :-
    ansi_format(comment,
                '% Node1 [falsecount] (affects -->) Node1 [falsecount]~n', []),
    ansi_format([bold], 'Affected nodes~n', []),
    forall(idg(t(M:Node,FFC,FAC), affected, t(_:To,TFC,TAC)),
           \+ \+ ( numbervars(Node),
                   numbervars(To),
                   print_edge(Node, FFC,FAC, To, TFC,TAC)
                 )),
    ansi_format([bold], 'Dependent nodes~n', []),
    forall(idg(t(_:From,FFC,FAC), affected, t(M:Node,TFC,TAC)),
           \+ \+ ( numbervars(From),
                   numbervars(Node),
                   print_edge(From, FFC,FAC, Node, TFC,TAC)
                 )).


print_edge(From, FFC,FAC, To, TFC,TAC) :-
    format('  '),
    print_node(From, FFC,FAC),
    format(' --> '),
    print_node(To, TFC,TAC),
    nl.

print_node(Variant, Falsecount, AnswerCount) :-
    pflags(Variant, Flags),
    format('~s ', [Flags]),
    ansi_format(code, '~p', [Variant]),
    format(' '),
    (   Falsecount == 0
    ->  ansi_format(comment, '[0]', [])
    ;   ansi_format([bg(red),fg(white)], '[~w]', [Falsecount])
    ),
    print_answer_count(AnswerCount).

print_answer_count(answers(Count)) =>
    format(' (~Da)', [Count]).
print_answer_count(clauses(Count)) =>
    format(' (~Dc)', [Count]).

pflags(Variant, Flags) :-
    findall(F, flag(Variant, F), Flags).

flag(Variant, Flag) :-
    (   pflag(Variant, dynamic,     'D', Flag)
    ;   pflag(Variant, incremental, 'I', Flag)
    ;   pflag(Variant, monotonic,   'M', Flag)
    ).

pflag(Variant, Property, Char, Flag) :-
    (   predicate_property(Variant, Property)
    ->  Flag = Char
    ;   Flag = ' '
    ).

idg(t(FM:From,FFC,FAC), Dir, t(TM:To,TFC,TAC)) :-
    '$tbl_variant_table'(VTrie),
    trie_gen(VTrie, FM:FVariant, ATrie),
    (   FM:'$table_mode'(From1, FVariant, _FModed)
    ->  true
    ;   From1 = FVariant                 % dynamic incremental/monotonic
    ),
    subsumes_term(From, From1),
    From = From1,
    fc(ATrie, From, FFC,FAC),
    '$idg_edge'(ATrie, Dir, DepTrie),
    '$tbl_table_status'(DepTrie, _Status, TM:TVariant, _Return),
    TM:'$table_mode'(To1, TVariant, _TModed),
    subsumes_term(To, To1),
    To = To1,
    fc(DepTrie, To, TFC,TAC).

fc(ATrie, Variant, FC, AC) :-
    (   predicate_property(Variant, tabled)
    ->  trie_property(ATrie, value_count(C)),
        AC = answers(C)
    ;   aggregate_all(count, clause(Variant,_), C),
        AC = clauses(C)
    ),
    (   '$idg_falsecount'(ATrie, FC0)
    ->  (   '$idg_forced'(ATrie)
        ->  FC = FC0/'F'
        ;   FC = FC0
        )
    ;   FC = 0
    ).

%!  summarize_idg is det.
%!  summarize_idg(+TopN) is det.
%
%   Implements XSB's statistics(summarize_idg)

:- module_transparent
    summarize_idg/0.

summarize_idg :-
    context_module(M),
    summarize_idg(infinite, M).

summarize_idg(M:Top) :-
    summarize_idg(Top, M).

summarize_idg(Top, M) :-
    tty_width(Width),
    header('Interior Nodes (Tabled Subgoals)', Width),
    format('Predicate~t #idg nodes~*|~n', [Width]),
    format('~`\u2015t~*|~n', [Width]),
    forall(limit(Top,
                 order_by([desc(Count),asc(PI)],
                          interior_nodes(_:_, M, PI, Count))),
           format('~q ~`.t ~D~*|~n', [PI, Count, Width])),
    nl,
    ColR is Width - 10,
    header('Leaf Nodes (Calls to Dynamic Predicates)', Width),
    format('Predicate~t #idg nodes~*|~t#facts~*|~n', [ColR, Width]),
    format('~`\u2015t~*|~n', [Width]),
    forall(limit(Top,
                 order_by([desc(Count),desc(Facts),asc(PI)],
                          leaf_nodes(_:_, M, PI, Count, Facts))),
           format('~q ~`.t ~D~*| ~`.t ~D~*|~n',
                  [PI, Count, ColR, Facts, Width])).

interior_nodes(Variant, M, PI, Count) :-
    predicate_property(Variant, tabled(incremental)),
    \+ predicate_property(Variant, imported_from(_)),
    idg_node_count(Variant, Count),
    pi_head(QPI, Variant),
    unqualify_pi(QPI, M, PI).

leaf_nodes(Variant, M, PI, Count, Facts) :-
    predicate_property(Variant, dynamic),
    predicate_property(Variant, incremental),
    \+ predicate_property(Variant, imported_from(_)),
    predicate_property(Variant, number_of_clauses(Facts)),
    idg_node_count(Variant, Count),
    pi_head(QPI, Variant),
    unqualify_pi(QPI, M, PI).


idg_node_count(Variant, Count) :-
    aggregate_all(count,
                  ( '$tbl_variant_table'(VTrie),
                    trie_gen(VTrie, Variant, _ATrie)
                  ),
                  Count).

unqualify_pi(M:PI, M, PI) :- !.
unqualify_pi(PI, _, PI).

tty_width(W) :-
    catch(tty_size(_, TtyW), _, fail),
    !,
    W is max(60, TtyW - 8).
tty_width(60).

header(Title, Width) :-
    format('~N~`\u2015t~*|~n', [Width]),
    ansi_format([bold], '~t~w~t~*|', [Title,Width]),
    nl.

