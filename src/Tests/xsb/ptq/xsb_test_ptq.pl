/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
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

:- module(xsb_test_ptq,
          [ xsb_test_ptq/0,

            ptq_run/5                           % +Query, -S, -TE1, -CN, -IV
          ]).
:- use_module(library(plunit)).
:- use_module('../xsb_test').
:- load_files(ptq:'ptq.P',         [ dialect(xsb) ]).
:- load_files(ptqlred:'ptqlred.P', [ dialect(xsb) ]).
:- load_files('parser.P',          [ dialect(xsb) ]).

xsb_test_ptq :-
    run_tests([xsb_ptq]).

:- meta_predicate
    write_table(:),
    write_table(+, :).

%!  ptq_test(+Query, +Base)
%
%   Run a test and compare the ouput

ptq_test(Query, Base) :-
    ptq_run(Query, S, TE1, CN, IV),
    ptq_cmp(Base, s, S, Ok),
    ptq_cmp(Base, te1, TE1, Ok),
    ptq_cmp(Base, cn, CN, Ok),
    ptq_cmp(Base, iv, IV, Ok),
    var(Ok).

ptq_cmp(Base, Suffix, OurOutput, _) :-
    source_file(ptq_test(_,_), File),
    file_directory_name(File, Dir),
    atomic_list_concat([Dir, /, Base, 'int_', Suffix, '1'], GoldFile),
    xsb_cmp_results(GoldFile, OurOutput),
    !.
ptq_cmp(_, _, _, false).

%!  ptq_run(+Query, -S, -TE, -CN, -IV)
%
%   Run a PTQ query and capture the result for the four tables.

ptq_run(Query, S, TE1, CN, IV) :-
    abolish_all_tables,
    call(Query),
    Query = M:_,
    with_output_to(string(S),   write_table(M:s(_,_,_,_,_))),
    with_output_to(string(TE1), write_table(M:te1(_,_,_,_,_,_,_))),
    with_output_to(string(CN),  write_table(M:cn(_,_,_,_,_,_))),
    with_output_to(string(IV),  write_table(M:iv(_,_,_,_,_,_))).

write_table(M:Goal) :-
    write_table(current_output, M:Goal).

write_table(Out, M:Goal) :-
    (   '$tbl_trienode'(Reserved),
        current_table(M2:Variant, Trie),
        subsumes_term(M:Goal, M2:Variant),
        '$tbl_table_status'(Trie, Status, M:Answer, Skeleton),
        assertion(Status == complete),
        trie_gen(Trie, Skeleton, Reserved),
        numbervars(Answer,0,_),
        write_term(Out, Answer,
                   [ numbervars(true),
                     ignore_ops(true),
                     quoted(true)
                   ]),
        nl(Out),
        fail
    ;   true
    ).

term_expansion(ptq(Id, Query),
               (test(Id) :- ptq_test(ptq:Query, Id))).
term_expansion(ptqlred(Id, Query),
               (test(Id) :- ptq_test(ptqlred:Query, Id))).


:- begin_tests(xsb_ptq, [sto(rational_trees)]).

ptqlred(jtaulred, tran([john,talks,about,a,unicorn])).
ptq(mbjfauahei, tran([mary,believes_that,john,finds,a,unicorn,and,he,eats,it])).
ptqlred(mbjfauaheilred, tran([mary,believes_that,john,finds,a,unicorn,and,he,eats,it])).
ptq(jtau, tran([john,talks,about,a,unicorn])).
ptq(jdm, tran([john,dates,mary])).
ptq(apr, tran([a,price,rises])).
ptq(jdh, tran([john,dates,him])).
%ptq(jwiap, tran([john,walks,in,a,park])).
%ptq(jfau, tran([john,finds,a,unicorn])).
ptq(jsau, tran([john,seeks,a,unicorn])).
%ptq(amtw, tran([a,man,tries_to,walk])).
ptq(awtra, tran([a,woman,tries_to,run,allegedly])).
ptq(emlaw, tran([every,man,loves,a,woman])).
ptq(awsswr, tran([a,woman,such_that,she,walks,runs])).
ptq(jsauamsi, tran([john,seeks,a,unicorn,and,mary,seeks,it])).

ptqlred(jdmlred, tran([john,dates,mary])).
%ptqlred(aprlred, tran([a,price,rises])).
ptqlred(jdhlred, tran([john,dates,him])).
ptqlred(jwiaplred, tran([john,walks,in,a,park])).
ptqlred(jfaulred, tran([john,finds,a,unicorn])).
ptqlred(jsaulred, tran([john,seeks,a,unicorn])).
%ptqlred(amtwlred, tran([a,man,tries_to,walk])).
ptqlred(awtralred, tran([a,woman,tries_to,run,allegedly])).
ptqlred(emlawlred, tran([every,man,loves,a,woman])).
ptqlred(awsswrlred, tran([a,woman,such_that,she,walks,runs])).
%ptqlred(jsauamsi, tran([john,seeks,a,unicorn,and,mary,seeks,it])).
%ptqlred(jbmwiaplred, tran([john,believes_that,mary,walks,in,a,park])).
ptqlred(jbmwwaiplred, tran([john,believes_that,mary,wishes_to,walk,in,a,park])).
ptqlred(jwfauaeilred, tran([john,wishes_to,find,a,unicorn,and,eat,it])).
ptqlred(jtfauaweilred, tran([john,tries_to,find,a,unicorn,and,wishes_to,eat,it])).
ptqlred(emlawsslhlred, tran([every,man,loves,a,woman,such_that,she,loves,him])).
ptqlred(emlawaslhlred, tran([every,man,loves,a,woman,and,she,loves,him])).
%ptqlred(emlawiapvaefeapslred, tran([every,man,loves,a,woman,in,a,park,voluntarily,and,every,fish,eats,a,pen,slowly])).

:- end_tests(xsb_ptq).
