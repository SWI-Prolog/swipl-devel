/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2011, University of Amsterdam
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

:- module(test_aggregate,
	  [ test_aggregate/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(aggregate)).

test_aggregate :-
	run_tests([ foreach,
		    aggregate
		  ]).

:- begin_tests(foreach).

test(forall, true) :-
	foreach(between(1, 10, X), integer(X)).
test(forall, fail) :-
	foreach(between(1, 2, X), _=X).

:- end_tests(foreach).

:- begin_tests(aggregate).

%%	country(Country, Area, Population)
%
%	Area  (Km2)  and  Popupation  (persons)  of  European  countries
%	(source, Wikipedia, 2002 estimate)

country('Monaco',		   2,	     31987).
country('Gibraltar (UK)',	   6,	     27714).
country('Vatican City',		   0.44,     900).
country('Malta',		   316,	     397499).
country('Guernsey (UK)',	   78,	     64587).
country('Jersey (UK)',		   116,	     89775).
country('San Marino',		   61,	     27730).
country('Netherlands',		   41526,    16491461).
country('Belgium',		   30510,    10274595).
country('United Kingdom',	   244820,   60587000).
country('Germany',		   357021,   83251851).
country('Liechtenstein',	   160,	     32842).
country('Italy',		   301230,   59715625).
country('Switzerland',		   41290,    7301994).
country('Luxembourg',		   2586,     448569).
country('Andorra',		   468,	     68403).
country('Moldova',		   33843,    4434547).
country('Czech Republic',	   78866,    10256760).
country('Isle of Man (UK)',	   572,	     73873).
country('Denmark',		   43094,    5368854).
country('Poland',		   312685,   38625478).
country('Albania',		   28748,    3544841).
country('Armenia',		   29800,    3330099).
country('Slovakia',		   48845,    5422366).
country('Serbia',		   88361,    9780000).
country('France',		   547030,   60765983).
country('Portugal',		   92391,    10084245).
country('Hungary',		   93030,    10075034).
country('Austria',		   83858,    8169929).
country('Slovenia',		   20273,    1932917).
country('Romania',		   238391,   22303552).
country('Azerbaijan',		   86600,    7798497).
country('Spain',		   504782,   45061274).
country('Turkey',		   780580,   67308928).
country('Cyprus',		   9250,     767314).
country('Republic of Macedonia',   25333,    2054800).
country('Greece',		   131940,   10645343).
country('Ukraine',		   603700,   48396470).
country('Croatia',		   56542,    4390751).
country('Bosnia and Herzegovina',  51129,    3964388).
country('Georgia',		   69700,    4960951).
country('Bulgaria',		   110910,   7621337).
country('Republic of Ireland',	   70280,    4234925).
country('Lithuania',		   65200,    3601138).
country('Belarus',		   207600,   10335382).
country('Latvia',		   64589,    2366515).
country('Montenegro',		   13812,    500000).
country('Faroe Islands (Denmark)', 1399,     46011).
country('Estonia',		   45226,    1415681).
country('Sweden',		   449964,   8876744).
country('Finland',		   337030,   5183545).
country('Norway',		   324220,   4525116).
country('Russia',		   17075200, 144978573).
country('Iceland',		   103000,   279384).
country('Svalbard (Norway)',	   62049,    2868).

age(sara, 25).
age(john, 25).
age(bob,  41).

test(aggregate_sum, Sum == 3) :-
	aggregate(sum(X), between(1,2,X), Sum).
test(aggregate_count, Count == 2) :-
	aggregate(count, X^between(1,2,X), Count).
test(aggregate_term, Result == term(2, 3)) :-
	aggregate(term(count, sum(X)), between(1,2,X), Result).
test(aggregate_term_bag, Result == term(2, [1,2])) :-
	aggregate(term(count, bag(X)), between(1,2,X), Result).
test(aggregate_term0, Result == term(0, [])) :-
	aggregate_all(term(count, bag(X)), between(1,0,X), Result).
test(aggregate_age, set(Name-Sum == [sara-25,john-25,bob-41])) :-
	aggregate(sum(X), age(Name, X), Sum).
test(aggregate_age_disc, Sum == 91) :-
	aggregate(sum(X), Name, age(Name, X), Sum).
test(max_density, Country == max(15993.5, 'Monaco')) :-
	aggregate(max(Pop/Area, Country), country(Country, Area, Pop), Country).
test(density_range, Result == r(max(DMon, 'Monaco'),
				min(DSva, 'Svalbard (Norway)'))) :-
	DMon is 31987/2,
	DSva is 2868/62049,
	aggregate(r(max(Pop/Area, Country), min(Pop/Area, Country)),
		  country(Country, Area, Pop), Result).
test(aggregate_all, Max == 3) :-
	List = [1,2,3],
	aggregate_all(r(max(A)), member(A,List), r(Max)).
test(e_vars, all(X == [1,2,3,4,5])) :-
	aggregate(r(sum(0)), Y^(between(1, 5, X), Y=1), _).

:- end_tests(aggregate).
