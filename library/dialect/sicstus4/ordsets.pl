/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(sicstus4_ordsets,
	  [ ord_nonmember/2		% +Item, +Set
	  ]).
:- reexport('../sicstus/ordsets').

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(ordsets, sicstus4_ordsets).

/** <module> SICStus 4 library(ordsets).

@tbd	This library is incomplete.
	As of SICStus 4.6.0, the following predicates are missing:

	* ord_setproduct/2
	* ord_disjoint_union/3
	* ordset_order/3

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus/lib_002dordsets.html
*/

ord_nonmember(Item, Set) :- \+ ord_memberchk(Item, Set).
