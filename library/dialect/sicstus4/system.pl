/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v.
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

:- module(sicstus4_system,
	  [ environ/2,			% ?Name, ?Value
	    environ/3			% ?Name, ?Value, +Source
	  ]).
:- reexport('../sicstus/system',
	    [ now/1,
	      datime/1,
	      datime/2,
	      sleep/1
	    ]).

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(system, sicstus4_system).

/** <module> SICStus 4-compatible library(system).

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dsystem.html
*/

%%	environ(?Name, ?Value) is nondet.
%%	environ(?Name, ?Value, +Source) is nondet.
%
%	True if Value is an atom associated with the environment variable
%	or system property Name.
%
%	Source may be =properties= (to get only system properties),
%	=environment= (to get only environment variables),
%	or =merged= (the default, to get both, with system properties
%	taking precedence over environment variables of the same name).
%
%	@tbd	Mode -Name is not supported.
%
%		Because SWI-Prolog doesn't have an obvious equivalent to
%		SICStus system properties, these predicates currently
%		behave as if no system properties are defined,
%		i. e. only environment variables are returned.
%
%	@compat SICStus 4

environ(_Name, _Value, properties) :- fail.
environ(Name, Value, environment) :- getenv(Name, Value).
environ(Name, Value, mixed) :-
	(   environ(Name, Value, properties)
	->  true
	;   environ(Name, Value, environment)
	).
environ(Name, Value) :- environ(Name, Value, mixed).
