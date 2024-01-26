/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(test_prolog_flag,
          [ test_prolog_flag/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(gensym)).

test_prolog_flag :-
    run_tests([ prolog_flags
              ]).

:- begin_tests(prolog_flags).

test(type, Value+Type == true+boolean) :-
    gensym(f, Name),
    set_prolog_flag(Name, on),
    flag_value_type(Name, Value, Type).
test(invalid, error(type_error(atom,42))) :-
    gensym(f, Name),
    create_prolog_flag(Name, 42, [type(atom)]).
test(preset, Value+Type == true+boolean) :-
    gensym(f, Name),
    set_prolog_flag(Name, true),
    create_prolog_flag(Name, false, [type(boolean),keep(true)]),
    flag_value_type(Name, Value, Type).
test(preset, Value+Type == true+boolean) :-
    gensym(f, Name),
    set_prolog_flag(Name, on),
    create_prolog_flag(Name, false, [type(boolean),keep(true)]),
    flag_value_type(Name, Value, Type).
test(preset, Value+Type == true+atom) :-
    gensym(f, Name),
    set_prolog_flag(Name, on),          % converted to `true`!
    create_prolog_flag(Name, false, [type(atom),keep(true)]),
    flag_value_type(Name, Value, Type),
    assertion(set_prolog_flag(Name, hello)).
test(preset, Value+Type == 42.0+float) :-
    gensym(f, Name),
    set_prolog_flag(Name, 42),
    create_prolog_flag(Name, 0.1, [type(float),keep(true)]),
    flag_value_type(Name, Value, Type),
    assertion(set_prolog_flag(Name, 0.4)).
test(preset, Value+Type == aap+oneof([aap,noot,mies])) :-
    gensym(f, Name),
    set_prolog_flag(Name, aap),
    create_prolog_flag(Name, noot, [type(oneof([aap,noot,mies])),keep(true)]),
    flag_value_type(Name, Value, Type),
    assertion(set_prolog_flag(Name, mies)).
test(preset, error(domain_error(oneof([aap,noot,mies]),zus))) :-
    gensym(f, Name),
    set_prolog_flag(Name, aap),
    create_prolog_flag(Name, noot, [type(oneof([aap,noot,mies])),keep(true)]),
    set_prolog_flag(Name, zus).
test(preset, Value+Type == noot+Type) :-
    gensym(f, Name),
    set_prolog_flag(Name, aapje),
    Type = oneof([aap,noot,mies]),
    catch_messages(warning,
                   create_prolog_flag(Name, noot, [type(Type),keep(true)]),
                   Msgs),
    assertion(Msgs = [prolog_flag_invalid_preset(Name, aapje, Type, noot)]),
    flag_value_type(Name, Value, Type).
test(preset, Value+Type == 42+Type) :-
    gensym(f, Name),
    set_prolog_flag(Name, aapje),
    Type = integer,
    catch_messages(warning,
                   create_prolog_flag(Name, 42, [type(Type),keep(true)]),
                   Msgs),
    assertion(Msgs = [prolog_flag_invalid_preset(Name, aapje, Type, 42)]),
    flag_value_type(Name, Value, Type).
test(preset, Value+Type == false+term) :-
    gensym(f, Name),
    set_prolog_flag(Name, false),
    create_prolog_flag(Name, default, [type(term),keep(true)]),
    flag_value_type(Name, Value, Type).

:- end_tests(prolog_flags).

flag_value_type(Name, Value, Type) :-
    '$current_prolog_flag'(Name, Value, _Scope, _Access, Type).

%!	catch_messages(+Kind, :Goal, -Messages) is semidet.

:- thread_local
	message/1.
:- meta_predicate
	catch_messages(?, 0, -).

catch_messages(Kind, Goal, Messages) :-
	setup_call_cleanup(
	    asserta((user:thread_message_hook(Term, Kind, _) :-
		        \+ \+ (prolog_load_context(variable_names, VarNames),
			       bind_variable_names(VarNames),
			       assertz(message(Term)))), Ref),
	    once(Goal),
	    erase(Ref)),
	findall(Msg, retract(message(Msg)), Messages).

bind_variable_names([]).
bind_variable_names([Name='$VAR'(Int)|T]) :- !,
	var_name(Int, Name),
	bind_variable_names(T).
bind_variable_names([_|T]) :-
	bind_variable_names(T).

var_name(N, Name) :-
	atom_codes(Name, [C]),
	between(0'A, 0'Z, C),
	N is C - 0'A.
