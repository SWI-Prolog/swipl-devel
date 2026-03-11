/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(test_prolog_flag,
          [ test_prolog_flag/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(gensym)).

test_prolog_flag :-
    run_tests([ prolog_flags,
                thread_prolog_flags
              ]).

:- meta_predicate
    in_thread(0).

:- begin_tests(prolog_flags).

test(type, Value+Type == true+boolean) :-
    gensym(f, Name),
    set_prolog_flag(Name, on),
    flag_value_type(Name, Value, Type).
test(invalid, error(type_error(atom,42))) :-
    gensym(f, Name),
    create_prolog_flag(Name, 42, [type(atom)]).
test(preset_bool, Value+Type == true+boolean) :-
    gensym(f, Name),
    set_prolog_flag(Name, true),
    create_prolog_flag(Name, false, [type(boolean),keep(true)]),
    flag_value_type(Name, Value, Type).
test(preset_new_flag, Value+Type == true+boolean) :-
    gensym(f, Name),
    set_prolog_flag(Name, on),
    create_prolog_flag(Name, false, [type(boolean),keep(true)]),
    flag_value_type(Name, Value, Type).
test(preset_to_bool, Value+Type == true+atom) :-
    gensym(f, Name),
    set_prolog_flag(Name, on),          % converted to `true`!
    create_prolog_flag(Name, false, [type(atom),keep(true)]),
    flag_value_type(Name, Value, Type),
    assertion(set_prolog_flag(Name, hello)).
test(preset_to_float, Value+Type == 42.0+float) :-
    gensym(f, Name),
    set_prolog_flag(Name, 42),
    create_prolog_flag(Name, 0.1, [type(float),keep(true)]),
    flag_value_type(Name, Value, Type),
    assertion(set_prolog_flag(Name, 0.4)).
test(preset_oneof_1, Value+Type == aap+oneof([aap,noot,mies])) :-
    gensym(f, Name),
    set_prolog_flag(Name, aap),
    create_prolog_flag(Name, noot, [type(oneof([aap,noot,mies])),keep(true)]),
    flag_value_type(Name, Value, Type),
    assertion(set_prolog_flag(Name, mies)).
test(preset_oneof_2, error(domain_error(oneof([aap,noot,mies]),zus))) :-
    gensym(f, Name),
    set_prolog_flag(Name, aap),
    create_prolog_flag(Name, noot, [type(oneof([aap,noot,mies])),keep(true)]),
    set_prolog_flag(Name, zus).
test(preset_oneof_3, Value+Type == noot+Type) :-
    gensym(f, Name),
    set_prolog_flag(Name, aapje),
    Type = oneof([aap,noot,mies]),
    catch_messages(warning,
                   create_prolog_flag(Name, noot, [type(Type),keep(true)]),
                   Msgs),
    assertion(Msgs = [prolog_flag_invalid_preset(Name, aapje, Type, noot)]),
    flag_value_type(Name, Value, Type).
test(preset_atom_to_int, Value+Type == 42+Type) :-
    gensym(f, Name),
    set_prolog_flag(Name, aapje),
    Type = integer,
    catch_messages(warning,
                   create_prolog_flag(Name, 42, [type(Type),keep(true)]),
                   Msgs),
    assertion(Msgs = [prolog_flag_invalid_preset(Name, aapje, Type, 42)]),
    flag_value_type(Name, Value, Type).
test(preset_bool_to_term, Value+Type == false+term) :-
    gensym(f, Name),
    set_prolog_flag(Name, false),
    create_prolog_flag(Name, default, [type(term),keep(true)]),
    flag_value_type(Name, Value, Type).

:- end_tests(prolog_flags).

:- begin_tests(thread_prolog_flags,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(create_local_in_thread) :-
    test_flag(Name),
    in_thread(create_prolog_flag(Name, 1, [local(true)])),
    in_thread(\+ current_prolog_flag(Name, _)).
test(set_local_in_thread, Val == 42) :-
    test_flag(Name),
    set_prolog_flag(Name, 1),
    in_thread(create_prolog_flag(Name, 42, [])),
    current_prolog_flag(Name, Val).
test(set_global_in_thread, Val == 42) :-
    test_flag(Name),
    in_thread(create_prolog_flag(Name, 42, [])),
    current_prolog_flag(Name, Val).
test(set_global_in_thread_2, Val == 42) :-
    test_flag(Name),
    in_thread((set_prolog_flag(Name, 1),
               create_prolog_flag(Name, 42, [global(true)]))),
    in_thread(current_prolog_flag(Name, Val)),
    current_prolog_flag(Name, Val).

:- end_tests(thread_prolog_flags).


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

%!  in_thread(:Goal)
%
%   Run Goal in a temporary thread.

in_thread(Goal) :-
    thread_self(Me),
    Secret is random(1<<62),
    term_variables(Goal, Vars),
    thread_create(reify(Goal, Vars, Me, Secret), Id),
    (   thread_get_message(Me, reply(Secret, Result),
                           [ timeout(10)
                           ])
    ->  thread_join(Id),
        (   Result = true(Vars)
        ->  true
        ;   call(Result)
        )
    ;   thread_signal(Id, abort),
        thread_join(Id)
    ).

reify(Goal, Vars, Me, Secret) :-
    (   catch(Goal, E, true)
    ->  (   var(E)
        ->  thread_send_message(Me, reply(Secret, true(Vars)))
        ;   thread_send_message(Me, reply(Secret, throw(E)))
        )
    ;   thread_send_message(Me, reply(Secret, fail))
    ).

test_flag(Name) :-
    gensym(test_flag, Name).
