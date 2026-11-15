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
                push_prolog_flag,
                thread_prolog_flags,
                shared_prolog_flags,
                theme_hook
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

test(user_flags_warning, [Val-Msgs == 1-[implicit_create_flag(Name)]]) :-
    %  With user_flags = warning, set_prolog_flag/2 prints a warning and
    %  creates the flag.
    gensym(f, Name),
    current_prolog_flag(user_flags, Old),
    setup_call_cleanup(
        set_prolog_flag(user_flags, warning),
        catch_messages(warning, set_prolog_flag(Name, 1), Msgs),
        set_prolog_flag(user_flags, Old)),
    current_prolog_flag(Name, Val).

:- end_tests(prolog_flags).

:- begin_tests(push_prolog_flag).

test(nested, X1+X2+X3+X4 == fail+warning+fail+error) :-
    set_prolog_flag(unknown, error),
    push_prolog_flag(unknown, fail),
    current_prolog_flag(unknown, X1),
    push_prolog_flag(unknown, warning),
    current_prolog_flag(unknown, X2),
    pop_prolog_flag(unknown),
    current_prolog_flag(unknown, X3),
    pop_prolog_flag(unknown),
    current_prolog_flag(unknown, X4).
test(cleanup_on_exception, Orig+After == false+false) :-
    set_prolog_flag(occurs_check, false),
    current_prolog_flag(occurs_check, Orig),
    catch(setup_call_cleanup(
              push_prolog_flag(occurs_check, error),
              throw(boom),
              pop_prolog_flag(occurs_check)),
          boom, true),
    current_prolog_flag(occurs_check, After).
test(absent_flag_pushed_then_removed) :-
    gensym(push_test_, Name),
    \+ current_prolog_flag(Name, _),
    push_prolog_flag(Name, hello),
    current_prolog_flag(Name, hello),
    pop_prolog_flag(Name),
    \+ current_prolog_flag(Name, _).
test(absent_flag_nested) :-
    gensym(push_test_, Name),
    push_prolog_flag(Name, first),
    push_prolog_flag(Name, second),
    current_prolog_flag(Name, second),
    pop_prolog_flag(Name),
    current_prolog_flag(Name, first),
    pop_prolog_flag(Name),
    \+ current_prolog_flag(Name, _).
test(pop_without_push, error(existence_error(pushed_flag, never_pushed_flag))) :-
    pop_prolog_flag(never_pushed_flag).
test(invalid_value_leaves_stack_unchanged,
     error(domain_error(unknown, notavalue))) :-
    set_prolog_flag(unknown, error),
    push_prolog_flag(unknown, notavalue).
test(invalid_value_no_stack_effect) :-
    set_prolog_flag(unknown, error),
    catch(push_prolog_flag(unknown, notavalue), _, true),
    catch(pop_prolog_flag(unknown), E, true),
    assertion(E = error(existence_error(pushed_flag, unknown), _)).
test(boolean_mask_toggle, V0+V1+V2 == false+true+false) :-
    set_prolog_flag(occurs_check, false),
    current_prolog_flag(occurs_check, V0),
    push_prolog_flag(occurs_check, true),
    current_prolog_flag(occurs_check, V1),
    pop_prolog_flag(occurs_check),
    current_prolog_flag(occurs_check, V2).

:- end_tests(push_prolog_flag).

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

:- begin_tests(shared_prolog_flags,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(set_in_thread, Val == b) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(atom), shared(true)]),
    in_thread(set_prolog_flag(Name, b)),
    current_prolog_flag(Name, Val).
test(seen_by_running_thread, Val == b) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(atom), shared(true)]),
    thread_self(Me),
    thread_create(( thread_get_message(go),
                    current_prolog_flag(Name, V),
                    thread_send_message(Me, seen(V))
                  ), Id),
    set_prolog_flag(Name, b),
    thread_send_message(Id, go),
    thread_get_message(seen(Val)),
    thread_join(Id).
test(not_shared, Val == a) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(atom)]),
    in_thread(set_prolog_flag(Name, b)),
    current_prolog_flag(Name, Val).
test(type, Type == atom) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(atom), shared(true)]),
    in_thread(set_prolog_flag(Name, b)),
    '$current_prolog_flag'(Name, _, _, _, Type).
test(term_value, Val =@= f(Y,Y)) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(term), shared(true)]),
    in_thread(set_prolog_flag(Name, f(X,X))),
    current_prolog_flag(Name, Val).
test(make_shared, Val == c) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(atom)]),
    create_prolog_flag(Name, b, [type(atom), shared(true)]),
    in_thread(set_prolog_flag(Name, c)),
    current_prolog_flag(Name, Val).
test(push, error(permission_error(push, prolog_flag, Name))) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [type(atom), shared(true)]),
    push_prolog_flag(Name, b).
test(local_and_shared, error(domain_error(prolog_flag_option, _))) :-
    test_flag(Name),
    create_prolog_flag(Name, a, [local(true), shared(true)]).
test(read_in_message, Seen == v) :-
    %  set_prolog_flag/2 prints the warning for creating a flag while it
    %  holds the lock on the flags.  Printing a message reads the shared
    %  flag `theme`, so reading a shared flag must not need this lock.
    test_flag(Name),
    create_prolog_flag(Name, v, [type(atom), shared(true)]),
    test_flag(New),
    current_prolog_flag(user_flags, UserFlags),
    setup_call_cleanup(
        ( set_prolog_flag(user_flags, warning),
          asserta((user:thread_message_hook(_, warning, _) :-
                       current_prolog_flag(Name, V),
                       nb_setval(test_prolog_flag_seen, V)), Ref)
        ),
        ignore(set_prolog_flag(New, 1)),
        ( erase(Ref),
          set_prolog_flag(user_flags, UserFlags)
        )),
    nb_getval(test_prolog_flag_seen, Seen).
test(concurrent) :-
    %  Smoke test for reading a flag that other threads write.  Readers
    %  copy the value while holding the lock, so they never use an atom
    %  that a writer released and AGC reclaimed.  That window is too
    %  narrow to hit reliably, so passing does not prove the copy.
    test_flag(Name),
    create_prolog_flag(Name, v0, [type(atom), shared(true)]),
    numlist(1, 4, Writers),
    maplist(writer(Name), Writers, Ids),
    forall(between(1, 20000, _),
           ( current_prolog_flag(Name, V),
             atom(V)
           )),
    maplist(thread_join, Ids).

writer(Name, I, Id) :-
    thread_create(forall(between(1, 2000, J),
                         ( format(atom(V), 'v~w_~w', [I, J]),
                           set_prolog_flag(Name, V),
                           (   J mod 500 =:= 0
                           ->  garbage_collect_atoms
                           ;   true
                           )
                         )), Id).

:- end_tests(shared_prolog_flags).

:- begin_tests(theme_hook,
               [ setup(theme_setup(Old)),
                 cleanup(theme_cleanup(Old))
               ]).

:- dynamic
    hook_active/0,
    hook_called/2.
:- multifile
    prolog:theme_hook/1.

prolog:theme_hook(Theme) :-
    hook_active,
    thread_self(Me),
    assertz(hook_called(Theme, Me)).
prolog:theme_hook(Theme) :-
    hook_active,
    Theme == broken,
    throw(error(broken_theme_hook, _)).
prolog:theme_hook(Theme) :-
    hook_active,
    member(Sol, [sol1, sol2]),
    assertz(hook_called(Theme, Sol)).
prolog:theme_hook(Theme) :-
    hook_active,
    current_prolog_flag(theme, Current),
    assertz(hook_called(Theme, flag(Current))).

theme_setup(Old) :-
    (   current_prolog_flag(theme, Old0)
    ->  Old = Old0
    ;   create_prolog_flag(theme, default, [type(atom), shared(true)]),
        Old = default
    ),
    assertz(hook_active).

theme_cleanup(Old) :-
    retractall(hook_active),
    set_prolog_flag(theme, Old),
    retractall(hook_called(_,_)).

hook_calls(Calls) :-
    findall(T-W, retract(hook_called(T, W)), Calls).

test(called, Calls == [t1-Me, t1-sol1, t1-sol2, t1-flag(t0)]) :-
    set_prolog_flag(theme, t0),
    retractall(hook_called(_,_)),
    thread_self(Me),
    set_prolog_flag(theme, t1),
    hook_calls(Calls).
test(unchanged, Calls == []) :-
    set_prolog_flag(theme, t2),
    retractall(hook_called(_,_)),
    set_prolog_flag(theme, t2),
    hook_calls(Calls).
test(exception, Theme+Calls == t3+[broken-Me]) :-
    set_prolog_flag(theme, t3),
    retractall(hook_called(_,_)),
    thread_self(Me),
    catch(set_prolog_flag(theme, broken), E, true),
    assertion(subsumes_term(error(broken_theme_hook, _), E)),
    current_prolog_flag(theme, Theme),
    hook_calls(Calls).
test(thread, [ condition(current_prolog_flag(threads, true)),
               Calls == [t4-Id, t4-sol1, t4-sol2, t4-flag(t3)]
             ]) :-
    set_prolog_flag(theme, t3),
    retractall(hook_called(_,_)),
    thread_create(set_prolog_flag(theme, t4), Id),
    thread_join(Id),
    hook_calls(Calls).

:- end_tests(theme_hook).


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
