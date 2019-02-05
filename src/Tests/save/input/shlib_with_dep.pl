:- set_prolog_flag(autoload, false).
:- use_module(library(shlib)).
:- use_module(library(qsave)).
:- use_foreign_library('input/shlib_with_dep.so').

:- assertz((
    user:exception(missing_shared_object,
                   _{ arch: _, file: Spec },
                   retry) :-
        handle_missing_shlib(Spec)

   )).

handle_missing_shlib(Spec) :-
        Spec = 'input/shlib_with_dep.so',
        qsave_foreign_libraries(Arch, 'input/shlib_with_dep.so', [Dep], [deps]),
        copy_file(Dep.entry,'libdep.so'). % linker finds it in current dir

% Test loading md54pl.so through user:exception
shlib_test :-
    shlib_fun(V),
    format('~q.',[V]).


% Helpers
copy_file(From, To) :-
    setup_call_cleanup(
        open(From, read, In, [type(binary)]),
        setup_call_cleanup(
            open(To, write, Out, [type(binary)]),
            copy_stream_data(In,Out),
            close(Out)
        ),
        close(In)).


% qsave hook
qsave:arch_shlib(_, File, File, ['input/libdep.so']) :-
    File == 'input/shlib_with_dep.so'.


% vim: sw=4 ft=prolog :
