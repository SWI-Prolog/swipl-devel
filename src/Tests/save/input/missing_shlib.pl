:- set_prolog_flag(autoload, false).
:- use_module(library(shlib)).
:- use_module(library(qsave)).
:- use_foreign_library('input/shlib_no_deps.so').

:- assertz((
    user:exception(missing_shared_object,
                   _{ arch: _, file: Spec },
                   retry) :-
        Spec = 'input/shlib_no_deps.so',
        rename_file('input/shlib_no_deps.so.bak',Spec)
   )).

% Test loading md54pl.so through user:exception
shlib_test :-
    shlib_fun(V),
    format('~q.',[V]).

% vim: sw=4 ft=prolog :
