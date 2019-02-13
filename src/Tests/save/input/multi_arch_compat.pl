:- set_prolog_flag(autoload, false).
:- use_module(library(shlib)).
:- use_module(library(qsave)).
:- asserta( user:file_search_path(foreign, './input') ).
:- use_foreign_library(foreign(shlib_no_deps)).

% Test loading shlib_no_deps.so ith another architecture
% which is compatible with the current one.
shlib_test :-
    shlib_fun(V),
    format('~q.',[V]).


:- multifile qsave:arch_shlib/3.
qsave:arch_shlib('x86_64-myarch', Spec, File, []) :-
    Spec == foreign(shlib_no_deps),
    absolute_file_name(Spec, File,
                       [ access(execute),
                         file_type(executable)
                       ]).

qsave:compat_arch(_, 'x86_64-myarch').

% vim: sw=4 ft=prolog :
