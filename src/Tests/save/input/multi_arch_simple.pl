:- use_foreign_library('./input/shlib_no_deps').

% Test loading shlib_no_deps from the
% saved state
shlib_test :-
    shlib_fun(V), % V == three_three_three
    format('~q.',[V]).

% vim: sw=4 ft=prolog :
