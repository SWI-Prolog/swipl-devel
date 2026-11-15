:- module(xref_classes, []).

% Data for test_prolog_xref.pl.  Only cross-referenced, never loaded.
% This is what xpce's pce_begin_class/3 expands into; using it directly
% avoids loading xpce.

:- pce_begin_class_definition(xref_test_class_a, @(default), object,
                              "First class").
:- pce_begin_class_definition(xref_test_class_b, @(default), object,
                              "Second class").
