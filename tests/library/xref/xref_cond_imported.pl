:- module(xref_cond_imported, [p/0]).
:- if((nb_setval(xref_cond_ran, true), fail)).
:- export(bad/0).
:- elif(current_prolog_flag(bounded, _)).
:- export(good/0).
:- else.
:- export(bad/0).
:- endif.

p.
good.
bad.
