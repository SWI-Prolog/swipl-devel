:- module(xref_cond_missing, []).
:- if(exists_source(library(xref_no_such_file_guarded))).
:- autoload(library(xref_no_such_file_guarded), [p/0]).
:- endif.
:- autoload(library(xref_no_such_file_plain), [q/0]).
