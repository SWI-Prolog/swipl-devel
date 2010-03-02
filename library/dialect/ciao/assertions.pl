:- new_declaration(comment/2).

%% To become obsolete? MH
:- op(975, xfx,(=>)).
:- op(978, xfx,(::)).

:- new_declaration(decl/1).            :- op(1150, fx,(decl)).
:- new_declaration(decl/2).            :- op(1150,xfx,(decl)).
:- new_declaration(pred/1).            :- op(1150, fx,(pred)).
:- new_declaration(pred/2).            :- op(1150,xfx,(pred)).
%% % Should be in functions library
%% :- new_declaration(func/1).            :- op(1150, fx,(func)).
%% :- new_declaration(func/2).            :- op(1150,xfx,(func)).
:- new_declaration(prop/1).            :- op(1150, fx,(prop)).
:- new_declaration(prop/2).            :- op(1150,xfx,(prop)).
:- new_declaration(modedef/1).         :- op(1150, fx,(modedef)).
%any sense?PBC :- new_declaration(modedef/2).         :- op(1150,xfx,(modedef)).

:- new_declaration(calls/1).           :- op(1150, fx,(calls)).
:- new_declaration(calls/2).           :- op(1150,xfx,(calls)).
:- new_declaration(success/1).         :- op(1150, fx,(success)).
:- new_declaration(success/2).         :- op(1150,xfx,(success)).
:- new_declaration(test/1).            :- op(1150, fx,(test)).
:- new_declaration(test/2).            :- op(1150,xfx,(test)).
:- new_declaration(comp/1).            :- op(1150, fx,(comp)).
:- new_declaration(comp/2).            :- op(1150,xfx,(comp)).

%% To become obsolete? MH 
:- new_declaration(entry/1).           :- op(1150, fx,(entry)).
%obsolete-PBC :- new_declaration(entry/2).           :- op(1150,xfx,(entry)).

%% DTM, new declaration
:- new_declaration(exit/1).            :- op(1150, fx,(exit)). 
:- new_declaration(exit/2).            :- op(1150,xfx,(exit)). 
