/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2025, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(app_splfr, []).

:- initialization(main, main).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  file  emulates  SICStus   the    splfr   program,  which  extracts
declarations for foreign  resources  from  a   Prolog  file,  creates  a
wrapper, compiles this and finally  generates   a  shared object that is
automatically loaded into SWI-Prolog.

Note that this implementation  is  only   partial.  It  was  written for
running Alpino (www.let.rug.nl/vannoord/alp/Alpino/) and  only processes
the commandline options needed for this.

To use this facility, copy this file to   a  directory in your $PATH and
edit the first line to reflect the location of SWI-Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(prolog_source)).
:- use_module(library(qpforeign)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(dialect)).
:- use_module(library(main)).

:- debug(swipl_frl).

main(Argv) :-
    argv_options(Argv, Inputs, Options, [unknown_option(pass)]),
    (   catch(swipl_frl(Inputs, Options), Error,
              (     print_message(error, Error),
                    halt(1)
              ))
    ->  halt
    ;   print_message(error, goal_failed(swipl_frl(Argv))),
        halt(1)
    ).

opt_type(cflag, cflag, atom).

opt_help(help(header),
	 md("# Emulator for SICStus splfr\n\n\c
             The `splfr` app provides a partial emulation for the \c
             SICStus Prolog way of linking foreign resources.
             ")).
opt_help(help(usage),
	 " [option ...] inputs ...").
opt_help(help(footer),
         md("The command line should have exactly one Prolog file. \c
             All remaining options except for the documented options are \c
             passed to the C compiler.")).
opt_help(cflag,
	 "Flags to pass to the C compiler").

swipl_frl(Positional, Options) :-
    partition(plfile, Positional, PlFiles, Rest),
    PlFiles = [PlFile],
    !,
    file_name_extension(Base, _Ext, PlFile),
    create_glue(PlFile, GlueFile),
    option(cflag(CFlags), Options, ''),
    atomic_list_concat([GlueFile,CFlags|Rest], ' ', Cmd0),
    format(atom(Cmd), 'swipl-ld -shared -o ~w ~w', [Base, Cmd0]),
    debug(swipl_frl, '~w', [Cmd]),
    shell(Cmd).
swipl_frl(_Positional, _Options) :-
    argv_usage(debug).

plfile(Name) :-
    \+ sub_atom(Name, 0, _, _, -),
    file_name_extension(_, pl, Name).

%!  create_glue(+PrologFile, -GlueFile) is det
%
%   Create the glue foreign resources  in   PrologFile.  The glue is
%   written to GlueFile.

create_glue(File, Glue) :-
    file_name_extension(Base, _Ext, File),
    atom_concat(Base, '_swi_glue', GlueBase),
    file_name_extension(GlueBase, c, Glue),
    load_resource_decls(File, Module),
    create_module_glue(Module, Base, GlueBase).

create_module_glue(Module, Base, GlueBase) :-
    Module:foreign_resource(Resource, _),
    make_foreign_resource_wrapper(Module:Resource, Base, GlueBase).


%!  load_resource_decls(+Source, -Module)
%
%   Load SICSTus/Quintus resource declarations   from Source. Module
%   is the module in which the resources are loaded.

load_resource_decls(Source, Module) :-
    expects_dialect(sicstus),
    prolog_canonical_source(Source, Id),
    setup_call_cleanup(prolog_open_source(Id, In),
                       process(In, no_module, Module),
                       prolog_close_source(In)).


process(In, State0, Module) :-
    prolog_read_source_term(In, _, Expanded, []),
    process_terms(Expanded, State0, State1),
    (   State1 = end_of_file(EndState)
    ->  state_module(EndState, Module)
    ;   process(In, State1, Module)
    ).

process_terms([], State, State) :- !.
process_terms([H|T], State0, State) :-
    !,
    process_term(H, State0, State1),
    (   State1 == end_of_file
    ->  State = State1
    ;   process_terms(T, State1, State)
    ).
process_terms(T, State0, State) :-
    process_term(T, State0, State).

process_term(end_of_file, State, end_of_file(State)) :- !.
process_term((:- module(Name, _)), _, module(Name)) :-
    !,
    clean_resources(Name).
process_term(Term, State, State) :-
    foreign_term(Term, Assert),
    !,
    state_module(State, M),
    assertz(M:Assert).
process_term(_, State, State).

foreign_term(foreign_resource(Name, Funcs), foreign_resource(Name, Funcs)).
foreign_term(foreign(Func, Pred), foreign(Func, c, Pred)).
foreign_term(foreign(Func, Lang, Pred), foreign(Func, Lang, Pred)).

state_module(module(M), M).
state_module(no_module, user).

clean_resources(Module) :-
    forall(foreign_term(_, T), Module:retractall(T)).
