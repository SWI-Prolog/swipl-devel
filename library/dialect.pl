/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2011, University of Amsterdam
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

:- module(prolog_dialect,
          [ expects_dialect/1,          % +Dialect
            exists_source/1,            % +Source
            source_exports/2            % +Source, ?Export
          ]).
:- use_module(library(error)).
:- use_module(library(lists)).

/**     <module> Support multiple Prolog dialects

The idea for this predicate  was  raised   by  Vitor  Santos  Costa in a
discussion to reach as a portability   framework  between SWI-Prolog and
YAP.

This library defines :- expects_dialect/1, telling  the system for which
Prolog dialect was written,  as  well   as  useful  tests in conditional
compilation:

        * exists_source/1
        * source_exports/2

@see    if/1, require/1, term_expansion/2, goal_expansion/2.
@author Jan Wielemaker
@author Vitor Santos Costa
*/

%!  expects_dialect(+Dialect:atom) is det.
%
%   Tell Prolog all subsequent code to the   end  of the file or the
%   next :- expects_dialect/1 directive is written for the indicated
%   Dialect.   The   current   dialect     is    available   through
%   prolog_load_context/2.
%
%   @tbd    Should we setup the dialect module only as autoload for
%           the current module?

expects_dialect(Dialect) :-
    must_be(atom, Dialect),
    set_prolog_flag(emulated_dialect, Dialect),
    (   Dialect == swi
    ->  true
    ;   attach_dialect(Dialect)
    ).


attach_dialect(Dialect) :-
    exists_source(library(dialect/Dialect)),
    !,
    prolog_load_context(module, Module),
    use_module(Module:library(dialect/Dialect)),
    (   current_predicate(Dialect:setup_dialect/0)
    ->  Dialect:setup_dialect
    ;   true
    ).
attach_dialect(_).


%!  exists_source(+Source) is semidet.
%
%   True if Source (a term  valid   for  load_files/2) exists. Fails
%   without error if this is not the case. The predicate is intended
%   to be used with  :-  if,  as   in  the  example  below. See also
%   source_exports/2.
%
%   ==
%   :- if(exists_source(library(error))).
%   :- use_module_library(error).
%   :- endif.
%   ==

exists_source(Source) :-
    exists_source(Source, _Path).

exists_source(Source, Path) :-
    absolute_file_name(Source, Path,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]).

%!  source_exports(+Source, +Export) is semidet.
%!  source_exports(+Source, -Export) is nondet.
%
%   True if Source exports Export. Fails   without  error if this is
%   not the case.  See also exists_source/1.
%
%   @tbd    Should we also allow for source_exports(-Source, +Export)?

source_exports(Source, Export) :-
    open_source(Source, In),
    catch(call_cleanup(exports(In, Exports), close(In)), _, fail),
    (   ground(Export)
    ->  memberchk(Export, Exports)
    ;   member(Export, Exports)
    ).

%!  open_source(+Source, -In:stream) is semidet.
%
%   Open a source location.

open_source(File, In) :-
    exists_source(File, Path),
    open(Path, read, In),
    (   peek_char(In, #)
    ->  skip(In, 10)
    ;   true
    ).

exports(In, Exports) :-
    read(In, Term),
    Term = (:- module(_Name, Exports)).

