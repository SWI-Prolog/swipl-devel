/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2025, University of Amsterdam
                              VU University Amsterdam
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

:- module(prolog_source,
          [ prolog_read_source_term/4,  % +Stream, -Term, -Expanded, +Options
            read_source_term_at_location/3, %Stream, -Term, +Options
            prolog_file_directives/3,   % +File, -Directives, +Options
            prolog_open_source/2,       % +Source, -Stream
            prolog_close_source/1,      % +Stream
            prolog_canonical_source/2,  % +Spec, -Id

            load_quasi_quotation_syntax/2, % :Path, +Syntax

            file_name_on_path/2,        % +File, -PathSpec
            file_alias_path/2,          % ?Alias, ?Dir
            path_segments_atom/2,       % ?Segments, ?Atom
            directory_source_files/3,   % +Dir, -Files, +Options
            valid_term_position/2       % +Term, +TermPos
          ]).
:- use_module(library(debug), [debug/3, assertion/1]).
:- autoload(library(apply), [maplist/2, maplist/3, foldl/4]).
:- autoload(library(error), [domain_error/2, is_of_type/2]).
:- autoload(library(lists), [member/2, last/2, select/3, append/3, selectchk/3]).
:- autoload(library(operators), [push_op/3, push_operators/1, pop_operators/0]).
:- autoload(library(option), [select_option/4, option/3, option/2]).
:- autoload(library(modules),[in_temporary_module/3]).


/** <module> Examine Prolog source-files

This module provides predicates  to  open,   close  and  read terms from
Prolog source-files. This may seem  easy,  but   there  are  a couple of
problems that must be taken care of.

        * Source files may start with #!, supporting PrologScript
        * Embedded operators declarations must be taken into account
        * Style-check options must be taken into account
        * Operators and style-check options may be implied by directives
        * On behalf of the development environment we also wish to
          parse PceEmacs buffers

This module concentrates these issues  in   a  single  library. Intended
users of the library are:

        $ prolog_xref.pl :   The Prolog cross-referencer
        $ prolog_clause.pl : Get details about (compiled) clauses
        $ prolog_colour.pl : Colourise source-code
        $ PceEmacs :         Emacs syntax-colouring
        $ PlDoc :            The documentation framework
*/

:- thread_local
    open_source/2,          % Stream, State
    mode/2.                 % Stream, Data

:- multifile
    requires_library/2,
    prolog:xref_source_identifier/2, % +Source, -Id
    prolog:xref_source_time/2,       % +Source, -Modified
    prolog:xref_open_source/2,       % +SourceId, -Stream
    prolog:xref_close_source/2,      % +SourceId, -Stream
    prolog:alternate_syntax/4,       % Syntax, +Module, -Setup, -Restore
    prolog:xref_update_syntax/2,     % +Directive, +Module
    prolog:quasi_quotation_syntax/2. % Syntax, Library


:- predicate_options(prolog_read_source_term/4, 4,
                     [ pass_to(system:read_clause/3, 3)
                     ]).
:- predicate_options(read_source_term_at_location/3, 3,
                     [ line(integer),
                       offset(integer),
                       module(atom),
                       operators(list),
                       error(-any),
                       pass_to(system:read_term/3, 3)
                     ]).
:- predicate_options(directory_source_files/3, 3,
                     [ recursive(boolean),
                       if(oneof([true,loaded])),
                       pass_to(system:absolute_file_name/3,3)
                     ]).


                 /*******************************
                 *           READING            *
                 *******************************/

%!  prolog_read_source_term(+In, -Term, -Expanded, +Options) is det.
%
%   Read a term from a Prolog source-file.  Options is a option list
%   that is forwarded to read_clause/3.
%
%   This predicate is intended to read the   file from the start. It
%   tracks  directives  to  update  its   notion  of  the  currently
%   effective syntax (e.g., declared operators).
%
%   @param Term     Term read
%   @param Expanded Result of term-expansion on the term
%   @see   read_source_term_at_location/3 for reading at an
%          arbitrary location.

prolog_read_source_term(In, Term, Expanded, Options) :-
    maplist(read_clause_option, Options),
    !,
    select_option(subterm_positions(TermPos), Options,
                  RestOptions, TermPos),
    read_clause(In, Term,
                [ subterm_positions(TermPos)
                | RestOptions
                ]),
    expand(Term, TermPos, In, Expanded),
    '$current_source_module'(M),
    update_state(Term, Expanded, M).
prolog_read_source_term(In, Term, Expanded, Options) :-
    '$current_source_module'(M),
    select_option(syntax_errors(SE), Options, RestOptions0, dec10),
    select_option(subterm_positions(TermPos), RestOptions0,
                  RestOptions, TermPos),
    (   style_check(?(singleton))
    ->  FinalOptions = [ singletons(warning) | RestOptions ]
    ;   FinalOptions = RestOptions
    ),
    read_term(In, Term,
              [ module(M),
                syntax_errors(SE),
                subterm_positions(TermPos)
              | FinalOptions
              ]),
    expand(Term, TermPos, In, Expanded),
    update_state(Term, Expanded, M).

read_clause_option(syntax_errors(_)).
read_clause_option(term_position(_)).
read_clause_option(process_comment(_)).
read_clause_option(comments(_)).

:- public
    expand/3.                       % Used by Prolog colour

expand(Term, In, Exp) :-
    expand(Term, _, In, Exp).

expand(Var, _, _, Var) :-
    var(Var),
    !.
expand(Term, _, _, Term) :-
    no_expand(Term),
    !.
expand(Term, _, _, _) :-
    requires_library(Term, Lib),
    ensure_loaded(user:Lib),
    fail.
expand(Term, _, In, Term) :-
    chr_expandable(Term, In),
    !.
expand(Term, Pos, _, Expanded) :-
    expand_term(Term, Pos, Expanded, _).

no_expand((:- if(_))).
no_expand((:- elif(_))).
no_expand((:- else)).
no_expand((:- endif)).
no_expand((:- require(_))).

chr_expandable((:- chr_constraint(_)), In) :-
    add_mode(In, chr).
chr_expandable((handler(_)), In) :-
    mode(In, chr).
chr_expandable((rules(_)), In) :-
    mode(In, chr).
chr_expandable(<=>(_, _), In) :-
    mode(In, chr).
chr_expandable(@(_, _), In) :-
    mode(In, chr).
chr_expandable(==>(_, _), In) :-
    mode(In, chr).
chr_expandable(pragma(_, _), In) :-
    mode(In, chr).
chr_expandable(option(_, _), In) :-
    mode(In, chr).

add_mode(Stream, Mode) :-
    mode(Stream, Mode),
    !.
add_mode(Stream, Mode) :-
    asserta(mode(Stream, Mode)).

%!  requires_library(+Term, -Library)
%
%   known expansion hooks.  May be expanded as multifile predicate.

requires_library((:- emacs_begin_mode(_,_,_,_,_)), library(emacs_extend)).
requires_library((:- draw_begin_shape(_,_,_,_)),   library(pcedraw)).
requires_library((:- use_module(library(pce))),    library(pce)).
requires_library((:- pce_begin_class(_,_)),        library(pce)).
requires_library((:- pce_begin_class(_,_,_)),      library(pce)).
requires_library((:- html_meta(_)),                library(http/html_decl)).

%!  update_state(+Term, +Expanded, +Module) is det.
%
%   Update operators and style-check options from Term or Expanded.

:- multifile
    pce_expansion:push_compile_operators/1,
    pce_expansion:pop_compile_operators/0.

update_state((:- pce_end_class), _, _) =>
    ignore(pce_expansion:pop_compile_operators).
update_state((:- pce_extend_class(_)), _, SM) =>
    pce_expansion:push_compile_operators(SM).
update_state(Raw, _, Module),
    catch(prolog:xref_update_syntax(Raw, Module),
          error(_,_),
          fail) =>
    true.
update_state(_Raw, Expanded, M) =>
    update_state(Expanded, M).

update_state(Var, _) :-
    var(Var),
    !.
update_state([], _) :-
    !.
update_state([H|T], M) :-
    !,
    update_state(H, M),
    update_state(T, M).
update_state((:- Directive), M) :-
    nonvar(Directive),
    !,
    catch(update_directive(Directive, M), _, true).
update_state((?- Directive), M) :-
    !,
    update_state((:- Directive), M).
update_state(MetaDecl, _M) :-
    MetaDecl = html_write:html_meta_head(_Head,_Module,_Meta),
    (   clause(MetaDecl, true)
    ->  true
    ;   assertz(MetaDecl)
    ).
update_state(_, _).

update_directive(Directive, Module) :-
    prolog:xref_update_syntax((:- Directive), Module),
    !.
update_directive(module(Module, Public), _) :-
    atom(Module),
    is_list(Public),
    !,
    '$set_source_module'(Module),
    maplist(import_syntax(_,Module, _), Public).
update_directive(M:op(P,T,N), SM) :-
    atom(M),
    ground(op(P,T,N)),
    !,
    update_directive(op(P,T,N), SM).
update_directive(op(P,T,N), SM) :-
    ground(op(P,T,N)),
    !,
    strip_module(SM:N, M, PN),
    push_op(P,T,M:PN).
update_directive(style_check(Style), _) :-
    ground(Style),
    style_check(Style),
    !.
update_directive(use_module(Spec), SM) :-
    ground(Spec),
    catch(module_decl(Spec, Path, Public), _, fail),
    is_list(Public),
    !,
    maplist(import_syntax(Path, SM, _), Public).
update_directive(use_module(Spec, Imports), SM) :-
    ground(Spec),
    is_list(Imports),
    catch(module_decl(Spec, Path, Public), _, fail),
    is_list(Public),
    !,
    maplist(import_syntax(Path, SM, Imports), Public).
update_directive(pce_begin_class_definition(_,_,_,_), SM) :-
    pce_expansion:push_compile_operators(SM),
    !.
update_directive(_, _).

%!  import_syntax(+Path, +Module, +Imports, +ExportStatement) is det.
%
%   Import syntax affecting aspects  of   a  declaration. Deals with
%   op/3 terms and Syntax/4  quasi   quotation  declarations.

import_syntax(_, _, _, Var) :-
    var(Var),
    !.
import_syntax(_, M, Imports, Op) :-
    Op = op(_,_,_),
    \+ \+ member(Op, Imports),
    !,
    update_directive(Op, M).
import_syntax(Path, SM, Imports, Syntax/4) :-
    \+ \+ member(Syntax/4, Imports),
    load_quasi_quotation_syntax(SM:Path, Syntax),
    !.
import_syntax(_,_,_, _).


%!  load_quasi_quotation_syntax(:Path, +Syntax) is semidet.
%
%   Import quasi quotation syntax Syntax from   Path into the module
%   specified by the  first  argument.   Quasi  quotation  syntax is
%   imported iff:
%
%     - It is already loaded
%     - It is declared with prolog:quasi_quotation_syntax/2
%
%   @tbd    We need a better way to know that an import affects the
%           syntax or compilation process.  This is also needed for
%           better compatibility with systems that provide a
%           separate compiler.

load_quasi_quotation_syntax(SM:Path, Syntax) :-
    atom(Path), atom(Syntax),
    source_file_property(Path, module(M)),
    functor(ST, Syntax, 4),
    predicate_property(M:ST, quasi_quotation_syntax),
    !,
    use_module(SM:Path, [Syntax/4]).
load_quasi_quotation_syntax(SM:Path, Syntax) :-
    atom(Path), atom(Syntax),
    prolog:quasi_quotation_syntax(Syntax, Spec),
    absolute_file_name(Spec, Path2,
                       [ file_type(prolog),
                         file_errors(fail),
                         access(read)
                       ]),
    Path == Path2,
    !,
    use_module(SM:Path, [Syntax/4]).

%!  module_decl(+FileSpec, -Source, -Exports) is semidet.
%
%   If FileSpec refers to a Prolog  module   file,  unify  Path with the
%   canonical file path to the file and Decl with the second argument of
%   the module declaration.

module_decl(Spec, Source, Exports) :-
    absolute_file_name(Spec, Path,
                       [ file_type(prolog),
                         file_errors(fail),
                         access(read)
                       ]),
    module_decl_(Path, Source, Exports).

module_decl_(Path, Source, Exports) :-
    file_name_extension(_, qlf, Path),
    !,
    '$qlf_module'(Path, Info),
    _{file:Source, exports:Exports} :< Info.
module_decl_(Path, Path, Exports) :-
    setup_call_cleanup(
        prolog_open_source(Path, In),
        read_module_decl(In, Exports),
        prolog_close_source(In)).

read_module_decl(In, Decl) :-
    read(In, Term0),
    read_module_decl(Term0, In, Decl).

read_module_decl((:- module(_, DeclIn)), _In, Decl) =>
    Decl = DeclIn.
read_module_decl((:- encoding(Enc)), In, Decl) =>
    set_stream(In, encoding(Enc)),
    read(In, Term2),
    read_module_decl(Term2, In, Decl).
read_module_decl(_, _, _) =>
    fail.


%!  read_source_term_at_location(+Stream, -Term, +Options) is semidet.
%
%   Try to read a Prolog term form   an  arbitrary location inside a
%   file. Due to Prolog's dynamic  syntax,   e.g.,  due  to operator
%   declarations that may change anywhere inside   the file, this is
%   theoreticaly   impossible.   Therefore,   this    predicate   is
%   fundamentally _heuristic_ and may fail.   This predicate is used
%   by e.g., clause_info/4 and by  PceEmacs   to  colour the current
%   clause.
%
%   This predicate has two ways to  find   the  right syntax. If the
%   file is loaded, it can be  passed   the  module using the module
%   option. This deals with  module  files   that  define  the  used
%   operators globally for  the  file.  Second,   there  is  a  hook
%   prolog:alternate_syntax/4 that can be used to temporary redefine
%   the syntax.
%
%   The options below are processed in   addition  to the options of
%   read_term/3. Note that  the  =line=   and  =offset=  options are
%   mutually exclusive.
%
%     * line(+Line)
%     If present, start reading at line Line.
%     * offset(+Characters)
%     Use seek/4 to go to the indicated location.  See seek/4
%     for limitations of seeking in text-files.
%     * module(+Module)
%     Use syntax from the given module. Default is the current
%     `source module'.
%     * operators(+List)
%     List of additional operator declarations to enforce while
%     reading the term.
%     * error(-Error)
%     If no correct parse can be found, unify Error with a term
%     Offset:Message that indicates the (character) location of
%     the error and the related message.  Adding this option
%     makes read_source_term_at_location/3 deterministic (=det=).
%
%   @see Use read_source_term/4 to read a file from the start.
%   @see prolog:alternate_syntax/4 for locally scoped operators.

:- thread_local
    last_syntax_error/2.            % location, message

read_source_term_at_location(Stream, Term, Options) :-
    retractall(last_syntax_error(_,_)),
    seek_to_start(Stream, Options),
    stream_property(Stream, position(Here)),
    '$current_source_module'(DefModule),
    option(module(Module), Options, DefModule),
    option(operators(Ops), Options, []),
    alternate_syntax(Syntax, Module, Setup, Restore),
    set_stream_position(Stream, Here),
    debug(read, 'Trying with syntax ~w', [Syntax]),
    push_operators(Module:Ops),
    call(Setup),
    Error = error(Formal,_),                 % do not catch timeout, etc.
    setup_call_cleanup(
        asserta(user:thread_message_hook(_,_,_), Ref), % silence messages
        catch(qq_read_term(Stream, Term0,
                           [ module(Module)
                           | Options
                           ]),
              Error,
              true),
        erase(Ref)),
    call(Restore),
    pop_operators,
    (   var(Formal)
    ->  !, Term = Term0
    ;   assert_error(Error, Options),
        fail
    ).
read_source_term_at_location(_, _, Options) :-
    option(error(Error), Options),
    !,
    setof(CharNo:Msg, retract(last_syntax_error(CharNo, Msg)), Pairs),
    last(Pairs, Error).

assert_error(Error, Options) :-
    option(error(_), Options),
    !,
    (   (   Error = error(syntax_error(Id),
                          stream(_S1, _Line1, _LinePos1, CharNo))
        ;   Error = error(syntax_error(Id),
                          file(_S2, _Line2, _LinePos2, CharNo))
        )
    ->  message_to_string(error(syntax_error(Id), _), Msg),
        assertz(last_syntax_error(CharNo, Msg))
    ;   debug(read, 'Error: ~q', [Error]),
        throw(Error)
    ).
assert_error(_, _).


%!  alternate_syntax(?Syntax, +Module, -Setup, -Restore) is nondet.
%
%   Define an alternative  syntax  to  try   reading  a  term  at an
%   arbitrary location in module Module.
%
%   Calls the hook prolog:alternate_syntax/4 with the same signature
%   to allow for user-defined extensions.
%
%   @param  Setup is a deterministic goal to enable this syntax in
%           module.
%   @param  Restore is a deterministic goal to revert the actions of
%           Setup.

alternate_syntax(prolog, _, true,  true).
alternate_syntax(Syntax, M, Setup, Restore) :-
    prolog:alternate_syntax(Syntax, M, Setup, Restore).


%!  seek_to_start(+Stream, +Options) is det.
%
%   Go to the location from where to start reading.

seek_to_start(Stream, Options) :-
    option(line(Line), Options),
    !,
    seek(Stream, 0, bof, _),
    seek_to_line(Stream, Line).
seek_to_start(Stream, Options) :-
    option(offset(Start), Options),
    !,
    seek(Stream, Start, bof, _).
seek_to_start(_, _).

%!  seek_to_line(+Stream, +Line)
%
%   Seek to indicated line-number.

seek_to_line(Fd, N) :-
    N > 1,
    !,
    skip(Fd, 10),
    NN is N - 1,
    seek_to_line(Fd, NN).
seek_to_line(_, _).


                 /*******************************
                 *       QUASI QUOTATIONS       *
                 *******************************/

%!  qq_read_term(+Stream, -Term, +Options)
%
%   Same  as  read_term/3,  but  dynamically    loads   known  quasi
%   quotations. Quasi quotations that  can   be  autoloaded  must be
%   defined using prolog:quasi_quotation_syntax/2.

qq_read_term(Stream, Term, Options) :-
    select(syntax_errors(ErrorMode), Options, Options1),
    ErrorMode \== error,
    !,
    (   ErrorMode == dec10
    ->  repeat,
        qq_read_syntax_ex(Stream, Term, Options1, Error),
        (   var(Error)
        ->  !
        ;   print_message(error, Error),
            fail
        )
    ;   qq_read_syntax_ex(Stream, Term, Options1, Error),
        (   ErrorMode == fail
        ->  print_message(error, Error),
            fail
        ;   ErrorMode == quiet
        ->  fail
        ;   domain_error(syntax_errors, ErrorMode)
        )
    ).
qq_read_term(Stream, Term, Options) :-
    qq_read_term_ex(Stream, Term, Options).

qq_read_syntax_ex(Stream, Term, Options, Error) :-
    catch(qq_read_term_ex(Stream, Term, Options),
          error(syntax_error(Syntax), Context),
          Error = error(Syntax, Context)).

qq_read_term_ex(Stream, Term, Options) :-
    stream_property(Stream, position(Here)),
    catch(read_term(Stream, Term, Options),
          error(syntax_error(unknown_quasi_quotation_syntax(Syntax, Module)), Context),
          load_qq_and_retry(Here, Syntax, Module, Context, Stream, Term, Options)).

load_qq_and_retry(Here, Syntax, Module, _, Stream, Term, Options) :-
    set_stream_position(Stream, Here),
    prolog:quasi_quotation_syntax(Syntax, Library),
    !,
    use_module(Module:Library, [Syntax/4]),
    read_term(Stream, Term, Options).
load_qq_and_retry(_Pos, Syntax, Module, Context, _Stream, _Term, _Options) :-
    print_message(warning, quasi_quotation(undeclared, Syntax)),
    throw(error(syntax_error(unknown_quasi_quotation_syntax(Syntax, Module)), Context)).

%!  prolog:quasi_quotation_syntax(+Syntax, -Library) is semidet.
%
%   True when the quasi quotation syntax   Syntax can be loaded from
%   Library.  Library  must  be   a    valid   first   argument  for
%   use_module/2.
%
%   This multifile hook is used   by  library(prolog_source) to load
%   quasi quotation handlers on demand.

prolog:quasi_quotation_syntax(html,       library(http/html_write)).
prolog:quasi_quotation_syntax(javascript, library(http/js_write)).


%!  prolog_file_directives(+File, -Directives, +Options) is det.
%
%   True when Directives is a list  of   directives  that  appear in the
%   source  file  File.  Reading   directives    stops   at   the  first
%   non-directive term. Processing deals with   expand_term/2 as well as
%   conditional compilation.  Options processed:
%
%     - canonical_source(-Source)
%       Unify Source with the canonical source identifier as also
%       used by library(prolog_xref).
%     - silent(+Boolean)
%       If `true` (default `false`), do not report syntax errors and
%       other errors.

prolog_file_directives(File, Directives, Options) :-
    option(canonical_source(Path), Options, _),
    prolog_canonical_source(File, Path),
    in_temporary_module(
        TempModule,
        true,
        read_directives(TempModule, Path, Directives, Options)).

read_directives(TempModule, Path, Directives, Options) :-
    setup_call_cleanup(
        read_directives_setup(TempModule, Path, In, State),
        phrase(read_directives(In, Options, [true]), Directives),
        read_directives_cleanup(In, State)).

read_directives_setup(TempModule, Path, In, state(OldM, OldXref)) :-
    prolog_open_source(Path, In),
    '$set_source_module'(OldM, TempModule),
    current_prolog_flag(xref, OldXref),
    set_prolog_flag(xref, true).

read_directives_cleanup(In, state(OldM, OldXref)) :-
    '$set_source_module'(OldM),
    set_prolog_flag(xref, OldXref),
    prolog_close_source(In).

read_directives(In, Options, State) -->
    {  E = error(_,_),
       repeat,
       catch(prolog_read_source_term(In, Term, Expanded,
                                     [ process_comment(true),
                                       syntax_errors(error)
                                     ]),
             E, report_syntax_error(E, Options))
    -> nonvar(Term),
       Term = (:-_)
    },
    !,
    terms(Expanded, State, State1),
    read_directives(In, Options, State1).
read_directives(_, _, _) --> [].

report_syntax_error(_, Options) :-
    option(silent(true), Options),
    !,
    fail.
report_syntax_error(E, _Options) :-
    print_message(warning, E),
    fail.

terms(Var, State, State) --> { var(Var) }, !.
terms([H|T], State0, State) -->
    !,
    terms(H, State0, State1),
    terms(T, State1, State).
terms((:-if(Cond)), State0, [True|State0]) -->
    !,
    { eval_cond(Cond, True) }.
terms((:-elif(Cond)), [True0|State], [True|State]) -->
    !,
    { eval_cond(Cond, True1),
      elif(True0, True1, True)
    }.
terms((:-else), [True0|State], [True|State]) -->
    !,
    { negate(True0, True) }.
terms((:-endif), [_|State], State) -->  !.
terms(H, State, State) -->
    (   {State = [true|_]}
    ->  [H]
    ;   []
    ).

eval_cond(Cond, true) :-
    catch(Cond, error(_,_), fail),
    !.
eval_cond(_, false).

elif(true,  _,    else_false) :- !.
elif(false, true, true) :- !.
elif(True,  _,    True).

negate(true,       false).
negate(false,      true).
negate(else_false, else_false).

                 /*******************************
                 *           SOURCES            *
                 *******************************/

%!  prolog_open_source(+CanonicalId:atomic, -Stream:stream) is det.
%
%   Open     source     with     given     canonical     id     (see
%   prolog_canonical_source/2)  and  remove  the  #!  line  if  any.
%   Streams  opened  using  this  predicate  must  be  closed  using
%   prolog_close_source/1. Typically using the skeleton below. Using
%   this   skeleton,   operator   and    style-check   options   are
%   automatically restored to the values before opening the source.
%
%   ==
%   process_source(Src) :-
%           prolog_open_source(Src, In),
%           call_cleanup(process(Src), prolog_close_source(In)).
%   ==

prolog_open_source(Src, Fd) :-
    '$push_input_context'(source),
    catch((   prolog:xref_open_source(Src, Fd)
          ->  Hooked = true
          ;   open(Src, read, Fd),
              Hooked = false
          ), E,
          (   '$pop_input_context',
              throw(E)
          )),
    skip_hashbang(Fd),
    push_operators([]),
    '$current_source_module'(SM),
    '$save_lex_state'(LexState, []),
    asserta(open_source(Fd, state(Hooked, Src, LexState, SM))).

skip_hashbang(Fd) :-
    catch((   peek_char(Fd, #)              % Deal with #! script
          ->  skip(Fd, 10)
          ;   true
          ), E,
          (   close(Fd, [force(true)]),
              '$pop_input_context',
              throw(E)
          )).

%!  prolog:xref_open_source(+SourceID, -Stream)
%
%   Hook  to  open   an   xref   SourceID.    This   is   used   for
%   cross-referencing non-files, such as XPCE   buffers,  files from
%   archives,  git  repositories,   etc.    When   successful,   the
%   corresponding  prolog:xref_close_source/2  hook  is  called  for
%   closing the source.


%!  prolog_close_source(+In:stream) is det.
%
%   Close  a  stream  opened  using  prolog_open_source/2.  Restores
%   operator and style options. If the stream   has not been read to
%   the end, we call expand_term(end_of_file,  _) to allow expansion
%   modules to clean-up.

prolog_close_source(In) :-
    call_cleanup(
        restore_source_context(In, Hooked, Src),
        close_source(Hooked, Src, In)).

close_source(true, Src, In) :-
    catch(prolog:xref_close_source(Src, In), _, false),
    !,
    '$pop_input_context'.
close_source(_, _Src, In) :-
    close(In, [force(true)]),
    '$pop_input_context'.

restore_source_context(In, Hooked, Src) :-
    (   at_end_of_stream(In)
    ->  true
    ;   ignore(catch(expand(end_of_file, _, In, _), _, true))
    ),
    pop_operators,
    retractall(mode(In, _)),
    (   retract(open_source(In, state(Hooked, Src, LexState, SM)))
    ->  '$restore_lex_state'(LexState),
        '$set_source_module'(SM)
    ;   assertion(fail)
    ).

%!  prolog:xref_close_source(+SourceID, +Stream) is semidet.
%
%   Called by prolog_close_source/1 to  close   a  source previously
%   opened by the hook prolog:xref_open_source/2.  If the hook fails
%   close/2 using the option force(true) is used.

%!  prolog_canonical_source(+SourceSpec:ground, -Id:atomic) is semidet.
%
%   Given a user-specification of a source,   generate  a unique and
%   indexable  identifier  for   it.   For    files   we   use   the
%   prolog_canonical absolute filename. Id must   be valid input for
%   prolog_open_source/2.

prolog_canonical_source(Source, Src) :-
    var(Source),
    !,
    Src = Source.
prolog_canonical_source(User, user) :-
    User == user,
    !.
prolog_canonical_source(Src, Id) :-             % Call hook
    prolog:xref_source_identifier(Src, Id),
    !.
prolog_canonical_source(Source, Src) :-
    source_file(Source),
    !,
    Src = Source.
prolog_canonical_source(Source, Src) :-
    absolute_file_name(Source, Src,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]),
    !.


%!  file_name_on_path(+File:atom, -OnPath) is det.
%
%   True if OnPath a description of File   based  on the file search
%   path. This performs the inverse of absolute_file_name/3.

file_name_on_path(Path, ShortId) :-
    (   file_alias_path(Alias, Dir),
        atom_concat(Dir, Local, Path)
    ->  (   Alias == '.'
        ->  ShortId = Local
        ;   file_name_extension(Base, pl, Local)
        ->  ShortId =.. [Alias, Base]
        ;   ShortId =.. [Alias, Local]
        )
    ;   ShortId = Path
    ).


%!  file_alias_path(-Alias, ?Dir) is nondet.
%
%   True if file Alias points to Dir.  Multiple solutions are
%   generated with the longest directory first.

:- dynamic
    alias_cache/2.

file_alias_path(Alias, Dir) :-
    (   alias_cache(_, _)
    ->  true
    ;   build_alias_cache
    ),
    (   nonvar(Dir)
    ->  ensure_slash(Dir, DirSlash),
        alias_cache(Alias, DirSlash)
    ;   alias_cache(Alias, Dir)
    ).

build_alias_cache :-
    findall(t(DirLen, AliasLen, Alias, Dir),
            search_path(Alias, Dir, AliasLen, DirLen), Ts),
    sort(0, >, Ts, List),
    forall(member(t(_, _, Alias, Dir), List),
           assert(alias_cache(Alias, Dir))).

search_path('.', Here, 999, DirLen) :-
    working_directory(Here0, Here0),
    ensure_slash(Here0, Here),
    atom_length(Here, DirLen).
search_path(Alias, Dir, AliasLen, DirLen) :-
    user:file_search_path(Alias, _),
    Alias \== autoload,             % TBD: Multifile predicate?
    Alias \== noautoload,
    Spec =.. [Alias,'.'],
    atom_length(Alias, AliasLen0),
    AliasLen is 1000 - AliasLen0,   % must do reverse sort
    absolute_file_name(Spec, Dir0,
                       [ file_type(directory),
                         access(read),
                         solutions(all),
                         file_errors(fail)
                       ]),
    ensure_slash(Dir0, Dir),
    atom_length(Dir, DirLen).

ensure_slash(Dir, Dir) :-
    sub_atom(Dir, _, _, 0, /),
    !.
ensure_slash(Dir0, Dir) :-
    atom_concat(Dir0, /, Dir).


%!  path_segments_atom(+Segments, -Atom) is det.
%!  path_segments_atom(-Segments, +Atom) is det.
%
%   Translate between a path  represented  as   a/b/c  and  an  atom
%   representing the same path. For example:
%
%     ==
%     ?- path_segments_atom(a/b/c, X).
%     X = 'a/b/c'.
%     ?- path_segments_atom(S, 'a/b/c'), display(S).
%     /(/(a,b),c)
%     S = a/b/c.
%     ==
%
%   This predicate is part of  the   Prolog  source  library because
%   SWI-Prolog  allows  writing  paths   as    /-nested   terms  and
%   source-code analysis programs often need this.

path_segments_atom(Segments, Atom) :-
    var(Atom),
    !,
    (   atomic(Segments)
    ->  Atom = Segments
    ;   segments_to_list(Segments, List, [])
    ->  atomic_list_concat(List, /, Atom)
    ;   throw(error(type_error(file_path, Segments), _))
    ).
path_segments_atom(Segments, Atom) :-
    atomic_list_concat(List, /, Atom),
    parts_to_path(List, Segments).

segments_to_list(Var, _, _) :-
    var(Var), !, fail.
segments_to_list(A/B, H, T) :-
    segments_to_list(A, H, T0),
    segments_to_list(B, T0, T).
segments_to_list(A, [A|T], T) :-
    atomic(A).

parts_to_path([One], One) :- !.
parts_to_path(List, More/T) :-
    (   append(H, [T], List)
    ->  parts_to_path(H, More)
    ).

%!  directory_source_files(+Dir, -Files, +Options) is det.
%
%   True when Files is a sorted list  of Prolog source files in Dir.
%   Options:
%
%     * recursive(boolean)
%     If =true= (default =false=), recurse into subdirectories
%     * if(Condition)
%     If =true= (default =loaded=), only report loaded files.
%
%   Other  options  are  passed    to  absolute_file_name/3,  unless
%   loaded(true) is passed.

directory_source_files(Dir, SrcFiles, Options) :-
    option(if(loaded), Options, loaded),
    !,
    absolute_file_name(Dir, AbsDir, [file_type(directory), access(read)]),
    (   option(recursive(true), Options)
    ->  ensure_slash(AbsDir, Prefix),
        findall(F, (  source_file(F),
                      sub_atom(F, 0, _, _, Prefix)
                   ),
                SrcFiles)
    ;   findall(F, ( source_file(F),
                     file_directory_name(F, AbsDir)
                   ),
                SrcFiles)
    ).
directory_source_files(Dir, SrcFiles, Options) :-
    absolute_file_name(Dir, AbsDir, [file_type(directory), access(read)]),
    directory_files(AbsDir, Files),
    phrase(src_files(Files, AbsDir, Options), SrcFiles).

src_files([], _, _) -->
    [].
src_files([H|T], Dir, Options) -->
    { file_name_extension(_, Ext, H),
      user:prolog_file_type(Ext, prolog),
      \+ user:prolog_file_type(Ext, qlf),
      dir_file_path(Dir, H, File0),
      absolute_file_name(File0, File,
                         [ file_errors(fail)
                         | Options
                         ])
    },
    !,
    [File],
    src_files(T, Dir, Options).
src_files([H|T], Dir, Options) -->
    { \+ special(H),
      option(recursive(true), Options),
      dir_file_path(Dir, H, SubDir),
      exists_directory(SubDir),
      !,
      catch(directory_files(SubDir, Files), _, fail)
    },
    !,
    src_files(Files, SubDir, Options),
    src_files(T, Dir, Options).
src_files([_|T], Dir, Options) -->
    src_files(T, Dir, Options).

special(.).
special(..).

% avoid dependency on library(filesex), which also pulls a foreign
% dependency.
dir_file_path(Dir, File, Path) :-
    (   sub_atom(Dir, _, _, 0, /)
    ->  atom_concat(Dir, File, Path)
    ;   atom_concat(Dir, /, TheDir),
        atom_concat(TheDir, File, Path)
    ).


%!  valid_term_position(@Term, @TermPos) is semidet.
%
%   Check that a Term has an   appropriate  TermPos layout. An incorrect
%   TermPos results in either failure of this predicate or an error.
%
%   If a position in TermPos  is  a   variable,  the  validation  of the
%   corresponding   part   of   Term   succeeds.    This   matches   the
%   term_expansion/4 treats "unknown" layout information.   If part of a
%   TermPos is given, then all its "from"   and "to" information must be
%   specified; for example,    string_position(X,Y)   is   an  error but
%   string_position(0,5) succeeds.   The position values are checked for
%   being plausible -- e.g., string_position(5,0) will fail.
%
%   This should always succeed:
%
%       read_term(Term, [subterm_positions(TermPos)]),
%       valid_term_position(Term, TermPos)
%
%   @arg Term Any Prolog term including a variable).
%   @arg TermPos The detailed layout of the term, for example
%        from using =|read_term(Term, subterm_positions(TermPos)|=.
%
%   @error existence_error(matching_rule, Subterm) if a subterm of Term
%          is inconsistent with the corresponding part of TermPos.
%
%   @see read_term/2, read_term/3, term_string/3
%   @see expand_term/4, term_expansion/4, expand_goal/4, expand_term/4
%   @see clause_info/4, clause_info/5
%   @see prolog_clause:unify_clause_hook/5

valid_term_position(Term, TermPos) :-
    valid_term_position(0, 0x7fffffffffffffff, Term, TermPos).

valid_term_position(OuterFrom, OuterTo, _Term, TermPos),
        var(TermPos),
        OuterFrom =< OuterTo => true.
valid_term_position(OuterFrom, OuterTo, Var, From-To),
        var(Var),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) => true.
valid_term_position(OuterFrom, OuterTo, Atom, From-To),
        atom(Atom),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) => true.
valid_term_position(OuterFrom, OuterTo, Number, From-To),
        number(Number),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) => true.
valid_term_position(OuterFrom, OuterTo, [], From-To),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) => true.
valid_term_position(OuterFrom, OuterTo, String, string_position(From,To)),
        (   string(String)
        ->  true
        ;   is_of_type(codes, String)
        ->  true
        ;   is_of_type(chars, String)
        ->  true
        ;   atom(String)
        ),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) => true.
valid_term_position(OuterFrom, OuterTo, {Arg},
                    brace_term_position(From,To,ArgPos)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    valid_term_position(From, To, Arg, ArgPos).
valid_term_position(OuterFrom, OuterTo, [Hd|Tl],
                    list_position(From,To,ElemsPos,none)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    term_position_list_tail([Hd|Tl], _HdPart, []),
    maplist(valid_term_position, [Hd|Tl], ElemsPos).
valid_term_position(OuterFrom, OuterTo, [Hd|Tl],
                    list_position(From, To, ElemsPos, TailPos)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    term_position_list_tail([Hd|Tl], HdPart, Tail),
    maplist(valid_term_position(From,To), HdPart, ElemsPos),
    valid_term_position(Tail, TailPos).
valid_term_position(OuterFrom, OuterTo, Term,
                    term_position(From,To, FFrom,FTo,SubPos)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    compound_name_arguments(Term, Name, Arguments),
    valid_term_position(Name, FFrom-FTo),
    maplist(valid_term_position(From,To), Arguments, SubPos).
valid_term_position(OuterFrom, OuterTo, Dict,
                    dict_position(From,To,TagFrom,TagTo,KeyValuePosList)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    dict_pairs(Dict, Tag, Pairs),
    valid_term_position(Tag, TagFrom-TagTo),
    foldl(valid_term_position_dict(From,To), Pairs, KeyValuePosList, []).
% key_value_position(From, To, SepFrom, SepTo, Key, KeyPos, ValuePos)
% is handled in valid_term_position_dict.
valid_term_position(OuterFrom, OuterTo, Term,
                    parentheses_term_position(From,To,ContentPos)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    valid_term_position(From, To, Term, ContentPos).
valid_term_position(OuterFrom, OuterTo, _Term,
                    quasi_quotation_position(From,To,
                                             SyntaxTerm,SyntaxPos,_ContentPos)),
        valid_term_position_from_to(OuterFrom, OuterTo, From, To) =>
    valid_term_position(From, To, SyntaxTerm, SyntaxPos).

valid_term_position_from_to(OuterFrom, OuterTo, From, To) :-
    integer(OuterFrom),
    integer(OuterTo),
    integer(From),
    integer(To),
    OuterFrom =< OuterTo,
    From =< To,
    OuterFrom =< From,
    To =< OuterTo.

:- det(valid_term_position_dict/5).
valid_term_position_dict(OuterFrom, OuterTo, Key-Value,
                         KeyValuePosList0, KeyValuePosList1) :-
    selectchk(key_value_position(From,To,SepFrom,SepTo,Key,KeyPos,ValuePos),
              KeyValuePosList0, KeyValuePosList1),
    valid_term_position_from_to(OuterFrom, OuterTo, From, To),
    valid_term_position_from_to(OuterFrom, OuterTo, SepFrom, SepTo),
    SepFrom >= OuterFrom,
    valid_term_position(From, SepFrom, Key, KeyPos),
    valid_term_position(SepTo, To, Value, ValuePos).

%!  term_position_list_tail(@List, -HdPart, -Tail) is det.
%
%   Similar to append(HdPart, [Tail], List) for   proper lists, but also
%   works for inproper lists, in which  case   it  unifies Tail with the
%   tail of the partial list. HdPart is always a proper list:
%
%   ```
%   ?- prolog_source:term_position_list_tail([a,b,c], Hd, Tl).
%   Hd = [a, b, c],
%   Tl = [].
%   ?- prolog_source:term_position_list_tail([a,b|X], Hd, Tl).
%   X = Tl,
%   Hd = [a, b].
%   ```

:- det(term_position_list_tail/3).
term_position_list_tail([X|Xs], HdPart, Tail) =>
    HdPart = [X|HdPart2],
    term_position_list_tail(Xs, HdPart2, Tail).
term_position_list_tail(Tail0, HdPart, Tail) =>
    HdPart = [],
    Tail0 = Tail.


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(quasi_quotation(undeclared, Syntax)) -->
    [ 'Undeclared quasi quotation syntax: ~w'-[Syntax], nl,
      'Autoloading can be defined using prolog:quasi_quotation_syntax/2'
    ].
