/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
                         CWI, Amsterdam
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

:- module(prolog_deps,
          [ file_autoload_directives/3,      % +File, -Directives, +Options
            file_auto_import/2               % +File, +Options
          ]).
:- use_module(library(apply), [convlist/3, maplist/3]).
:- use_module(library(filesex), [copy_file/2]).
:- use_module(library(lists), [select/3, subtract/3, append/3, member/2]).
:- use_module(library(option), [option/2, option/3]).
:- use_module(library(pairs), [group_pairs_by_key/2]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(prolog_code), [pi_head/2]).
:- use_module(library(prolog_source),
              [ file_name_on_path/2,
                path_segments_atom/2,
                prolog_open_source/2,
                prolog_read_source_term/4,
                prolog_close_source/1
              ]).
:- use_module(library(prolog_xref),
              [ xref_source/1,
                xref_module/2,
                xref_called/4,
                xref_defined/3,
                xref_built_in/1
              ]).
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module(library(solution_sequences), [distinct/2]).

/** <module> Compute file dependencies

This module computes  file  dependencies  for   _modules_  as  a  set of
directives.
*/

:- multifile user:file_search_path/2.

user:file_search_path(noautoload, library(.)).
user:file_search_path(noautoload, library(semweb)).
user:file_search_path(noautoload, library(lynx)).
user:file_search_path(noautoload, library(tipc)).
user:file_search_path(noautoload, library(cql)).
user:file_search_path(noautoload, library(http)).
user:file_search_path(noautoload, library(dcg)).
user:file_search_path(noautoload, library(unicode)).
user:file_search_path(noautoload, library(clp)).
user:file_search_path(noautoload, library(pce(prolog/lib))).


%!  file_autoload_directives(+File, -Directives, +Options) is det.
%
%   Compute the dependencies as autoload/2 directives.  Options
%
%     - missing(+Bool)
%       If `true` (default `false`), only generate directives
%       for called predicates that are not already imported.
%
%     - directive(+Directive)
%       Directive to use for adding dependencies.  Defined
%	options are:
%
%       - use_autoload/2
%         (Default).  This uses use_module/2 for files that
%         cannot be imported using use_autoload/2.
%       - use_autoload/1
%         This uses use_module/1 for files that cannot be
%	  imported using use_autoload/1.
%       - use_module/2
%       - use_module/1
%
%     - update(Old)
%       Updated an existing set of directives.  The returned
%       set of Directive starts with copies of Old.  If a
%       member of Old is autoload/2 or use_module/2, new
%       dependencies are added at the end of this list.
%       New dependent files are added after the modified
%       copies of Old.  Declared dependencies are never
%       removed, even if no proof of usage is found.
%
%       If no directive(+Directive) option is provided a
%       default is determined from the given directives.

file_autoload_directives(File, Directives, Options) :-
    xref_source(File),
    findall(Head, distinct(Head, undefined(File, Head, Options)), Missing),
    convlist(missing_autoload(File), Missing, Pairs),
    keysort(Pairs, Pairs1),
    group_pairs_by_key(Pairs1, Grouped),
    directives(Grouped, Directives, Options).

%!  undefined(+File, -Callable, +Options)
%
%   Callable is called in File, but no   definition can be found. If
%   File is not a module file we   consider other files that are not
%   module files.

undefined(File, Undef, Options) :-
    xref_module(File, _),
    !,
    xref_called_cond(File, Undef, Cond),
    \+ (   available(File, Undef, How, Options),
           How \== plain_file
       ),
    included_if_defined(Cond, Undef),
    Undef \= (_:_).
undefined(File, Undef, Options) :-
    xref_called_cond(File, Undef, Cond),
    \+ available(File, Undef, _, Options),
    included_if_defined(Cond, Undef),
    Undef \= (_:_).

%!  included_if_defined(+Condition, +Callable) is semidet.

included_if_defined(true, _)  :- !.
included_if_defined(false, _) :- !, fail.
included_if_defined(fail, _)  :- !, fail.
included_if_defined(current_predicate(Name/Arity), Callable) :-
    \+ functor(Callable, Name, Arity),
    !.
included_if_defined(\+ Cond, Callable) :-
    !,
    \+ included_if_defined(Cond, Callable).
included_if_defined((A,B), Callable) :-
    !,
    included_if_defined(A, Callable),
    included_if_defined(B, Callable).
included_if_defined((A;B), Callable) :-
    !,
    (   included_if_defined(A, Callable)
    ;   included_if_defined(B, Callable)
    ).

xref_called_cond(Source, Callable, Cond) :-
    xref_called(Source, Callable, By, Cond),
    By \= Callable.                 % recursive calls

%!  available(+File, +Callable, -HowDefined, +Options)
%
%   True if Callable is available in File.

available(File, Called, How, Options) :-
    xref_defined(File, Called, How0),
    (   How0 = imported(_)
    ->  option(missing(true), Options)
    ;   true
    ),
    !,
    How = How0.
available(_, Called, How, _) :-
    built_in_predicate(Called),
    !,
    How = builtin.
available(_, Called, How, _) :-
    Called = _:_,
    defined(_, Called),
    !,
    How = module_qualified.
available(_, M:G, How, _) :-
    defined(ExportFile, G),
    xref_module(ExportFile, M),
    !,
    How = module_overruled.
available(_, Called, How, _) :-
    defined(ExportFile, Called),
    \+ xref_module(ExportFile, _),
    !,
    How == plain_file.

%!  built_in_predicate(+Callable)
%
%   True if Callable is a built-in

built_in_predicate(Goal) :-
    strip_module(Goal, _, Plain),
    xref_built_in(Plain).

%!  defined(?File, ?Callable)
%
%   True if Callable is defined in File and not imported.

defined(File, Callable) :-
    xref_defined(File, Callable, How),
    How \= imported(_).


		 /*******************************
		 *       GENERATE OUTPUT	*
		 *******************************/

missing_autoload(Src, Head, From-Head) :-
    xref_defined(Src, Head, imported(From)),
    !.
missing_autoload(_Src, Head, File-Head) :-
    predicate_property(Head, autoload(File0)),
    !,
    (   absolute_file_name(File0, File,
                           [ access(read),
                             file_type(prolog),
                             file_errors(fail)
                           ])
    ->  true
    ;   File = File0
    ).
missing_autoload(_Src, Head, File-Head) :-
    noautoload(Head, File),
    !.
missing_autoload(_Src, Head, _) :-
    pi_head(PI, Head),
    print_message(warning,
                  error(existence_error(procedure, PI), _)),
    fail.

%!  directives(+FileAndHeads, -Directives, +Options) is det.
%
%   Assemble the final set of directives. Uses the option update(Old).

directives(FileAndHeads, Directives, Options) :-
    option(update(Old), Options, []),
    phrase(update_directives(Old, FileAndHeads, RestDeps), Directives, Rest),
    update_style(Old, Options, Options1),
    maplist(directive(Options1), RestDeps, Rest0),
    sort(Rest0, Rest).

update_directives([], Deps, Deps) -->
    [].
update_directives([:-(H)|T], Deps0, Deps) -->
    { update_directive(H, Deps0, Deps1, Directive) },
    !,
    [ :-(Directive) ],
    update_directives(T, Deps1, Deps).
update_directives([H|T], Deps0, Deps) -->
    [ H ],
    update_directives(T, Deps0, Deps).

update_directive(Dir0, Deps0, Deps, Dir) :-
    directive_file(Dir0, FileSpec),
    absolute_file_name(FileSpec, File,
                       [ file_type(prolog),
                         file_errors(fail),
                         access(read)
                       ]),
    select(DepFile-Heads, Deps0, Deps),
    same_dep_file(DepFile, File),
    !,
    (   Dir0 =.. [Pred,File0,Imports]
    ->  maplist(pi_head, PIs, Heads),
        subtract(PIs, Imports, New),
        append(Imports, New, NewImports),
        Dir =.. [Pred,File0,NewImports]
    ;   Dir = Dir0
    ).

directive_file(use_module(File),   File).
directive_file(use_module(File,_), File).
directive_file(autoload(File),     File).
directive_file(autoload(File,_),   File).

same_dep_file(File, File) :-
    !.
same_dep_file(Dep, _File) :-
    exists_file(Dep),
    !,
    fail.
same_dep_file(Dep, File) :-
    user:prolog_file_type(Ext, prolog),
    file_name_extension(Dep, Ext, DepFile),
    same_file(DepFile, File),
    !.


%!  update_style(+OldDirectives, +Options0, -Options)
%
%   Determine  the  directive  to  use    for   new  dependencies.  This
%   establishes a default based on existing dependencies.

update_style(_Old, Options, Options) :-
    option(directive(_), Options),
    !.
update_style(Old, Options, [directive(autoload/2)|Options]) :-
    memberchk((:- autoload(_,_)), Old),
    !.
update_style(Old, Options, [directive(autoload/1)|Options]) :-
    memberchk((:- autoload(_)), Old),
    !.
update_style(Old, Options, [directive(use_module/2)|Options]) :-
    memberchk((:- use_module(_,_)), Old),
    !.
update_style(Old, Options, [directive(use_module/1)|Options]) :-
    memberchk((:- use_module(_)), Old),
    !.
update_style(_, Options, Options).


%!  directive(+Options, +FileAndHeads, -Directive)
%
%   Create a directive to import Heads from File.

directive(Options, File-Heads, Directive) :-
    file_name_extension(File, pl, LibFile),
    file_name_on_path(LibFile, Lib0),
    segments(Lib0, Lib),
    maplist(pi_head, PIs, Heads),
    make_directive(Lib, PIs, Directive, Options).

segments(Term0, Term) :-
    Term0 =.. [Alias,Atom],
    path_segments_atom(Segments, Atom),
    format(atom(Atom), '~q', [Segments]),
    !,
    Term =.. [Alias,Segments].
segments(FilePL, File) :-
    atom(FilePL),
    file_name_extension(File, pl, FilePL),
    !.
segments(Term, Term).

:- multifile
    prolog:no_autoload_module/1.

make_directive(Lib, Import, (:- use_module(Lib, Import)), Options) :-
    option(directive(use_module/2), Options, use_autoload/2),
    !.
make_directive(Lib, _Import, (:- use_module(Lib)), Options) :-
    option(directive(use_module/1), Options, use_autoload/2),
    !.
make_directive(Lib, _Import, (:- use_module(Lib)), Options) :-
    option(directive(use_autoload/1), Options, use_autoload/2),
    prolog:no_autoload_module(Lib),
    !.
make_directive(Lib, Import, (:- use_module(Lib, Import)), _) :-
    prolog:no_autoload_module(Lib),
    !.
make_directive(Lib, _Import, (:- autoload(Lib)), Options) :-
    option(directive(use_autoload/1), Options, use_autoload/2),
    !.
make_directive(Lib, Import, (:- autoload(Lib, Import)), _).


		 /*******************************
		 *          NO AUTOLOAD		*
		 *******************************/

:- dynamic
    library_index/3,                % Head x Module x Path
    autoload_directories/1,         % List
    index_checked_at/1.             % Time
:- volatile
    library_index/3,
    autoload_directories/1,
    index_checked_at/1.

noautoload(Head, File) :-
    functor(Head, Name, Arity),
    context_module(Here),
    '$autoload':load_library_index(Here:Name, Arity, Here:noautoload('INDEX')),
    library_index(Head, _, File).


		 /*******************************
		 *           REPLACE		*
		 *******************************/

%!  file_auto_import(+File, +Options)
%
%   Update the autoload/2 directives for File. This predicate __modifies
%   the file in place__. Defined options are:
%
%     - backup(+Extension)
%       Create a backup of File using Extension.

file_auto_import(File, Options) :-
    absolute_file_name(File, Path,
                       [ file_type(prolog),
                         access(read)
                       ]),
    file_autoload_directives(Path, Directives, Options),
    (   option(backup(Ext), Options)
    ->  file_name_extension(Path, Ext, Old),
        copy_file(Path, Old)
    ;   true
    ),
    Edit = _{import:Directives, done:_},
    (   has_import(Path)
    ->  edit_file(Old, Path, Edit.put(replace,true))
    ;   edit_file(Old, Path, Edit.put(new,true))
    ).

has_import(InFile) :-
    setup_call_cleanup(
        prolog_open_source(InFile, In),
        (   repeat,
            prolog_read_source_term(In, Term, _Expanded, []),
            (   Term == end_of_file
            ->  !
            ;    true
            )
        ),
        prolog_close_source(In)),
    nonvar(Term),
    import_directive(Term),
    !.

import_directive((:- use_module(_))).
import_directive((:- use_module(_, _))).

%!  rewrite_term(+In, -Keep, -OutList, +Options) is semidet.

rewrite_term(Never,_,_,_) :-
    never_rewrite(Never),
    !,
    fail.
rewrite_term(Import,false,[],Options) :-
    Options.done == true,
    !,
    import_directive(Import).
rewrite_term(In,false,Directives,Options) :-
    import_directive(In),
    !,
    append(Options.import, [nl], Directives),
    Options.done = true.
rewrite_term(In,true,Directives,Options) :-
    In = (:- module(_,_)),
    Options.get(new) == true,
    !,
    append(Options.import, [nl], Directives),
    Options.done = true.

never_rewrite((:- use_module(_, []))).

edit_file(InFile, OutFile, Options) :-
    read_file_to_string(InFile, String, []),
    setup_call_cleanup(
        prolog_open_source(InFile, In),
        setup_call_cleanup(
            open(OutFile, write, Out),
            rewrite(In, Out, String, Options),
            close(Out)),
        prolog_close_source(In)).

rewrite(In, Out, String, Options) :-
    prolog_read_source_term(
        In, Term, _Expanded,
        [ term_position(StartPos),
          subterm_positions(TermPos),
          comments(Comments)
        ]),
    stream_position_data(char_count, StartPos, StartChar),
    copy_comments(Comments, StartChar, String, Out),
    (   Term == end_of_file
    ->  true
    ;   (   nonvar(Term),
            rewrite_term(Term, Keep, List, Options)
        ->  (   Keep == true
            ->  copy_term_string(TermPos, String, Out)
            ;   true
            ),
            forall(member(T, List),
                   output_term(Out, T)),
            (   append(_, [nl], List)
            ->  skip_blanks(In)
            ;   true
            )
        ;   copy_term_string(TermPos, String, Out)
        ),
        rewrite(In, Out, String, Options)
    ).

output_term(Out, nl) :-
    !,
    nl(Out).
output_term(Out, Term) :-
    print_term(Term, [output(Out)]),
    format(Out, '.~n', []).

copy_comments([Pos-H|T], StartChar, String, Out) :-
    stream_position_data(char_count, Pos, Start),
    Start < StartChar,
    !,
    string_length(H, Len),
    sub_string(String, Start, Len, _, Comment),
    End is Start+Len+1,
    layout_after(End, String, Layout),
    format(Out, '~s~s', [Comment, Layout]),
    copy_comments(T, StartChar, String, Out).
copy_comments(_, _, _, _).

copy_term_string(TermPos, String, Out) :-
    arg(1, TermPos, Start),
    arg(2, TermPos, End),
    Len is End - Start,
    sub_string(String, Start, Len, _, TermString),
    End1 is End + 1,
    full_stop_after(End1, String, Layout),
    format(Out, '~s~s', [TermString, Layout]).

layout_after(Index, String, [H|T]) :-
    string_code(Index, String, H),
    code_type(H, space),
    !,
    Index2 is Index+1,
    layout_after(Index2, String, T).
layout_after(_, _, []).

full_stop_after(Index, String, [H|T]) :-
    string_code(Index, String, H),
    Index2 is Index+1,
    (   code_type(H, space)
    ->  !, full_stop_after(Index2, String, T)
    ;   H == 0'.
    ->  !, layout_after(Index2, String, T)
    ).
full_stop_after(_, _, []).

skip_blanks(In) :-
    peek_code(In, C),
    code_type(C, space),
    !,
    get_code(In, _),
    skip_blanks(In).
skip_blanks(_).
