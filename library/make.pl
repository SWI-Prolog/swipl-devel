/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2014, University of Amsterdam
                              VU University Amsterdam
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

:- module(make,
          [ make/0
          ]).
:- use_module(library(check)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- set_prolog_flag(generate_debug_info, false).

/** <module>  Reload modified source files

This module provides the SWI-Prolog   `make'  facility that synchronises
Prolog internal database after loaded files have been edited.

@bug    Dependency tracking is incomplete.  Notably, there is no
        dependency tracking if compilation of one module depends
        on goal_expansion/2 or term_expansion/2 rules provided by
        another.
*/

:- multifile
    prolog:make_hook/2.


%!  make
%
%   Reload all source files that have   been changed since they were
%   loaded. This predicate peforms the following steps:
%
%     1. Compute the set of files that need to be reloaded.
%     2. Call the hook prolog:make_hook(before, Files)
%     3. Reload the files
%     4. Call the hook prolog:make_hook(after, Files)
%     5. If (4) fails, call list_undefined/0.
%
%   The hooks are called  with  an  empty   list  if  no  files need
%   reloading.

make :-
    notrace(make_no_trace).

make_no_trace :-
    '$update_library_index',
    findall(File, modified_file(File), Reload0),
    list_to_set(Reload0, Reload),
    (   prolog:make_hook(before, Reload)
    ->  true
    ;   true
    ),
    print_message(silent, make(reload(Reload))),
    maplist(reload_file, Reload),
    print_message(silent, make(done(Reload))),
    (   prolog:make_hook(after, Reload)
    ->  true
    ;   list_undefined,
        list_void_declarations
    ).

%!  modified_file(-File) is nondet.
%
%   True when File is modified after it has been loaded.
%
%   (*) A file is considered modified if   the  modification time of the
%   file is at least 1ms later that when  it was loaded. The 1ms relaxed
%   matching is used to compensate for   inconsistent float handling and
%   possible timing jitter.
%
%   @see https://github.com/SWI-Prolog/swipl-devel/issues/303

modified_file(File) :-
    source_file_property(Source, modified(Time)),
    \+ source_file_property(Source, included_in(_,_)),
    Time > 0.0,                     % See source_file/1
    (   source_file_property(Source, derived_from(File, LoadTime))
    ->  true
    ;   File = Source,
        LoadTime = Time
    ),
    (   catch(time_file(File, Modified), _, fail),
        Modified - LoadTime > 0.001             % (*)
    ->  true
    ;   source_file_property(Source, includes(Included, IncLoadTime)),
        catch(time_file(Included, Modified), _, fail),
        Modified - IncLoadTime > 0.001          % (*)
    ->  true
    ).


%!  reload_file(File)
%
%   Reload file into the proper module.
%
%   @bug    If modules import each other, we must load them in the
%           proper order for import/export dependencies.

:- public reload_file/1.                % Used by PDT

reload_file(File) :-
    source_base_name(File, Compile),
    findall(M-Opts,
            source_file_property(File, load_context(M, _, Opts)),
            Modules),
    (   Modules = [First-OptsFirst|Rest]
    ->  Extra = [ silent(false),
                  register(false)
                ],
        merge_options([if(true)|Extra], OptsFirst, OFirst),
        debug(make, 'Make: First load ~q', [load_files(First:Compile, OFirst)]),
        load_files(First:Compile, OFirst),
        forall(member(Context-Opts, Rest),
               ( merge_options([if(not_loaded)|Extra], Opts, O),
                 debug(make, 'Make: re-import: ~q',
                       [load_files(Context:Compile, O)]),
                 load_files(Context:Compile, O)
               ))
    ;   load_files(user:Compile)
    ).

source_base_name(File, Compile) :-
    file_name_extension(Compile, Ext, File),
    user:prolog_file_type(Ext, prolog),
    !.
source_base_name(File, File).


%!  prolog:make_hook(+When, +Files) is semidet.
%
%   This hook is called by make/0. It   is called with the following
%   values for When:
%
%     - before
%     The hook is called before reloading starts. The default
%     action is to do nothing.
%
%     - after
%     The hook is called after reloading completed.  The default
%     action is to call list_undefined/1 as:
%
%       ==
%       list_undefined([scan(local)]).
%       ==
%
%   The hook can be  used  to   change  program  validation, stop or
%   restart services, etc.
%
%   @arg Files is a list holding the canonical file names of the
%        files that need to be reloaded.  This list can be empty.

