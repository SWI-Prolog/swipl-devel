/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1995-2018, University of Amsterdam
                              VU University Amsterdam
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

:- module(shlib,
          [ load_foreign_library/1,     % :LibFile
            load_foreign_library/2,     % :LibFile, +InstallFunc
            unload_foreign_library/1,   % +LibFile
            unload_foreign_library/2,   % +LibFile, +UninstallFunc
            current_foreign_library/2,  % ?LibFile, ?Public
            reload_foreign_libraries/0,
                                        % Directives
            use_foreign_library/1,      % :LibFile
            use_foreign_library/2       % :LibFile, +InstallFunc
          ]).
:- use_module(library(lists), [reverse/2]).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Utility library for loading foreign objects (DLLs, shared objects)

This   section   discusses   the   functionality   of   the   (autoload)
library(shlib), providing an interface to   manage  shared libraries. We
describe the procedure for using a foreign  resource (DLL in Windows and
shared object in Unix) called =mylib=.

First, one must  assemble  the  resource   and  make  it  compatible  to
SWI-Prolog. The details for this vary between platforms. The swipl-ld(1)
utility can be used to deal with this  in a portable manner. The typical
commandline is:

        ==
        swipl-ld -o mylib file.{c,o,cc,C} ...
        ==

Make  sure  that  one  of   the    files   provides  a  global  function
=|install_mylib()|=  that  initialises  the  module    using   calls  to
PL_register_foreign(). Here is a  simple   example  file  mylib.c, which
creates a Windows MessageBox:

    ==
    #include <windows.h>
    #include <SWI-Prolog.h>

    static foreign_t
    pl_say_hello(term_t to)
    { char *a;

      if ( PL_get_atom_chars(to, &a) )
      { MessageBox(NULL, a, "DLL test", MB_OK|MB_TASKMODAL);

        PL_succeed;
      }

      PL_fail;
    }

    install_t
    install_mylib()
    { PL_register_foreign("say_hello", 1, pl_say_hello, 0);
    }
    ==

Now write a file mylib.pl:

    ==
    :- module(mylib, [ say_hello/1 ]).
    :- use_foreign_library(foreign(mylib)).
    ==

The file mylib.pl can be loaded as a normal Prolog file and provides the
predicate defined in C.
*/

:- meta_predicate
    load_foreign_library(:),
    load_foreign_library(:, +),
    use_foreign_library(:),
    use_foreign_library(:, +).

:- dynamic
    loading/1,                      % Lib
    error/2,                        % File, Error
    foreign_predicate/2,            % Lib, Pred
    current_library/5.              % Lib, Entry, Path, Module, Handle

:- volatile                             % Do not store in state
    loading/1,
    error/2,
    foreign_predicate/2,
    current_library/5.

:- (   current_prolog_flag(open_shared_object, true)
   ->  true
   ;   print_message(warning, shlib(not_supported)) % error?
   ).

% The flag `res_keep_foreign` prevents deleting  temporary files created
% to load shared objects when set  to   `true`.  This  may be needed for
% debugging purposes.

:- create_prolog_flag(res_keep_foreign, false,
                      [ keep(true) ]).


                 /*******************************
                 *           DISPATCHING        *
                 *******************************/

%!  find_library(+LibSpec, -Lib, -Delete) is det.
%
%   Find a foreign library from LibSpec.  If LibSpec is available as
%   a resource, the content of the resource is copied to a temporary
%   file and Delete is unified with =true=.

find_library(Spec, TmpFile, true) :-
    '$rc_handle'(Zipper),
    term_to_atom(Spec, Name),
    setup_call_cleanup(
        zip_lock(Zipper),
        setup_call_cleanup(
            open_foreign_in_resources(Zipper, Name, In),
            setup_call_cleanup(
                tmp_file_stream(binary, TmpFile, Out),
                copy_stream_data(In, Out),
                close(Out)),
            close(In)),
        zip_unlock(Zipper)),
    !.
find_library(Spec, Lib, Copy) :-
    absolute_file_name(Spec, Lib0,
                       [ file_type(executable),
                         access(read),
                         file_errors(fail)
                       ]),
    !,
    lib_to_file(Lib0, Lib, Copy).
find_library(Spec, Spec, false) :-
    atom(Spec),
    !.                  % use machines finding schema
find_library(foreign(Spec), Spec, false) :-
    atom(Spec),
    !.                  % use machines finding schema
find_library(Spec, _, _) :-
    throw(error(existence_error(source_sink, Spec), _)).

%!  lib_to_file(+Lib0, -Lib, -Copy) is det.
%
%   If Lib0 is not a regular file  we   need  to  copy it to a temporary
%   regular file because dlopen()  and   Windows  LoadLibrary() expect a
%   file name. On some systems this can   be  avoided. Roughly using two
%   approaches (after discussion with Peter Ludemann):
%
%     - On FreeBSD there is shm_open() to create an anonymous file in
%       memory and than fdlopen() to link this.
%     - In general, we could redefine the system calls open(), etc. to
%       make dlopen() work on non-files.  This is highly non-portably
%       though.
%     - We can mount the resource zip using e.g., `fuse-zip` on Linux.
%       This however fails if we include the resources as a string in
%       the executable.
%
%   @see https://github.com/fancycode/MemoryModule for Windows

lib_to_file(Res, TmpFile, true) :-
    sub_atom(Res, 0, _, _, 'res://'),
    !,
    setup_call_cleanup(
        open(Res, read, In, [type(binary)]),
        setup_call_cleanup(
            tmp_file_stream(binary, TmpFile, Out),
            copy_stream_data(In, Out),
            close(Out)),
        close(In)).
lib_to_file(Lib, Lib, false).

open_foreign_in_resources(Zipper, Name, Stream) :-
    zipper_goto(Zipper, file(Name)),
    zipper_open_current(Zipper, Stream,
                        [ type(binary),
                          release(true)
                        ]).

base(Path, Base) :-
    atomic(Path),
    !,
    file_base_name(Path, File),
    file_name_extension(Base, _Ext, File).
base(_/Path, Base) :-
    !,
    base(Path, Base).
base(Path, Base) :-
    Path =.. [_,Arg],
    base(Arg, Base).

entry(_, Function, Function) :-
    Function \= default(_),
    !.
entry(Spec, default(FuncBase), Function) :-
    base(Spec, Base),
    atomic_list_concat([FuncBase, Base], '_', Function).
entry(_, default(Function), Function).

                 /*******************************
                 *          (UN)LOADING         *
                 *******************************/

%!  load_foreign_library(:FileSpec) is det.
%!  load_foreign_library(:FileSpec, +Entry:atom) is det.
%
%   Load a _|shared object|_  or  _DLL_.   After  loading  the Entry
%   function is called without arguments. The default entry function
%   is composed from =install_=,  followed   by  the file base-name.
%   E.g.,    the    load-call    below      calls    the    function
%   =|install_mylib()|=. If the platform   prefixes extern functions
%   with =_=, this prefix is added before calling.
%
%     ==
%           ...
%           load_foreign_library(foreign(mylib)),
%           ...
%     ==
%
%   @param  FileSpec is a specification for absolute_file_name/3.  If searching
%           the file fails, the plain name is passed to the OS to try the default
%           method of the OS for locating foreign objects.  The default definition
%           of file_search_path/2 searches <prolog home>/lib/<arch> on Unix and
%           <prolog home>/bin on Windows.
%
%   @see    use_foreign_library/1,2 are intended for use in directives.

load_foreign_library(Library) :-
    load_foreign_library(Library, default(install)).

load_foreign_library(Module:LibFile, Entry) :-
    with_mutex('$foreign',
               load_foreign_library(LibFile, Module, Entry)).

load_foreign_library(LibFile, _Module, _) :-
    current_library(LibFile, _, _, _, _),
    !.
load_foreign_library(LibFile, Module, DefEntry) :-
    retractall(error(_, _)),
    find_library(LibFile, Path, Delete),
    asserta(loading(LibFile)),
    retractall(foreign_predicate(LibFile, _)),
    catch(Module:open_shared_object(Path, Handle), E, true),
    (   nonvar(E)
    ->  delete_foreign_lib(Delete, Path),
        assert(error(Path, E)),
        fail
    ;   delete_foreign_lib(Delete, Path)
    ),
    !,
    (   entry(LibFile, DefEntry, Entry),
        Module:call_shared_object_function(Handle, Entry)
    ->  retractall(loading(LibFile)),
        assert_shlib(LibFile, Entry, Path, Module, Handle)
    ;   foreign_predicate(LibFile, _)
    ->  retractall(loading(LibFile)),    % C++ object installed predicates
        assert_shlib(LibFile, 'C++', Path, Module, Handle)
    ;   retractall(loading(LibFile)),
        retractall(foreign_predicate(LibFile, _)),
        close_shared_object(Handle),
        findall(Entry, entry(LibFile, DefEntry, Entry), Entries),
        throw(error(existence_error(foreign_install_function,
                                    install(Path, Entries)),
                    _))
    ).
load_foreign_library(LibFile, _, _) :-
    retractall(loading(LibFile)),
    (   error(_Path, E)
    ->  retractall(error(_, _)),
        throw(E)
    ;   throw(error(existence_error(foreign_library, LibFile), _))
    ).

delete_foreign_lib(true, Path) :-
    \+ current_prolog_flag(res_keep_foreign, true),
    !,
    catch(delete_file(Path), _, true).
delete_foreign_lib(_, _).


%!  use_foreign_library(+FileSpec) is det.
%!  use_foreign_library(+FileSpec, +Entry:atom) is det.
%
%   Load and install a foreign   library as load_foreign_library/1,2
%   and register the installation using   initialization/2  with the
%   option =now=. This is similar to using:
%
%     ==
%     :- initialization(load_foreign_library(foreign(mylib))).
%     ==
%
%   but using the initialization/1 wrapper causes  the library to be
%   loaded _after_ loading of  the  file   in  which  it  appears is
%   completed,  while  use_foreign_library/1  loads    the   library
%   _immediately_. I.e. the  difference  is   only  relevant  if the
%   remainder of the file uses functionality of the C-library.

use_foreign_library(FileSpec) :-
    initialization(load_foreign_library(FileSpec), now).

use_foreign_library(FileSpec, Entry) :-
    initialization(load_foreign_library(FileSpec, Entry), now).

%!  unload_foreign_library(+FileSpec) is det.
%!  unload_foreign_library(+FileSpec, +Exit:atom) is det.
%
%   Unload a _|shared object|_ or  _DLL_.   After  calling  the Exit
%   function, the shared object is  removed   from  the process. The
%   default exit function is composed from =uninstall_=, followed by
%   the file base-name.

unload_foreign_library(LibFile) :-
    unload_foreign_library(LibFile, default(uninstall)).

unload_foreign_library(LibFile, DefUninstall) :-
    with_mutex('$foreign', do_unload(LibFile, DefUninstall)).

do_unload(LibFile, DefUninstall) :-
    current_library(LibFile, _, _, Module, Handle),
    retractall(current_library(LibFile, _, _, _, _)),
    (   entry(LibFile, DefUninstall, Uninstall),
        Module:call_shared_object_function(Handle, Uninstall)
    ->  true
    ;   true
    ),
    abolish_foreign(LibFile),
    close_shared_object(Handle).

abolish_foreign(LibFile) :-
    (   retract(foreign_predicate(LibFile, Module:Head)),
        functor(Head, Name, Arity),
        abolish(Module:Name, Arity),
        fail
    ;   true
    ).

system:'$foreign_registered'(M, H) :-
    (   loading(Lib)
    ->  true
    ;   Lib = '<spontaneous>'
    ),
    assert(foreign_predicate(Lib, M:H)).

assert_shlib(File, Entry, Path, Module, Handle) :-
    retractall(current_library(File, _, _, _, _)),
    asserta(current_library(File, Entry, Path, Module, Handle)).


                 /*******************************
                 *       ADMINISTRATION         *
                 *******************************/

%!  current_foreign_library(?File, ?Public)
%
%   Query currently loaded shared libraries.

current_foreign_library(File, Public) :-
    current_library(File, _Entry, _Path, _Module, _Handle),
    findall(Pred, foreign_predicate(File, Pred), Public).


                 /*******************************
                 *            RELOAD            *
                 *******************************/

%!  reload_foreign_libraries
%
%   Reload all foreign libraries loaded (after restore of a state
%   created using qsave_program/2.

reload_foreign_libraries :-
    findall(lib(File, Entry, Module),
            (   retract(current_library(File, Entry, _, Module, _)),
                File \== -
            ),
            Libs),
    reverse(Libs, Reversed),
    reload_libraries(Reversed).

reload_libraries([]).
reload_libraries([lib(File, Entry, Module)|T]) :-
    (   load_foreign_library(File, Module, Entry)
    ->  true
    ;   print_message(error, shlib(File, load_failed))
    ),
    reload_libraries(T).


                 /*******************************
                 *     CLEANUP (WINDOWS ...)    *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from Halt() in pl-os.c (if it  is defined), *after* all at_halt/1
hooks have been executed, and after   dieIO(),  closing and flushing all
files has been called.

On Unix, this is not very useful, and can only lead to conflicts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unload_all_foreign_libraries :-
    current_prolog_flag(unload_foreign_libraries, true),
    !,
    forall(current_library(File, _, _, _, _),
           unload_foreign(File)).
unload_all_foreign_libraries.

%!  unload_foreign(+File)
%
%   Unload the given foreign file and all `spontaneous' foreign
%   predicates created afterwards. Handling these spontaneous
%   predicates is a bit hard, as we do not know who created them and
%   on which library they depend.

unload_foreign(File) :-
    unload_foreign_library(File),
    (   clause(foreign_predicate(Lib, M:H), true, Ref),
        (   Lib == '<spontaneous>'
        ->  functor(H, Name, Arity),
            abolish(M:Name, Arity),
            erase(Ref),
            fail
        ;   !
        )
    ->  true
    ;   true
    ).

                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:message(shlib(LibFile, load_failed)) -->
    [ '~w: Failed to load file'-[LibFile] ].
prolog:message(shlib(not_supported)) -->
    [ 'Emulator does not support foreign libraries' ].

prolog:error_message(existence_error(foreign_install_function,
                                     install(Lib, List))) -->
    [ 'No install function in ~q'-[Lib], nl,
      '\tTried: ~q'-[List]
    ].
