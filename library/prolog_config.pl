/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2019, VU University Amsterdam
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

:- module(prolog_config,
          [ prolog_dump_runtime_variables/0,
            apple_bundle_libdir/1
          ]).

/** <module> Provide configuration information

This module provides information about   the configuration to facilitate
linking against Prolog, embedding Prolog or calling Prolog.
*/

:- multifile
    prolog:runtime_config/2.

%!  prolog_dump_runtime_variables
%
%   Dump the current configuration in shell   format.  This predicate is
%   called  when  Prolog  is  started    using  the  commandline  option
%   `--dump-runtime-variables`

prolog_dump_runtime_variables :-
    (   '$cmd_option_val'(config, Format),
        Format \== ''
    ->  prolog_dump_runtime_variables(Format)
    ;   prolog_dump_runtime_variables(sh)
    ).

prolog_dump_runtime_variables(Format) :-
    print_flag(home,                      'PLBASE',    Format),
    print_flag(arch,                      'PLARCH',    Format),
    print_flag(address_bits,              'PLBITS',    Format),
    print_flag(version,                   'PLVERSION', Format),
    print_flag(shared_object_extension,   'PLSOEXT',   Format),
    print_flag(shared_object_search_path, 'PLSOPATH',  Format),
    print_flag(c_libdir,                  'PLLIBDIR',  Format),
    print_flag(c_lib,                     'PLLIB',     Format),
    print_flag(open_shared_object,        'PLSHARED',  Format),
    print_flag(threads,                   'PLTHREADS', Format).

print_flag(Flag, Var, Format) :-
    (   prolog:runtime_config(Flag, Value)
    ->  print_config(Format, Var, Value)
    ;   flag_value(Flag, Value)
    ->  print_config(Format, Var, Value)
    ;   true
    ).

flag_value(Flag, Value) :-
    boolean_flag(Flag),
    (   current_prolog_flag(Flag, true)
    ->  Value = yes
    ;   Value = no
    ).
flag_value(c_libdir, Value) :-
    current_prolog_flag(home, Home),
    (   current_prolog_flag(c_libdir, Rel)
    ->  atomic_list_concat([Home, Rel], /, Value)
    ;   current_prolog_flag(windows, true)
    ->  atomic_list_concat([Home, bin], /, Value)
    ;   apple_bundle_libdir(LibDir)
    ->  Value = LibDir
    ;   current_prolog_flag(arch, Arch)
    ->  atomic_list_concat([Home, lib, Arch], /, Value)
    ).
flag_value(c_lib, '-lswipl').
flag_value(Flag, Value) :-
    current_prolog_flag(Flag, Value).

%!  apple_bundle_libdir(-LibDir)
%
%   If we are part of a MacOS bundle   the C libraries are in the bundle
%   ``Frameworks``  directory  and  the  executable  is  in  the  bundle
%   ``MacOS`` directory.

apple_bundle_libdir(LibDir) :-
    current_prolog_flag(apple, true),
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, ExeDir),
    file_base_name(ExeDir, 'MacOS'),
    file_directory_name(ExeDir, ContentsDir),
    file_base_name(ContentsDir, 'Contents'),
    atomic_list_concat([ContentsDir, 'Frameworks'], /, LibDir),
    exists_directory(LibDir).

boolean_flag(threads).
boolean_flag(open_shared_object).

print_config(sh, Var, Value) :-
    format('~w=\"~w\";~n', [Var, Value]).
print_config(cmd, Var, Value) :-
    (   file_var(Var)
    ->  prolog_to_os_filename(Value, OSValue),
        format('SET ~w=~w~n', [Var, OSValue])
    ;   format('SET ~w=~w~n', [Var, Value])
    ).

file_var('PLBASE').
