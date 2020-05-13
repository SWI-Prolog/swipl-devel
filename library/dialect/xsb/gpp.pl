/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(gpp, []).
:- use_module(library(error)).

/** <module> Use XSB gpp preprocessor

This library enables  using  the  XSB   gpp  preprocessor  on  .P files.
Currently the `gpp` program is not provided with SWI-Prolog. If you need
this compatibility get it from XSB. The   XSB sources provide `gpp.c`, a
simple stand alone C file that can be  compiled with any C compiler. The
`gpp` program must be installed in   a  directory accessible through the
``PATH`` environment variable.  The preprocessor is enabled using

    :- use_module(library(dialect/xsb/gpp)).

Running code through gpp can be useful for several reasons:

 - Share (numeric) constants with C code.  Note that SWI-Prolog's
   [ffi](https://www.swi-prolog.org/pack/list?p=ffi) pack provides
   an alternative for this.
 - Provide a source translation mechanism that appeals more to e.g.,
   C programmers and can deal with partial Prolog expressions and/or
   syntax that is illegal to some Prolog implementation.
 - Load legacy XSB programs.

There are also good reasons not to use a preprocessor:

 - gpp doesn't propagate source locations, so error messages typically
   report the wrong location.
 - If ``#include file`` is used it will report the wrong file for
   errors.  In addition, make/0 will not be able to pick up that an
   included file has changed and thus the main file needs to be
   reloaded.
 - Wrong source locations also imply tools such as edit/1 or the
   graphical debugger no longer work.
 - Advanced tooling such as PceEmacs cannot correctly highlight code
   that uses the preprocessor.
*/

:- multifile
    prolog:open_source_hook/3,
    prolog:gpp_options/1.
:- dynamic
    prolog:gpp_options/1.

%!  prolog:open_source_hook(+Path, -Stream, +Options)
%
%   Implementation  of  the  open  source  hook   to  use  the  XSB  gpp
%   preprocessor  on  .P  files.  This  requires    `gpp`  on  the  PATH
%   environment variable.

prolog:open_source_hook(Path, Stream, _Options) :-
    file_name_extension(_, 'P', Path),
    gpp_command(Path, Command),
    open(pipe(Command), read, Stream),
    set_stream(Stream, file_name(Path)).

gpp_command(Path, Command) :-
    gpp_executable(Gpp),
    quoted_os_path(Gpp, OSGpp),
    quoted_os_path(Path, OSPath),
    gpp_option_string(Options),
    format(string(Command), '~w ~w ~w', [OSGpp, Options, OSPath]).

quoted_os_path(PrologPath, QOSPath) :-
    prolog_to_os_filename(PrologPath, OSPath),
    (   \+ sub_atom(OSPath, _, _, _, '"'),
        \+ sub_atom(OSPath, _, _, _, '$')
    ->  format(string(QOSPath), '"~w"', [OSPath])
    ;   \+ sub_atom(OSPath, _, _, _, '\'')
    ->  format(string(QOSPath), '\'~w\'', [OSPath])
    ;   representation_error(path)
    ).

gpp_executable(Gpp) :-
    exe_options(ExeOptions),
    absolute_file_name(path(gpp), Gpp, ExeOptions).

exe_options(Options) :-
    current_prolog_flag(windows, true),
    !,
    Options = [ extensions(['',exe,com]), access(read) ].
exe_options(Options) :-
    Options = [ access(execute) ].

gpp_option_string(String) :-
    prolog:gpp_options(String),
    !.
gpp_option_string(String) :-
    findall(Opt, gpp_option(Opt), Opts),
    atomics_to_string(Opts, " ", String).

gpp_option('-P').
gpp_option('-m').
gpp_option('-nostdinc').
%gpp_option('-nocurinc').
gpp_option('-DSWI_PROLOG').

%!  prolog:gpp_options(-Options)
%
%   This multifile dynamic hook can be used to specify options for the
%   gpp preprocessor.  The default options are
%
%       -P -m -nostdinc -DSWI_PROLOG
