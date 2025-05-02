/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(app_sys, []).

/** <module> Perform adminstrative tasks for SWI-Prolog

This _app_ implements administrative tasks  for SWI-Prolog. It currently
implements the following sub commands

  - desktop
    Install or remove desktop integration.  The `xdg-mime` program can
    be used to verify the result, e.g.

        > xdg-mime query filetype myfile.prolog
        application/x-prolog
        > xdg-mime query default application/x-prolog
        swipl-win.desktop
*/

:- use_module(library(main)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(filesex)).
:- use_module(library(apply)).

:- if(current_prolog_flag(unix,true)).
freedesktop.   % at least, it can have freedesktop and all should compile
:- else.
freedesktop :- fail.
:- endif.

:- initialization(main, main).

main([Command|Argv]) :-
    sys(Command, Argv),
    !.
main(_) :-
    usage,
    halt(1).

:- if(freedesktop).
sys(desktop, Argv) =>
    argv_options(sys_desktop:Argv, Pos, Options),
    cli_sys_desktop(Pos, Options).
:- endif.
sys(_, _) =>
    fail.

:- if(freedesktop).
sys_desktop:opt_type(global,   global,   boolean).
sys_desktop:opt_type(g,        global,   boolean).
sys_desktop:opt_type(mime,     mime,     boolean).
sys_desktop:opt_type(m,        mime,     boolean).

sys_desktop:opt_help(help(usage),
                     " desktop [install|remove] [option ...]").
sys_desktop:opt_help(global,
                     "Install for all users").
sys_desktop:opt_help(mime,
                     "Use --no-mime to stop installing \c
                      the x-prolog MIME type").

:- use_module(library(process)).

cli_sys_desktop([], Options) =>
    cli_sys_desktop([install], Options).
cli_sys_desktop([install], Options) =>
    install_desktop(swi('desktop/swipl-win.desktop'), Options),
    install_desktop(swi('desktop/swipl.desktop'), Options),
    publish_desktop_files(Options),
    install_mime(swi('desktop/prolog-mime.xml'), Options),
    publish_mime_files(Options).
cli_sys_desktop([remove], Options) =>
    dest_dir(application, AppDir, Options),
    maplist(del_from_dir(AppDir),
            [ 'swipl.desktop',
              'swipl-win.desktop'
            ]),
    publish_desktop_files([remove(true)|Options]),
    dest_dir(mime, MimeDir, Options),
    del_from_dir(MimeDir, 'packages/prolog-mime.xml'),
    publish_mime_files([remove(true)]).

%!  install_desktop(+File, +Options) is det.

install_desktop(Spec, Options) :-
    absolute_file_name(Spec, File,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !,
    file_base_name(File, Base),
    file_name_extension(App, desktop, Base),
    read_file_to_codes(File, Codes, []),
    fix_desktop_paths(Codes, Updated, App),
    dest_dir(application, DestDir, Options),
    write_desktop_file(DestDir, Base, Updated, Options).
install_desktop(_, _).

fix_desktop_paths(Codes0, Codes, App) :-
    app_executable(App, Exec),
    app_icon(App, IconBase),
    replace_desktop_var('Exec', Exec, Codes0, Codes1),
    current_prolog_flag(home, Home),
    format(string(Icon), '~w/desktop/~w', [Home, IconBase]),
    replace_desktop_var('Icon', Icon, Codes1, Codes).

replace_desktop_var(Var, Value, Codes0, Codes) :-
    atom_codes(Var, VarCodes),
    atom_codes(Value, ValueCodes),
    phrase((string(Pre), "\n",
            string(VarCodes), "=", whites, string(_), blank(C),
            remainder(Post)),
           Codes0),
    !,
    append([Pre, `\n`, VarCodes, `=`, ValueCodes, [C], Post], Codes).

blank(C) --> [C], { code_type(C, space) }.

%!  install_mime(+Spec, +Options) is det.

install_mime(_Spec, Options) :-
    option(mime(false), Options),
    !.
install_mime(Spec, Options) :-
    absolute_file_name(Spec, File,
                       [ access(read)
                       ]),
    !,
    file_base_name(File, Base),
    read_file_to_codes(File, Codes, []),
    dest_dir(mime, DestDir, Options),
    directory_file_path(DestDir, packages, MimeDir),
    write_desktop_file(MimeDir, Base, Codes, Options).

%!  write_desktop_file(+DestDir, +Base, +Codes, +Options).

write_desktop_file(DestDir, Base, Codes, _Options) :-
    directory_file_path(DestDir, Base, File),
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, '~s', [Codes]),
        close(Out)),
    print_message(informational, sys(installed(File))).

del_from_dir(DestDir, File) :-
    directory_file_path(DestDir, File, AbsFile),
    exists_file(AbsFile),
    catch(delete_file(AbsFile), E,
          print_message(error, E)),
    !,
    print_message(informational, sys(removed(AbsFile))).
del_from_dir(_DestDir, _File).

%!  publish_desktop_files(+Options) is det.
%
%   Rurn `update-desktop-database` to make the   new desktop definitions
%   available.

publish_desktop_files(Options) :-
    dest_dir(application, DestDir, Options),
    process_create(path('update-desktop-database'),
                   [ DestDir ],
                   []),
    print_message(informational,
                  sys(updated("freedesktop.org desktop files"))).

publish_mime_files(Options) :-
    option(mime(false), Options),
    !.
publish_mime_files(Options) :-
    dest_dir(mime, DestDir, Options),
    process_create(path('update-mime-database'),
                   [ DestDir ],
                   []),
    print_message(informational,
                  sys(updated("freedesktop.org MIME files"))).


%!  app_executable(+App, -Exec) is det.
%
%   Find the executable for App.  App is one of `swipl` or `swipl-win`.

app_executable(App, AbsExe) :-
    current_prolog_flag(executable, Exe),
    file_base_name(Exe, Prog),
    file_name_extension(App, _, Prog),
    !,
    absolute_file_name(Exe, AbsExe).
app_executable(App, AbsExe) :-
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, ExeDir),
    directory_file_path(ExeDir, App, AppExe),
    exists_file(AppExe),
    !,
    absolute_file_name(AppExe, AbsExe).
app_executable('swipl-win', AbsExe) :-
    current_prolog_flag(executable, Exe),
    file_base_name(Exe, swipl),
    file_directory_name(Exe, ExeDir),
    file_directory_name(ExeDir, BuildDir),
    directory_file_path(BuildDir, 'packages/swipl-win/swipl-win', SwiplWin),
    exists_file(SwiplWin),
    !,
    absolute_file_name(SwiplWin, AbsExe).
app_executable(App, _AbsExe) :-
    print_message(error, sys(no_app(App))),
    halt(1).

:- det(app_icon/2).
app_icon(swipl,       'swipl-cli.png').
app_icon('swipl-win', 'swipl.png').

%!  dest_dir(+Type, -Dir, +Options) is det.

dest_dir(Type, Dir, Options) :-
    option(global(true), Options),
    !,
    location(Type, global, Pattern),
    expand_file_name(Pattern, [Dir]).
dest_dir(Type, Dir, _Options) :-
    location(Type, user, Pattern),
    expand_file_name(Pattern, [Dir]).

location(mime,        user,   '~/.local/share/mime').
location(mime,        global, '/usr/share/mime').
location(application, user,   '~/.local/share/applications').
location(application, global, '/usr/share/applications').
:- endif. /*freedesktop*/

                /*******************************
                *       OVERALL COMMANDS       *
                *******************************/

sys_command(desktop, "Install desktop integration files").

usage :-
    argv_usage(debug).

opt_help(help(header),
         [ ansi(bold, 'Prolog admin tasks',  []),
           nl
         ]).
opt_help(help(usage),
         [ ' [option ...] '-[],
           ansi(bold, 'command', []),
           ' [arg ...]'-[]
         ]).
opt_help(help(footer),
         [ nl, ansi(comment, 'Available commands:', []), nl, nl,
           \commands,
           nl,
           'For help on a command use -h as command argument'
         ]).

commands -->
    foreach(sys_command(Cmd, Comment),
            [ ansi(bold, '~w', [Cmd]),
              ansi(comment, '~t~15|~s~n', [Comment])
            ]).

                /*******************************
                *           MESSAGES           *
                *******************************/

:- multifile prolog:message//1.

prolog:message(sys(installed(File))) -->
    [ 'Installed '-[], url(File) ].
prolog:message(sys(removed(File))) -->
    [ 'Removed '-[], ansi(code, '~w', [File]) ].
prolog:message(sys(updated(What))) -->
    [ 'Updated ~w'-[What] ].
prolog:message(sys(no_app(App))) -->
    [ 'Could not find executable ', ansi(code, '~w', [App]) ].
