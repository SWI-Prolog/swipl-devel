/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2019, University of Amsterdam
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

:- module(win_menu,
          [ init_win_menus/0
          ]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- set_prolog_flag(generate_debug_info, false).
:- op(200, fy, @).
:- op(990, xfx, :=).

/** <module> Console window menu

This library sets up the menu of  *swipl-win.exe*. It is called from the
system initialisation file =plwin-win.rc=, predicate gui_setup_/0.
*/

:- if(current_prolog_flag(console_menu_version, qt)).
% The traditional swipl-win.exe predefines some menus.  The Qt version
% does not.  Here, we predefine the same menus to make the remainder
% compatiple.
menu('&File',
     [ 'E&xit' = pqConsole:quit_console
     ],
     [
     ]).
menu('&Edit',
     [ '&Copy'  = pqConsole:copy,
       '&Paste' = pqConsole:paste
     ],
     []).
menu('&Settings',
     [ '&Font ...' = pqConsole:select_font,
       '&Colors ...' = pqConsole:select_ANSI_term_colors
     ],
     []).
menu('&Run',
     [ '&Interrupt' = interrupt,
       '&New thread' = interactor
     ],
     []).

menu(File,
     [ '&Consult ...' = action(user:load_files(+file(open,
                                                     'Load file into Prolog'),
                                               [silent(false)])),
       '&Edit ...'    = action(user:edit(+file(open,
                                               'Edit existing file'))),
       '&New ...'     = action(edit_new(+file(save,
                                              'Create new Prolog source'))),
       --
     | MRU
     ], [before_item('E&xit')]) :-
    File = '&File',
    findall(Mru=true, mru_info(File, Mru, _, _, _), MRU, MRUTail),
    MRUTail = [ --,
                '&Reload modified files' = user:make,
                --,
                '&Navigator ...' = prolog_ide(open_navigator),
                --
              ].

:- else.

menu('&File',
     [ '&Consult ...' = action(user:load_files(+file(open,
                                                     'Load file into Prolog'),
                                               [silent(false)])),
       '&Edit ...'    = action(user:edit(+file(open,
                                               'Edit existing file'))),
       '&New ...'     = action(edit_new(+file(save,
                                              'Create new Prolog source'))),
       --,
       '&Reload modified files' = user:make,
       --,
       '&Navigator ...' = prolog_ide(open_navigator),
       --
     ],
     [ before_item('&Exit')
     ]).
:- endif.

menu('&Settings',
     [ --,
       '&User init file ...'  = prolog_edit_preferences(prolog),
       '&GUI preferences ...' = prolog_edit_preferences(xpce)
     ],
     []).
menu('&Debug',
     [ %'&Trace'             = trace,
       %'&Debug mode'        = debug,
       %'&No debug mode'     = nodebug,
       '&Edit spy points ...' = user:prolog_ide(open_debug_status),
       '&Edit exceptions ...' = user:prolog_ide(open_exceptions(@on)),
       '&Threads monitor ...' = user:prolog_ide(thread_monitor),
       'Debug &messages ...'  = user:prolog_ide(debug_monitor),
       'Cross &referencer ...'= user:prolog_ide(xref),
       --,
       '&Graphical debugger' = user:guitracer
     ],
     [ before_menu(-)
     ]).
menu('&Help',
     [ '&About ...'                             = about,
       '&Help ...'                              = help,
       'Browse &PlDoc ...'                      = doc_browser,
       --,
       'SWI-Prolog website ...'                 = www_open(swipl),
       '  &Manual ...'                          = www_open(swipl_man),
       '  &FAQ ...'                             = www_open(swipl_faq),
       '  &Quick Start ...'                     = www_open(swipl_quick),
       '  Mailing &List ...'                    = www_open(swipl_mail),
       '  &Download ...'                        = www_open(swipl_download),
       '  &Extension packs ...'                 = www_open(swipl_pack),
       --,
       '&XPCE (GUI) Manual ...'                 = manpce,
       --,
       '&Check installation'                    = check_installation,
       'Submit &Bug report ...'                 = www_open(swipl_bugs)
     ],
     [ before_menu(-)
     ]).


init_win_menus :-
    (   menu(Menu, Items, Options),
        (   memberchk(before_item(Before), Options)
        ->  true
        ;   Before = (-)
        ),
        (   memberchk(before_menu(BM), Options)
        ->  true
        ;   BM = (-)
        ),
        win_insert_menu(Menu, BM),
        (   '$member'(Item, Items),
            (   Item = (Label = Action)
            ->  true
            ;   Item == --
            ->  Label = --
            ),
            win_insert_menu_item(Menu, Label, Before, Action),
            fail
        ;   true
        ),
        fail
    ;   current_prolog_flag(associated_file, File),
        add_to_mru(load, File)
    ;   insert_associated_file
    ),
    refresh_mru.

associated_file(File) :-
    current_prolog_flag(associated_file, File),
    !.
associated_file(File) :-
    '$cmd_option_val'(script_file, OsFiles),
    OsFiles = [OsFile],
    !,
    prolog_to_os_filename(File, OsFile).

insert_associated_file :-
    associated_file(File),
    !,
    file_base_name(File, Base),
    atom_concat('Edit &', Base, Label),
    win_insert_menu_item('&File', Label, '&New ...', edit(file(File))).
insert_associated_file.


:- if(current_predicate(win_has_menu/0)).
:- initialization
   (   win_has_menu
   ->  init_win_menus
   ;   true
   ).
:- endif.

                 /*******************************
                 *            ACTIONS           *
                 *******************************/

edit_new(File) :-
    call(edit(file(File))).         % avoid autoloading

www_open(Id) :-
    Spec =.. [Id, '.'],
    call(expand_url_path(Spec, URL)),
    print_message(informational, opening_url(URL)),
    call(www_open_url(URL)),        % avoid autoloading
    print_message(informational, opened_url(URL)).

html_open(Spec) :-
    absolute_file_name(Spec, [access(read)], Path),
    call(win_shell(open, Path)).

:- if(current_predicate(win_message_box/2)).

about :-
    message_to_string(about, AboutSWI),
    (   current_prolog_flag(console_menu_version, qt)
    ->  message_to_string(about_qt, AboutQt),
        format(atom(About), '<p>~w\n<p>~w', [AboutSWI, AboutQt])
    ;   About = AboutSWI
    ),
    atomic_list_concat(Lines, '\n', About),
    atomic_list_concat(Lines, '<br>', AboutHTML),
    win_message_box(
        AboutHTML,
        [ title('About swipl-win'),
          image(':/swipl.png'),
          min_width(700)
        ]).

:- else.

about :-
    print_message(informational, about).

:- endif.

load(Path) :-
    (   \+ current_prolog_flag(associated_file, _)
    ->  file_directory_name(Path, Dir),
        working_directory(_, Dir),
        set_prolog_flag(associated_file, Path)
    ;   true
    ),
    user:load_files(Path).


                 /*******************************
                 *       HANDLE CALLBACK        *
                 *******************************/

action(Action) :-
    strip_module(Action, Module, Plain),
    Plain =.. [Name|Args],
    gather_args(Args, Values),
    Goal =.. [Name|Values],
    call(Module:Goal).

gather_args([], []).
gather_args([+H0|T0], [H|T]) :-
    !,
    gather_arg(H0, H),
    gather_args(T0, T).
gather_args([H|T0], [H|T]) :-
    gather_args(T0, T).

:- if(current_prolog_flag(console_menu_version, qt)).

gather_arg(file(open, Title), File) :-
    !,
    source_types_desc(Desc),
    pqConsole:getOpenFileName(Title, _, Desc, File),
    add_to_mru(edit, File).

gather_arg(file(save, Title), File) :-
    source_types_desc(Desc),
    pqConsole:getSaveFileName(Title, _, Desc, File),
    add_to_mru(edit, File).

source_types_desc(Desc) :-
    findall(Pattern, prolog_file_pattern(Pattern), Patterns),
    atomic_list_concat(Patterns, ' ', Atom),
    format(atom(Desc), 'Prolog Source (~w)', [Atom]).

:- else.

gather_arg(file(Mode, Title), File) :-
    findall(tuple('Prolog Source', Pattern),
            prolog_file_pattern(Pattern),
            Tuples),
    '$append'(Tuples, [tuple('All files', '*.*')], AllTuples),
    Filter =.. [chain|AllTuples],
    current_prolog_flag(hwnd, HWND),
    working_directory(CWD, CWD),
    call(get(@display, win_file_name,       % avoid autoloading
             Mode, Filter, Title,
             directory := CWD,
             owner := HWND,
             File)).

:- endif.

prolog_file_pattern(Pattern) :-
    user:prolog_file_type(Ext, prolog),
    atom_concat('*.', Ext, Pattern).


:- if(current_prolog_flag(windows, true)).

                 /*******************************
                 *          APPLICATION         *
                 *******************************/

%!  init_win_app
%
%   If Prolog is started using --win_app, try to change directory
%   to <My Documents>\Prolog.

init_win_app :-
    current_prolog_flag(associated_file, _),
    !.
init_win_app :-
    '$cmd_option_val'(win_app, true),
    !,
    catch(my_prolog, E, print_message(warning, E)).
init_win_app.

my_prolog :-
    win_folder(personal, MyDocs),
    atom_concat(MyDocs, '/Prolog', PrologDir),
    (   ensure_dir(PrologDir)
    ->  working_directory(_, PrologDir)
    ;   working_directory(_, MyDocs)
    ).


ensure_dir(Dir) :-
    exists_directory(Dir),
    !.
ensure_dir(Dir) :-
    catch(make_directory(Dir), E, (print_message(warning, E), fail)).


:- initialization
   init_win_app.

:- endif. /*windows*/


                 /*******************************
                 *             MacOS            *
                 *******************************/

:- if(current_prolog_flag(console_menu_version, qt)).

:- multifile
    prolog:file_open_event/1.

:- create_prolog_flag(app_open_first, load, []).
:- create_prolog_flag(app_open,       edit, []).

%!  prolog:file_open_event(+Name)
%
%   Called when opening a file  from   the  MacOS finder. The action
%   depends on whether this is the first file or not, and defined by
%   one of these flags:
%
%     - =app_open_first= defines the action for the first open event
%     - =app_open= defines the action for subsequent open event
%
%   On the _first_ open event, the  working directory of the process
%   is changed to the directory holding the   file. Action is one of
%   the following:
%
%     * load
%     Load the file into Prolog
%     * edit
%     Open the file in the editor
%     * new_instance
%     Open the file in a new instance of Prolog and load it there.

prolog:file_open_event(Path) :-
    (   current_prolog_flag(associated_file, _)
    ->  current_prolog_flag(app_open, Action)
    ;   current_prolog_flag(app_open_first, Action),
        file_directory_name(Path, Dir),
        working_directory(_, Dir),
        set_prolog_flag(associated_file, Path),
        insert_associated_file
    ),
    must_be(oneof([edit,load,new_instance]), Action),
    file_open_event(Action, Path).

file_open_event(edit, Path) :-
    edit(Path).
file_open_event(load, Path) :-
    add_to_mru(load, Path),
    user:load_files(Path).
:- if(current_prolog_flag(apple, true)).
file_open_event(new_instance, Path) :-
    current_app(Me),
    print_message(informational, new_instance(Path)),
    process_create(path(open), [ '-n', '-a', Me, Path ], []).
:- else.
file_open_event(new_instance, Path) :-
    current_prolog_flag(executable, Exe),
    process_create(Exe, [Path], [process(_Pid)]).
:- endif.


:- if(current_prolog_flag(apple, true)).
current_app(App) :-
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, MacOSDir),
    atom_concat(App, '/Contents/MacOS', MacOSDir).

%!  go_home_on_plain_app_start is det.
%
%   On Apple, we start in the users   home dir if the application is
%   started by opening the app directly.

go_home_on_plain_app_start :-
    current_prolog_flag(os_argv, [_Exe]),
    current_app(App),
    file_directory_name(App, Above),
    working_directory(PWD, PWD),
    same_file(PWD, Above),
    expand_file_name(~, [Home]),
    !,
    working_directory(_, Home).
go_home_on_plain_app_start.

:- initialization
    go_home_on_plain_app_start.

:- endif.
:- endif.

:- if(current_predicate(win_current_preference/3)).

mru_info('&File', 'Edit &Recent', 'MRU2',    path, edit).
mru_info('&File', 'Load &Recent', 'MRULoad', path, load).

add_to_mru(Action, File) :-
    mru_info(_Top, _Menu, PrefGroup, PrefKey, Action),
    (   win_current_preference(PrefGroup, PrefKey, CPs), nonvar(CPs)
    ->  (   select(File, CPs, Rest)
        ->  Updated = [File|Rest]
        ;   length(CPs, Len),
            Len > 10
        ->  append(CPs1, [_], CPs),
            Updated = [File|CPs1]
        ;   Updated = [File|CPs]
        )
    ;   Updated = [File]
    ),
    win_set_preference(PrefGroup, PrefKey, Updated),
    refresh_mru.

refresh_mru :-
    (   mru_info(FileMenu, Menu, PrefGroup, PrefKey, Action),
        win_current_preference(PrefGroup, PrefKey, CPs),
        maplist(action_path_menu(Action), CPs, Labels, Actions),
        win_insert_menu_item(FileMenu, Menu/Labels, -, Actions),
        fail
    ;   true
    ).

action_path_menu(ActionItem, Path, Label, win_menu:Action) :-
    file_base_name(Path, Label),
    Action =.. [ActionItem, Path].

:- else.

add_to_mru(_, _).
refresh_mru.

:- endif.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(opening_url(Url)) -->
    [ 'Opening ~w ... '-[Url], flush ].
prolog:message(opened_url(_Url)) -->
    [ at_same_line, 'ok' ].
prolog:message(new_instance(Path)) -->
    [ 'Opening new Prolog instance for ~p'-[Path] ].
:- if(current_prolog_flag(console_menu_version, qt)).
prolog:message(about_qt) -->
    [ 'Qt-based console by Carlo Capelli' ].
:- endif.
