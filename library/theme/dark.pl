/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2025, VU University Amsterdam
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

:- module(prolog_theme_dark, []).
:- autoload(library(lists), [member/2]).
:- autoload(library(pce), [send/2]).

/** <module> SWI-Prolog theme file -- dark

To enable the dark theme, use

    :- use_module(library(theme/dark)).
*/

:- multifile
    prolog:theme/1,
    prolog:console_color/2,
    pldoc_style:theme/3.

prolog:theme(dark).                             % make ourselves known

:- if(current_predicate(win_window_color/2)).
set_window_colors :-
    win_window_color(background, rgb(0,0,0)),
    win_window_color(foreground, rgb(255,255,255)),
    win_window_color(selection_background, rgb(0,255,255)),
    win_window_color(selection_foreground, rgb(0,0,0)).

:- initialization
    set_window_colors.
:- endif.

		 /*******************************
		 *       PROLOG MESSAGES	*
		 *******************************/

% code embedded in messages (not used much yet)
prolog:console_color(var,                    [hfg(cyan)]).
prolog:console_color(code,                   [hfg(yellow)]).
% Alert level
prolog:console_color(comment,                [hfg(green)]).
prolog:console_color(warning,                [fg(yellow)]).
prolog:console_color(error,                  [bold, fg(red)]).
% toplevel truth value (undefined for well founded semantics)
prolog:console_color(truth(false),           [bold, fg(red)]).
prolog:console_color(truth(true),            [bold]).
prolog:console_color(truth(undefined),       [bold, fg(cyan)]).
prolog:console_color(wfs(residual_program),  [fg(cyan)]).
% trace output
prolog:console_color(frame(level),           [bold]).
prolog:console_color(port(call),             [bold, fg(green)]).
prolog:console_color(port(exit),             [bold, fg(green)]).
prolog:console_color(port(fail),             [bold, fg(red)]).
prolog:console_color(port(redo),             [bold, fg(yellow)]).
prolog:console_color(port(unify),            [bold, fg(blue)]).
prolog:console_color(port(exception),        [bold, fg(magenta)]).
% the goal of successive steps alternates between two backgrounds, which
% separates the steps of a trace.  The first argument is the port, which
% allows for colouring the goal by port instead of (or in addition to)
% striping.
prolog:console_color(goal(_, odd),           [hfg(yellow), bg8(238)]).
prolog:console_color(goal(_, even),          [hfg(yellow), bg8(240)]).
% interactive toplevel.  The command line (prompt and the text typed by
% the user) has its own background.  The answers to a single query
% alternate between two backgrounds, which separates the answers of a
% non-deterministic query.
prolog:console_color(prompt,                 [bold, fg8(h(cyan)), bg8(21)]).
prolog:console_color(input,                  [bg8(21)]).
prolog:console_color(answer(odd),            [bg8(238)]).
prolog:console_color(answer(even),           [bg8(240)]).
prolog:console_color(binding(name),          [bold, fg8(h(yellow))]).
% tag that indicates the kind of a predicate in a list of candidates.
prolog:console_color(predicate(iso),         [italic, hfg(cyan)]).
prolog:console_color(predicate(built_in),    [italic, hfg(cyan)]).
prolog:console_color(predicate(foreign),     [italic, hfg(cyan)]).
prolog:console_color(predicate(library(_)),  [italic, hfg(green)]).
prolog:console_color(predicate(module(_)),   [italic, hfg(green)]).
prolog:console_color(predicate(user),        [italic, fg(default)]).
prolog:console_color(predicate(undefined),   [italic, fg(red)]).
% print message. the argument for debug(_) is the debug channel.
prolog:console_color(message(informational), [hfg(green)]).
prolog:console_color(message(information),   [hfg(green)]).
prolog:console_color(message(debug(_)),      [hfg(yellow)]).
prolog:console_color(message(Level),         Attrs) :-
    nonvar(Level),
    prolog:console_color(Level, Attrs).

		 /*******************************
		 *          ONLINE HELP		*
		 *******************************/

%!  pldoc_style:theme(+Element, +Condition, -CSSAttributes) is semidet.
%
%   Return a set of CSS properties to modify on the specified Element if
%   Condition   holds.   color(Name)   is   mapped   to   fg(Name)   and
%   color(bright_Name) to hfg(Name).

pldoc_style:theme(var,  true,                  [color(bright_cyan)]).
pldoc_style:theme(code, true,                  [color(bright_yellow)]).
pldoc_style:theme(pre,  true,                  [color(bright_yellow)]).
pldoc_style:theme(p,    class(warning),        [color(yellow)]).
pldoc_style:theme(span, class('synopsis-hdr'), [color(bright_green)]).
pldoc_style:theme(span, class(autoload),       [color(bright_green)]).

		 /*******************************
		 *           IDE TOOLS		*
		 *******************************/

:- multifile
    pce_theme:colour/3.

pce_theme:colour(dark, Name, Value) :-
    colour(Name, Value).

%!  colour(?Name, ?Value)
%
%   Values for the semantic colours of xpce in the dark theme.  See
%   library(pce_theme).  The `syntax_*` colours are used by PceEmacs
%   to highlight the syntax classes of library(prolog_colour).  For
%   example, `syntax_goal_built_in` is the colour for goal(built_in,_).
%   After making modifications, run ``?- make.`` and
%   ``?- apply_theme(dark).`` to see the effect.  Use
%   ``?- check_theme(dark).`` to verify the coverage.

% Basic colours.  If the system colours are dark, use them.  Otherwise
% this is a dark theme on a light desktop.

colour(ui_window_background,            Colour) :-
    system_or(sys_window_background, black, Colour).
colour(ui_window_foreground,            Colour) :-
    system_or(sys_window_foreground, white, Colour).
colour(ui_dialog_background,            Colour) :-
    system_or(sys_dialog_background, grey80, Colour).
colour(ui_dialog_foreground,            Colour) :-
    system_or(sys_dialog_foreground, black, Colour).
colour(ui_selection_background,         Colour) :-
    system_or(sys_selection_background, white, Colour).
colour(ui_selection_foreground,         Colour) :-
    system_or(sys_selection_foreground, black, Colour).
colour(ui_margin_background,            grey20).
colour(ui_scrollbar_background,         grey30).

% Text.  Selection and search backgrounds are dark, such that the
% (syntax) colours of the text remain readable.

colour(ui_text_selection_background,    '#264f78').
colour(ui_isearch_background,           '#806000').
colour(ui_isearch_other_background,     '#2f4f4f').
colour(ui_link,                         dodger_blue).
colour(ui_fold,                         grey60).
colour(ui_cursor,                       firebrick1).
colour(ui_cursor_inactive,              grey50).

% Dialog items

colour(ui_inactive,                     grey50).
colour(ui_placeholder,                  grey50).
colour(ui_accelerator,                  grey70).

% Epilog terminal ANSI colours

colour(ansi_black,                      black).
colour(ansi_red,                        firebrick1).
colour(ansi_green,                      forestgreen).
colour(ansi_yellow,                     goldenrod).
colour(ansi_blue,                       steelblue).
colour(ansi_magenta,                    mediumorchid).
colour(ansi_cyan,                       darkturquoise).
colour(ansi_white,                      lightgray).
colour(ansi_bright_black,               gray40).
colour(ansi_bright_red,                 orangered).
colour(ansi_bright_green,               limegreen).
colour(ansi_bright_yellow,              khaki).
colour(ansi_bright_blue,                dodgerblue).
colour(ansi_bright_magenta,             violet).
colour(ansi_bright_cyan,                cyan).
colour(ansi_bright_white,               snow).

% PceEmacs syntax highlighting

colour(syntax_goal_built_in,            cyan).
colour(syntax_goal_imported,            cyan).
colour(syntax_goal_autoload,            dark_cyan).
colour(syntax_goal_global,              dark_cyan).
colour(syntax_goal_global_dynamic,      magenta).
colour(syntax_goal_undefined,           orange).
colour(syntax_goal_thread_local,        magenta).
colour(syntax_goal_dynamic,             magenta).
colour(syntax_goal_multifile,           pale_green).
colour(syntax_goal_expanded,            cyan).
colour(syntax_goal_extern,              cyan).
colour(syntax_goal_extern_private,      red).
colour(syntax_goal_extern_public,       cyan).
colour(syntax_goal_meta,                red4).
colour(syntax_goal_foreign,             darkturquoise).
colour(syntax_goal_constraint,          darkcyan).
colour(syntax_goal_not_callable_bg,     orange).

colour(syntax_function,                 cyan).
colour(syntax_no_function,              orange).

colour(syntax_option_name,              dodgerblue).
colour(syntax_no_option_name,           orange).

colour(syntax_head_exported,            cyan).
colour(syntax_head_public,              '#016300').
colour(syntax_head_extern,              cyan).
colour(syntax_head_dynamic,             magenta).
colour(syntax_head_multifile,           pale_green).
colour(syntax_head_unreferenced,        red).
colour(syntax_head_hook,                cyan).
colour(syntax_head_constraint,          darkcyan).
colour(syntax_head_imported,            darkgoldenrod4).
colour(syntax_head_built_in_bg,         orange).
colour(syntax_head_iso_bg,              orange).
colour(syntax_head_def_iso,             cyan).
colour(syntax_head_def_swi,             cyan).
colour(syntax_head_test,                '#01bdbd').
colour(syntax_rule_condition_bg,        darkgreen).

colour(syntax_module,                   light_slate_blue).
colour(syntax_comment,                  green).

colour(syntax_directive_bg,             grey20).

colour(syntax_var,                      orangered1).
colour(syntax_singleton,                orangered1).
colour(syntax_unbound,                  red).
colour(syntax_quoted_atom,              pale_green).
colour(syntax_string,                   pale_green).
colour(syntax_rational,                 light_steel_blue).
colour(syntax_codes,                    pale_green).
colour(syntax_chars,                    pale_green).
colour(syntax_nofile,                   red).
colour(syntax_file,                     cyan).
colour(syntax_file_no_depend,           cyan).
colour(syntax_file_no_depend_bg,        dark_violet).
colour(syntax_directory,                cyan).
colour(syntax_class_built_in,           cyan).
colour(syntax_class_library,            pale_green).
colour(syntax_class_undefined,          red).
colour(syntax_prolog_data,              cyan).
colour(syntax_flag_name,                cyan).
colour(syntax_known_flag_name,          cyan).
colour(syntax_known_flag_name_bg,       maroon).
colour(syntax_no_flag_name,             red).
colour(syntax_unused_import,            cyan).
colour(syntax_unused_import_bg,         maroon).
colour(syntax_undefined_import,         red).

colour(syntax_constraint,               darkcyan).

colour(syntax_keyword,                  cyan).
colour(syntax_expanded,                 cyan).
colour(syntax_hook,                     cyan).
colour(syntax_macro,                    cyan).
colour(syntax_op_type,                  cyan).

colour(syntax_qq,                       cyan).
colour(syntax_qq_content,               coral2).

colour(syntax_dict_function,            pale_green).
colour(syntax_dict_return_op,           cyan).

colour(syntax_dcg_right_hand_ctx_bg,    '#609080').

colour(syntax_error_bg,                 orange).
colour(syntax_type_error_bg,            orange).
colour(syntax_domain_error_bg,          orange).
colour(syntax_syntax_error_bg,          orange).
colour(syntax_instantiation_error_bg,   orange).

		 /*******************************
		 *         GUI DEFAULTS         *
		 *******************************/

:- op(200, fy,  @).
:- op(800, xfx, :=).

:- multifile
    pce:on_load/0.

pce:on_load :-
    pce_set_defaults(true).

:- initialization
    setup_if_loaded.

setup_if_loaded :-
    current_predicate(pce:send/2),
    !,
    pce_set_defaults(true).
setup_if_loaded.

%!  pce_set_defaults(+Loaded)
%
%   Adjust xpce defaults. This can either be   run before xpce is loaded
%   or as part of the xpce initialization.

pce_set_defaults(Loaded) :-
    pce_style(Class, Properties),
    member(Prop, Properties),
    Prop =.. [Name,Value],
    term_string(Value, String),
    send(@default_table, append, Name, vector(Class, String)),
    update_class_variable(Loaded, Class, Name, Value),
    fail ; true.

%!  system_or(+SystemColour, +Colour, -Value) is det.
%
%   Value is SystemColour if the system colours are dark and Colour
%   otherwise.

system_or(System, _, System) :-
    dark_system_colours,
    !.
system_or(_, Colour, Colour).

%!  dark_system_colours is semidet.
%
%   True when the system colours are already dark.  This is the case on
%   Windows in dark mode or using a contrast theme such as "Night sky",
%   on MacOS in dark mode, on KDE using a dark colour scheme and on
%   GNOME using the dark style.  These colours follow the desktop
%   settings while xpce is running.

dark_system_colours :-
    get(@pce, convert, sys_window_background, colour, Colour),
    get(Colour, intensity, I),
    I < 128.

update_class_variable(true, ClassName, Name, Value) :-
    get(@(classes), member, ClassName, Class),
    !,
    get(Class, class_variable, Name, ClassVar),
    (   get(ClassVar, context, ContextClass),
        get(ContextClass, name, ClassName)
    ->  send(ClassVar, value, Value)
    ;   new(_, class_variable(ClassName, Name, Value))
    ).
update_class_variable(_, _, _, _).

%!  pce_style(+Class, -Attributes)
%
%   Set XPCE class variables for Class. This is normally done by loading
%   a _resource file_, but doing it from   Prolog keeps the entire theme
%   in a single file.

pce_style(emacs_toc_bookmark,
          [ style_hit(style(background := yellow, colour := black))
          ]).

% Graphical debugger

pce_style(prolog_stack_view,
          [ background(black)
          ]).
pce_style(prolog_stack_frame,
          [ background(black),
            colour(white)
          ]).
pce_style(prolog_stack_link,
          [ colour(white)
          ]).
pce_style(prolog_bindings_view,
          [ background_active(black),
            background_inactive(grey50)
          ]).
pce_style(prolog_source_structure,
          [ background(black),
            colour(white)
          ]).

% Profiler

pce_style(prof_details,
          [ header_background(khaki3)
          ]).
pce_style(prof_node_text,
          [ colour('dodger_blue')
          ]).

% Debug messages

pce_style(prolog_debug_browser,
          [ enabled_style(style(colour := green))
          ]).

% Cross referencer

pce_style(xref_predicate_text,
          [ colour(green),
            colour_autoload(steel_blue),
            colour_global(steel_blue)
          ]).
pce_style(xref_file_graph_node,
          [ colour(white),
            background(grey35)
          ]).

% XPCE manual

pce_style(man_editor,
          [ jump_style(style(colour := green,
                             underline := true))
          ]).

%!  prolog_source_view:port_style(+Port, -StyleAttributes)
%
%   Override style attributes for indicating  a   specific  port  in the
%   source view. Ports are:  `call`,   `break`,  `exit`, `redo`, `fail`,
%   `exception`, `unify`, `choice`, `frame and `breakpoint`.

:- multifile
    prolog_source_view:port_style/2.

prolog_source_view:port_style(call, [background(forest_green), colour(black)]).
prolog_source_view:port_style(fail, [background(indian_red),   colour(black)]).
prolog_source_view:port_style(redo, [background(yellow3),      colour(black)]).
prolog_source_view:port_style(Type, [colour(black)]) :-
    Type \== breakpoint.
