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
    pce:on_load/0,
    prolog_colour:style/2.

prolog_colour:style(Class, Style) :-
    style(Class, Style).

%!  style(+Class, -Style)
%
%   Map style classes defined in   library(prolog_colour)  to xpce style
%   objects. After making modifications the effect can be tested without
%   restarting using this sequence:
%
%     1. Run ``?- make`` in Prolog
%     2. In the editor, use ``M-x reload_styles``

style(goal(built_in,_),          [colour(cyan)]).
style(goal(imported(_),_),       [colour(cyan)]).
style(goal(autoload(_),_),       [colour(dark_cyan)]).
style(goal(global,_),            [colour(dark_cyan)]).
style(goal(global(dynamic,_),_), [colour(magenta)]).
style(goal(global(_,_),_),       [colour(dark_cyan)]).
style(goal(undefined,_),         [colour(orange)]).
style(goal(thread_local(_),_),   [colour(magenta), underline(true)]).
style(goal(dynamic(_),_),        [colour(magenta)]).
style(goal(multifile(_),_),      [colour(pale_green)]).
style(goal(expanded,_),          [colour(cyan), underline(true)]).
style(goal(extern(_),_),         [colour(cyan), underline(true)]).
style(goal(extern(_,private),_), [colour(red)]).
style(goal(extern(_,public),_),  [colour(cyan)]).
style(goal(recursion,_),         [underline(true)]).
style(goal(meta,_),              [colour(red4)]).
style(goal(foreign(_),_),        [colour(darkturquoise)]).
style(goal(local(_),_),          []).
style(goal(constraint(_),_),     [colour(darkcyan)]).
style(goal(not_callable,_),      [background(orange)]).

style(function,                  [colour(cyan)]).
style(no_function,               [colour(orange)]).

style(option_name,               [colour(dodgerblue)]).
style(no_option_name,            [colour(orange)]).

style(head(exported,_),          [colour(cyan), bold(true)]).
style(head(public(_),_),         [colour('#016300'), bold(true)]).
style(head(extern(_),_),         [colour(cyan), bold(true)]).
style(head(dynamic,_),           [colour(magenta), bold(true)]).
style(head(multifile,_),         [colour(pale_green), bold(true)]).
style(head(unreferenced,_),      [colour(red), bold(true)]).
style(head(hook,_),              [colour(cyan), underline(true)]).
style(head(meta,_),              []).
style(head(constraint(_),_),     [colour(darkcyan), bold(true)]).
style(head(imported(_),_),       [colour(darkgoldenrod4), bold(true)]).
style(head(built_in,_),          [background(orange), bold(true)]).
style(head(iso,_),               [background(orange), bold(true)]).
style(head(def_iso,_),           [colour(cyan), bold(true)]).
style(head(def_swi,_),           [colour(cyan), bold(true)]).
style(head(_,_),                 [bold(true)]).
style(rule_condition,            [background(darkgreen)]).

style(module(_),                 [colour(light_slate_blue)]).
style(comment(_),                [colour(green)]).

style(directive,                 [background(grey20)]).
style(method(_),                 [bold(true)]).

style(var,                       [colour(orangered1)]).
style(singleton,                 [bold(true), colour(orangered1)]).
style(unbound,                   [colour(red), bold(true)]).
style(quoted_atom,               [colour(pale_green)]).
style(string,                    [colour(pale_green)]).
style(codes,                     [colour(pale_green)]).
style(chars,                     [colour(pale_green)]).
style(nofile,                    [colour(red)]).
style(file(_),                   [colour(cyan), underline(true)]).
style(file_no_depend(_),         [colour(cyan), underline(true),
                                  background(dark_violet)]).
style(directory(_),              [colour(cyan)]).
style(class(built_in,_),         [colour(cyan), underline(true)]).
style(class(library(_),_),       [colour(pale_green), underline(true)]).
style(class(local(_,_,_),_),     [underline(true)]).
style(class(user(_),_),          [underline(true)]).
style(class(user,_),             [underline(true)]).
style(class(undefined,_),        [colour(red), underline(true)]).
style(prolog_data,               [colour(cyan), underline(true)]).
style(flag_name(_),              [colour(cyan)]).
style(no_flag_name(_),           [colour(red)]).
style(unused_import,             [colour(cyan), background(maroon)]).
style(undefined_import,          [colour(red)]).

style(constraint(_),             [colour(darkcyan)]).

style(keyword(_),                [colour(cyan)]).
style(identifier,                [bold(true)]).
style(delimiter,                 [bold(true)]).
style(expanded,                  [colour(cyan), underline(true)]).
style(hook(_),                   [colour(cyan), underline(true)]).
style(op_type(_),                [colour(cyan)]).

style(qq_type,                   [bold(true)]).
style(qq(_),                     [colour(cyan), bold(true)]).
style(qq_content(_),             [colour(coral2)]).

style(dict_tag,                  [bold(true)]).
style(dict_key,                  [bold(true)]).
style(dict_function(_),          [colour(pale_green)]).
style(dict_return_op,            [colour(cyan)]).

style(hook,                      [colour(cyan), underline(true)]).
style(dcg_right_hand_ctx,        [background('#609080')]).

style(error,                     [background(orange)]).
style(type_error(_),             [background(orange)]).
style(syntax_error(_,_),         [background(orange)]).
style(instantiation_error,       [background(orange)]).

style(table_option(_),           [bold(true)]).
style(table_mode(_),             [bold(true)]).


		 /*******************************
		 *         GUI DEFAULTS         *
		 *******************************/

:- op(200, fy,  @).
:- op(800, xfx, :=).

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
    update_instances(Class, Prop),
    fail ; true.

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

update_instances(display, Prop) :-
    send(@display, Prop).

%!  pce_style(+Class, -Attributes)
%
%   Set XPCE class variables for Class. This is normally done by loading
%   a _resource file_, but doing it from   Prolog keeps the entire theme
%   in a single file.

% General

pce_style(display,
          [ foreground(white),
            background(black)
          ]).

pce_style(window,
          [ colour(white),
            background(black)
          ]).

pce_style(dialog,
          [ colour(black),
            background(grey80)
          ]).

pce_style(graphical,
          [ selected_foreground(black),
            selected_background(white)
          ]).

pce_style(text,
          [ selection_style(style(background := yellow3,
                                  colour := black))
          ]).

% Epilog (next generation swipl-win)

pce_style(terminal_image,
          [ background(black),
            colour(white),
            selection_style(style(background := yellow, colour := black)),
            ansi_colours(vector(colour(black),	   % black
                                colour(firebrick1),    % red
                                colour(forestgreen),   % green
                                colour(goldenrod),     % yellow
                                colour(steelblue),     % blue
                                colour(mediumorchid),  % magenta
                                colour(darkturquoise), % cyan
                                colour(lightgray),     % white
                                /* Bright versions */
                                colour(gray40),	   % black
                                colour(orangered),     % red
                                colour(limegreen),     % green
                                colour(khaki),         % yellow
                                colour(dodgerblue),    % blue
                                colour(violet),        % magenta
                                colour(cyan),          % cyan
                                colour(snow)           % white
                               ))
          ]).

pce_style(text_cursor,
          [ colour(firebrick1)
          ]).

% Dialog

pce_style(text_item,
          [ text_colour(white),
            elevation(elevation('0,25mm', background := black))
          ]).

pce_style(menu,
          [ text_colour(white)
          ]).

pce_style(list_browser,
          [ selection_style(style(background := yellow, colour := black)),
            isearch_style(style(background := green, colour := black))
          ]).

% PceEmacs

pce_style(text_image,
          [ background(black),
            colour(white)
          ]).
pce_style(text_margin,
          [ background(grey20)
          ]).
pce_style(editor,
          [ selection_style(style(background := yellow, colour := black)),
            isearch_style(style(background := green, colour := black)),
            isearch_other_style(style(background := pale_turquoise,
                                      colour := black))
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
