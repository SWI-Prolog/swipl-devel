/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, CWI, Amsterdam
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


:- module(html_text,
          [ html_text/1,                        % +FileName
            html_text/2                         % +FileName, Options
          ]).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(aggregate)).

:- use_module(library(lynx/format)).
:- use_module(library(lynx/html_style)).

%!  html_text(+Input) is det.
%!  html_text(+Input, Options) is det.
%
%   Render HTML from Input to `current_output`.  Input is either an HTML
%   DOM or a valid input for load_html/3. Options defined are:
%
%     - margin_left(+N)
%     - margin_right(+N)
%       Initial margins.
%     - width(+N)
%       Total preceived line width.
%     - text_align(+Align)
%       One of `justify` or `left`.  Default is `justify`.

html_text(Input) :-
    html_text(Input, []).

html_text(Input, Options) :-
    (   xml_is_dom(Input)
    ->  DOM = Input
    ;   load_html(Input, DOM, Options)
    ),
    default_state(State0),
    state_options(Options, State0, State),
    init_nl,
    format_dom(DOM, State).

state_options([], State, State).
state_options([H|T], State0, State) :-
    H =.. [Key,Value],
    (   fmt_option(Key, Type, _Default)
    ->  must_be(Type, Value),
        State1 = State0.put(Key,Value)
    ;   State1 = State0
    ),
    state_options(T, State1, State).

fmt_option(margin_left,  integer, 0).
fmt_option(margin_right, integer, 0).
fmt_option(text_align,   oneof([justify, left]), justify).
fmt_option(width,        between(10,1000), 72).

default_state(State) :-
    findall(Key-Value, fmt_option(Key, _, Value), Pairs),
    dict_pairs(Dict, _, Pairs),
    State = Dict.put(_{ style:[], list:[]}).

%!  format_dom(+DOM, +State) is det.
%
%   Format the given HTML DOM to `current_output` according to State.

format_dom([], _) :-
    !.
format_dom([H|T], State) :-
    format_dom(H, State),
    !,
    format_dom(T, State).
format_dom(Content, State) :-
    Content = [H0|_],
    \+ is_block_element(H0),
    !,
    (   append(Inline, [H|T], Content),
        is_block_element(H)
    ->  true
    ;   Inline = Content
    ),
    format_dom(element(p, [], Inline), State),
    format_dom([H|T], State).
format_dom(element(html, _, Content), State) :-
    !,
    format_dom(Content, State).
format_dom(element(head, _, _), _) :-
    !.
format_dom(element(body, _, Content), State) :-
    !,
    format_dom(Content, State).
format_dom(element(E, Attrs, Content), State) :-
    !,
    (   format_element(E, Attrs, Content, State)
    ->  true
    ;   debug(format(html), 'Skipped block element ~q', [E])
    ).

format_element(pre, Attrs, [Content], State) :-
    !,
    block_element(pre, Attrs, Top-Bottom, BlockAttrs, Style),
    update_style(Style, State, State1),
    ask_nl(Top),
    emit_code(Content, BlockAttrs, State1),
    ask_nl(Bottom).
format_element(table, Attrs, Content, State) :-
    !,
    block_element(table, Attrs, Top-Bottom, BlockAttrs, Style),
    update_style(Style, State, State1),
    state_par_properties(State1, BlockAttrs, BlockOptions),
    ask_nl(Top),
    emit_nl,
    format_table(Content, Attrs, BlockOptions, State1),
    ask_nl(Bottom).
format_element(hr, Attrs, _, State) :-
    !,
    block_element(hr, Attrs, Top-Bottom, BlockAttrs, Style),
    update_style(Style, State, State1),
    state_par_properties(State1, BlockAttrs, BlockOptions),
    ask_nl(Top),
    emit_nl,
    emit_hr(Attrs, BlockOptions, State1),
    ask_nl(Bottom).
format_element(Elem, Attrs, Content, State) :-
    block_element(Elem, Attrs, Top-Bottom, BlockAttrs, Style),
    !,
    update_style(Style, State, State1),
    block_words(Content, SubBlocks, Words, State1),
    (   Words == []
    ->  true
    ;   ask_nl(Top),
        emit_block(Words, BlockAttrs, State1),
        ask_nl(Bottom)
    ),
    (   SubBlocks \== []
    ->  update_state_par_properties(BlockAttrs, State1, State2),
        format_dom(SubBlocks, State2)
    ;   true
    ).
format_element(Elem, Attrs, Content, State) :-
    list_element(Elem, Attrs, Top-Bottom, State, State1),
    !,
    open_list(Elem, State1, State2),
    ask_nl(Top),
    format_list(Content, Elem, 1, State2),
    ask_nl(Bottom).
format_element(Elem, Attrs, Content, State) :-
    format_list_element(element(Elem, Attrs, Content), none, 0, State).

%!  block_element(+El, +Attrs, -Margin, -ParOPtions, -Style)
%
%   Describe a block element

block_element(El, Attrs, Margins, ParOptions, Style) :-
    block_element(El, Margins0, ParOptions0, Style0),
    (   nonvar(Attrs),
        element_css(El, Attrs, CSS)
    ->  css_block_options(CSS, Margins0, Margins, ParOptions, Style1),
        append(Style1, Style0, Style2),
        list_to_set(Style2, Style)
    ;   Margins = Margins0,
        ParOptions = ParOptions0,
        Style = Style0
    ).

block_element(p,          1-2, [],                                []).
block_element(div,        1-1, [],                                []).
block_element(hr,         1-1, [],                                []).
block_element(h1,         2-2, [],                                [bold]).
block_element(h2,         2-2, [],                                [bold]).
block_element(h3,         2-2, [],                                [bold]).
block_element(h4,         2-2, [],                                [bold]).
block_element(pre,        2-2, [],                                []).
block_element(blockquote, 2-2, [margin_left(4), margin_right(4)], []).
block_element(table,      2-2, [],                                []).

list_element(ul, _, Margins, State0, State) :-
    margins(4, 4, State0, State),
    list_level_margins(State, Margins).
list_element(ol, _, Margins, State0, State) :-
    margins(4, 4, State0, State),
    list_level_margins(State, Margins).
list_element(dl, _, 2-2, State, State).

list_element(ul).
list_element(ol).
list_element(dl).

list_level_margins(State, 2-2) :-
    nonvar(State),
    State.get(list) == [],
    !.
list_level_margins(_, 0-0).

format_list([], _, _, _).
format_list([H|T], Type, Nth, State) :-
    format_list_element(H, Type, Nth, State),
    (   T == []
    ->  true
    ;   Nth1 is Nth + 1,
        format_list(T, Type, Nth1, State)
    ).

format_list_element(element(LE, Attrs, Content), Type, Nth, State) :-
    setup_list_element(LE, Attrs, Type, Nth, ListParProps, State, State1),
    block_words(Content, Blocks, Words, State1),
    emit_block(Words, ListParProps, State1),
    (   Blocks \== []
    ->  update_state_par_properties(ListParProps, State1, State2),
        format_dom(Blocks, State2)
    ;   true
    ).

setup_list_element(li, _Attrs, _Type, Nth, ListParProps, State, State) :-
    list_par_properties(State.list, Nth, ListParProps).
setup_list_element(dt, _Attrs, _Type, _Nth, [], State, State2) :-
    margins(0, 0, State, State1),
    update_style([bold], State1, State2).
setup_list_element(dd, _Attrs, _Type, _Nth, [], State, State1) :-
    margins(4, 0, State, State1).

list_item_element(li).
list_item_element(dt).
list_item_element(dd).

list_par_properties([ul|_More], _, [bullet('\u2022')]).
list_par_properties([ol|_More], N, [bullet(N)]).


%!  block_words(+Content, -RestContent, -Words, +State)
%
%   Turn Content into a list of words with attributes and spaces.

block_words(Content, RC, Words, State) :-
    phrase(bwords(Content, RC, State), Words0),
    join_whitespace(Words0, Words1),
    text_format:trim_spaces(Words1, Words).

bwords([], [], _) -->
    !.
bwords([H|T], Rest, _State) -->
    { var(Rest),
      is_block_element(H),
      !,
      Rest = [H|T]
    }.
bwords([H|T], Rest, State) -->
    !,
    bwordsel(H, State),
    bwords(T, Rest, State).

is_block_element(element(E,_,_)) :-
    (   block_element(E, _, _, _)
    ;   list_element(E)
    ;   list_item_element(E)
    ),
    debug(format(html), 'Found block ~q', [E]),
    !.

bwordsel(element(Elem, Attrs, Content), State) -->
    { styled_inline(Elem, Attrs, Margins, Style),
      !,
      update_style(Style, State, State1)
    },
    left_margin(Margins),
    bwords(Content, [], State1),
    right_margin(Margins).
bwordsel(element(br, _, _), _State) -->
    [br([])].
bwordsel(CDATA, State) -->
    { atomic(CDATA),
      !,
      split_string(CDATA, " \n\t\r", "", Words)
    },
    words(Words, State).
bwordsel(element(Elem, _Attrs, _Content), _State) -->
    { debug(format(html), 'Skipped inline element ~q', [Elem]) }.

left_margin(0-_) --> !.
left_margin(N-_) --> [b(N,_)].

right_margin(_-0) --> !.
right_margin(_-N) --> [b(N,_)].

styled_inline(El, Attrs, Margins, Style) :-
    styled_inline(El, Style0),
    (   nonvar(Attrs),
        element_css(El, Attrs, CSS)
    ->  css_inline_options(CSS, Margins, Style1),
        append(Style1, Style0, Style2),
        list_to_set(Style2, Style)
    ;   Style = Style0
    ).

styled_inline(b,      [bold]).
styled_inline(strong, [bold]).
styled_inline(em,     [bold]).
styled_inline(span,   []).
styled_inline(i,      [underline]).
styled_inline(a,      [underline]).
styled_inline(var,    []).
styled_inline(code,   []).

%!  words(+Tokens, +State)//
%
%   Generate a list of w(Word,Len,Attrs) and   b(Len,_)  terms for words
%   and (breakable) white space.

words([], _) --> [].
words([""|T0], State) -->
    !,
    { skip_leading_spaces(T0, T) },
    space,
    words(T, State).
words([H|T], State) -->
    word(H, State),
    (   {T==[]}
    ->  []
    ;   { skip_leading_spaces(T, T1) },
        space,
        words(T1, State)
    ).

skip_leading_spaces([""|T0], T) :-
    !,
    skip_leading_spaces(T0, T).
skip_leading_spaces(L, L).

word(W, State) -->
    { string_length(W, Len),
      (   Style = State.get(style)
      ->  true
      ;   Style = []
      )
    },
    [w(W, Len, Style)].

space -->
    [b(1,_)].

%!  join_whitespace(Elements, Joined)
%
%   Join consequtive space elements into a single white space element.

join_whitespace([], []).
join_whitespace([H0|T0], [H|T]) :-
    join_whitespace(H0, H, T0, T1),
    !,
    join_whitespace(T1, T).
join_whitespace([H|T0], [H|T]) :-
    join_whitespace(T0, T).

join_whitespace(b(Len0,_), b(Len,_), T0, T) :-
    take_whitespace(T0, T, Len0, Len).

take_whitespace([b(Len1,_)|T0], T, Len0, Len) :-
    !,
    Len2 is max(Len1,Len0),
    take_whitespace(T0, T, Len2, Len).
take_whitespace(L, L, Len, Len).


		 /*******************************
		 *       STATE MANAGEMENT	*
		 *******************************/

%!  update_style(+Style:list, +State0, -State)
%
%   Add Style to the current state.

update_style([], State, State) :-
    !.
update_style(Extra, State0, State) :-
    (   get_dict(style, State0, Style0, State, Style)
    ->  add_style(Extra, Style0, Style)
    ;   add_style(Extra, [], Style),
        put_dict(style, State0, Style, State)
    ).

add_style(Extra, Style0, Style) :-
    reverse(Extra, RevExtra),
    foldl(add1_style, RevExtra, Style0, Style).

%!  add1_style(+New, +Style0, -Style) is det.
%
%   Modify the current text style.

add1_style(New, Style0, Style) :-
    (   style_overrides(New, Add, Overrides)
    ->  delete_all(Overrides, Style0, Style1),
        append(Add, Style1, Style)
    ;   Style = [New|Style0]
    ).

delete_all([], List, List).
delete_all([H|T], List0, List) :-
    delete(List0, H, List1),
    delete_all(T, List1, List).

style_overrides(normal,           [],      [bold]).
style_overrides(fg(C),            [fg(C)], [fg(_), hfg(_)]).
style_overrides(bg(C),            [bg(C)], [bg(_), hbg(_)]).
style_overrides(underline(false), [],      [underline]).

margins(Left, Right, State0, State) :-
    _{ margin_left:ML0, margin_right:MR0 } >:< State0,
    ML is ML0 + Left,
    MR is MR0 + Right,
    State = State0.put(_{margin_left:ML, margin_right:MR}).

open_list(Type, State0, State) :-
    get_dict(list, State0, Lists, State, [Type|Lists]).

update_state_par_properties([], State, State).
update_state_par_properties([H|T], State0, State) :-
    H =.. [ Key, Value ],
    State1 = State0.put(Key,Value),
    update_state_par_properties(T, State1, State).

%!  state_par_properties(+State, -ParProps)
%
%   Get the paragraph shape properties from  State. Eventually these two
%   should be merged!

state_par_properties(State, Props) :-
    Props0 = [ margin_left(LM),
               margin_right(RM),
               text_align(TA),
               width(W),
               pad(Pad)
             ],
    _{margin_left:LM, margin_right:RM, text_align:TA, width:W,
      pad:Pad} >:< State,
    filled_par_props(Props0, Props).

filled_par_props([], []).
filled_par_props([H|T0], [H|T]) :-
    arg(1, H, A),
    nonvar(A),
    !,
    filled_par_props(T0, T).
filled_par_props([_|T0], T) :-
    filled_par_props(T0, T).


state_par_properties(State, Options, BlockOptions) :-
    state_par_properties(State, Options0),
    foldl(merge_par_option, Options, Options0, BlockOptions).

merge_par_option(margin_left(ML0), Options0, [margin_left(ML)|Options1]) :-
    !,
    select_option(margin_left(ML1), Options0, Options1, 0),
    ML is ML0+ML1.
merge_par_option(margin_right(MR0), Options0, [margin_right(MR)|Options1]) :-
    !,
    select_option(margin_right(MR1), Options0, Options1, 0),
    MR is MR0+MR1.
merge_par_option(Opt, Options0, Options) :-
    merge_options([Opt], Options0, Options).

%!  emit_block(+Words, +Options, +State) is det.
%
%   Format a block given Words inline elements, Options and State. Calls
%   format_paragraph/2 after finalizing the paragraph   shape  and using
%   the newline logic.

emit_block([], _, _) :-
    !.
emit_block(Words, Options, State) :-
    state_par_properties(State, Options, BlockOptions),
    ask_nl(1),
    emit_nl,
    format_paragraph(Words, BlockOptions),
    ask_nl(1).

%!  init_nl is det.
%!  init_nl(-State) is det.
%!  exit_nl(+State) is det.
%
%   Initialize/finalize the newline logic.

init_nl :-
    nb_setval(nl_pending, start).

init_nl(Old) :-
    (   nb_current(nl_pending, Old)
    ->  true
    ;   Old = []
    ),
    nb_setval(nl_pending, start).
exit_nl(Old) :-
    nb_setval(nl_pending, Old).

ask_nl(N) :-
    (   nb_current(nl_pending, N0)
    ->  (   N0 == start
        ->  true
        ;   integer(N0)
        ->  N1 is max(N0, N),
            nb_setval(nl_pending, N1)
        ;   nb_setval(nl_pending, N)
        )
    ;   nb_setval(nl_pending, N)
    ).

emit_nl :-
    (   nb_current(nl_pending, N),
        integer(N)
    ->  forall(between(1,N,_), nl)
    ;   true
    ),
    nb_setval(nl_pending, 0).


		 /*******************************
		 *             PRE		*
		 *******************************/

%!  emit_code(+Content, +BlockAttrs, +State)

emit_code(Content, BlockAttrs, State) :-
    Style = State.style,
    split_string(Content, "\n", "", Lines),
    option(margin_left(LM0), BlockAttrs, 4),
    LM is LM0+State.margin_left,
    ask_nl(1),
    emit_nl,
    emit_code_lines(Lines, 1, LM, Style),
    ask_nl(1).

emit_code_lines([], _, _, _).
emit_code_lines([H|T], LineNo, LM, Style) :-
    emit_code_line(H, LineNo, LM, Style),
    LineNo1 is LineNo + 1,
    emit_code_lines(T, LineNo1, LM, Style).

emit_code_line(Line, _LineNo, LM, Style) :-
    emit_nl,
    emit_indent(LM),
    (   Style == []
    ->  write(Line)
    ;   ansi_format(Style, '~s', [Line])
    ),
    ask_nl(1).

emit_indent(N) :-
    forall(between(1, N, _),
           put_char(' ')).


		 /*******************************
		 *            TABLES		*
		 *******************************/

%!  format_table(+Content, +Attrs, +BlockAttrs, +State) is det.

format_table(Content, Attrs, BlockAttrs, State) :-
    tty_state(TTY),
    option(margin_left(ML), BlockAttrs, 0),
    option(margin_right(MR), BlockAttrs, 0),
    MaxTableWidth is State.width - ML - MR,
    table_cell_state(Attrs, State, CellState),
    phrase(rows(Content), Rows),
    columns(Rows, Columns),
    maplist(auto_column_width(CellState.put(tty,false)), Columns, Widths),
    column_widths(Widths, MaxTableWidth, ColWidths),
    maplist(format_row(ColWidths, CellState.put(tty,TTY), ML), Rows).

tty_state(TTY) :-
    stream_property(current_output, tty(true)),
    !,
    TTY = true.
tty_state(false).


%!  column_widths(+AutoWidths, +MaxTableWidth, -Widths) is det.
%
%   Establish the widths of the columns. AutoWidths  is a list of widths
%   for each of the columns if no folding is applied.

column_widths(Widths, MaxTableWidth, Widths) :-
    sum_list(Widths, AutoWidth),
    AutoWidth =< MaxTableWidth,
    !.
column_widths(AutoWidths, MaxTableWidth, Widths) :-
    sort(0, >=, AutoWidths, Sorted),
    append(Wrapped, Keep, Sorted),
    sum_list(Keep, KeepWidth),
    KeepWidth < MaxTableWidth/2,
    length(Wrapped, NWrapped),
    WideWidth is round((MaxTableWidth-KeepWidth)/NWrapped),
    (   [KeepW|_] = Keep
    ->  true
    ;   KeepW = 0
    ),
    !,
    maplist(truncate_column(KeepW,WideWidth), AutoWidths, Widths).

truncate_column(Keep, WideWidth, AutoWidth, Width) :-
    (   AutoWidth =< Keep
    ->  Width = AutoWidth
    ;   Width = WideWidth
    ).

table_cell_state(Attrs, State, CellState) :-
    (   element_css(table, Attrs, CSS)
    ->  true
    ;   CSS = []
    ),
    option(padding_left(PL), CSS, 1),
    option(padding_right(PR), CSS, 1),
    CellState = State.put(_{margin_left:PL, margin_right:PR}).


%!  rows(+Content, -Rows) is det.

rows([]) --> [].
rows([H|T]) --> rows(H), rows(T).
rows([element(tbody,_,Content)|T]) --> rows(Content), rows(T).
rows([element(tr,Attrs,Columns)|T]) --> [row(Columns, Attrs)], rows(T).

%!  columns(+Rows, -Columns) is det.
%
%   Transpose the table, filling missing  columns   with  an  empty `td`
%   element as needed.

columns(Rows, Columns) :-
    columns(Rows, 1, Columns).

columns(Rows, I, Columns) :-
    maplist(row_column(I, Found), Rows, H),
    (   Found == true
    ->  Columns = [H|T],
        I2 is I + 1,
        columns(Rows, I2, T)
    ;   Columns = []
    ).

row_column(I, Found, row(Columns, _Attrs), Cell) :-
    (   nth1(I, Columns, Cell)
    ->  Found = true
    ;   Cell = element(td,[],[])
    ).

auto_column_width(State, Col, Width) :-
    maplist(auto_cell_width(State), Col, Widths),
    max_list(Widths, Width).

auto_cell_width(State, Cell, Width) :-
    cell_colspan(Cell, 1),
    !,
    format_cell_to_string(Cell, 1_000, State, String),
    split_string(String, "\n", "", Lines),
    maplist(string_length, Lines, LineW),
    max_list(LineW, Width0),
    Width is Width0 + State.margin_right.
auto_cell_width(_, _, 0).

%!  format_row(+ColWidths, +State, +MarginLeft, +Row)
%
%   Format a single row.

format_row(ColWidths, State, MarginLeft, Row) :-
    hrule(Row, ColWidths, MarginLeft),
    format_cells(ColWidths, CWSpanned, 1, Row, State, Cells),
    format_row_lines(1, CWSpanned, Cells, MarginLeft).

hrule(row(_, Attrs), ColWidths, MarginLeft) :-
    attrs_classes(Attrs, Classes),
    memberchk(hline, Classes),
    !,
    sum_list(ColWidths, RuleLen),
    format('~N~t~*|~`-t~*+', [MarginLeft, RuleLen]).
hrule(_, _, _).

format_row_lines(LineNo, Widths, Cells, MarginLeft) :-
    nth_row_line(Widths, 1, LineNo, Cells, CellLines, Found),
    (   Found == true
    ->  emit_nl,
        emit_indent(MarginLeft),
        maplist(emit_cell_line, CellLines),
        ask_nl(1),
        LineNo1 is LineNo + 1,
        format_row_lines(LineNo1, Widths, Cells, MarginLeft)
    ;   true
    ).

emit_cell_line(Line-Pad) :-
    write(Line),
    forall(between(1,Pad,_), put_char(' ')).

nth_row_line([], _, _, _, [], _).
nth_row_line([ColW|CWT], CellNo, LineNo, Cells, [CellLine-Pad|ColLines],
             Found) :-
    nth1(CellNo, Cells, CellLines),
    (   nth1(LineNo, CellLines, CellLine)
    ->  Found = true,
        Pad = 0
    ;   CellLine = '', Pad = ColW
    ),
    CellNo1 is CellNo + 1,
    nth_row_line(CWT, CellNo1, LineNo, Cells, ColLines, Found).


%!  format_cells(+ColWidths, -CWSpanned, +Col0, +Row, +State, -Cells)
%
%   Format the cells for Row. The  resulting   Cells  list  is a list of
%   cells, where each cell is a  list   of  strings, each representing a
%   line.

format_cells([], [], _, _, _, []) :- !.
format_cells(CWidths, [HW|TW], Column, Row, State, [HC|TC]) :-
    Row = row(Columns, _Attrs),
    nth1(Column, Columns, Cell),
    cell_colspan(Cell, CWidths, HW, TW0),
    cell_align(Cell, Align),
    format_cell_to_string(Cell, HW, State.put(_{pad:' ', text_align:Align}), String),
    split_string(String, "\n", "", HC),
    Column1 is Column+1,
    format_cells(TW0, TW, Column1, Row, State, TC).

cell_colspan(Cell, CWidths, HW, TW) :-
    cell_colspan(Cell, Span),
    length(SpanW, Span),
    append(SpanW, TW, CWidths),
    sum_list(SpanW, HW).

cell_colspan(element(_,Attrs,_), Span) :-
    (   memberchk(colspan=SpanA, Attrs),
        atom_number(SpanA, SpanN)
    ->  Span = SpanN
    ;   Span = 1
    ).

%!  cell_align(+Cell, -Align) is det.
%
%   Determine the cell alignment. Currently   supports  the (deprecated)
%   HTML4  `align=Align`  possibility  and  very    naively  parsed  CSS
%   ``text-align:center``, etc.

cell_align(element(_,Attrs,_), Align) :-
    (   memberchk(align=AlignA, Attrs)
    ->  Align = AlignA
    ;   memberchk(style=Style, Attrs),
        style_css_attrs(Style, Props),
        memberchk('text-align'(AlignA), Props)
    ->  Align = AlignA
    ;   Align = left
    ).


%!  format_cell_to_string(+Cell, +ColWidth, +State, -String) is det.
%
%   Format Cell to a String, given the state and column width.

format_cell_to_string(element(_,_,[]), ColWidth, State, String) :-
    Pad = State.get(pad),
    !,
    length(Chars, ColWidth),
    maplist(=(Pad), Chars),
    atomics_to_string(Chars, String).
format_cell_to_string(Cell, ColWidth, State, String) :-
    setup_call_cleanup(
        init_nl(NlState),
        with_output_to(
            string(String),
            format_cell(Cell, ColWidth, State)),
        exit_nl(NlState)).

format_cell(element(E, _Attrs, Content), ColWidth, State) :-
    set_stream(current_output, tty(State.tty)),
    cell_element(E, Style),
    update_style(Style, State.put(width, ColWidth), CellState),
    block_words(Content, Blocks, Words, CellState),
    emit_block(Words, [], CellState),
    (   Blocks \== []
    ->  format_dom(Blocks, CellState)
    ;   true
    ).

cell_element(td, [normal]).
cell_element(th, [bold]).


%!  emit_hr(+Attrs, +BlockOptions, +State)
%
%   Emit a horizontal rule.

emit_hr(_Attrs, BlockAttrs, State) :-
    option(margin_left(ML), BlockAttrs, 0),
    option(margin_right(MR), BlockAttrs, 0),
    RuleWidth is State.width - ML - MR,
    Style = State.style,
    emit_indent(ML),
    (   Style == []
    ->  format('~|~*t~*+', [0'-, RuleWidth])
    ;   ansi_format(Style, '~|~*t~*+', [0'-, RuleWidth])
    ).
