% This demo illustrates how we can manipulate the output window.  The
% output is inserted into a <div> with `id="output"`.  Each query is a
% <div> with class `query-container` that holds the query and a series
% of <div> with class `answer`.  The current answer div is accessible
% through the function `current_answer()`.

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).

%!  clr
%
%   Clear the output window.

clr :-
    document.getElementById("output").innerHTML := "".

%!  html
%
%   Insert  HTML  into  the  current answer  using  SWI-Prolog's  HTML
%   generation infra structure.   For example:
%
%       ?- html(['Hello ', b(world)]).

html(Term) :-
    phrase(html(Term), Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    Div := document.createElement("div"),
    Div.innerHTML := HTML,
    _ := current_answer().appendChild(Div).

%!  flag_table
%
%   Emit a table holding all current Prolog flags and their value.

flag_table :-
    html(\flag_table).

flag_table -->
    html(table(\foreach(order_by([asc(Name)], current_prolog_flag(Name, Value)),
                        html(tr([th(Name), td('~p'-[Value])]))))).
