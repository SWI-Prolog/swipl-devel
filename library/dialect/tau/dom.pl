/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions b.v.
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

:- module(tau_dom,
          [ add_class/2,                % +Elem, +Class
            append_child/2,             % +Elem, +Child
            attr/3,                     % +Elem, +Name, ?Value
            body/1,                     % -Body
            create/2,                   % +TagName, --Elem
            document/1,                 % -Document
            get_attr/3,                 % +Elem, +Name, -Value
            get_attribute/3,            % +Elem, +Name, -Value
            get_by_class/2,             % +Class, -Element
            get_by_class/3,             % +Parent, +Class, -Element
            get_by_id/2,                % +Id, -Element
            get_by_name/2,              % +Name, -Elem
            get_by_tag/2,               % +TagName, -Elem
            get_html/2,			% +Elem, -HTML:string
            get_style/3,                % +Elem, +Attr, =Value
            has_class/2,                % +Elem, +Class
            head/1,                     % -Elem
            html/2,                     % +Elem, ?InnerHTML
            insert_after/2,             % +Elem, +Reference
            insert_before/2,            % +Elem, +Reference
            parent_of/2,                % ?Child, ?Parent
            prepend_child/2,            % +Elem, +Child
            remove/1,                   % +Elem
            remove_class/2,             % +Elem, +Class
            set_attr/3,                 % +Elem, +Attr, +Value
            set_html/2,                 % +Elem, +HTML:string
            set_style/3,                % +Elem, +Attr, +Value
            sibling/2,                  % ?Elem1, ?Elem2
            style/3,                    % +Elem, +Attr, ?Style
            bind/4,                     % +Elem, +EventType, -Event, :Goal
            event_property/3,           % +Event, +Prop, -Value
            prevent_default/1,          % +Event
            unbind/2,                   % +Elem, +EventType
            hide/1,                     % +Elem
            show/1,                     % +Elem
            toggle/1                    % +Elem
          ]).
:- use_module('../../wasm').
:- use_module('../../dom', [bind/4, unbind/2]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(error), [representation_error/1]).
:- autoload(library(lists), [member/2]).
:- autoload(library(dcg/basics), [number/3, whites/2, string/3]).

/** <module> Tau-Prolog compatible DOM manipulation

This  module  is  part  of  the  WASM  distribution  of  SWI-Prolog.  It
implements the Tau-Prolog DOM  library.

@see https://tau-prolog.org/documentation#prolog
*/

                /*******************************
                *       DOM MANIPULATION       *
                *******************************/

%!  add_class(+Elem, +Class) is det.
%
%   Add classes to a Elem. Class is either  a list of classes or an atom
%   (or string) containing one or more classes separated by white space.

add_class(Elem, Class), is_list(Class) =>
    maplist(atom_string, Class, Classes),
    Add =.. [add|Classes],
    _ := Elem.classList.Add.
add_class(Elem, Class) =>
    split_string(Class, " \t\n", " \t\n", Classes),
    Add =.. [add|Classes],
    _ := Elem.classList.Add.

%!  append_child(+Elem, +Child) is det.
%
%   Add Child as a child to Elem.

append_child(Elem, Child) :-
    _ := Elem.appendChild(Child).

%!  attr(+Elem, +Name, ?Value) is det.
%
%   Set (if Value is ground) or unify  an attribute value. This used the
%   setAttribute() or getAttribute() methods  unless   Name  is `value`.
%   @see get_attr/3 and set_attr/3.

attr(Elem, Attr, Value), ground(Value) =>
    set_attr(Elem, Attr, Value).
attr(Elem, Attr, Value) =>
    get_attribute(Elem, Attr, Value).

%!  body(-Body) is det.
%
%   True when Body is the HTML Element that holds the body.

body(Body) :-
    Body := document.body.

%!  create(+TagName, --Elem) is det.
%
%   Create a node from TagName and make it available as Elem.

create(TagName, Elem) :-
    Elem := document.createElement(#TagName).

%!  document(=Document) is det.
%
%   True when Document is the HTML element representing the document.

document(Document) :-
    Document := document.

%!  get_attr(+Elem, +Name, =Value) is semidet.
%!  get_attribute(+Elem, +Name, =Value) is semidet.
%
%   Get an attribute (property) from a   JavaScript object. Fails if the
%   attribute is `undefined`.
%
%   Note that this predicate conflicts with SWI-Prolog get_attr/3 to get
%   attributes from a variable.  For  this   reason  we  also  make this
%   predicate available as get_attribute/3.

get_attribute(Elem, Name, Value) :-
    Value := Elem[Name],
    Value \== undefined.

get_attr(Elem, Name, Value) :-
    get_attribute(Elem, Name, Value).

%!  get_by_class(+Class, -Elem) is nondet.
%
%   True when Elem is an HTML element with class Class.

get_by_class(Class, Elem) :-
    get_by_class(document, Class, Elem).

%!  get_by_class(+Parent, +Class, -Elem) is nondet.
%
%   True when Elem is an HTML element with class Class below Parent.

get_by_class(Parent, Class, Elem) :-
    Set := Parent.getElementsByClassName(#Class).toList(),
    member(Elem, Set).

%!  get_by_id(+Id, -Element) is semidet.
%
%   True when the current document has Element with Id.

get_by_id(Id, Elem) :-
    Elem := document.getElementById(#Id),
    Elem \== null.

%!  get_by_name(+Name, -Elem) is nondet.
%
%   True when Elem is an HTML element with name Name.

get_by_name(Name, Elem) :-
    Set := document.getElementsByName(#Name).toList(),
    member(Elem, Set).

%!  get_by_tag(+TagName, =Elem) is nondet.
%
%   True when Elem is an HTML element with tag Tag.

get_by_tag(Tag, Elem) :-
    Set := document.getElementsByTagName(#Tag).toList(),
    member(Elem, Set).

%!  get_html(+Elem, -HTML:string) is det.
%
%   Get the innerHTML of an element.

get_html(Elem, HTML) :-
    HTML := Elem.innerHTML.

%!  get_style(+Elem, +Attr, =Value) is semidet.
%
%   True when Value is the computed value for the given style attribute.
%   If the computed style is undefined, Value  is unified to the element
%   style. The DOM  style  string  is   translated  into  a  Prolog term
%   according to these rules:
%
%     - px(N) maps to Npx
%     - '%'(N) maps to N%
%     - url(URL) maps to url("URL")
%     - rgb(R,G,B) maps to rgb(R,G,B)

get_style(Elem, Attr, Value) :-
    (   String0 := document.defaultView.getComputedStyle(Elem).Attr,
        String0 \== undefined,
        String0 \== ""
    ->  String = String0
    ;   String := Elem.style.Attr,
        String \== undefined
    ),
    string_codes(String, Codes),
    (   phrase(style_to_prolog(Value0), Codes)
    ->  Value = Value0
    ;   Value = String
    ).

style_to_prolog(Value) -->
    number(N), whites,
    (   "px", whites
    ->  { Value = px(N) }
    ;   "%", whites
    ->  { Value = '%'(N) }
    ).
style_to_prolog(Value) -->
    "url(", quote(Q), string(S), quote(Q), ")",
    !,
    { atom_codes(URL, S),
      Value = url(URL)
    }.
style_to_prolog(Value) -->
    "rgb(", wnumber(R), ",", wnumber(G), ",", wnumber(B), ")",
    !,
    { Value = rgb(R,G,B) }.

quote(double) --> "\"".
quote(single) --> "\'".

wnumber(N) --> whites, number(N), whites.

%!  has_class(+Elem, +Class) is semidet.
%
%   True if Elem has Class.

has_class(Elem, Class) :-
    true := Elem.classList.contains(#Class).

%!  head(=Elem) is det.
%
%   True when Elem is the HTML Element that holds the head.

head(Head) :-
    Head := document.head.

%!  html(+Elem, ?InnerHTML) is det.
%
%   Get  or  set  the  innerHTML  if   Elem.  See  also  get_html/2  and
%   set_html/2.

html(Elem, InnerHTML), ground(InnerHTML) =>
    Elem.innerHTML := #InnerHTML.
html(Elem, InnerHTML) =>
    InnerHTML = Elem.innerHTML.

%!  insert_after(+Elem, +Reference) is det.
%
%   Insert Elem after Reference.

insert_after(Elem, Reference) :-
    _ := Reference.after(Elem).

%!  insert_before(+Elem, +Reference) is det.
%
%   Insert Elem before Reference.

insert_before(Elem, Reference) :-
    _ := Reference.before(Elem).

%!  parent_of(?Child, ?Parent) is nondet.
%
%   True when Child is a direct Child   of  Parent. One of the arguments
%   must be instantiated.

parent_of(Child, Parent), nonvar(Child) =>
     Parent := Child.parent.
parent_of(Child, Parent), nonvar(Parent) =>
    Children := Parent.children.toList(),
    member(Child, Children).

%!  prepend_child(+Elem, +Child) is det.
%
%   Add Child as first child of Elem.

prepend_child(Elem, Child) :-
    FirstChild := Elem.firstChild,
    (   FirstChild == null
    ->  _ := Elem.appendChild(Child)
    ;   FirstChild.before(Child)
    ).

%!  remove(+Elem) is det.
%
%   Remove an element from the DOM tree.

remove(Elem) :-
    _ := Elem.remove().

%!  remove_class(+Elem, +Class) is det.
%
%   Remove Class from the classList of Elem.

remove_class(Elem, Class) :-
    _ := Elem.classList.remove(#Class).

%!  set_attr(+Elem, +Attr, +Value) is det.
%
%   Use the setAttribute() interface to set  Attr   to  Value. If Attr =
%   `value`, use ``Elem.value = Value``.

set_attr(Elem, value, Value) =>
    Elem.value := #Value.
set_attr(Elem, Name, Value) =>
    _ := Elem.setAttribute(#Name, #Value).


%!  set_html(+Elem, +HTML:string) is det.
%
%   Set the `innerHTML` of Elem.

set_html(Elem, HTML) :-
    Elem.innerHTML := #HTML.

%!  set_style(+Elem, +Attr, +Value) is det.
%
%   Set a style attribute for Elem.  Value is either an atom, string or
%   term as defined by get_style/3.

set_style(Elem, Attr, Value) :-
    prolog_to_style(Value, String),
    Elem.style.Attr := #String.

prolog_to_style(px(N), Style) =>
    format(string(Style), '~wpx', [N]).
prolog_to_style('%'(N), Style) =>
    format(string(Style), '~w%', [N]).
prolog_to_style(url(URL), Style) =>
    (   \+ sub_atom(URL, _, _, _, '"')
    ->  format(string(Style), 'url("~w")', [URL])
    ;   \+ sub_atom(URL, _, _, _, '\'')
    ->  format(string(Style), 'url(\'~w\')', [URL])
    ;   representation_error(url)
    ).
prolog_to_style(rgb(R,G,B), Style) =>
    format(string(Style), 'rgb(~w,~w,~w)', [R,G,B]).
prolog_to_style(Value, Style) =>
    Style = Value.

%!  sibling(?Elem1, ?Elem2) is semidet.
%
%   Get the next or previous sibling depending   on  the mode. This uses
%   the  `nextElementSibling`  or    `previousElementSibling`,  skipping
%   possible intermediate nodes. Fails for getting   the previous of the
%   first or next of the last.

sibling(Elem, Next), var(Next) =>
    Next := Elem.nextElementSibling,
    Next \== null.
sibling(Prev, Elem), var(Prev) =>
    Prev := Elem.previousElementSibling,
    Prev \== null.

%!  style(+Elem, +Attr, ?Style) is det.
%
%   Set or get a style attribute.

style(Elem, Attr, Style), ground(Style) =>
    set_style(Elem, Attr, Style).
style(Elem, Attr, Style) =>
    get_style(Elem, Attr, Style).


                /*******************************
                *            EVENTS            *
                *******************************/

%!  event_property(+Event, +Prop, =Value) is semidet.
%
%   Extract a property from the event.

event_property(Event, Prop, Value) :-
    Value := Event.Prop,
    Value \== undefined.

%!  prevent_default(+Event) is det.
%
%   Prevent default behaviour in an event.

prevent_default(Event) :-
    _ := Event.preventDefault().



                /*******************************
                *           EFFECTS            *
                *******************************/

%!  hide(+Elem) is det.
%!  show(+Elem) is det.
%!  toggle(+Elem) is det.
%
%   Manage the visibility of Elem. The   predicate  hide/1 saves the old
%   `display` value, which is restored by  show/1.   If  there is no old
%   display value, show/1 uses `block`.

hide(Elem) :-
    save_displayed(Elem),
    Elem.style.display := "none".

show(Elem) :-
    Old := Elem.tau_display,
    (   Old == undefined
    ->  State = block
    ;   State = Old
    ),
    Elem.style.display := #State.

toggle(Elem) :-
    (   get_style(Elem, display, none)
    ->  show(Elem)
    ;   hide(Elem)
    ).

save_displayed(Elem) :-
    State := document.defaultView.getComputedStyle(Elem).display,
    (   State == undefined
    ->  true
    ;   State == "none"
    ->  true
    ;   Elem.tau_display := #State
    ).
