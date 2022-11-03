/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(dom,
          [ get_by_id/2,                % +Id,-Element
            get_prop/3,                 % +Elem,+Name,-Value
            get_attr/3,                 % +Elem,+Name,-Value
            remove/1,                   % +Elem
            create/2,                   % TagName,Elem
            set_html/2,                 % +Elem,+HTML:string
            add_class/2,                % Elem,Class
            append_child/2,             % +Elem,+Child
            parent_of/2,                % ?Child,?Parent
            bind/4                      % +Elem,+EventType,-Event,:Goal
          ]).
:- use_module(wasm).

/** <module> Tau-Prolog compatible DOM manipulation

This  module  is  part  of  the  WASM  distribution  of  SWI-Prolog.  It
implements the Tau-Prolog DOM library.

@tbd This library is incomplete  and   error  conditions are most likely
incompatible.
*/

:- meta_predicate
    bind(+,+,-,:).

%!  get_by_id(+Id, -Element) is semidet.
%
%   True when the current document has Element with Id.

get_by_id(Id, Elem) :-
    Elem := document.getElementById(#Id),
    Elem \== undefined.

%!  get_attr(+Elem, +Name, -Value) is semidet.
%!  get_prop(+Elem, +Name, -Value) is semidet.
%
%   Get an attribute (property) from a   JavaScript object. Fails if the
%   attribute is `undefined`.
%
%   Note that this predicate conflicts with SWI-Prolog get_attr/3 to get
%   attributes from a variable.  For  this   reason  we  also  make this
%   predicate available as get_prop/3.

get_prop(Elem, Name, Value) :-
    Value := Elem[Name],
    Value \== undefined.

get_attr(Elem, Name, Value) :-
    get_prop(Elem, Name, Value).

%!  remove(+Elem) is det.
%
%   Remove an element from the DOM tree.

remove(Elem) :-
    _ := Elem.remove().

%!  create(TagName, Elem) is det.
%
%   Create a node from TagName and make it available as Elem.

create(TagName, Elem) :-
    Elem := document.createElement(#TagName).

%!  set_html(+Elem, +HTML:string) is det.
%
%   Set the `innerHTML` of Elem.

set_html(Elem, HTML) :-
    Elem.innerHTML := #HTML.

%!  add_class(Elem, Class) is det.
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

%!  parent_of(?Child, ?Parent) is nondet.
%
%   True when Child is a direct Child   of  Parent. One of the arguments
%   must be instantiated.

parent_of(Child, Parent), nonvar(Child) =>
     Parent := Child.parent.
parent_of(Child, Parent), nonvar(Parent) =>
    Children := Parent.children.toList(),
    member(Child, Children).

%!  bind(+Elem, +EventType, -Event, :Goal)
%
%   Bind EventType on Elem to call Goal. If  Event appears in Goal is is
%   bound to the current event.

bind(Elem, On, Ev, Goal) :-
    term_string(Goal, String, [variable_names(['Event__'=Ev])]),
    _ := prolog.bind(Elem, #On, String).

