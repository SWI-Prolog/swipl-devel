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

:- module(dom,
          [ html//1,                    % +Spec
            append_html/2,              % +Elem, :Spec
            bind/4,                     % +Elem, +EventType, -Event, :Goal
            bind_async/4,               % +Elem, +EventType, -Event, :Goal
            unbind/2,                   % +Elem, +EventType
            wait/3,                     % +Elem, +EventType, =Event

            (html_meta)/1,              % +Spec

            op(1150, fx, html_meta)
          ]).
:- use_module(wasm).
:- use_module(library(http/html_decl),
              [(html_meta)/1, html_no_content/1, op(_,_,_)]).
:- autoload(library(apply), [maplist/2, maplist/3]).
:- autoload(library(lists), [member/2]).
:- autoload(library(terms), [foldsubterms/5]).

:- meta_predicate
    bind(+,+,-,0),
    bind_async(+,+,-,0).

:- html_meta
    append_html(+, html),
    html(html, ?, ?).

:- multifile
    html_dom/2.                         % HTMLAttr, DOMAttr

/** <module> Browser DOM manipulation

This library allows manipulating the browser DOM and bind event handlers
to create and manage interactive pages. All manipulations to the DOM can
be done using `:=/2` from library(wasm), e.g.

    ?- Elem := document.createElement("div").

This library leverages html//1 interface   as introduced for server-side
HTML generation to create complex DOM   structures from Prolog terms. It
provides three ways to facilitate reuse.

  - Use `\Rule` in the global structure. This calls the DCG `Rule`,
    which should call html//1 to add any structure in this place. This
    approach is compatible to html//1 as used for server-side
    generation.
  - Create the global structure and use `Var=Spec` to get access to some
    of the _containers_ and later fill them using append_html/2.
  - Create sub structures using html//1 and use them in html//1 calls
    that create the global structure.

@see library(dialect/tau/dom).
*/

%!  append_html(+Elem, :HTML) is det.
%
%   Extend the HTMLElement Elem using  DOM   elements  created from Spec
%   using html//1.   For example:
%
%       ?- Elem := document.getElementById("mydiv"),
%          append_html(Elem, [ "Hello ", b(world), "!"]).
%
%   @see html//1.

append_html(Elem, Spec) :-
    phrase(html(Spec), NewChildren),
    forall(member(Child, NewChildren),
           _ := Elem.appendChild(Child)).

%!  html(:Spec)//
%
%   This DCG transforms a DOM specification   into a list of HTMLElement
%   objects.   This   predicate   is    similar     to    html//1   from
%   library(http/html_write).  The differences are:
%
%     - This predicate creates a list of HTMLElement JavaScript objects
%       rather than a list of HTML _tokens_.  The translation is done by
%       means of JavaScript calls that create and manage objects.
%     - This version allows for nodes to be specified as `Var=Spec`.
%       This processes Spec normally and binds `Var` to the created
%       element.
%
%   The following terms are processed:
%
%     - Var=Spec
%       Create Spec and bind Var to the created node.
%     - Format-Args
%       Create a text node from the result of calling format/3.
%     - &(Entity)
%       Create a text node with one character if Entity is an
%       integer or a ``<span>`` holding the entity otherwise.
%     - \Rule
%       Call call(Rule), allowing for a user-defined rule.  This
%       rule should call html//1 to produce elements.
%     - M:Rule
%       As `\Rule`, but calling in a module.  This is the same as
%       \(M:Rule).
%     - List
%       Emit each element of the list.
%     - Compound
%       This must but a term Tag(Content) or Tag(Attributes,Content).
%       If Tag is an HTML element that has no content, a single
%       argument is intepreted as Tag(Attributes).  Attributes
%       is either a single attribute of a list of attributes.  Each
%       attribute is either term Attr(Value) or `Attr=Value`.  If
%       Value is a list, it is concatenated with a separating space.
%       The attribute names can either be the HTML name in lowercase
%       or the DOM camelCase attribute name.   HTML names are mapped
%       by the multifile predicate html_dom/2.
%     - JavaScriptObjecty
%       This should be an HTMLElement.  It is inserted at this place.
%     - Atomic
%       Atomic data creates a text node.

html(M:Spec) -->
    html(Spec, M).

html(Var=Spec, M) ==>
    peek(Var),
    html(Spec, M).
html(Fmt-Args, M) ==>
    { format(string(Text), Fmt, M:Args),
      Elem := document.createTextNode(Text)
    },
    [Elem].
html(&(Code), _), integer(Code) ==>
    { string_codes(Text, [Code]),
       Elem := document.createTextNode(Text)
    },
    [Elem].
html(&(Entity), _), atom(Entity) ==>
    { Elem := document.createElement("span"),
      format(string(String), '&#~d;', [Entity]),
      Elem.innerHTML = String
    },
    [Elem].
html(M:Rule, _) ==>
    call(M:Rule).
html(\Rule, M) ==>
    call(M:Rule).
html(List, M), is_list(List) ==>
    html_list(List, M).
html(Spec, M),
	compound(Spec),
	compound_name_arguments(Spec, Tag, Args) ==>
    html_(Tag, Args, M).
html(Spec, _), string(Spec) ==>
    { Elem := document.createTextNode(Spec)
    },
    [Elem].
html(Spec, _), is_object(Spec) ==>
    [Spec].
html(Spec, _), atomic(Spec) ==>
    { Elem := document.createTextNode(#Spec)
    },
    [Elem].

html_list([], _) ==>
    [].
html_list([H|T], M) ==>
    html(H, M),
    html_list(T, M).

html_(Tag, Args, M) -->
    [Elem],
    { Elem := document.createElement(#Tag),
      configure_element(Args, Tag, Elem, Content),
      phrase(html(Content, M), Children),
      forall(member(Child, Children),
             _ := Elem.appendChild(Child))
    }.

%!  configure_element(+Args, +Tag, +Elem, -Content) is det.
%
%   configure Elem by applying attributes. Content is either `[]` or the
%   member of Args that specifies the content.

configure_element([Attrs,Content0], _, Elem, Content) =>
    apply_attributes(Attrs, Elem),
    Content = Content0.
configure_element([Attrs], Tag, Elem, Content), html_no_content(Tag) =>
    Content = [],
    apply_attributes(Attrs, Elem).
configure_element([Content0], _, _, Content) =>
    Content = Content0.
configure_element([], _, _, Content) =>
    Content = [].

%!  apply_attributes(+Attrs, +Elem) is det.
%
%   Apply all attributes to Elem.  In   general  that calls `Elem.Attr =
%   Value`, but some attributes need  to   be  treated  special. Notably
%   `class`  must  set  `className`   (or    modify   `classList`)   and
%   `'data-field'=Value` must modify the `dataset` attribute.

apply_attributes(Attrs, Elem), is_list(Attrs) =>
    maplist(apply_attribute(Elem), Attrs).
apply_attributes(Attr, Elem) =>
    apply_attribute(Elem, Attr).

apply_attribute(Elem, Attr),
	compound(Attr),
	compound_name_arguments(Attr, Name, [Value]) =>
    apply_attribute(Elem, Name, Value).
apply_attribute(Elem, Name=Value) =>
    apply_attribute(Elem, Name, Value).

apply_attribute(Elem, Name, List), is_list(List) =>
    atomics_to_string(List, ' ', Value),
    apply_attribute_(Elem, Name, Value).
apply_attribute(Elem, Name, Value), string(Value) =>
    apply_attribute_(Elem, Name, Value).
apply_attribute(Elem, Name, A+B) =>
    string_concat(A, B, Value),
    apply_attribute_(Elem, Name, Value).
apply_attribute(Elem, Name, Value) =>
    apply_attribute_(Elem, Name, #Value).

apply_attribute_(Elem, HTMLAttr, Classes),
    html_dom(HTMLAttr, DOMAttr) =>
    Elem.DOMAttr := Classes.
apply_attribute_(Elem, data-Data, Value) =>
    set_data(Elem, Data, Value).
apply_attribute_(Elem, Attr, Value), atom_concat('data-', Data, Attr) =>
    set_data(Elem, Data, Value).
apply_attribute_(Elem, Attr, Value) =>
    Elem.Attr := Value.

%!  html_dom(?HTMLAttr, ?DOMAttr)
%
%   Mapping of HTML attribute names to DOM element attributes.
%
%   @see https://stackoverflow.com/questions/14544481/is-there-a-mapping-from-html-property-names-to-dom-propety-names

html_dom(acceptcharset, acceptCharset).
html_dom(accesskey, accessKey).
html_dom(bgcolor, bgColor).
html_dom(cellindex, cellIndex).
html_dom(cellpadding, cellPadding).
html_dom(cellspacing, cellSpacing).
html_dom(choff, chOff).
html_dom(class, className).
html_dom(codebase, codeBase).
html_dom(codetype, codeType).
html_dom(colspan, colSpan).
html_dom(datetime, dateTime).
html_dom(checked, defaultChecked).
html_dom(selected, defaultSelected).
html_dom(value, defaultValue).
html_dom(frameborder, frameBorder).
html_dom(httpequiv, httpEquiv).
html_dom(longdesc, longDesc).
html_dom(marginheight, marginHeight).
html_dom(marginwidth, marginWidth).
html_dom(maxlength, maxLength).
html_dom(nohref, noHref).
html_dom(noresize, noResize).
html_dom(noshade, noShade).
html_dom(nowrap, noWrap).
html_dom(readonly, readOnly).
html_dom(rowindex, rowIndex).
html_dom(rowspan, rowSpan).
html_dom(sectionrowindex, sectionRowIndex).
html_dom(selectedindex, selectedIndex).
html_dom(tabindex, tabIndex).
html_dom(tbodies, tBodies).
html_dom(tfoot, tFoot).
html_dom(thead, tHead).
html_dom(url, 'URL').
html_dom(usemap, useMap).
html_dom(valign, vAlign).
html_dom(valuetype, valueType).

%!  set_data(+Elem, +Name, +Value) is det.
%
%   Add a camelCase version of Name  to   Elem.dataset  with  Value as a
%   string.

set_data(Elem, D1-Ds, Value) =>
    maplist(camelCase1, Ds, DL),
    atomic_list_concat([D1|DL], Data),
    Elem.dataset.Data := #Value.
set_data(Elem, Data0, Value), sub_atom(Data0, _, _, _, -) =>
    dataCamelCase(Data0, Data),
    Elem.dataset.Data := #Value.
set_data(Elem, Data, Value) =>
    Elem.dataset.Data := #Value.

camelCase1(Atom, Camel) :-
    atom_codes(Atom, Codes),
    phrase(camelCase(Codes), CamelCodes),
    atom_codes(Camel, CamelCodes).

dataCamelCase(Name, Camel) :-
    atom_codes(Name, Codes),
    phrase(camelSkip(Codes), CamelCodes),
    atom_codes(Camel, CamelCodes).

camelCase([]) --> [].
camelCase([H|T]) -->
    { code_type(H, to_lower(U)) },
    [U],
    camelSkip(T).

camelSkip([]) --> [].
camelSkip([0'-|T]) --> !, camelCase(T).
camelSkip([H|T]) --> !, [H], camelSkip(T).

peek(Var, L, L) :- L = [Var|_].


                /*******************************
                *        EVENT HANDLING        *
                *******************************/

%!  bind(+Elem, +EventType, -Event, :Goal) is det.
%!  bind_async(+Elem, +EventType, -Event, :Goal) is det.
%
%   Bind EventType on Elem to call Goal. If  Event appears in Goal is is
%   bound to the current  event.
%
%   The bind_async/4 variation runs the event   handler  on a new Prolog
%   _engine_ using Prolog.forEach().  This implies that the handler runs
%   asynchronously and all its solutions are enumerated.
%
%   @compat bind_async/5 is a SWI-Prolog extension to the Tau library

bind(Elem, On, Ev, Goal) :-
    bind(Elem, On, Ev, Goal, #{}).

bind_async(Elem, On, Ev, Goal) :-
    bind(Elem, On, Ev, Goal, #{async:true}).

bind(Elem, On, Ev, Goal, Options) :-
    foldsubterms(map_object, Goal, Goal1, t(1,[],[]), t(_,VarNames,Map)),
    Map \== [],
    dict_pairs(Input, #, Map),
    term_string(Goal1, String, [variable_names(['Event__'=Ev|VarNames])]),
    _ := prolog.bind(Elem, #On, String, Input, Options).
bind(Elem, On, Ev, Goal, Options) :-
    term_string(Goal, String, [variable_names(['Event__'=Ev])]),
    _ := prolog.bind(Elem, #On, String, Options).

map_object(Obj, Var, t(N0,VN,Map), t(N,[VarName=Var|VN], [VarName-Obj|Map])) :-
    is_object(Obj),
    N is N0+1,
    format(atom(VarName), 'JsObject__~d__', [N0]).

%!  unbind(+Elem, +EventType) is det.
%
%   Remove the event listener for EventType.

unbind(Elem, EventType) :-
    _ := Elem.removeEventListener(#EventType).

%!  unbind(+Elem, +EventType, :Goal) is det.
%
%   Remove the event listener for EventType that executes Goal.
%   @tbd Implement.  How do we do this?  We somehow need to be
%   able to find the function from Goal.

%!  wait(+Elem, +EventType, =Event) is det.
%
%   Make the calling task wait for EventType   on  Elem. If the event is
%   triggered, Event is unified with the event object.

wait(Elem, EventType, Event) :-
    must_be_async(wait/3),
    Promise := prolog.promise_event(Elem, #EventType),
    await(Promise, Event).
