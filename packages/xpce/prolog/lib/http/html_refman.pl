/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pce_html_refman,
	  [ html_description/2,		% +Object, -HTMLString
	    atom_to_method/2,		% +Atom, -Behaviour
	    collect_behaviour/2,	% +Class, -Behaviour
	    group_objects/2,		% +Behaviour, -Groups
	    cluster_behaviour/2,	% +Chain, -NestedChain
	    group_summary/2		% +Group, -Summary
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_manual)).

:- dynamic
	drain/1,			% the output object (editor)
	regex/2,			% cache for created regular expr.
	fetched_description/2,		% cache for computed descriptions
	no_inherit_description/3.	% Forbit inheritance of description

%	atom_to_method(+Spec, -Method)
%
%	Find XPCE object holding documentation from a textual specification.

atom_to_method(String, Object) :-
	(   new(Re, regex('\\([A-Z]?[a-z_]*\\)\\s *\\(<?->?\\)\\([a-z_]+\\)\\(:.*\\)?$')),
	    send(Re, match, String)
	->  get(Re, register_value, String, 1, name, Class0),
	    get(Re, register_value, String, 2, name, What0),
	    get(Re, register_value, String, 3, name, Selector),
	    get(Class0, downcase, Class),
	    (	What0 == '<->'
	    ->	member(What, ['->', '<-'])
	    ;	What = What0
	    ),
	    Term =.. [What, Class, Selector]
	;   new(Re, regex('@\\(.+\\)$')),
	    send(Re, match, String)
	->  get(Re, register_value, String, 1, name, Reference),
	    Term = @Reference
	;   Term = String
	),
	pce_manual:method(Term, Object).



:- pce_global(@documented, new(chain)).	% Chain with documented objects

excluded(Obj, InheritedFrom, _Description) :-
	no_inherit_description(Type, FromClass, Selector),
	send(Obj, instance_of, Type),
	get(Obj, name, Selector),
	send(InheritedFrom?context, is_a, FromClass).

fetch_description(@Obj, Description) :-
	fetched_description(Obj, Description), !.
fetch_description(Obj, Description) :-
	get(Obj, '_class_name', var), !,
	get(@manual, self, _),		% force creation
	Obj = @Ref,
	new(Global, man_global(Ref)),
	fetch_description(Global, Description),
	assert(fetched_description(Ref, Description)).
fetch_description(Obj, Description) :-
	get(@manual, self, _),		% force creation
	(   get(Obj, attribute, man_description, S0)
	;   get(Obj, man_attribute, description, S0)
	;   get(Obj, man_inherited_attribute, description, tuple(From, S0)),
	    \+ excluded(Obj, From, S0)
	;   send(Obj, has_get_method, summary),
	    get(Obj, summary, S0)
	;   new(S0, string),
	    send(S0, lock_object, @on)
	),
	S0 \== @nil, !,
	Obj = @Ref,
	assert(fetched_description(Ref, S0)),
	Description = S0.
	    

:- pce_extend_class(object).

fetch_description(Obj, Description) :<-
	"Cached description slot using manual rules"::
	fetch_description(Obj, Description).

:- pce_end_class.

html_description(Obj, S1) :-
	fetch_description(Obj, S0),
	get(S0, copy, S1),
	desc_to_html(S1, Obj).

%	to_regex(+Pattern, -Regex)
%
%	Convert pattern to regex, maintaining a store of regex objects
%	to avoid unnecessary recompilation.  We cannot blindly reuse
%	the regex as they are used recursively.

to_regex(Pattern, Regex) :-
	retract(regex(Pattern, Regex)), !.
to_regex(Pattern, Regex) :-
	new(Regex, regex(Pattern)),
	send(Regex, lock_object, @on),
	send(Regex, compile, @on).

done_regex(Pattern, Regex) :-
	assert(regex(Pattern, Regex)).

substitute(_, []) :- !.
substitute(S, [Search, Replace | Rest]) :-
	to_regex(Search, Re),
	(   Replace = call(Head)
	->  Head =.. [Pred|Args],
	    append(Args, [@arg1, @arg2], AllArgs),
	    Msg =.. [message, @prolog, Pred | AllArgs],
	    send(Re, for_all, S, Msg)
	;   send(Re, for_all, S,
		 message(@arg1, replace, @arg2, Replace))
	),
	done_regex(Search, Re),
	substitute(S, Rest).
	

desc_to_html(S, Obj) :-
	send(S, ensure_nl),
	html_escape(S),
	substitute(S, ['\n\\s +\n', '\n\n']), % cannonise
	html_lists(S),
	substitute(S,
		   [ %   HEADING
		     '\n\n\\s *\\([A-Z ?!._-]+\\)\\s *\n\n',
		     	call(header),	% uses \1
 		     %   ** SubHeader
		     '\n\n\\*\\*+\\s +\\(.*\\)\n\n',
 			'\n\n<h4>\\1</h4>\n\n',
		     %   *bold*
		     '\\*\\([^ ]+\\)\\*',
		        '<b>\\1</b>'
		   ]),
	substitute(S, ['\n\n+', '\n\n<p>\n']), % paragraphs
	hyperlinks(S, Obj),
	send(S, strip).


html_lists(S) :-
	substitute(S,
		   [ %   1)
		     %   2)
		     '\n\n+\\(\t\\sd+).*\\(\n\t.*\\|\n *\\)*\\)',
		     	call(enumerate),
		     %   aap	noot
		     %   zus	jet
		     '\n\n\\(\\(\n*\t[^\t\n]+\t.*\n\\)+\\)',
		     	call(table),
		     %   * header
		     %   text
		     '\n\n+\\(\t\\*.*\\(\n\t.*\\|\n *\\)*\\)',
		     	call(itemize),
		     %	 # text
		     %   more text
		     '\n\n+\\(\t#.*\\(\n\t.*\\|\n *\\)*\\)',
		     	call(description),
		     %   Indented by tabs
%		     '\n\n+\\(\\(\n*\t.*\\)+\\)',	% fails on regex 0.12
		     '\n+\\(\\(\n+\t.*\\)+\\)',
		     	call(example)
		   ]).
	

example(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2, ['^\t\\(.*\\)', '\\1']),
	send(S2, untabify, 4),
	send(S2, prepend, string('\n<pre>')),
	send(S2, append, string('\n</pre>\n')),
	send(Re, register_value, String, S2).


list(description, Re, String) :-
	get(Re, register_value, String, 1, L1),
	get(Re, register_value, String, 2, RestLines),
	substitute(RestLines, ['\n\t', '\n']),
	send(RestLines, strip, trailing),
	send(RestLines, ensure_nl),
	html_lists(RestLines),	% sub-lists
	send(RestLines, strip),
	send(RestLines, ensure_nl),
	send(RestLines, prepend, string('<dt>&nbsp;<br>%s<dd>\n', L1)),
	send(Re, register_value, String, RestLines).
list(itemize, Re, String) :-
	get(Re, register_value, String, 1, L1),
	get(Re, register_value, String, 2, RestLines),
	new(S0, string('\n%s%s', L1, RestLines)),
	substitute(S0, ['\n\t', '\n']),
	send(S0, strip, trailing),
	send(S0, ensure_nl),
	html_lists(S0),	% sub-lists
	send(S0, strip),
	send(S0, ensure_nl),
	send(S0, prepend, string('<li>\n')),
	send(Re, register_value, String, S0).
	

itemize(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t\\*\\s *\\(.*\\)\\(\\(\n\t[^*].*\\|\n *\\)*\\)',
		     	call(list(itemize))
		   ]),
	send(S2, prepend, string('\n<p><ul>\n')),
	send(S2, ensure_nl),
	send(S2, append, string('</ul><p>\n')),
	send(Re, register_value, String, S2).


description(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t#\\s *\\(.*\\)\\(\\(\n\t[^#].*\\|\n *\\)*\\)',
		        call(list(description))
		   ]),
	send(S2, ensure_nl),
	send(S2, prepend, string('\n<dl>\n')),
	send(S2, append, string('</dl>\n')),
	send(Re, register_value, String, S2).
	

enumerate(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t\\sd+)\\s *\\(.*\\)\\(\\(\n\t[^0-9].*\\|\n *\\)*\\)',
			call(list(itemize))
		   ]),
	send(S2, ensure_nl),
	send(S2, prepend, string('\n<ol>\n')),
	send(S2, append, string('</ol>\n')),
	send(Re, register_value, String, S2).
	

table(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t\\([^\t]+\\)\t+\\(.*\\)\n',
		       '<tr><td>\\1<td>\\2</tr>\n'
		   ]),
	send(S2, prepend, string('\n<p><table align=center border=1 width=50%>\n')),
	send(S2, append, string('</table>\n')),
	send(Re, register_value, String, S2).


header(Re, String) :-
	get(Re, register_value, String, 1, ALLCAPITALS),
	get(ALLCAPITALS, capitalise, Capitals),
	send(Re, register_value, String,
	     string('<h4>%s</h4>', Capitals), 1).


		 /*******************************
		 *	   HYPERLINKS		*
		 *******************************/

%	hyperlinks(+String, +Object)
%
%	Use the typographical conventions in the XPCE manual description
%	to automatically create hyperlinks.

hyperlinks(S, Obj) :-
	substitute(S,
		   [ '`\\([^`\']+\\)\'',
		     	call(make_link),
		     '-&gt;\\([a-z_]+\\)',
		        call(make_sendmethod_link(Obj)),
		     '&lt;-\\([a-z_]+\\)',
		        call(make_getmethod_link(Obj)),
		     '\\(@[a-z_]+\\)',
		        call(make_link),
		     '\\b[Cc]lass\\s +\\([a-z_]+\\)',
		        call(make_link),
		     '\\([a-z_]+\\)\\s +object\\b',
		        call(make_link)
		   ]).

make_link(Re, String) :-
	get(Re, register_value, String, 1, Spec),
	html_unescape(Spec),
	get(Spec, value, Atom),
	atom_to_method(Atom, _), !,
	www_form_encode(Atom, Encoded),
	send(Re, register_value, String, 
	    string('<a href="/man?for=%s">%s</a>', Encoded, Spec), 1).
make_link(_, _).

make_sendmethod_link(Obj, Re, String) :-
	get(Re, register_value, String, 1, name, Method),
	context_class(Obj, Class),
	get(string('%s->%s', Class, Method), value, Atom),
	atom_to_method(Atom, _), !,
	www_form_encode(Atom, Encoded),
	get(Re, register_value, String, 0, In),
	send(Re, register_value, String,
	     string('<a href="/man?for=%s">%s</a>', Encoded, In)).
make_sendmethod_link(_,_,_).

make_getmethod_link(Obj, Re, String) :-
	get(Re, register_value, String, 1, name, Method),
	context_class(Obj, Class),
	get(string('%s<-%s', Class, Method), value, Atom),
	atom_to_method(Atom, _), !,
	www_form_encode(Atom, Encoded),
	get(Re, register_value, String, 0, In),
	send(Re, register_value, String,
	     string('<a href="/man?for=%s">%s</a>', Encoded, In)).
make_getmethod_link(_,_,_).


context_class(Class, Name) :-
	send(Class, instance_of, class), !,
	get(Class, name, Name).
context_class(Obj, Name) :-
	send(Obj, has_get_method, context),
	get(Obj, context, Class),
	send(Class, instance_of, class), !,
	get(Class, name, Name).


		 /*******************************
		 *	     ESCAPING		*
		 *******************************/

html_escape(S) :-
	substitute(S, [ '&',   '&amp;',
			'<',   '&lt;',
			'>',   '&gt;'
		      ]).

html_unescape(S) :-
	substitute(S, [ '&amp;', '&',
			'&lt;',  '<',
			'&gt;',  '>'
		      ]).


		 /*******************************
		 *	     BEHAVIOUR		*
		 *******************************/

collect_behaviour(Class, Behaviour) :-
	new(Behaviour, chain),

	new(Merge, message(Behaviour, append, @arg1)),

	send(Class?get_methods, for_all, Merge),
	send(Class?send_methods, for_all, Merge),
	send(Class?instance_variables, for_all,
	     if(@arg1?context == Class,
		message(Behaviour, append, @arg1))).


group_objects(Chain, Groups) :-
	new(Groups, sheet),
	Group = when(@arg1?group, @arg1?group, miscellaneous),
	send(Chain, for_all,
	     if(message(Groups, is_attribute, Group),
		message(?(Groups, value, Group), append, @arg1),
		message(Groups, value, Group,
			?(@pce, instance, chain, @arg1)))),

	SortByName = ?(@arg1?name, compare, @arg2?name),

	order_groups(Groups),

	send(Groups?members, for_all,
	     message(@arg1?value, sort,
		     quote_function(SortByName))).

order_groups(Sheet) :-
	get(@manual, module, groups, @on, GroupModule),
	get(GroupModule, id_table, Table),
	get(Sheet, members, Chain),
	new(Unordered, chain),
	send(Chain, for_all,
	     if(not(?(Table, member, @arg1?name)),
		and(message(Unordered, append, @arg1),
		    message(Chain, delete, @arg1)))),
	send(Chain, sort,
	     ?(?(Table, member, @arg1?name)?index, compare,
	       ?(Table, member, @arg2?name)?index)),
	send(Chain, merge, Unordered).


cluster_behaviour(Chain, Combined) :-
	new(Combined, chain),
	send(Chain, for_all,
	     and(assign(new(B, var), @arg1),
		 or(and(assign(new(Ch, var),
			       ?(Combined, find,
				 message(@arg1?head?fetch_description, equal,
					 B?fetch_description))),
			message(Ch, append, B)),
		    message(Combined, append,
			    ?(@pce, instance, chain, B))))),
	send(Combined, for_all, message(@prolog, sort_cluster, @arg1)).


sort_cluster(Chain) :-
	send(Chain, sort, ?(@prolog, compare_cluster_elements,
			    @arg1?class_name, @arg2?class_name)).

compare_cluster_elements(X, 		    X, equal).
compare_cluster_elements(delegate_variable, _, smaller).
compare_cluster_elements(variable,	    _, smaller).
compare_cluster_elements(get_method,	    _, smaller).
compare_cluster_elements(send_method,	    _, larger).
compare_cluster_elements(X,		    _, _) :-
	format('[WARNING: compare_cluster_elements/3: Illegal first element ~w]~n', X),
	fail.

group_summary(Group, Summary) :-
	get(@manual, module, groups, @on, Module),
	get(Module?id_table, member, Group, GroupCard),
	get(GroupCard, summary, Summary),
	Summary \== @nil.


		 /*******************************
		 *	    HELP X-REF		*
		 *******************************/

:- multifile
	prolog:called_by/2.

prolog:called_by(substitute(_, []), []) :- !.
prolog:called_by(substitute(S, [_,call(Head)|Rest]), [H|T]) :-
	catch(Head =.. L, _, fail), !,
	append(L, [_,_], L2),
	H =.. L2,
	prolog:called_by(substitute(S, Rest), T).
prolog:called_by(substitute(S, [_,_|Rest]), Called) :-
	prolog:called_by(substitute(S, Rest), Called).




