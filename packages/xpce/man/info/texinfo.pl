/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(texinfo,
	  [ texinfo_open/0,
	    texinfo_open/1,
	    texinfo_close/0,
	    texinfo_class/1,
	    texi/0,
	    texi/1
	  ]).
:- use_module(library(pce)).
:- require([ flush/0
	   , start_emacs/0
	   , append/3
	   , between/3
	   , concat/3
	   , format/1
	   , ignore/1
	   , is_list/1
	   , shell/1
	   , term_to_atom/2
	   ]).

:- use_module(library(re_parse)).

:- dynamic
	drain/1,			% the output object (editor)
	class_needs_page/0,		% flag to insert @page
	fetched_description/2,		% cache for computed descriptions
	regex/2,			% cache for created regular expr.
	no_inherit_description/3.	% Forbit inheritance of description

:- pce_global(@documented, new(chain)).	% Chain with documented objects


texinfo_class(Class) :-
	start('Class %s', Class?name),
	(   class_needs_page
	->  output('@page\n')
	;   assert(class_needs_page)
	),
	label_object(Class),
	summary(Class, Summary),
	name_of(Class, ClassName),
	output('@class{%s}\n', ClassName),
	output('@section Class %s --- %s\n', [ClassName, Summary]),
	output('@ciindex %s\n\n', ClassName),
	cindex(Class),
	class_inheritance(Class),
	description(Class),
	output('\n'),
	instance_variables(Class),
	behaviour(Class),
	end, !.
texinfo_class(_) :-
	trace,
	fail.

summary(Obj, Summary) :-
	get(Obj, summary, Summary),
	Summary \== @nil, !.
summary(_, '').


name_of(Obj, TeXInfoName) :-
	get(Obj, name, Name),
	texinfo_escape(Name, TeXInfoName).


		 /*******************************
		 *    INHERITANCE/DELEGATION	*
		 *******************************/

class_inheritance(Class) :-
	output('@b{Inheritance:} '),
	show_inheritance(Class),
	output('\n'),
	(   get(Class, delegate, Chain),
	    \+ send(Chain, empty)
	->  output('@b{Delegation:} '),
	    show_delegation(Chain, 1)
	;   true
	),
	output('\n').
	       

show_inheritance(@nil) :- !,
	output('\n').
show_inheritance(Class) :-
	name_of(Class, ClassName),
	output('@result{} %s ', [ClassName]),
	get(Class, super_class, Super),
	show_inheritance(Super).

show_delegation(Chain, N) :-
	get(Chain, nth1, N, Var), !,
	output('%s (%s)', [Var?name, Var?type?name]),
	(   get(Chain, tail, Var)
	->  output('\n')
	;   output(', '),
	    NN is N + 1,
	    show_delegation(Chain, NN)
	).
show_delegation(_, _) :-
	output('\n').


		 /*******************************
		 *	   DESCRIPTION		*
		 *******************************/

excluded(Obj, InheritedFrom, _Description) :-
	no_inherit_description(Type, FromClass, Selector),
	send(Obj, instance_of, Type),
	get(Obj, name, Selector),
	send(InheritedFrom?context, is_a, FromClass).


texi_description(@Obj, Description) :-
	fetched_description(Obj, Description), !.
texi_description(Obj, Description) :-
	(   get(Obj, attribute, man_description, S0)
	;   get(Obj, man_attribute, description, S0)
	;   get(Obj, man_inherited_attribute, description, tuple(From, S0)),
	    \+ excluded(Obj, From, S0)
	;   get(Obj, summary, S0)
	;   new(S0, string),
	    send(S0, lock_object, @on)
	),
	S0 \== @nil, !,
	Obj = @Ref,
	assert(fetched_description(Ref, S0)),
	Description = S0.
	    

:- pce_extend_class(object).

texi_description(Obj, Description) :<-
	"Cached description slot for texinfo generation"::
	texi_description(Obj, Description).

:- pce_end_class.

description(Obj) :-
	texi_description(Obj, S0),
	get(S0, copy, S1),
	texinfo_description(S1),
	output('%s\n', S1).

to_regex(Pattern, Regex) :-
	get(@pce, convert, string(Pattern), regex, Regex).

/*	Dangerous: reantrance in subloops!
to_regex(Pattern, Regex) :-
	regex(Pattern, Regex), !.
to_regex(Pattern, Regex) :-
	get(@pce, convert, string(Pattern), regex, Regex),
	send(Regex, lock_object, @on),
	send(Regex, compile, @on),	% optimised
	assert(regex(Pattern, Regex)).
*/


substitute(_, []) :- !.
substitute(S, [Search, Replace | Rest]) :-
	to_regex(Search, Re),
	(   Replace = call(Head)
	->  Head =.. [Pred|Args],
	    append(Args, [@arg1, @arg2], AllArgs),
	    Msg =.. [message, @prolog, Pred | AllArgs],
	    send(Re, for_all, S, Msg)
	;   send(Re, for_all, S,
		 message(@arg1, replace, @arg2, string(Replace)))
	),
%	done_regex(Re),
	substitute(S, Rest).
	

texinfo_description(S) :-
	send(S, ensure_nl),
	substitute(S, ['\n\\s +\n', '\n\n']), % cannonise
	texinfo_escape(S),
	texinfo_lists(S),
	substitute(S,
		   [ %   HEADING
		     '\n\n\\s *\\([A-Z ?!._-]+\\)\\s *\n\n',
		     	call(header),
 		     %   ** SubHeader
		     '\n\n\\*\\*+\\s +\\(.*\\)\n\n',
 			'\n\n@subsubheading \\1\n\n',
		     %   *bold*
		     '\\*\\([^ ]+\\)\\*',
		        '@strong{\\1}'
		   ]),
	send(S, strip).


texinfo_lists(S) :-
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
		     '\n\n+\\(\\(\n*\t.*\\)+\\)',
		     	call(example)
		   ]).
	

example(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2, ['^\t', '#X#', '^#X#',  '']), % HACK!
	send(S2, untabify, 4),
	send(S2, prepend, string('\n\n@example\n')),
	send(S2, append, string('@end example\n\n')),
	send(Re, register_value, String, S2).


list(description, Re, String) :-
	get(Re, register_value, String, 1, L1),
	get(Re, register_value, String, 2, RestLines),
	substitute(RestLines, ['\n\t', '\n']),
	send(RestLines, strip, trailing),
	send(RestLines, ensure_nl),
	texinfo_lists(RestLines),	% sub-lists
	send(RestLines, strip),
	send(RestLines, ensure_nl),
	send(RestLines, prepend, string('@item %s @*\n', L1)),
	send(Re, register_value, String, RestLines).
list(itemize, Re, String) :-
	get(Re, register_value, String, 1, L1),
	get(Re, register_value, String, 2, RestLines),
	new(S0, string('\n%s%s', L1, RestLines)),
	substitute(S0, ['\n\t', '\n']),
	send(S0, strip, trailing),
	send(S0, ensure_nl),
	texinfo_lists(S0),	% sub-lists
	send(S0, strip),
	send(S0, ensure_nl),
	send(S0, prepend, string('@item\n')),
	send(Re, register_value, String, S0).
	

itemize(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t\\*\\s *\\(.*\\)\\(\\(\n\t[^*].*\\|\n *\\)*\\)',
		     	call(list(itemize))
		   ]),
	send(S2, prepend, string('\n\n@itemize @bullet\n')),
	send(S2, ensure_nl),
	send(S2, append, string('@end itemize\n\n')),
	send(Re, register_value, String, S2).


description(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t#\\s *\\(.*\\)\\(\\(\n\t[^#].*\\|\n *\\)*\\)',
		        call(list(description))
		   ]),
	send(S2, ensure_nl),
	send(S2, prepend, string('\n\n@itemize @bullet\n')),
	send(S2, append, string('@end itemize\n\n')),
	substitute(S2, ['@\\*\n@\\(item\\|end\\)', '\n@\\1']),
	send(Re, register_value, String, S2).
	

enumerate(Re, String) :-
	get(Re, register_value, String, 1, S2),
	substitute(S2,
		   [ '\n*\t\\sd+)\\s *\\(.*\\)\\(\\(\n\t[^0-9].*\\|\n *\\)*\\)',
			call(list(itemize))
		   ]),
	send(S2, ensure_nl),
	send(S2, prepend, string('\n\n@enumerate\n')),
	send(S2, append, string('@end enumerate\n\n')),
	send(Re, register_value, String, S2).
	

table(Re, String) :-
	get(Re, register_value, String, 1, S2),
	texinfo_unescape(S2),
	tex_escape(S2),
	substitute(S2,
		   [ '\n*\t\\([^\t]+\\)\t+\\(.*\\)\n',
		     	'\\1&\\2\\cr\n'
		   ]),
	send(S2, prepend,
	     string('\\halign{\\hskip\\tableindent#\\hfil&#\\hfil\\cr\n')),
	send(S2, prepend,
	     string('\n\n@quotation\n@tex\n\\vskip 0.8ex\\tabskip 1em\n')),
	send(S2, append, string('}\n@end tex\n@end quotation\n\n')),
	send(Re, register_value, String, S2).


header(Re, String) :-
	get(Re, register_value, String, 1, ALLCAPITALS),
	get(ALLCAPITALS, capitalise, Capitals),
	send(Re, register_value, String,
	     string('@heading %s', Capitals), 1).


		 /*******************************
		 *	     ESCAPING		*
		 *******************************/

texinfo_escape(S) :-
	substitute(S, [ '@',   '@@',
			'{',   '@{',
			'}',   '@}',
			'<->', '@botharrow{}',
			'->',  '@sendarrow{}',
			'<-',  '@getarrow{}'
		      ]).
texinfo_escape(In, Out) :-
	new(Out, string('%s', In)),
	texinfo_escape(Out).

texinfo_identifier(S) :-
	substitute(S, [ '@',   'atsign'
		      ]).
texinfo_identifier(In, Out) :-
	new(Out, string('%s', In)),
	texinfo_identifier(Out).

texinfo_unescape(S) :-
	substitute(S, [ '@@',           '@',
			'@{',           '{',
			'@}',           '}',
			'@botharrow{}', '<->',
			'@sendarrow{}', '->',
			'@getarrow{}',  '<-'
		      ]).

tex_escape(S) :-
	substitute(S, [ '\\$',  '@@$@@',
			'\\\\',	'$\\backslash$',
			'@@$@@','\\$',
			'@',	'\\@',			% for texinfo only
			'~',	'$\\sim$',
			'\\^',	'\\^~',
			'<->',	'$\\leftrightarrow$',
			'->',	'$\\rightarrow$',
			'<-',	'$\\leftarrow$',
			'>',	'$>$',
			'<',	'$<$',
			'_',	'\\_',
			'#',	'\\#',
			'|',	'$\\mid$',
			'%',	'\\%',
			'&',	'\\&',
			'{',	'$\\lbrace$',
			'}',	'$\\rbrace$'
		      ]).
tex_escape(In, Out) :-
	new(Out, string('%s', In)),
	tex_escape(Out).


		 /*******************************
		 *	     BEHAVIOUR		*
		 *******************************/

behaviour(Class) :-
	output('@subheading Behaviour\n\n'),
	collect_behaviour(Class, Chain),
	group_objects(Chain, Groups),
	send(Groups, for_all,
	     message(@prolog, texinfo_group, Class, @arg1?name, @arg1?value)).
	

collect_behaviour(Class, Behaviour) :-
	new(Behaviour, chain),

	new(Merge, message(Behaviour, append, @arg1)),

	send(Class?get_methods, for_all, Merge),
	send(Class?send_methods, for_all, Merge),
	send(Class?instance_variables, for_all,
	     if(@arg1?context == Class,
		message(Behaviour, append, @arg1))),
	exclude(Class, Behaviour).


group_objects(Chain, Groups) :-
	new(Groups, sheet),
	Group = when(@arg1?group, @arg1?group, behaviour),
	send(Chain, for_all,
	     if(Group,
		if(message(Groups, is_attribute, Group),
		   message(?(Groups, value, Group), append, @arg1),
		   message(Groups, value, Group,
			   ?(@pce, instance, chain, @arg1))),
		if(@arg1?message == @nil,
		   message(@arg1, report, warning,
			   'Skipped %N: no group', @arg1)))),

	SortByName = ?(@arg1?name, compare, @arg2?name),

	order_groups(Groups),
	send(Groups?members, for_all,
	     message(@arg1?value, sort,
		     quote_function(SortByName))).


texinfo_group(Class, Group, Elements) :-
	get(Group, capitalise, CGroup),
	(   group_summary(Group, Summary)
	->  output('@subsection %s --- %s\n\n', [CGroup, Summary])
	;   output('@subsection %s\n\n', [CGroup])
	),
	output('@giindex %s\n', Group),
	(   group_description(Class, Group, S0)
	->  get(S0, copy, S1),
	    texinfo_description(S1),
	    output('%s\n\n', S1)
	;   true
	),
	combine_behaviour(Elements, Clusters),
	send(Clusters, for_all,
	     message(@prolog, texinfo_cluster, @arg1)).


combine_behaviour(Chain, Combined) :-
	new(Combined, chain),
	send(Chain, for_all,
	     and(assign(new(B, var), @arg1),
		 or(and(assign(new(Ch, var),
			       ?(Combined, find,
				 message(@arg1?head?texi_description, equal,
					 B?texi_description))),
			message(Ch, append, B)),
		    message(Combined, append,
			    ?(@pce, instance, chain, B))))).


texinfo_cluster(Chain) :-
	send(@documented, merge, Chain),
	send(Chain, sort, ?(@prolog, compare_cluster_elements,
			    @arg1?class_name, @arg2?class_name)),
	send(Chain, for_all,
	     message(@prolog, label_object, @arg1)),
	send(Chain, for_all,
	     and(if(@arg1 == Chain?head,
		    message(@prolog, output, '@deffn '),
		    message(@prolog, output, '@deffnx ')),
		 message(@prolog, texinfo_behaviour, @arg1),
		 message(@prolog, cindex, @arg1))),
	get(Chain, head, First),
	refinement_of(First),
	description(First),
	bugs(Chain),
	output('@end deffn\n\n').

compare_cluster_elements(X, 		    X, equal).
compare_cluster_elements(delegate_variable, _, smaller).
compare_cluster_elements(variable,	    _, smaller).
compare_cluster_elements(get_method,	    _, smaller).
compare_cluster_elements(send_method,	    _, larger).
compare_cluster_elements(X,		    _, _) :-
	format('[WARNING: compare_cluster_elements/3: Illegal first element ~w]~n', X),
	fail.


refinement_of(M) :-
	send(M, has_get_method, inherited_from),
	get(M, inherited_from, SuperMethod), !,
	get(SuperMethod, man_name, S0),
	substitute(S0, ['.*\t', '']),
	output('@b{Refinement of:} %s @*', S0).
refinement_of(_).

bugs(Chain) :-
	get(Chain, map, ?(@arg1, man_attribute, bugs), Bugs),
	send(Bugs, unique),
	send(Bugs, for_all, message(@prolog, footnote, @arg1)).

footnote(Text) :-
	new(S0, string('%s', Text)),
	texinfo_escape(S0),
	substitute(S0, [ '^\\s *[*#]', '@bullet ' ]),
	output('@footnote{%s}\n', S0).

texinfo_behaviour(M) :-
	send(M, instance_of, send_method), !,
	get(M, name, Name),
	get(M, types, TypeVector),
	output('{Send Method} '),
	output(' %s ', Name),
	argument_vector(TypeVector),
	output('\n'),
	get(M?context, name, CName),
	texinfo_escape(Name, TeXName),
	texinfo_escape(CName, TeXCName),
	output('@biindex %s (@sendarrow{} %s)\n', [TeXName, TeXCName]).
texinfo_behaviour(M) :-
	send(M, instance_of, get_method), !,
	get(M, name, Name),
	get(M, return_type, Type),
	get(M, types, TypeVector),
	output('{Get Method} '),
	output(' %s ', Name),
	argument_vector(TypeVector),
	output(' @result{'),
	type(Type),
	output('}\n'),
	output('@biindex %s (@getarrow %s)\n',
	       [Name, M?context?name]).
texinfo_behaviour(M) :-
	send(M, instance_of, variable), !,
	get(M, name, Name),
	get(M, type, Type),
	output('{Variable} '),
	output(' %s ', Name),
	type(Type),
	get(M, access_arrow, Arrow),
	new(SA, string(' (%s)', Arrow)),
	texinfo_escape(SA),		% get arrows right
	output('%s', SA),
	(   get(M?context, resource, Name, Resource)
	->  get(Resource, string_value, String),
	    get(Resource, resource_class_name, Class),
	    new(R0, string(' [Pce.%s.%s: %s]', Class, Name, String)),
	    texinfo_escape(R0),
	    output('%s', R0)
	;   true
	),
	get(M, access_arrow, ArrowName),
	texinfo_escape(ArrowName, TexInfoName),
	output('\n@biindex %s (%s %s)\n', [Name, TexInfoName, M?context?name]).

cindex(Obj) :-
	get(Obj, attribute, ci_index, Concepts), !,
	send(Concepts, for_all,
	     message(@prolog, write_cindex, @arg1)).
cindex(_).

write_cindex(Kwd) :-
	texinfo_escape(Kwd, S0),
	output('@cindex %s\n', S0).


man_id(Obj, TheId) :-
	(   send(Obj, '_instance_of', function)
	->  get(Obj, '_man_id', Id)
	;   get(Obj, man_id, Id)
	),
	new(S, string('%s', Id)),
	substitute(S,
		   [ '\\\\==', ne,
		     '@=', atsign
		   ]),
	get(S, value, TheId).

label_object(Obj) :-
	man_id(Obj, Id),
	texinfo_escape(Id, TeXId),
	output('@setref{%s}\n', TeXId).


		 /*******************************
		 *      INSTANCE VARIABLES	*
		 *******************************/

instance_variables(Class) :-
	get(Class, instance_variables, Vector),
	new(Local, chain),
	new(Inherited, chain),
	send(Vector, for_all,
	     if(@arg1?context == Class,
		message(Local, append, @arg1),
		message(Inherited, append, @arg1))),
	exclude(Class, Local),
	exclude(Class, Inherited),
	(   send(Local, empty), send(Inherited, empty)
	->  true
	;   output('@subheading Instance Variables\n\n'),
	    (	\+ send(Local, empty)
	    ->	% output('@heading Locals\n\n'),
		texi_instance_variables(Local)
	    ;	true
	    ),
	    (	\+ send(Inherited, empty),
		fail			% not sure
	    ->	output('@heading Inherited\n\n'),
		texi_instance_variables(Inherited)
	    ;	true
	    )
	).

parbox_column(Width) :-
	output('\\parbox{%s}{#}', [Width]).

texi_instance_variables(Vars) :-
	group_objects(Vars, Groups),
	output('@tex\n\\vskip 1.5ex\n\\tabskip=1em\n'),
	output('\\halign{'),
	parbox_column('22ex'), output('&'),		% name
	output('\\hfil#\\hfil&'),			% access arrow
	parbox_column('30ex'), output('&'),		% type
	parbox_column('45ex'), output('\\cr\n'),	% type
	send(Groups, for_all,
	     message(@prolog, texi_instance_variable_group,
		     @arg1?name, @arg1?value)),
	output('}\n@end tex\n\n').

texi_instance_variable_group(Name, Vars) :-
	tex_escape(Name, S0),
	output('\\hskip -1em {\\bf %s}\\cr\n', [S0]),
	send(Vars, for_all,
	     message(@prolog, texi_instance_variable_summary, @arg1)).


texi_instance_variable_summary(Var) :-
	get(Var, name, Name),
	get(Var, access_arrow, AccessArrow),
	get(Var?type, name, Type),
	get(Var, summary, Summary),
	tex_escape(Name, S0),
	tex_escape(AccessArrow, S1),
	tex_escape(Type, S2), substitute(S2, [',', ',\\penalty0']),
	tex_escape(Summary, S3),
	output('\\mbox{%s}&%s&%s&%s\\cr\n', [S0, S1, S2, S3]).


		 /*******************************
		 *	       TYPE		*
		 *******************************/

type(Type) :-
	get(Type, name, Name),
	texinfo_escape(Name, S),
	output('%s', S).

		 /*******************************
		 *	  ARGUMENT VECTOR	*
		 *******************************/

argument_vector(V) :-
	get(V, size, Size),
	between(1, Size, Arg),
	    get(V, element, Arg, Type),
	    get(Type, name, TypeName),
	    texinfo_escape(TypeName, TN),
	    (	get(Type, argument_name, ArgName),
		ArgName \== @nil
	    ->	output('@var{%s}=%s', [ArgName, TN])
	    ;	output('%s', TN)
	    ),
	    (Size \== Arg -> output(', ') ; true),
	fail ; true.


		 /*******************************
		 *	   GLOBAL OBJECTS	*
		 *******************************/

texinfo_global_objects :-
	start('Global Objects'),
	get(@manual, module, objects, @on, Module),
	new(Globals, chain),
	send(Module?id_table, for_all, message(Globals, append, @arg2)),
	send(Globals, sort,
	     ?(@arg1?identifier, compare, @arg2?identifier)),
	send(Globals, for_all,
	     if(not(message(@prolog, check_global, @arg1)),
		message(Globals, delete, @arg1))),
	combine_behaviour(Globals, Combined),
	send(Combined, for_all,
	     message(@prolog, texinfo_global_cluster, @arg1)),
	end.


check_global(Card) :-
	texi_description(Card, _),	% has description?
	get(Card, identifier, Id),
	concat('O.', Ref, Id),
	object(@Ref).			% object exists


texinfo_global_cluster(Cluster) :-
	send(Cluster, for_all,
	     if(Cluster?head == @arg1,
		message(@prolog, global_deffn, @arg1, deffn),
		message(@prolog, global_deffn, @arg1, deffnx))),
	get(Cluster, head, Head),
	description(Head),
	output('@end deffn\n\n').


global_deffn(Card, Type) :-
	get(Card, identifier, Id),
	concat('O.', Ref, Id),
	object(@Ref),
	label_object(@Ref),
	get(@Ref, '_class_name', ClassName),
	get(ClassName, capitalise, TypeName),
	output('@%s {%s} @@%s\n', [Type, TypeName, Ref]).


		 /*******************************
		 *	   OUTPUT SECTION	*
		 *******************************/

output(Format) :-
	output(Format, []).
output(Format, Arg) :-
	\+ is_list(Arg), !,
	output(Format, [Arg]).
output('%s\n', [String]) :- !,	% HACK to avoid crash on long description
	drain(Obj),
	send(Obj, append, String),
	send(Obj, format, '\n').
output(Format, Args) :-
	drain(Obj),
	Term =.. [send, Obj, format, Format | Args],
	Term.

texinfo_open(Obj) :-
	asserta(drain(Obj)).
texinfo_close :-
	retract(drain(Obj)),
	ignore(pce_catch_error(no_behaviour, send(Obj, close))).


start(Format) :-
	start(Format, []).
start(Format, Arg) :-
	\+ is_list(Arg), !,
	start(Format, [Arg]).
start(Format, Args) :-
	Term =.. [string, Format | Args],
	get(Term, value, Atom),
	format(' (~w', Atom), flush.

end :-
	format(')'), flush.


:- pce_global(@texinfo_view, make_texinfo_view).

make_texinfo_view(V) :-
	new(V, view('TexInfo test view')),
	send(new(D, dialog), below, V),
	send(D, append, button(clear, message(V, clear))),
	send(D, append, button(quit, message(D, destroy))),
	send(V, open).

texinfo_open :-
	send(@texinfo_view, open),
	texinfo_open(@texinfo_view).


		 /*******************************
		 *	 DEFINITION FILE	*
		 *******************************/

:- pce_global(@document, make_document).

make_document(Sheet) :-
	new(Sheet, sheet),
	send(Sheet, append,
	     attribute(group_list, new(chain))),
	send(Sheet, append,
	     attribute(document_list, new(chain))),
	send(Sheet, append,
	     attribute(group_summaries, new(hash_table))),
	send(Sheet, append,
	     attribute(classes, new(hash_table))),
	load_definitions('manual.def').

%	ACCESS PREDICATES

document_class_properties(Class, Name, Properties) :-
	get(@document, value, classes, HashTable),
	get(HashTable, member, Class, Sheet),
	get(Sheet, value, Name, Properties).


%	exclude(+Class, +Object)
%	True if object is to be excluded from the class description

exclude(Class, Object) :-
	document_class_properties(Class, exclude, Exclude),
	send(Exclude, member, Object).

exclude(Class, Chain) :-
	document_class_properties(Class, exclude, Exclude),
	send(Chain, subtract, Exclude).


%	group_description(+Class, +Group, -String)

group_description(Class, Group, String) :-
	document_class_properties(Class, group_descriptions, DescriptionSheet),
	get(DescriptionSheet, value, Group, String).


group_summary(Group, Summary) :-
	get(@document?group_summaries, member, Group, Summary).


%	order_groups(Groups)
%	Order chain of groups

order_groups(Sheet) :-
	get(@document, group_list, AllGroupsChain),
	get(Sheet, members, Chain),
	new(Unordered, chain),
	send(Chain, for_all,
	     if(not(message(AllGroupsChain, member, @arg1?name)),
		and(message(@document, report, warning,
			    'No group named %s', @arg1?name),
		    message(Unordered, append, @arg1),
		    message(Chain, delete, @arg1)))),
	send(Chain, sort,
	     message(AllGroupsChain, before, @arg1?name, @arg2?name)),
	send(Chain, merge, Unordered).


%	LOADING

load_definitions(File) :-
	start('%s', File),
	new(F, file(File)),
	send(F, open, read),
	re_parse_loop(F, Re, A, global_pattern(F, Re, A), @nil),
	send(F, close),
	end.


global_pattern(_,				% blank line
	       '\\s *$',
	       true).
global_pattern(_,				% comment line
	       '\\s *%',
	       true).
global_pattern(_,
	       '#group\\s *\\(\\w+\\)\\s *"\\(.*\\)"\\s *$',
	       def_group(1:name, 2)).
global_pattern(F,
	       '#class\\s *\\(.*\\)',
	       def_class(F, 1:name)).
global_pattern(F,
	       '#chapter\\s +\\(.*\\)',
	       def_chapter(F, 1)).
global_pattern(F,
	       '#texinfo\\s *',
	       def_texinfo(F)).
global_pattern(F,
	       '#includealways\\s *\\(.*\\)',
	       includealways(F, 1)).
global_pattern(F,
	       '#includeonly\\s *\\(.*\\)',
	       includeonly(F, 1)).
global_pattern(F,
	       '#include\\s *\\([^ \n]*\\)',
	       include(F, 1:name)).
global_pattern(_,
	       '#call\\s *\\(.*\\)',
	       call_atom(1:name)).
global_pattern(_,
	       '#noinheritdescription\\s *\\(\\w+\\)\\s *\\(<?->?\\)\\s *\\(\\w+\\)',
	       def_no_inherit_description(1:name, 2:name, 3:name)).

:- pce_global(@file_name_regex, new(regex('[a-zA-Z.#@%_+-]+'))).

includealways(F, String) :-
	new(C, chain),
	send(@file_name_regex, for_all, String,
	     message(C, append,
		     ?(@file_name_regex, register_value, String)?value)),
	send(F, attribute, attribute(includealways, C)).

includeonly(F, String) :-
	new(C, chain),
	send(@file_name_regex, for_all, String,
	     message(C, append,
		     ?(@file_name_regex, register_value, String)?value)),
	send(F, attribute, attribute(includeonly, C)).
		       
include(F, Name) :-
	(   get(F, attribute, includeonly, Only),
	    (   (   send(Only, member, Name)
		;   get(F, attribute, includealways, Always),
		    send(Always, member, Name)
		)
	    ->  load_definitions(Name)
	    ;   true
	    )
	;   load_definitions(Name)
	).


map_arrow((->), send_method).
map_arrow((<-), get_method).
map_arrow((-),  variable).

def_no_inherit_description(ClassName, Access, Selector) :-
	map_arrow(Access, Type),
	assert(no_inherit_description(Type, ClassName, Selector)).


call_atom(Atom) :-
	term_to_atom(Code, Atom),
	send(@document?document_list, append, Code).


def_group(Name, Summary) :-
	send(@document?group_list, append, Name),
	send(@document?group_summaries, append, Name, Summary).


def_texinfo(File) :-
	new(S, string),
	re_parse_loop(File, Re, A,
		      description_pattern(S, Re, A),
		      '#end\\s +texinfo'),
	send(@document?document_list, append, S).


def_chapter(File, Title) :-
	new(S, string),
	re_parse_loop(File, Re, A,
		      description_pattern(S, Re, A),
		      '#end\\s +chapter'),
	texinfo_description(S),
	send(S, prepend,
	     string('\n\n@node %s\n@chapter %s\n\n', Title, Title)),
	send(S, append, string('\n\n')),
	send(@document?document_list, append, S),
	send(@document?document_list, append, % HACK ...
	     message(@prolog, retractall, class_needs_page)).


def_class(File, ClassName) :-
	get(@pce, convert, ClassName, class, Class),
	send(@document?document_list, append, Class),
	send(@document?classes, append, Class,
	     sheet(attribute(exclude, new(chain)),
		   attribute(group_descriptions, new(sheet)))),
	re_parse_loop(File, Re, A,
		      class_pattern(File, Class, Re, A),
		      '#end\\s +class').


class_pattern(_, _,				% blank line
	      '\\s *$',
	      true).
class_pattern(_, _,				% comment line
	      '\\s *%',
	      true).
class_pattern(_, Class,
	      '#exclude\\s +->\\(\\w+\\)\\s *$',
	      class_exclude(Class, send_method, 1:name)).
class_pattern(_, Class,
	      '#exclude\\s +<-\\(\\w+\\)\\s *$',
	      class_exclude(Class, get_method, 1:name)).
class_pattern(File, Class,
	      '#description\\s +group\\s +\\(.*\\)',
	      class_group_description(File, Class, 1:name)).
class_pattern(File, Class,
	      '#description\\s *\\(<-\\|->\\|-\\)\\(\\w+\\)',
	      class_behaviour_description(File, Class, 1:name, 2:name)).
class_pattern(File, Class,
	      '#description\\s *$',
	      class_description(File, Class)).
	      
class_exclude(Class, Type, Name) :-
	get(Class, Type, Name, Method),
	get(@document, classes, Classes),
	get(Classes, member, Class, Sheet),
	get(Sheet, value, exclude, Chain),
	send(Chain, add, Method).

class_group_description(File, Class, Group) :-
	new(S, string),
	re_parse_loop(File, Re, A,
		      description_pattern(S, Re, A),
		      '#end\\s +description'),
	get(@document, classes, Classes),
	get(Classes, member, Class, Sheet),
	get(Sheet, value, group_descriptions, DescriptionSheet),
	send(DescriptionSheet, value, Group, S).

%	#description
%	#end description

class_description(File, Class) :-
	new(S, string),
	re_parse_loop(File, Re, A,
		      description_pattern(S, Re, A),
		      '#end\\s +description'),
	send(Class, attribute, attribute(man_description, S)).

%	#description -|<-|->selector
%	#end description

class_behaviour_description(File, Class, Arrow, Name) :-
	new(S, string),
	arrow_behaviour(Arrow, BehaviourType),
	(   get(Class, BehaviourType, Name, Behaviour)
	->  re_parse_loop(File, Re, A,
			  description_pattern(S, Re, A),
			  '#end\\s +description'),
	    send(Behaviour, attribute, attribute(man_description, S))
	;   send(Class, report, warning,
		 'Cannot find %s %s', BehaviourType, Name)
	).

arrow_behaviour(-,	instance_variable).
arrow_behaviour('<-',	get_method).
arrow_behaviour('->',	send_method).

description_pattern(S, '.*\n', send(S, append, 0)).


		 /*******************************
		 *	  GROUP SUMMARY		*
		 *******************************/

group_summary :-
	start('Group summary'),
	group_objects(@documented, Groups),
	send(Groups?members, sort, ?(@arg1?name, compare, @arg2?name)),
	send(Groups, for_all,
	     message(@prolog, texi_group_summary, @arg1?name, @arg1?value)),
	end.

texi_group_summary(Name, Objects) :-
	start(Name),
	(   group_summary(Name, Description)
	->  output('\n@section %s --- %s\n\n', [Name, Description])
	;   output('\n@section %s\n\n', [Name])
	),
	send(Objects, sort,
	     ?(@prolog, compare_group_members, @arg1, @arg2)),
	output('@tex\n\\vskip 1.5ex\n\\tabskip=0.2em\n'),
	output('\\def\\ibox#1{\\vtop{\\hsize 2em\\hfil#1\\hfil}}\n'),
	output('\\def\\nbox#1{\\vtop{\\hsize 3em\\hfill#1}}\n'),
	output('\\halign{\\ibox{#}&'),			% identifier
	parbox_column('35ex'), output('&'),		% name
	parbox_column('50ex'), output('&'),		% summary
	output('\\nbox{#}\\cr\n'),			% page number
	send(Objects, for_all,
	     message(@prolog, object_summary, @arg1)),
	output('}\n@end tex\n\n'),
	end.

compare_group_members(O1, O2, Result) :-
	get(O1?name, compare, O2?name, Result),
	Result \== equal, !.
compare_group_members(O1, O2, Result) :-
	get(O1?context?name, compare, O2?context?name, Result),
	Result \== equal, !.
compare_group_members(O1, O2, Result) :-
	get(O1, class_name, C1),
	get(O2, class_name, C2),
	compare_cluster_elements(C1, C2, Result).

object_summary(Obj) :-
	get(Obj, man_indicator, Id),
	get(Obj, summary, Summary),
	get(Obj, name, Name),
	get(Obj, access_arrow, Arrow),
	get(Obj?context, name, ClassName),
	man_id(Obj, Identifier),

	tex_escape(Name, S1),
	tex_escape(Arrow, S2),
	tex_escape(ClassName, S3),
	tex_escape(Summary, S4),
	texinfo_identifier(Identifier, S5),

	output('%s&\\mbox{%s} (%s%s)&%s&\\pageref{%s}\\cr\n',
	       [Id, S1, S2, S3, S4, S5]).


		/********************************
		*        SETTING VARIABLES	*
		********************************/

texi_set_version :-			% @set version 4.5.8
	get(@pce, version, Version),
	new(V, string('%s', Version)),
	substitute(V, [',.*', '']),
	output('@set version %s\n', V).

texi_set_month :-			% @set update-month July 1993
	get(@pce, version, Version),
	new(V, string('%s', Version)),
	substitute(V, ['[^,]*,\\s *', '']),
	output('@set update-month %s\n', V).


		 /*******************************
		 *   KEYWORDS (CONCEPT INDEX)   *
		 *******************************/

attach_keywords :-
	start('keywords'),
	get(@manual, module, keywords, @on, Module),
	send(Module?id_table, for_all,
	     message(@prolog, attach_keyword, @arg2)),
	end.

attach_keyword(Card) :-
	get(Card, related, see_also, Objects),
	new(Synonyms, chain(Card?name)),
	new(IsKwd, message(@arg1, instance_of, man_keyword_card)),
	send(Objects, for_all,
	     if(IsKwd, message(Synonyms, add, @arg1?name))),
	get(Synonyms, map,
	    when(message(@arg1, suffix, '*'),
		 ?(@arg1, delete_suffix, '*'),
		 @arg1?self), S2),
	send(S2, unique),
	send(Objects, for_all,
	     if(not(IsKwd),
		if(message(@prolog, add_ci_index,
			   @arg1?object, S2)))).

add_ci_index(Obj, Concepts) :-
	get(Obj, attribute, ci_index, Chain), !,
	send(Chain, union, Concepts).
add_ci_index(Obj, Concepts) :-
	send(Obj, attribute, attribute(ci_index, Concepts)).


		 /*******************************
		 *	     CHECKING		*
		 *******************************/

check_documented_all_classes :-
	get(@document, document_list, List),
 	check_documented_all_classes(List).


check_documented_all_classes(List) :-
	get(List, map,
	    when(message(@arg1, instance_of, class), @arg1),
	    DocumentedClasses),
	new(AllClasses, chain),
	send(@classes, for_all,
	     if(@arg2?creator == built_in,
		message(AllClasses, append, @arg2))),
	send(AllClasses, subtract, DocumentedClasses),
	(   send(AllClasses, empty)
	->  true
	;   format('[WARNING: Undocumented classes:~n'),
	    send(AllClasses, for_all,
		 message(@pce, format, '\t%s\n', @arg1?name))
	).
	


		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

texi(ClassName) :-
	get(@pce, convert, ClassName, class, Class),
	make_preview_view(V),
	send(V, label, string('TexInfo for %s %s',
			      Class?class_name, Class?name)),
	send(V, confirm_done, @off),
	generate(V, chain(Class)),
	send(V, open).


texi :-
	make_preview_view(V),
	free(@document),
	get(@document, document_list, ClassList),
	generate(V, ClassList).


make_preview_view(V) :-
	new(V, view(string('TexInfo for XPCE reference manual'))),
	send(new(D, dialog), below, V),
	send(D, append, new(label)),	% report messages
	send(D, append, button(generate,
			       message(@prolog, generate, V))),
	send(D, append, button('TeX',
			       message(@prolog, tex, V))),
	send(D, append, button('TeX Index',
			       message(@prolog, texindex, V))),
	send(D, append, button(preview,
			       message(@prolog, preview, V))),
	send(D, append, button(quit,
			       message(D?frame, destroy))),
	send(V, open).
	


generate(V, Classes) :-
	send(V, attribute, attribute(document_list, Classes)),
	retractall(fetched_description(_,_)),
	free(@documented),
	generate(V).


generate(V) :-
	get(V, document_list, Document),
	texinfo_open(V),
	send(V, clear),

	attach_keywords,

	Class = ?(@pce, convert, @arg1, class),

	send(Document, for_all,
	     if(message(@arg1, instance_of, char_array),
		message(@prolog, output, '%s', @arg1),
		if(message(@arg1, instance_of, code),
		   message(@arg1, forward),
		   or(message(@prolog, texinfo_class, Class),
		      message(Class, report,
			      warning, 'Failed for make texinfo'))))),

	output('@bye\n'),
	send(V, save, 'refman.texinfo'),
	send(V, caret, 0),
	texinfo_close.


tex(V) :-
	send(V, save),
	get(string('tex %s', V?file?name), value, Cmd),
	unix(Cmd).


texindex(V) :-
	get(V?file, name, Path),
	concat(Base, '.texinfo', Path),
	get(string('texindex %s.??', Base), value, Cmd),
	shell(Cmd).


preview(V) :-
	get(V?file, name, Path),
	concat(Base, '.texinfo', Path),
	get(string('xdvi %s &', Base), value, Cmd),
	shell(Cmd).


unix(Command) :-
	start_emacs,
	new(P, process('/bin/sh', '-c', Command)),
	new(B, emacs_process_buffer(P, string('*%s*', Command))),
	send(B, open),
	send(B, start_process).
