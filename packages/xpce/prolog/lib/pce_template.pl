/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_class_template,
	  [ use_class_template/1,
	    use_class_template/2
	  ]).
:- use_module(library(pce)).
:- require([ pce_error/1
	   ]).

:- pce_begin_class(template, object, "use_class_template/1 super-class").
:- pce_end_class.


%	use_class_template(+TemplateClassName)
%
%	Include methods and variables from `TemplateClassName' into the
%	current class (i.e. between :- pce_begin_class/3 and
%	:- pce_end_class/0).
%
%	Including a template has the same semantics and literally including
%	the methods and variables defined on the template class into the
%	current class.  Code is shared between multiple users of the same
%	template class.
%
%	Template classes is a `cheap' alternative to multiple inheritance.
%	For an example, see $PCEHOME/prolog/lib/draw/shapes.pl.

use_class_template(Template) :-
	(   pce_compiling(Class)
	;   send(@class, instance_of, class),
	    get(@class, name, Class)
	;   pce_error(no_context_class_for_template(Template))
	) ->
	use_class_template(Class, Template).
   
%	use_class_template(+ClassName, +TemplateClassName)
%
%	Same as use_template/1, but explicitely imports in the named class
%	rather than the currently compiling class.

use_class_template(ClassSpec, TemplateSpec) :-
	get(@pce, convert, ClassSpec, class, Class),
	get(@pce, convert, TemplateSpec, class, Template),
	use_variables(Class, Template),
	use_methods(Class, Template, send_methods),
	use_methods(Class, Template, get_methods).


use_variables(Class, Template) :-
	send(Template?instance_variables, for_all,
	     if(message(@arg1?context, is_a, template),
		message(@prolog, use_variable, Class, @arg1))).


use_methods(Class, Template, Which) :-
	new(Done, chain),
	use_methods(Class, Template, Which, Done),
	send(Done, done).

use_methods(Class, Template, Which, Done) :-
	send(Template?Which, for_all,
	     if(and(not(message(Done, member, @arg1?name)),
		    message(@arg1?context, is_a, template)),
		and(message(Done, append, @arg1?name),
		    message(@prolog, use_method, Class, @arg1)))),
	get(Template, super_class, Super),
	(   Super == @nil
	->  send(Class, report, error,
		 'A template-class should be a subclass of template'),
	    fail
	;   get(Super, name, template)
	->  true
	;   use_methods(Class, Super, Which, Done)
	).

use_variable(Class, Var) :-
	get(Var, clone, V2),
	send(V2, slot, context, @nil),
	send(Class, instance_variable, V2).

use_method(Class, Method) :-
	get(Method, instantiate_template, M2),
	(   send(Method, instance_of, send_method)
	->  send(Class, send_method, M2)
	;   send(Class, get_method, M2)
	).
