/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(man_data,
	[
	]).

:- use_module(library(pce)).
:- use_module(util).
:- consult(classmap).
:- require([ absolute_file_name/3
	   , append/3
	   , between/3
	   , concat/3
	   , term_to_atom/2
	   ]).

%	find_module(+Name, +Create, -Module)
%
%	Find/create a manual module with the given name.  Bypasses
%	@manual to avoid having to use the GUI.

find_module(Name, Create, Module) :-
	new(Space, man_space(reference)),
	(   send(Space, ensure_loaded, Name)
	->  get(Space, module, Name, Module)
	;   Create == @on
	->  new(Module, man_module(Space, Name))
	;   fail
	).


		/********************************
		*     SPECIFIC MANUAL CARDS	*
		********************************/

:- pce_begin_class(man_class_card(identifier), man_card,
		   "Manual card of a class").

variable(user_interface,	string*,	get,
	 "Description of user interface").
variable(bugs,			string*,	get,
	 "Known bugs/anomalities").


initialise(C, Class:class) :->
	"Initialise from class"::
	send(C, send_super, initialise,
	     Class?man_module, Class?name, Class?man_id).


object(C, Class:class) :<-
	"Get associated class"::
	get(C, identifier, Name),
	name(Name, [0'C, 0'. | S0]),
	name(ClassName, S0),
	get(@classes, member, ClassName, Class).

:- pce_end_class.


:- pce_begin_class(man_variable_card(identifier), man_card,
		   "Manual card of an instance variable").

variable(defaults,	string*,	get, "Default value").

initialise(C, Var:variable) :->
	"Initialise from variable"::
	send(C, send_super, initialise,
	     Var?man_module, Var?name, Var?man_id).

object(C, Var:variable) :<-
	"Get associated instance variable"::
	get(C, identifier, Name),
	name(Name, [0'V, 0'. |S0]),
	append(S1, [0'.|S2], S0),
	name(ClassName, S1),
	name(VarName, S2),
	get(@classes, member, ClassName, Class),
	get(Class, instance_variable, VarName, Var).

:- pce_end_class.


:- pce_begin_class(man_method_card(identifier), man_card,
		   "Manual card of a method").

variable(diagnostics,	string*,	both,
	 "Possible error conditions/messages").
variable(defaults,	string*,	get,
	 "Default value").
variable(bugs,		string*,	get,
	 "Known problems").

initialise(C, M:method) :->
	"Initialise from method"::
	send(C, send_super, initialise, M?man_module, M?name, M?man_id).

object(C, Method:method) :<-
	"Get associated method"::
	get(C, identifier, Name),
	name(Name, [0'M, 0'. |S0]),
	append(S1, [0'.,T, 0'.|S2], S0),
	name(ClassName, S1),
	name(MethodName, S2),
	get(@classes, member, ClassName, Class),
	(   T == 0'S
	->  get(Class, send_method, MethodName, Method)
	;   get(Class, get_method, MethodName, Method)
	).

:- pce_end_class.


:- pce_begin_class(man_class_variable_card(identifier), man_card,
		   "Manual card of a class variable").

variable(defaults,	string*,	get,
	 "Default value").

initialise(C, R:class_variable) :->
	"Initialise from resource"::
	send(C, send_super, initialise, R?man_module, R?name, R?man_id).

object(C, R:class_variable) :<-
	"Get associated resource"::
	get(C, identifier, Name),
	name(Name, [0'R, 0'. |S0]),
	append(S1, [0'.|S2], S0),
	name(ClassName, S1),
	name(ResName, S2),
	get(@classes, member, ClassName, Class),
	get(Class, class_variable, ResName, R).

:- pce_end_class.


:- pce_begin_class(man_resource_card, man_class_variable_card,
		   "Backward compatibility handling").
:- pce_end_class.


:- pce_begin_class(man_error_card(identifier), man_card,
		   "Manual card of an error").


initialise(C, E:error) :->
	"Initialise from method"::
	send(C, send_super, initialise, E?man_module, E?id, E?man_id).


object(C, Error:error) :<-
	"Get associated error"::
	get(C, identifier, ManId),
	concat('!.', ErrId, ManId),
	get(@pce, convert, ErrId, error, Error).

:- pce_end_class.


:- pce_begin_class(man_group_card(name), man_card,
		   "Describe functional group of methods").

variable(index, int, get, "Index to preserve the order").

initialise(G, Module:man_module, Name:name, Idx:int, Summary:[string]) :->
	"Initialise from group name"::
	send(G, send_super, initialise, Module, Name, Name),
	(   Summary \== @default
	->  send(G, store, summary, Summary)
	;   true
	),
	send(G, store, index, Idx).


object(G, Name:name) :<-
	"Get associated group name"::
	get(G, name, Name).

:- pce_end_class.


		/********************************
		*      OTHER MANUAL CARDS	*
		********************************/

:- pce_begin_class(man_topic_card(name), man_card,
		   "Hierarchical organisation on topics").

variable(super,		chain*,		get, "Super topic(s)").
variable(subs,		chain*,		get, "Sub topics").

man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = 'T'.

:- pce_end_class.


:- pce_begin_class(man_object_card(name), man_card,
		   "Description of global PCE object").

initialise(C, G:man_global) :->
	"Initialise from global object holder"::
	send(C, send_super, initialise, G?man_module, G?name, G?man_id).


man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = 'O'.


object(C, O:man_global) :<-
	"Get associated global object"::
	get(C, identifier, Name),
	concat('O.', Reference, Name),
	new(O, man_global(Reference)).

delete_unreferenced(C) :->
	get(C, identifier, Name),
	(   concat('O.', Reference, Name),
	    object(@Reference)
	->  true
	;   format(user_error, 'Deleting card ~w~n', [Name]),
	    free(C)
	).

:- pce_end_class.

:- pce_begin_class(man_predicate_card(name), man_card,
		   "Description of a Prolog predicate").

variable(diagnostics,	string*,	both,
	 "Possible error conditions/messages").

initialise(Card, M:man_module, Name:name) :->
	"Define id to be the predicate name"::
	send(Card, slot, name, Name),
	get(Card, predicate_name, Id),
	send(Card, send_super, initialise, M, Name, Id).


store(Card, Slot:name, Value:any) :->
	"Change id if name changes"::
	send(Card, send_super, store, Slot, Value),
	(   Slot == name
	->  get(Card, predicate_name, Id),
	    send(Card, identifier, Id)
	;   true
	).


man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = 'P'.

predicate_name(Card, PredName:name) :<-
	get(Card, name, Name),
	new(R, regex('\(\w+\)')),
	send(R, search, Name),
	get(R, register_value, Name, 1, name, PredName).

:- pce_end_class.

:- pce_begin_class(man_example_card(name), man_card,
		   "Example code").

initialise(Card, M:man_module, Name:name) :->
	"Define id to be the predicate name"::
	send(Card, slot, name, Name),
	get(Card, id, Id),
	send(Card, send_super, initialise, M, Name, Id),
	send(Card, store, description, 'Enter description here'),
	send(Card, store, code, 'Enter code here').


store(Card, Slot:name, Value:any) :->
	"Change id if name changes"::
	send(Card, send_super, store, Slot, Value),
	(   Slot == name
	->  get(Card, id, Id),
	    send(Card, identifier, Id)
	;   true
	).


man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = 'E'.

variable(code,		string*,	both,
	 "Source code of example").

id(Card, Id) :<-
	get(Card, name, Name),
	new(S, string('%s', Name)),
	send(S, downcase),
	send(S, translate, ' ', '_'),
	get(S, value, Id).

:- pce_end_class.

:- pce_begin_class(man_browser_card(name), man_card,
		   "Documentation of a Manual Browser").

variable(tool_name,		name*,		both,
	 "Name of the tool documented").
variable(user_interface,	string*,	get,
	 "Description of UI behaviour").
variable(bugs,			string*,	get,
	 "Known problems").

man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = 'B'.

:- pce_end_class.

:- pce_begin_class(man_change_card(name), man_card,
		   "Documentation of a change to PCE").

man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = '~'.

:- pce_end_class.


:- pce_begin_class(man_bug_card(name), man_card,
		   "Documentation of a bug fix to PCE").

man_id(_Card, Id) :<-
	"Identifier of card type"::
	Id = '+'.

:- pce_end_class.


		/********************************
		*           MAN_GLOBAL		*
		********************************/

:- pce_global(@man_globals, new(hash_table)).

:- pce_begin_class(man_global(reference), object).

variable(reference,	name,	 get,	"Reference name of object").
variable(man_summary,	string,  get,	"Summary string (if available)").

initialise(G, Name:name, Summary:[string]*) :->
	"Create from name"::
	send(G, slot, reference, Name),
	(   (Summary == @default ; Summary == @nil)
	->  object_summary(Name, S)
	;   S = Summary
	),
	class_name(@Name, ClassName),
	send(G, slot, man_summary,
	     string('O\t@%s/%s\t%s', Name, ClassName, S)),
	send(@man_globals, append, Name, G).

lookup(_, Name:name, G) :<-
	"Lookup existing one"::
	get(@man_globals, member, Name, G).
			   

group(G, Group:name) :<-
	"Group (class name)"::
	get(G, reference, Reference),
	get(@Reference, '_class_name', Group).


summary(_G, _:string) :<-
	fail.

class_name(Ref, ClassName) :-
	object(Ref), !,
	get(Ref, '_class_name', ClassName).

object_summary(Name, Summary) :-
	object(@Name), !,
	(   get(@Name, '_class', Class),
	    get(Class, get_method, summary, _),
	    get(@Name, summary, Summary)
	->  true
	;   object(@Name, Term),
	    term_to_atom(Term, Summary)
	).


man_module(_G, Create:[bool], Module:man_module) :<-
	"objects module"::
	find_module(objects, Create, Module).

man_id(G, Id:name) :<-
	get('O.', append, G?reference, Id).


name(G, Name:name) :<-
	"@Reference"::
	get(G, reference, Reference),
	get(@, append, Reference, Name).


man_name(G, Name:string) :<-
	"Name for relation browser"::
	new(Name, string('O\t@%s', G?reference)).


man_card_class(_G, Class:class) :<-
	"Name for documentation card"::
	get(@pce, convert, man_object_card, class, Class).

context(G, Class:class) :<-
	"Return context class for jumping"::
	get(G, reference, Id),
	object(@Id),
	get(@Id, '_class', Class).

has_source(_G) :->
	"Just fail"::
	fail.

:- pce_end_class.


		/********************************
		*           EXTENSIONS		*
		********************************/

:- pce_extend_class(object).
:- pce_group(manual).

man_module_name(_Obj, Module) :<-
	"Module name for global objects"::
	Module = objects.


man_module(Obj, Create:[bool], Module) :<-
	"Module for global objects"::
	new(Space, man_space(reference)),
	get(Obj, man_module_name, ModuleName),
	(   get(Space, module, ModuleName, @on, Module)
	->  true
	;   Create == @on
	->  new(Module, man_module(Space, ModuleName))
	).


man_card(Obj, Create:[bool], Card) :<-
	"Manual card for object"::
	get(Obj, man_module, @on, Module),
	(   get(Module, card, Obj?man_id, Card)
	->  true
	;   Create == @on
	->  get(Obj, man_create_card, Card)
	).


man_documented(Obj) :->
	"Test if object is documented"::
	(   get(Obj, man_card, Card),
	    (   get(Card, description, Description), Description \== @nil
	    ;   get(Card, related, see_also, _)
	    )
	;   get(Obj, man_inherited_attribute, description, _)
	).


man_create_card(Obj, Card) :<-
	"Create manual card for object"::
	send(Obj, has_get_method, man_card_class),
	get(Obj?man_card_class, instance, Obj, Card).


man_attribute(Obj, Slot:name, Value:string*) :->
	"Store a slot of the manual card"::
	send(?(Obj, man_card, @on), store, Slot, Value).


man_attribute(Obj, Slot:name, Value) :<-
	"Fetch a manual attribute"::
	(   get(Obj, man_card, Card),
	    get(Card, fetch, Slot, Value)
	->  true
	;   send(Obj, has_get_method, Slot),
	    get(Obj, Slot, Value)
	),
	Value \== @nil.


man_inherited_attribute(Obj, Att:name, Tuple:tuple) :<-
	"Default inherited value"::
	(   get(Obj, man_inherit_object, Att, From),
	    get(From, man_attribute, Att, Value)
	->  new(Tuple, tuple(From, Value))
	;   get(Obj, man_card, Card),
	    get(Card, inherited_fetch, Att, Tuple)
	).


man_inherit_object(_Obj, _Att:name, _Obj2:object) :<-
	"Object from which to inherit attribute"::
	fail.


man_relate(Obj1, Type:name, Obj2:object) :->
	"Create a manual relation"::
	send(?(Obj1, man_card, @on), relate,
	     Type, ?(Obj2, man_card, @on)).


man_unrelate(Obj1, Type:name, Obj2:object) :->
	"Destroy a manual relation"::
	send(?(Obj1, man_card, @on), unrelate,
	     Type, ?(Obj2, man_card, @on)).


man_related(Obj1, Type:name, Obj2:object) :->
	"Create a manual relation"::
	send(?(Obj1, man_card), related, Type, Obj2?man_card).


man_related(Obj, Type:name, Chain) :<-
	"New chain with related objects"::
	get(?(?(Obj, man_card), related, Type), map,
	    new(?(@arg1, object)), Chain).


man_name(Obj, Name) :<-
	"Name for relation browser"::
	new(Name, string),
	send(Name, format, 'O\t@%s', Obj?object_reference).

man_creator(_Obj, _) :<-
	"Global default"::
	fail.

:- pce_end_class.

:- pce_extend_class(class).
:- pce_group(manual).

man_module_name(Class, Module) :<-
	"Manual module name for class"::
	get(Class, name, Name),
	(   mapped_class_name(Name, Mapped)
	;   Mapped = Name
	), !,
	concat('class/', Mapped, Module).


man_card_class(_Class, Class:class) :<-
	"Manual card type"::
	get(@pce, convert, man_class_card, class, Class).


man_name(Class, Name:string) :<-
	"Name for relation browser"::
	new(Name, string('C\t%s', Class?name)).
	

has_source(Class) :->
	"Test if object may have associated sources"::
	\+ get(Class, creator, built_in).


source(Class, Loc:source_location) :<-
	"Find souce location of class definition"::
	get(Class, slot, source, Loc), Loc \== @nil,
	get(Loc, line_no, LineNo), LineNo \== @nil,
	fix_source_path(Loc).

fix_source_path(Loc) :-
	get(Loc, file_name, Name),
	send(file(Name), exists, @on), !.
fix_source_path(Loc) :-
	(   pce_host:property(system_source_prefix(Prefix)),
	    atom_chars(Prefix, PrefixChars),
	    get(Loc, file_name, Name),
	    atom_chars(Name, Chars),
	    append(_, S1, Chars),
	    append(PrefixChars, PwLocalChars, S1)
	->  atom_chars(PwLocal, PwLocalChars),
	    absolute_file_name(pce(PwLocal),
			       [ access(read)
			       ],
			       Path),
	    send(Loc, slot, file_name, Path)
	).

man_header(Class, Str:string) :<-
	"Header for class browser"::
	get(Class, name, ClassName),
	new(Str, string('%s(', ClassName)),
	get(Class, send_method, initialise, IM),
	get(IM, types, Types),
	get(Class, term_names, Names),
	append_arguments(Types, Names, Str),
	send(Str, append, ')').

append_arguments(Types, Names, Str) :-
	between(1, 10000, Idx),
	(   get(Types, element, Idx, Type)
	->  (Idx \== 1 -> send(Str, append, ', ') ; true),
	    get(Type, name, TypeName),
	    (   get(Type, argument_name, ArgName),
		ArgName \== @nil,
		ArgName \== TypeName
	    ->	send(Str, append, string('%s=%s', ArgName, TypeName))
	    ;   Names \== @nil,
	        get(Names, element, Idx, ArgName)
	    ->  send(Str, append, string('%s=%s', ArgName, TypeName))
	    ;   send(Str, append, TypeName)
	    ),
	    fail
	;   !
	).

man_delegate_header(Class, Str:string) :<-
	"Description of delegation behaviour"::
	new(Str, string),
	(   get(Class, delegate, Chain),
	    Chain \== @nil,
	    \+ send(Chain, empty)
	->  send(Chain, for_all,
		 and(if(Chain?head \== @arg1,
			message(Str, append, ', ')),
		     message(Str, append,
			     create(string, '%s (%s)',
				    @arg1?name, @arg1?type?name))))
	;   true
	).


man_creator(Class, Creator:name) :<-
	"Creator used by manual filters"::
	get(Class, creator, Creator).

:- pce_end_class.

:- pce_extend_class(variable).
:- pce_group(manual).

man_module_name(Var, Module) :<-
	"Manual module name for variable"::
	get(Var?context, man_module_name, Module).

man_card_class(_Var, Class:class) :<-
	"Manual card type"::
	get(@pce, convert, man_variable_card, class, Class).

man_name(Var, ManName:string) :<-
	"Name for relation browser"::
	get(Var, context_name, ClassName),
	get(Var, access_arrow, Arrow),
	get(Var, name, Name),
	new(ManName, string('V\t%s %s%s', ClassName, Arrow, Name)).
	

man_header(Var, Header:string) :<-
	"Header for card viewer"::
	get(Var, context_name, ClassName),
	get(Var, access_arrow, Arrow),
	get(Var, name, Name),
	get(Var, type, Type),
	get(Type, name, TypeName),
	new(Header, string('V\t%s %s%s: %s',
			   ClassName, Arrow, Name, TypeName)).

has_source(Var) :->
	"Test if object may have associated sources"::
	send(Var?context, has_source).

source(Var, Src) :<-
	"Find source (same as related class"::
	get(Var, context, Class), Class \== @nil,
	get(Class, source, Src).

man_inherit_object(Var, Att:name, R:class_variable) :<-
	"Lookup default in class-variable"::
	Att == defaults,
	get(Var?context, class_variable, Var?name, R).

man_creator(Var, Creator:name) :<-
	"<-creator of the <-context"::
	get(Var?context, creator, Creator).

:- pce_end_class.

super_class(Class, Super) :-
	get(Class, super_class, Super), Super \== @nil.
super_class(Class, Super) :-
	get(Class, super_class, Above), Above \== @nil,
	super_class(Above, Super).

:- pce_extend_class(method).
:- pce_group(manual).

man_module_name(M, Module) :<-
	"Manual module name for method"::
	get(M?context, man_module_name, Module).


man_card_class(_M, Class:class) :<-
	"Manual card type"::
	get(@pce, convert, man_method_card, class, Class).


has_source(M) :->
	"Test if object may have associated sources"::
	get(M, slot, source, Loc), Loc \== @nil,
	get(Loc, line_no, LineNo), LineNo \== @nil.


source(M, Loc) :<-
	"Find source definition"::
	get(M, slot, source, Loc), Loc \== @nil,
	get(Loc, line_no, LineNo), LineNo \== @nil,
	fix_source_path(Loc).


man_documented(M) :->
	"Look for inherited too"::
	(   send(M, send_super, man_documented)
	->  true
	;   get(M, context, Class),
	    get(M, name, Selector),
	    get(Class, instance_variable, Selector, Var),
	    send(Var, man_documented)
	).


man_creator(M, Creator:name) :<-
	"<-creator of the <-context"::
	get(M?context, creator, Creator).

:- pce_end_class.

:- pce_extend_class(error).
:- pce_group(manual).

man_module_name(_E, Module:name) :<-
	"Manual module name for method"::
	Module = errors.

summary(E, Summary:string) :<-
	get(E, format, Summary).

name(E, Name:name) :<-
	get(E, id, Name).

man_summary(E, Summary:string) :<-
	"Summary string"::
	new(Summary, string('!\t%s\t%s\t%s',
			    E?id, E?kind, E?format)),
	send(Summary, translate, '\n', ' '),
	(   send(E, man_documented)
	->  send(Summary, append, ' (+)')
	;   true
	).

man_card_class(_E, Class:class) :<-
	"Manual card type"::
	get(@pce, convert, man_error_card, class, Class).

man_id(E, Id:name) :<-
	"Identifier of object"::
	get(E, id, ErrId),
	get('!.', append, ErrId, Id).

man_name(E, Name:name) :<-
	"Name for relation browser"::
	get(E, id, ErrId),
	get('! ', append, ErrId, Name).

man_creator(_E, Creator:name) :<-
	"For now, always returns built_in"::
	Creator = built_in.

:- pce_end_class.

%	Type pretty printing

method_types(M, Str) :-
	get(M, types, Types),
	get(Types, size, Size),
	(   Size > 0
	->  send(Str, append, ': ')
	;   true
	),
	between(1, Size, Arg),
	    get(Types, element, Arg, Type),
	    get(Type, fullname, Name),
	    send(Str, append, Name),
	    (	Arg < Size
	    ->	send(Str, append, ', ')
	    ;	true
	    ),
	fail ; true.


:- pce_extend_class(send_method).
:- pce_group(manual).

man_name(M, Name) :<-
	"Name for relation browser"::
	new(Name, string('M\t%s->%s', M?context?name, M?name)).
	

man_header(M, Header:string) :<-
	"Header for card browser"::
	get(M, context, Ctx),
	get(Ctx, name, ClassName),
	get(M, name, Name),
	new(Header, string('M\t%s->%s', ClassName, Name)),
	method_types(M, Header).


man_inherit_object(M, Att:name, Impl:behaviour) :<-
	"Inherit from variable if not available"::
	get(M, context, Class),
	get(M, name, Selector),
	(   get(Class, instance_variable, Selector, Impl)
	->  true
	;   super_class(Class, Super),
	    get(Super, send_method, Selector, Impl),
	    (   (  get(Impl, man_attribute, Att, _)
		;  \+ super_class(Super, _)
		)
	    ->  !
	    )
	).

:- pce_end_class.

:- pce_extend_class(get_method).
:- pce_group(manual).

man_name(M, Name) :<-
	"Name for relation browser"::
	new(Name, string),
	send(Name, format, 'M\t%s<-%s', M?context?name, M?name).
	
man_header(M, Header:string) :<-
	"Header for card browser"::
	get(M, context, Ctx),
	get(Ctx, name, ClassName),
	get(M, name, Name),
	new(Header, string('M\t%s<-%s', ClassName, Name)),
	method_types(M, Header),
	send(Header, append, ' ==>'),
	get(M, return_type, Type),
	get(Type, fullname, TypeName),
	send(Header, append, TypeName).

man_inherit_object(M, Att:name, Impl:'variable|method') :<-
	"Inherit from variable if not available"::
	get(M, context, Class),
	get(M, name, Selector),
	(   get(Class, instance_variable, Selector, Impl)
	->  true
	;   super_class(Class, Super),
	    get(Super, get_method, Selector, Impl),
	    (   (  get(Impl, man_attribute, Att, _)
		;  \+ super_class(Super, _)
		)
	    ->  !
	    )
	).


:- pce_end_class.

:- pce_extend_class(class_variable).
:- pce_group(manual).

man_module_name(R, Module) :<-
	"Manual module name for method"::
	get(R?context, man_module_name, Module).


man_card_class(_R, Class:class) :<-
	"Manual card type"::
	get(@pce, convert, man_class_variable_card, class, Class).


man_name(R, Name) :<-
	"Name for relation browser"::
	get(R, value, Value),
	portray_object(Value, Term),
	term_to_atom(Term, ValueDescription),
	new(Name, string('R\t%s.%s: %s',
			 R?context?name, R?name, ValueDescription)).
	

has_source(_R) :->
	"Test if object may have associated sources"::
	true.


man_attribute(R, Att:name, Value) :<-
	"Get default value of class variable"::
	(   Att == defaults
	->  get(R, default, Value)
	;   get(R, get_super, man_attribute, Att, Value)
	).


man_inherited_attribute(R, Att:name, Tuple:tuple) :<-
	"Inherit description from variable"::
	Att == description,
	get(R, context, Class),
	get(R, name, Selector),
	get(Class, instance_variable, Selector, Var),
	get(Var, man_attribute, Att, Value),
	new(Tuple, tuple(Var, Value)).


source(R, Src) :<-
	"Find source (same as related class"::
	get(R, context, Class),
	get(Class, source, Src).


man_creator(R, Creator:name) :<-
	"<-creator of the <-context"::
	get(R?context, creator, Creator).

:- pce_end_class.

