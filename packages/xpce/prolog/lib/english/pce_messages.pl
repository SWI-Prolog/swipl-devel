/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Author: Jim Crammond, Quintus

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
    Copyright (C) 1995 Quintus Co.
*/

:- module(pce_messages,
	  [ pce_message/3,
	    pce_message_context/3
	  ]).
:- require([ append/3
	   ]).

					% Messages from the interface
pce_message(pce(bad_object_description, Culprit)) -->
	['Illegal object description: `~w'''-[Culprit], nl].
pce_message(pce(bad_selector, Culprit)) -->
	['Illegal selector: `~w'''-[Culprit], nl].
pce_message(pce(bad_reference, Culprit)) -->
	['Illegal reference: `~w'''-[Culprit], nl].
pce_message(pce(bad_integer_reference, Culprit)) -->
	['Invalid integer reference: `~w'''-[Culprit], nl].
pce_message(pce(bad_string_argument, Culprit)) -->
	['Cannot create string from: `~w'''-[Culprit], nl].
pce_message(pce(unknown_reference, Culprit)) -->
	['Unknown object reference: `@~w'''-[Culprit], nl].
pce_message(pce(open_qeury)) -->
	['Internal interface error: open_query() failed'-[], nl].
pce_message(pce(inconsistent_argc)) -->
	['Internal interface error: inconsistent argument count'-[], nl].
pce_message(pce(no_predicate_reference)) -->
	['Internal interface error: not a predicate reference'-[], nl].

pce_message(help_goal(Goal)) -->
	[nl,'For HELP, please invoke the predicate `~w''.'-[Goal],nl,nl].
pce_message(pce_host_send_or_get) -->
	['PCE/Prolog Error in Send or Get'-[],nl].

pce_message(loading(Goal)) -->
	{ functor(Goal, Name, Arity) },
	['Autoloading ~w/~d.  Please wait ...'-[Name, Arity], nl].

pce_message(builtin_class_not_redefined(ClassName)) -->
	['Cannot redefine built-in class: ~w'-[ClassName],nl].
pce_message(superclass_not_changed(ClassName)) -->
	['Cannot change super-class of class ~w'-[ClassName],nl].
pce_message(superclass_not_exist(Super,Class)) -->
	['Superclass ~w of ~w does not exist'-[Super, Class],nl].
pce_message(expand_failed(Term)) -->
	['Failed to expand ~w'-[Term],nl].
pce_message(instantiation_error(Goal)) -->
	['Instantiation error in ~w'-[Goal]].
pce_message(summary_not_closed(Text)) -->
	['Class summary "~s" not closed by `::'''-[Text],nl].
pce_message(get_resource_failed(Name, Obj)) -->
	['Failed to get resource ~p of ~p'-[Name,Obj],nl]. 

pce_message(loading_class(ClassName)) -->
	['Loading PCE class ~w'-[ClassName],nl].
pce_message(reloading_class(ClassName)) -->
	['Reloading PCE class ~w'-[ClassName],nl].
pce_message(extending_class(ClassName)) -->
	['Extending PCE class ~w'-[ClassName],nl].
pce_message(loaded_class(ClassName)) -->
	['PCE class ~w loaded'-[ClassName],nl].
pce_message(expand_send(ClassName,Selector)) -->
	['~t~8|~w :->~w ... ok'-[ClassName,Selector],nl].
pce_message(expand_get(ClassName,Selector)) -->
	['~t~8|~w :<-~w ... ok'-[ClassName,Selector],nl].

pce_message(renamed_reference(Ref,NewRef)) -->
	[':- pce_global: Renamed @~w into @~w'-[Ref,NewRef],nl].
pce_message(object_already_defined(Ref,Mod)) -->
	['Global object @~w already defined in module ~w'-[Ref,Mod],nl].
pce_message(create_failed(Term)) -->
	[':- pce_global/2: create failed: ~w'-[Term],nl].
pce_message(goal_failed(Goal)) -->
	[':- pce_global/2: goal failed: ~w'-[Goal],nl].


pce_message(index_not_found(File)) -->
	['library index ~w not found'-[File],nl].
pce_message(required_predicate_not_found(Name,Arity)) -->
	['required predicate ~w/~d not found'-[Name,Arity],nl].
pce_message(illegal_term_in_index(Term,Index)) -->
	['illegal term ~w in index file ~w'-[Term,Index],nl].
pce_message(loaded_library_index(File)) -->
	['Loaded require index ~w'-[File],nl].

pce_message(no_pw3_predicate(P/N)) -->
	['XPCE/Prolog predicate ~w/~d not part of ProWindows'-[P,N],nl].

pce_message(preformatted(Fmt, Args)) -->
	[Fmt-Args, nl].

pce_message_context(noclass) --> ['outside any class definition'-[]].
pce_message_context(nomethod) --> ['to be an incomplete method'-[]].
