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

:- module(pce_messages,
	  [ pce_message/3,
	    pce_message_context/3
	  ]).
:- require([ append/3
	   , get/3
	   ]).

:- multifile
	pce_message/3.

pce_message(error(pce(ErrorId, Args), _Context)) -->
	{ Msg =.. [format|Args],
	  get(error(ErrorId), Msg, String),
	  get(String, value, Text)
	},
	[Text].
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
	['Unknown object reference: `~w'''-[Culprit], nl].
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
pce_message(get_class_variable_failed(Name, Obj)) -->
	['Failed to get class_variable ~p of ~p'-[Name,Obj],nl]. 

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
pce_message(recursive_loading_class(ClassName)) -->
	['Trying to load class ~w recursively'-[ClassName], nl].
pce_message(end_class_mismatch(Spec, Current)) -->
	['Class ~w is ended by :- pce_end_class(~w)'-[Current,Spec], nl].
pce_message(no_class_to_end) -->
	['There is no class to end here', nl].

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

pce_message(pce_trace(Port, Goal)) -->
	['XPCE ~w: ~p'-[Port, pce_principal:Goal],nl].

pce_message(preformatted(Fmt, Args)) -->
	[Fmt-Args, nl].

%	Resource --> class-variable compatibility messages

pce_message(compatibility(resource)) -->
	['Class resource has been renamed to class_variable'-[],nl]. 
pce_message(compatibility(resource(Value, NewVal))) -->
	['Converted class-variable value:'-[], nl,
	 '    From: ~q'-[Value], nl,
	 '    Into: ~q'-[NewVal], nl
	].

pce_message(pce(no_threads)) -->
	[ 'This version of XPCE does not support multi-threading'
	].
pce_message(pce(no_help(What))) -->
	[ 'No XPCE help on ~w'-[What]
	].

		 /*******************************
		 *	     CONTEXT		*
		 *******************************/

pce_message_context(noclass)     --> ['outside any class definition'-[]].
pce_message_context(nomethod)    --> ['to be an incomplete method'-[]].
pce_message_context(nodirective) --> ['can only be used as directive'-[]].
pce_message_context(nosuper)     --> ['can only be used in a method'-[]].
