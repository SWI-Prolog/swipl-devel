/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(man_word_index,
	  [ pce_make_manual_index/1	% +File
	  , pce_make_manual_index/2	% +File, -Reference
	  ]).
:- use_module(library(pce)).
:- require([ concat_atom/2,
	     can_open_file/2
	   ]).

:- pce_begin_class(man_index_manager, object,
		   "Dummy object to exploit XPCE autoloader").

make_index(_IM, IndexFile:name, Index:chain_table) :<-
	pce_make_manual_index(IndexFile, Index).

:- pce_end_class.


%	make_manual_index(+File, [-Reference])
%
%	Creates a word index for the manual system.  This index is a
%	chain_table mapping all words that appear in the manual onto
%	a chain of card identifiers.
%
%	At the moment only all class-related cards (classes, variables,
%	methods and resources are processed.


pce_ifhostproperty(prolog(quintus),
(   access_file(File, Mode) :-
	can_open_file(File, Mode))).

pce_make_manual_index(File, @man_tmp_index) :-
	pce_make_manual_index(File).
pce_make_manual_index(File) :-
	access_file(File, write), !,	% just make sure!
	new(Ch, chain),			% gather all built-in classes
	send(@man_tmp_view, format, 'Collecting built-in classes ... '),
	send(@man_tmp_view, flush),
	send(@classes, for_all,
	     if(@arg2?creator == built_in,
		message(Ch, append, @arg2))),
	send(Ch, sort, ?(@arg1?name, compare, @arg2?name)),
	send(@man_tmp_view, format, '%d classes.\n', Ch?size),
	send(@man_tmp_view, flush),
	send(Ch, for_all, message(@prolog, make_class_index, @arg1)),
	make_module_index,
	send(@man_tmp_view, format, '\n\nCleaning index table'),
	send(@man_tmp_view, flush),
	clean_index(@man_tmp_index),
	send(@man_tmp_view, format, 'Cleaning done\n\n'),
	send(@man_tmp_view, format, 'Saving index table to %s ... ', File),
	send(@man_tmp_view, flush),
	send(@man_tmp_index, save_in_file, File),
	send(@man_tmp_view, format, '%d bytes. Finished.\n', file(File)?size),
	send(@man_tmp_view, flush).
make_manual_index(File) :-
	send(file(File), report, error, 'Cannot write %s', File),
	fail.

make_class_index(Class) :-
	get(Class, name, Name),
	(   get(@man_tmp_view, column, C),
	    C > 64
	->  send(@man_tmp_view, newline)
	;   true
	),
	send(@man_tmp_view, format, '(%s', Name),
	send(@man_tmp_view, flush),
	new(Objs, chain),
	send(Objs, merge, Class?send_methods),
	send(Objs, merge, Class?get_methods),
	send(Objs, for_all,
	     if(message(@arg1?message, instance_of, c_pointer),
		message(Objs, delete, @arg1))),
	send(Class?instance_variables, for_all,
	     message(Objs, append, @arg1)),
	send(Objs, merge, Class?resources),
	send(Objs, append, Class),
	send(Objs, for_all,
	     message(@prolog, make_card_index, @arg1)),
	send(Objs, done),
	send(@man_tmp_view, format, ') ', Name),
	send(@man_tmp_view, flush).

make_card_index(Card) :-
	get(Card, man_id, Id),
	(   get(Card, man_summary, Summary)
	->  word_index(Summary, Id)
	;   true
	),
	(   get(Card, man_description, Description)
	->  word_index(Description, Id)
	;   true
	).


:- pce_global(@man_tmp_view, make_man_tmp_view).
:- pce_global(@man_tmp_index, new(chain_table)).
:- pce_global(@man_tmp_index_regex, make_index_regex).

make_index_regex(R) :-
	new(R, regex('@?[a-zA-Z]\w+')),
	send(R, compile, @on).

make_man_tmp_view(V) :-
	new(V, view('Build PCE manual index')),
	send(new(D, dialog), below, V),
	send(D, append, button(quit, message(D, destroy))),
	send(D, append, label(reporter), right),
	send(V, open),
	send(V, wait),
	send(V, format, 'Building PCE manual index.\n'),
	send(V, format, 'This process will take several minutes.\n\n').

word_index(Text, Id) :-
	send(@man_tmp_index_regex, for_all, Text,
	     message(@man_tmp_index, append,
		     ?(@arg1, register_value, @arg2, 0, name)?downcase, Id)).

		 /*******************************
		 *	  NON-CLASS CARDS	*
		 *******************************/

non_class_module(predicates).
non_class_module(objects).
non_class_module(tools).

make_module_index :-
	send(@man_tmp_view, format, '\n\n'),
	non_class_module(Module),
	send(@man_tmp_view, format, '%s index ... ', Module),
	send(@man_tmp_view, flush),
	make_module_index(Module),
	send(@man_tmp_view, format, 'done.\n'),
	send(@man_tmp_view, flush),
	fail.
make_module_index.

make_module_index(ModuleName) :-
	get(@manual, space, ManSpace),
	get(ManSpace, module, ModuleName, @on, Module),
	send(Module?id_table, for_all,
	     message(@prolog, index_card, ModuleName, @arg1, @arg2)).


index_card(Module, CardId, Card) :-
	concat_atom([$, Module, $, CardId], Id),
	(   get(Card, man_summary, Summary)
	->  word_index(Summary, Id)
	;   true
	),
	(   get(Card, man_description, Description)
	->  word_index(Description, Id)
	;   true
	).


		 /*******************************
		 *	       CLEAN		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These words appear in more then 500 cards and probably have little value
in specifying search  options.   Deleting   them  significantly  reduces
memory and saves time with loading and processing the index.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

noindex(and).
noindex(will).
noindex(was).
noindex(using).
noindex(is).
noindex(or).
noindex(not).
noindex(the).
noindex(a).
noindex(an).
noindex(class).
noindex(object).
noindex(method).
noindex(that).
noindex(also).
noindex(see).
noindex(it).
noindex(to).
noindex(used).
noindex(when).
noindex(by).
noindex(of).
noindex(with).
noindex(from).
noindex(if).
noindex(in).
noindex(are).
noindex(be).
noindex(this).

clean_index(Table) :-
	noindex(Word),
	get(Table, member, Word, Chain),
	get(Chain, size, Size),
	send(@man_tmp_view, format, 'Deleted "%s", %d entries\n', Word, Size),
	send(@man_tmp_view, flush),
	send(Table, delete, Word),
	fail.
clean_index(Table) :-
	send(Table, for_all, message(@arg2, unique)).
