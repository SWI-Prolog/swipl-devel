/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(asciman,
	  [ card_to_asci/1		% ManCard --> @out
	  , module_to_asci/1		% Cards of module --> @out
	  , space_to_asci/0		% Entire module space --> @out
	  , update_cards/1		% Reload file
	  , spell_module/1		% (I)Spell a module
	  , spell_manual/0
	  ]).

:- use_module(library(re_parse)).
:- use_module(library('demo/ispell')).

		 /*******************************
		 *	       SPACE		*
		 *******************************/

space_to_asci :-
	get(@manual, space, Space),
	send(Space, load_all_modules),
	get(Space, modules, HashTable),
	new(Modules, chain),
	send(HashTable, for_all, message(Modules, append, @arg1)),
	send(Modules, sort),
	send(Modules, for_all,
	     message(@prolog, module_to_asci, @arg1)).


		 /*******************************
		 *	      MODULES		*
		 *******************************/

module_to_asci(ModuleName) :-
	get(@manual, space, Space),
	get(Space, module, ModuleName, @on, Module),
	get(Module, id_table, HashTable),
	new(Members, chain),
	send(HashTable, for_all, message(Members, append, @arg2)),
%	send(Members, sort,
%	     ?(@arg1?identifier, compare, @arg2?identifier)),
	output('#module %s\n\n', ModuleName),
	send(Members, for_all,
	     message(@prolog, card_to_asci, @arg1)),
	output('#end module\n\n').


		 /*******************************
		 *		CARDS		*
		 *******************************/

card_to_asci(Card) :-
	get(Card, identifier, Id),
	output('#card %s\n\n', Id),
	attribute(Card, summary),
	attribute(Card, description),
	output('#end card\n\n').

attribute(Card, Attribute) :-
	get(Card, Attribute, Value),
	(   Value == @nil
	->  true
	;   output('#attribute %s\n%s\n#end attribute\n', [Attribute, Value])
	).


		 /*******************************
		 *	   OUTPUT SECTION	*
		 *******************************/

output(Format) :-
	output(Format, []).
output(Format, Arg) :-
	\+ is_list(Arg), !,
	output(Format, [Arg]).
output('%s\n', [String]) :- !,	% HACK to avoid crash on long description
	send(@out, append, String),
	send(@out, format, '\n').
output(Format, Args) :-
	Term =.. [send, @out, format, Format | Args],
	Term.


:- pce_global(@out, make_output).

make_output(Out) :-
	new(Out, view('Output')),
	send(new(D, dialog), below, Out),
	send(D, append, new(label)),
	send(D, append, button(quit, message(Out, destroy))),
	send(D, append, button(clear, message(Out, clear))),
	send(D, append,
	     file_item('Save in file', '',
		       message(Out, save, @arg1)),
	     right),
	send(Out, open).


		 /*******************************
		 *	     RELOADING		*
		 *******************************/

update_cards(File) :-
	free(@modified),
	new(@modified, number(0)),
	new(F, file(File)),
	send(F, open, read),
	start(''),
	re_parse_loop(F, Re, A,
		      module_pattern(F, Re, A),
		      @nil),
	send(F, close),
	end,
	send(@pce, format, '\nModified %d attributes\n', @modified),
	free(@modified).


module_pattern(_, '\\s *$', true).		% empty lines
module_pattern(F,
	       '#module\\s +\\(.*\\)',
	       do_module(F, 1:name)).

do_module(F, Name) :-
	get(@manual, space, Space),
	get(Space, module, Name, @on, Module),
	start(Name),
	re_parse_loop(F, Re, A,
		      card_pattern(Module, F, Re, A),
		      '#end module'),
	end.

card_pattern(_, _, '\\s *$', true).
card_pattern(Module, F,
	     '#card\\s +\\(.*\\)',
	     do_card(F, Module, 1:name)).

do_card(F, Module, Id) :-
	name(Id, S),
	name(Rid, S),
	get(Module, card, Rid, Card),
	re_parse_loop(F, Re, A,
		      attribute_pattern(Card, F, Re, A),
		      '#end card').

attribute_pattern(_, _, '\\s *$', true).
attribute_pattern(Card, F,
		  '#attribute\\s *\\(.*\\)',
		  do_attribute(F, Card, 1:name)).

:- pce_global(@newline, new(string('\n'))).

do_attribute(F, Card, Name) :-
	new(S, string),
	re_parse_loop(F, Re, A,
		      string_pattern(S, Re, A),
		      '#end attribute'),
	send(S, strip),
	get(Card, Name, OldValue),
	(   send(S, equal, OldValue)
	->  true
	;   %   get(Card?object, man_name, CardName),
	    %   send(@pce, format, 'Modified %s of %s\n', Name, CardName),
	    %   send(@pce, format, '%s===================%s', OldValue, S),
	    send(Card, store, Name, S),
	    send(@modified, plus, 1)
	).

string_pattern(S, '.*\n', send(S, append, 0)).

		 /*******************************
		 *	FEEDBACK PROGRESS	*
		 *******************************/

start(Id) :-
	format('(~w', Id), flush.
end :-
	format(') '), flush.


		 /*******************************
		 *	   SPELL TOPLEVEL	*
		 *******************************/

spell(Goal) :-
	FileName = 'xpce-manual-spell-buffer',

	free(@out),
	new(@out, file(FileName)),
	send(@out, open, write),
	Goal,
	send(@out, close),
	new(Ispell, ispell(FileName)),
	send(Ispell, save_action,
	     and(message(Ispell, save),
		 message(@prolog, update_cards, FileName)),
	     reload_cards),
	send(Ispell, open),
	send(Ispell, spell).

spell_module(ModuleName) :-
	spell(module_to_asci(ModuleName)).

spell_manual :-
	spell(space_to_asci).
