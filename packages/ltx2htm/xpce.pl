/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(latex2html4xpce, []).
:- use_module(library(latex2html)).

:- latex2html_module.
:- tex_load_commands(xpce).

%	XPCE <-> ProWindows switch

:- dynamic
	pwtrue/0.				% ProWindows 3.1

cmd(makepw, _, []) :-
	assert(pwtrue).
cmd(ifpw({If}, {Else}), Mode, HTML) :-
	(   pwtrue
	->  translate(If, Mode, HTML)
	;   translate(Else, Mode, HTML)
	).

env(pwonly(_, Tokens), HTML) :-
	(   pwtrue
	->  translate(Tokens, normal, HTML)
	;   HTML = []
	).
env(xpceonly(_, Tokens), HTML) :-
	(   pwtrue
	->  HTML = []
	;   translate(Tokens, normal, HTML)
	).

%cmd(product, 'ProWindows') :- pwtrue.
%cmd(product, 'XPCE').
%cmd(productpl, 'ProWindows') :- pwtrue.
%cmd(productpl, 'XPCE/Prolog').
%cmd(productversion, '3.1') :- pwtrue.
%cmd(productversion, '4.9.3').		% dynamic!

cmd(objectname({Name}),		#b([nospace(@), Name])).
cmd(noclass({Name}),		#b(Name)).
cmd(class({Name}),		#lref(Label, Name)) :-
	concat('class:', Name, Label),
	add_to_index(Name).
cmd(classs({Name}),		#lref(Label, NameS)) :-
	concat('class:', Name, Label),
	concat(Name, s, NameS),
	add_to_index(Name).
cmd(tool({Name}),		#strong(+Name)).
cmd(demo({Name}),		#strong(+Name)).
cmd(type({Name}),		#b([#code(+Name)])).
cmd(send({Name}),		#b([#code(nospace(->)), Name])).
cmd(get({Name}),		#b([#code(nospace(<-)), Name])).
cmd(both({Name}),		#b([#code(nospace(<->)), Name])).
cmd(classsend({Class}, {Name}),	#b([+Class, #code(nospace(->)), +Name])).
cmd(classget({Class}, {Name}),	#b([+Class, #code(nospace(<-)), +Name])).
cmd(classboth({Class}, {Name}),	#b([+Class, #code(nospace(<->)), +Name])).
cmd(sendmethod(_M, {Class}, {Selector}, {Args}),
    #defitem([ #strong([Class, ' ', nospace('->'), Selector, nospace(':')]),
	       ' ', #var(+Args)
	     ])).
cmd(getmethod(_M, {Class}, {Selector}, {Args}),
    #defitem([ #strong([Class, ' ', nospace('<-'), Selector, nospace(':')]),
	       ' ', #var(+Args)
	     ])).
cmd(bothmethod(_M, {Class}, {Selector}, {Args}),
    #defitem([ #strong([Class, ' ', nospace('<->'), Selector, nospace(':')]),
	       ' ', #var(+Args)
	     ])).
cmd(manualtool({Descr}, {Menu}),
    #defitem([ #strong(+Descr), ' ', #i(#embrace(+Menu))])).
cmd(secoverview({Label}, {Title}),
    [ html('<LI>'), #lref(RefName, +Title) ]) :-
	sformat(RefName, 'sec:~w', Label).
cmd(classsummary(_M, {RawClass}, {Args}, {_FigRef}),
    #defitem(#label(Label, [#strong(Class), #embrace(#var(+Args))]))) :-
	clean_tt(RawClass, Class),
	concat('class:', Class, Label),
	add_to_index(Class, +Label).
cmd(fontalias({Alias}, {Term}), #defitem([#code(Alias), #i(+Term)])).
cmd(noargpredicate(Name), HTML) :-
	cmd(predicate(Name, {'0'}, {[]}), HTML).
%cmd(idx({Term}), nospace(Term)) :-	% If only index to section is wanted
%	add_to_index(Term).
cmd(glossitem({Term}), #defitem(#label(RefName, #strong(Term)))) :-
	canonise_glossitem(Term, Ref),
	sformat(RefName, 'gloss:~w', [Ref]).
cmd(g({Term}),	#lref(RefName, Term)) :-
	canonise_glossitem(Term, Ref),
	sformat(RefName, 'gloss:~w', [Ref]).
cmd(line({Tokens}), #quote(Line)) :-
	translate(Tokens, normal, Line).
cmd(classvar({Class}, {Var}), #b([#code([+Class,nospace('.'),+Var])])).
cmd(tab, #code(verb('	'))).
cmd(opt({Arg}), #embrace("[]", +Arg)).
cmd(zom({Arg}), #embrace("{}", +Arg)).
cmd(fnm({Mark}), +Mark).
cmd(hr, html('<HR>')).
cmd(nameof({Names}), #embrace("{}", #code(Names))).

cmd(setupfancyplain, []).

env(tabularlp(_, Tokens), HTML) :-
	translate_table('|l|p{3in}|', Tokens, HTML).	      
	      
canonise_glossitem(In, Out) :-
	downcase_atom(In, In1),
	atom_chars(In1, Chars0),
	(   append(Chars1, "s", Chars0)
	->  true
	;   Chars1 = Chars0
	),
	maplist(canonical_char, Chars1, Chars2),
	atom_chars(Out, Chars2).

canonical_char(0' , 0'-) :- !.
canonical_char(0'_, 0'-) :- !.
canonical_char(X, X).

