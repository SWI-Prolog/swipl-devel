/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(latex2html4pl, []).
:- use_module(library(latex2html)).

:- latex2html_module.
:- tex_load_commands(pl).

		 /*******************************
		 *	       MACROS		*
		 *******************************/

#(defitem(Label),	[html('<P>'), html('<DT>'), Label, html('<DD>')]).

		 /*******************************
		 *	    INDEX HACK		*
		 *******************************/

cmd(+, index, nospace('\+')).
cmd(=, index, nospace('\=')).


		 /*******************************
		 *	     COMMANDS		*
		 *******************************/

cmd(bug({TeX}), HTML) :-
	translate_footnote(['BUG:', ' '|TeX], HTML).
cmd(fileext({Ext}), #code(Text)) :-
	sformat(Text, '.~w', [Ext]).

cmd(var(		{A1}), #var(+A1)).
cmd(arg(		{A1}), #var(+A1)).
cmd(metafile(		{A1}), #code(+A1)).
cmd(file(		{A1}), #code(A1)).
cmd(clib(		{A1}), #code(+A1)).
cmd(cglobalvar(		{A1}), #code(+A1)).
cmd(ctype(		{A1}), #code(+A1)).
cmd(type(		{A1}), #code(+A1)).
cmd(pllib(		{A1}), #code([library, #embrace(+A1)])).
cmd(hook(		{_A1}), #i(#embrace(hook))).
cmd(env(		{A1}), #code(+A1)).
cmd(program(		{A1}), #b(A1)).
cmd(exam(		{A1}), #code(+A1)).
cmd(jargon(		{A1}), #em(+A1)).
cmd(chr(		{A1}), #code(+A1)).
cmd(const(		{A1}), #code(+A1)).
cmd(key(		{A1}), #code(+A1)).
cmd(plflag(		{A1}), #code(+A1)).
cmd(module(		{A1}), #code(+A1)).
cmd(except(		{A1}), #code(+A1)).
cmd(op(			{A1}), #strong(+A1)).
cmd(cmdlineoption(	{A1}), #strong(+A1)).
cmd(menu({A1},{[]}),	       #strong(+A1)).
cmd(menu({A1},{A2}),	       [#strong(+A1), ' ', #embrace(#code(+A2))]).
cmd(longoption(	   {A1},{[]}), [#strong([nospace(--), +A1])]).
cmd(longoption(	   {A1},{A2}), [#strong([nospace(--), +A1,
					 nospace(=)]), #var(+A2)]).
cmd(fmtseq(		{A1}), #code(A1)).
cmd(versionshort,	    _, nospace(Version)) :-
	feature(version, V),
	Major is V // 10000,
	Minor is (V // 100) mod 100,
	Patch is V mod 100,
	concat_atom([Major, Minor, Patch], '.', Version).
cmd(bnfor, '|').
cmd(bnfmeta({Meta}), [nospace('<'), #var(+Meta), nospace('>')]).
cmd(argoption({RawName}, {ArgName}),
    [ #strong(Name), ' ', #var(ArgName)
    ]) :-
	clean_tt(RawName, Name).
cmd(predref({RawName}, {Arity}), #lref(RefName, Text)) :-
	clean_name(RawName, Name),
	predicate_refname(Name, Arity, RefName),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(functor({RawName}, {Arity}), Text) :-
	clean_name(RawName, Name),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(compound({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(term({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(errorterm({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(infixterm({RawName},{A1},{A2}), #code([+A1, Name, +A2])) :-
	clean_name(RawName, Name).
cmd(manref({RawName}, {Section}),
    [#strong(Name), #embrace(Section)]) :-
	clean_tt(RawName, Name).
cmd(funcref({RawName}, {Args}),
    #lref(RefName, [Name, #embrace(+Args)])) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w()', [Name]).
cmd(definition({Tag}),
    #defitem(+Tag)).
cmd('DCG'(A,B,C), X) :-
	cmd(predicate(A,B,C), X).
cmd(predicate({RawName}, {'0'}, {_}),
    #defitem(#label(RefName, #strong(Name)))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w/0', [Name]),
	add_to_index(RefName, +RefName).
cmd(predicate({RawName}, {Arity}, {Args}),
    #defitem(#label(RefName,
		    [ #strong(Name), #embrace(#var(+Args))
		    ]))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(directive({RawName}, {Arity}, {Args}),
    #defitem(#label(RefName,
		    [ #strong(Name), #embrace(#var(Args))
		    ]))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(cfunction({RType}, {RawName}, {Args}),
    #defitem(#label(RefName,
		    [ #var(RType), ' ', #strong(+RawName), #embrace(#var(+Args))
		    ]))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w()', [Name]),
	add_to_index(RefName, +RefName).
cmd(cmacro({RType}, {Name}, {Args}),
    #defitem(#label(RefName,
		    [ #var(RType), ' ', #strong(Name), #embrace(#var(+Args))
		    ]))) :-
	sformat(RefName, '~w()', [Name]),
	add_to_index(RefName, +RefName).
cmd(prefixop({RawName}, {Arg}),
    #defitem(#label(RefName, [#strong(Name), ' ', #var(Arg)]))) :-
	clean_name(RawName, Name),
	predicate_refname(Name, 1, RefName),
	add_to_index(RefName, +RefName).
cmd(infixop({RawName}, {Arg1}, {Arg2}),
    #defitem(#label(RefName,
		    [ #var(Arg1), ' ', #strong(Name), ' ', #var(Arg2)
		    ]))) :-
	clean_name(RawName, Name),
	predicate_refname(Name, 2, RefName),
	add_to_index(RefName, +RefName).
cmd(termitem({Name}, {[]}), #defitem(#strong(+Name))).
cmd(termitem({Name}, {Arg}),
    #defitem([#strong(+Name), #embrace(#var(+Arg))])).
cmd(prologflagitem({Name}, {Type}, {Access}),
    #defitem([#strong(Name), #embrace([#var(Type)|Change])])) :-
	(   Access == r
	->  Change = []
	;   Change = nospace(', changeable')
	).
cmd(fmtchar({Name}), #defitem(#code(+Name))).
cmd(optionval({Value}), #defitem(#strong(+Value))).
cmd(cmdlineoptionitem(M, {Option}, {Arg}),
    #defitem([#strong(+Option), Sep, #var(+Arg)])) :-
	(   M = *
	->  Sep = []
	;   Sep = [' ']
	).
cmd(longoptionitem({Name}, {[]}), #defitem(#strong([nospace(--), +Name]))).
cmd(longoptionitem({Name}, {Arg}), #defitem(#strong([nospace(--), +Name,
						     nospace(=),
						     #var(+Arg)]))).
cmd(optionarg({Option}, {Arg}),
    #defitem([#strong(Option), #var(Arg)])).
cmd(traceoption({CharSpec}, {Name}, {Description}),
    [ #defitem([#strong(Name), ' ', #embrace(#code(Char))]),
      +Description
    ]) :-
	clean_name(CharSpec, Char).
cmd(pleaseoption({Name}, {Type}, {Default}),
    #defitem([ #strong(Name), ' ', #embrace(#var(Type)), ' ',
	       'Default:', ' ', Default
	     ])).
cmd(featureoption({Name}, {Type}),
    #defitem([#strong(Name), ' ', #embrace(#var(Type))])).
cmd(escapeitem({Name}), #defitem(#code([nospace('\\'), +Name]))).
cmd(ttdef({Def}), #defitem(#code(+Def))).
cmd(predicatesummary({RawName}, {Arity}, {Summary}),
    #row([#predref(Name, Arity), +Summary])) :-
	clean_name(RawName, Name).
cmd(oppredsummary({RawName}, {Arity}, {_Assoc}, {_Pri}, {Summary}),
    #row([#predref(Name, Arity), +Summary])) :-
	clean_name(RawName, Name).
cmd(functionsummary({RawName}, {Arity}, {Summary}),
    #row([#predref(Name, Arity), +Summary])) :-
	clean_name(RawName, Name).
cmd(opfuncsummary({RawName}, {Arity}, {_Assoc}, {_Pri}, {Summary}),
    #row([#predref(Name, Arity), +Summary])) :-
	clean_name(RawName, Name).
cmd(opsummary({Pri}, {Assoc}, {RawName}, {Summary}),
    #row([Pri, Assoc, Name, +Summary])) :-
	clean_name(RawName, Name).

cmd(texcmd({Name}), #code([nospace(\), Name])).
cmd(texenv({Name}), #code(Name)).
cmd(texmode({Name}), #var(Name)).

% C++ Documentation (packages/cpp)

cmd(classitem({Class}),
    #defitem(#label(RefName, #strong(Class)))) :-
	sformat(RefName, 'class:~w', [Class]).
cmd(constructor({Class}, {Args}),
    #defitem([#strong([Class, ::, Class]), #embrace(#var(+Args))])).
cmd(destructor({Class}),
    #defitem([#strong([~, Class]), #embrace(#var(''))])).
cmd(cppcast({Class}, {Type}),
    #defitem([#strong([Class, '::operator', Type]), #embrace(#var(void))])).
cmd(nodescription, []).

% Some XPCE things

cmd(class({Name}),              #lref(Label, Name)) :-
        concat('class:', Name, Label),
        add_to_index(Name).

% Glossary support

cmd(glossitem({Term}), #defitem(#label(RefName, #strong(Term)))) :-
	canonise_glossitem(Term, Ref),
	sformat(RefName, 'gloss:~w', [Ref]).
cmd(g({Term}),	#lref(RefName, Term)) :-
	canonise_glossitem(Term, Ref),
	sformat(RefName, 'gloss:~w', [Ref]).

% library stuff
cmd(libdoc({Name}, {Summary}),
    [HTML, #label(Name, [], Tag)]) :-
	translate_section(2, -,
			  ['library(', Name, '): ', Summary],
			  HTML,
			  Name),
	tex:label_tag(Tag).


		 /*******************************
		 *	     GLOSSARY		*
		 *******************************/

canonise_glossitem(In, Out) :-
	downcase_atom(In, In1),
	atom_codes(In1, Chars0),
	(   append(CharsPre, [0'[|_], Chars0)
	->  remove_trailing_spaces(CharsPre, Chars1)
	;   Chars1 = Chars0
	),
	(   append(Chars2, "s", Chars1)
	->  true
	;   Chars2 = Chars1
	),
	maplist(canonical_char, Chars2, Chars),
	atom_codes(Out0, Chars),
	canonical(Out0, Out).

canonical(unified, unify) :- !.
canonical(bound, binding) :- !.
canonical(proven, prove) :- !.
canonical(succeeded, succeed) :- !.
canonical(compiled, compile) :- !.
canonical(propertie, property) :- !.	% s has alredy gone
canonical(X, X).

canonical_char(0' , 0'-) :- !.
canonical_char(0'_, 0'-) :- !.
canonical_char(X, X).

remove_trailing_spaces([], []).
remove_trailing_spaces([0' |T], []) :-
	checklist(=(0' ), T), !.
remove_trailing_spaces([H|T0], [H|T]) :-
	remove_trailing_spaces(T0, T).

		 /*******************************
		 *               C		*
		 *******************************/

cmd(Cmd, HTML) :-
	special(Cmd, Atom),
	HTML = #code(Atom).


special('Sexe', '#!').
special('Scut', !).
special('Scomma',  (,)).
special('Sifthen',  (->)).
special('Ssoftcut',  (*->)).
special('Sdot', '.').
special('Ssemicolon',  (;)).
special('Slt', <).
special('Seq', =).
special('Suniv', =..).
special('Saeq', =:=).
special('Sle', =<).
special('Sequal', ==).
special('Sstructeq', =@=).
special('Sstructneq', \=@=).
special('Sane', =\=).
special('Sgt', >).
special('Sge', >=).
special('Stlt', @<).
special('Stle', @=<).
special('Stgt', @>).
special('Stge', @>=).
special('Snot', \+).
special('Sne', \=).
special('Snequal', \==).
special('Shat', ^).
special('Sbar',  ('|')).
special('Stimes', *).
special('Spow', **).
special('Splus', +).
special('Sminus', -).
special('Sdiv', /).
special('Sidiv', //).
special('Sand', /\).
special('Slshift', <<).
special('Srshift', >>).
special('Sneg', \).
special('Sesc', \).
special('Sor', \/).
special('Sdollar', $).
special('Squest', ?).
special('Smodule', :).
special('Sneck',  (:-)).
special('Sdirective',  (?-)).
special('Sdcg',  (-->)).
special('Bc', '\\c').
special('Bn', '\\n').
special('Br', '\\r').
special('Bl', '\\l').
special('BB', \\).
special('Stilde', ~).
special('Spercent', '%').
special('Shash', #).
special(backslash, \).

clean_name([\Special], Out) :-
	special(Special, Out), !.
clean_name([\tt, Out], Out) :- !.
clean_name($(Out), Out) :- !.
clean_name([Out], Out) :- !.
clean_name(X, X) :-
	atomic(X), !.
clean_name(L, Out) :-
	maplist(clean_name, L, L2),
	concat_atom(L2, Out).
	
predicate_refname(Symbol, Arity, Ref) :-
	symbol_name(Symbol, Name), !,
	concat_atom([Name, /, Arity], Ref).
predicate_refname(Name, Arity, Ref) :-
	concat_atom([Name, /, Arity], Ref).

symbol_name('->',	send_arrow).
symbol_name('<-',	get_arrow).
