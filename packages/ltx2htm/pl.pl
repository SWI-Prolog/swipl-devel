/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
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
		 *	     COMMANDS		*
		 *******************************/

cmd(bug({TeX}), HTML) :-
	translate_footnote(['BUG:', ' '|TeX], HTML).
cmd(fileext({Ext}), #code(Text)) :-
	sformat(Text, '.~w', [Ext]).

cmd(var(		{A1}), #var(+A1)).
cmd(arg(		{A1}), #var(+A1)).
cmd(file(		{A1}), #code(+A1)).
cmd(clib(		{A1}), #code(+A1)).
cmd(cglobalvar(		{A1}), #code(+A1)).
cmd(ctype(		{A1}), #code(+A1)).
cmd(pllib(		{A1}), #code([library, #embrace(+A1)])).
cmd(hook(		{_A1}), #i(#embrace(hook))).
cmd(env(		{A1}), #code(+A1)).
cmd(program(		{A1}), #b(A1)).
cmd(exam(		{A1}), #code(+A1)).
cmd(jargon(		{A1}), #em(+A1)).
cmd(chr(		{A1}), #code(+A1)).
cmd(const(		{A1}), #code(+A1)).
cmd(op(			{A1}), #strong(+A1)).
cmd(cmdlineoption(	{A1}), #strong(+A1)).

cmd(versionshort,		 _, nospace(Version)) :-
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
cmd(predref({RawName}, {Arity}), #lref(Text, Text)) :-
	clean_tt(RawName, Name),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(functor({RawName}, {Arity}), Text) :-
	clean_tt(RawName, Name),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(compound({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(term({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(manref({RawName}, {Section}),
    [#strong(Name), #embrace(Section)]) :-
	clean_tt(RawName, Name).
cmd(funcref({RawName}, {Args}),
    #lref(RefName, [Name, #embrace(+Args)])) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w()', [Name]).
cmd(definition({Tag}),
    #defitem(+Tag)).
cmd(predicate({RawName}, {'0'}, {_}),
    #defitem(#label(RefName, #strong(Name)))) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w/0', [Name]),
	add_to_index(RefName, +RefName).
cmd(predicate({RawName}, {Arity}, {Args}),
    #defitem(#label(RefName,
		    [ #strong(Name), #embrace(#var(+Args))
		    ]))) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(directive({RawName}, {Arity}, {Args}),
    #defitem(#label(RefName,
		    [ #strong(Name), #embrace(#var(Args))
		    ]))) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(cfunction({RType}, {Name}, {Args}),
    #defitem(#label(RefName,
		    [ #var(RType), ' ', #strong(Name), #embrace(#var(+Args))
		    ]))) :-
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
	clean_tt(RawName, Name),
	sformat(RefName, '~w/1', [Name]),
	add_to_index(RefName, +RefName).
cmd(ttprefixop({RawName}, {Arg}),
    #defitem(#label(RefName, [#code(Name), ' ', #var(Arg)]))) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w/1', [Name]),
	add_to_index(RefName, +RefName).
cmd(infixop({RawName}, {Arg1}, {Arg2}),
    #defitem(#label(RefName,
		    [ #var(Arg1), ' ', #strong(Name), ' ', #var(Arg2)
		    ]))) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w/2', [Name]),
	add_to_index(RefName, +RefName).
cmd(ttinfixop({RawName}, {Arg1}, {Arg2}),
    #defitem(#label(RefName,
		    [ #var(Arg1), ' ', #code(Name), ' ', #var(Arg2)
		    ]))) :-
	clean_tt(RawName, Name),
	sformat(RefName, '~w/2', [Name]),
	add_to_index(RefName, +RefName).
cmd(termitem({Name}, {[]}), #defitem(#strong(+Name))).
cmd(termitem({Name}, {Arg}),
    #defitem([#strong(+Name), #embrace(#var(+Arg))])).
cmd(optionval({Value}), #defitem(#strong(+Value))).
cmd(cmdlineoptionitem(M, {Option}, {Arg}),
    #defitem([#strong(+Option), Sep, #var(+Arg)])) :-
	(   M = *
	->  Sep = []
	;   Sep = [' ']
	).
cmd(optionarg({Option}, {Arg}),
    #defitem([#strong(Option), #var(Arg)])).
cmd(traceoption({Char}, {Name}, {Description}),
    [ #defitem([#strong(Name), ' ', #embrace(#code(Char))]),
      +Description
    ]).
cmd(pleaseoption({Name}, {Type}, {Default}),
    #defitem([ #strong(Name), ' ', #embrace(#var(Type)), ' ',
	       'Default:', ' ', Default
	     ])).
cmd(featureoption({Name}, {Type}),
    #defitem([#strong(Name), ' ', #embrace(#var(Type))])).
cmd(escapeitem({Name}), #defitem(#code([nospace('\'), +Name]))).
cmd(ttdef({Def}), #defitem(#code(+Def))).
cmd(predicatesummary({Name}, {Arity}, {Summary}),
    #row([#predref(Name, Arity), +Summary])).
cmd(oppredsummary({Name}, {Arity}, {_Assoc}, {_Pri}, {Summary}),
    #row([#predref(Name, Arity), +Summary])).
cmd(functionsummary({Name}, {Arity}, {Summary}),
    #row([#predref(Name, Arity), +Summary])).
cmd(opfuncsummary({Name}, {Arity}, {_Assoc}, {_Pri}, {Summary}),
    #row([#predref(Name, Arity), +Summary])).
cmd(opsummary({Pri}, {Assoc}, {RawName}, {Summary}),
    #row([Pri, Assoc, Name, +Summary])) :-
	clean_tt(RawName, Name).

cmd(texcmd({Name}), #code([nospace(\), Name])).
cmd(texenv({Name}), #code(Name)).
cmd(texmode({Name}), #var(Name)).

cmd(tthat, '^').
cmd(ttbackslash, \).
