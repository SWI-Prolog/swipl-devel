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
cmd(fmtseq(		{A1}), #code(A1)).

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
	clean_name(RawName, Name),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(functor({RawName}, {Arity}), Text) :-
	clean_name(RawName, Name),
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
	clean_name(RawName, Name),
	sformat(RefName, '~w/1', [Name]),
	add_to_index(RefName, +RefName).
cmd(infixop({RawName}, {Arg1}, {Arg2}),
    #defitem(#label(RefName,
		    [ #var(Arg1), ' ', #strong(Name), ' ', #var(Arg2)
		    ]))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w/2', [Name]),
	add_to_index(RefName, +RefName).
cmd(termitem({Name}, {[]}), #defitem(#strong(+Name))).
cmd(termitem({Name}, {Arg}),
    #defitem([#strong(+Name), #embrace(#var(+Arg))])).
cmd(fmtchar({Name}), #defitem(#code(+Name))).
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

		 /*******************************
		 *               C		*
		 *******************************/

cmd(Cmd, HTML) :-
	special(Cmd, Atom),
	HTML = #code(Atom).

special('Sexe', '#!').
special('Scut', '!').
special('Scomma', ',').
special('Sifthen', '->').
special('Ssoftcut', '*->').
special('Sdot', '.').
special('Ssemicolon', ';').
special('Slt', '<').
special('Seq', '=').
special('Suniv', '=..').
special('Saeq', '=:=').
special('Sle', '=<').
special('Sequal', '==').
special('Sstructeq', '=@=').
special('Sstructneq', '\=@=').
special('Sane', '=\=').
special('Sgt', '>').
special('Sge', '>=').
special('Stlt', '@<').
special('Stle', '@=<').
special('Stgt', '@>').
special('Stge', '@>=').
special('Snot', '\+').
special('Sne', '\=').
special('Snequal', '\==').
special('Shat', '^').
special('Sbar', '|').
special('Stimes', '*').
special('Spow', '**').
special('Splus', '+').
special('Sminus', '-').
special('Sdiv', '/').
special('Sidiv', '//').
special('Sand', '/\').
special('Slshift', '<<').
special('Srshift', '>>').
special('Sneg', '\').
special('Sesc', '\').
special('Sor', '\/').
special('Sdollar', '$').
special('Squest', '?').
special('Smodule', ':').
special('Sneck', ':-').
special('Sdirective', '?-').
special('Sdcg', '-->').
special('Bc', '\c').
special('Bn', '\n').
special('Br', '\r').
special('Bl', '\l').
special('BB', '\\').
special('Stilde', '~').
special('Spercent', '%').

clean_name([\Special], Out) :-
	special(Special, Out), !.
clean_name([Out], Out).
	
