/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

#(defitem(Class,Label),	[ html(Begin), Label, html('</DT>'),
			  html('<DD class="defbody">')
			]) :-
	format(atom(Begin), '<DT class="~w">', [Class]).
#(defitem(Label),	[ html('<DT>'), Label,
			  html('<DD class="defbody">')
			]).
#(predtag(Value),	[ html('<span class="pred-tag">'), Value,
			  html('</span>')
			]).

		 /*******************************
		 *	   ENVIRONMENTS		*
		 *******************************/

list_command(tags,     _, html('<DL>'), html('</DL>')).


		 /*******************************
		 *	    INDEX HACK		*
		 *******************************/

cmd(+, index, nospace('\+')).
cmd(=, index, nospace('\=')).

		 /*******************************
		 *	     COMMANDS		*
		 *******************************/

cmd(spaces({X}), html(Spaces)) :-
	atom_number(X, N),
	n_list(N, '&nbsp;', L),
	concat_atom(L, Spaces).
cmd(hrule, html('<HR>')).
cmd(bug({TeX}), #footnote(bug, +TeX)).
cmd(fileext({Ext}), #code(Text)) :-
	sformat(Text, '.~w', [Ext]).

cmd(var(		{A1}), #var(+A1)).
cmd(arg(		{A1}), #var(+A1)).
cmd(metafile(		{A1}), #code(+A1)).
cmd(file(		{A0}), #code(A1)) :-
	clean_tt(A0, A1).
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
cmd(resource(		{A1}), #code(A1)).
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
cmd(predref({RawName}, {Arity}), #lref(pred, RefName, Text)) :-
	clean_name(RawName, Name),
	predicate_refname(Name, Arity, RefName),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(dcgref({RawName}, {DCGArity}), #lref(pred, RefName, Text)) :-
	clean_name(RawName, Name),
	atom_number(DCGArity, ArityInt),
	Arity is ArityInt + 2,
	predicate_refname(Name, Arity, RefName),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(nopredref({RawName}, {Arity}), Text) :-
	clean_name(RawName, Name),
	sformat(Text, '~w/~w', [Name, Arity]).
cmd(prologflag({Name}), #lref(flag, RefName, Name)) :-
	atom_concat('flag:', Name, RefName).
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
    #defitem(#b(+Tag))).
cmd('DCG'(A,B,C), X) :-
	cmd(predicate(A,B,C), X).
cmd(predicate(A, {RawName}, {'0'}, {_}),
    #defitem(pubdef, Content)) :-
	pred_tag(A, Content, [#label(RefName, #strong(Name))]),
	clean_name(RawName, Name),
	sformat(RefName, '~w/0', [Name]),
	add_to_index(RefName, +RefName).
cmd(predicate(A, {RawName}, {Arity}, {Args}),
    #defitem(pubdef, Content)) :-
	pred_tag(A, Content,
		 [#label(RefName, [#strong(Name), #embrace(#var(+Args))])]),
	clean_name(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(dcg(A, {RawName}, {'0'}, {_}),
    #defitem(pubdef, Content)) :-
	pred_tag(A, Content, [#label(RefName, #strong(Name)), #code(//)]),
	clean_name(RawName, Name),
	sformat(RefName, '~w/0', [Name]),
	add_to_index(RefName, +RefName).
cmd(dcg(A, {RawName}, {Arity}, {Args}),
    #defitem(pubdef, Content)) :-
	pred_tag(A, Content, 
		 [ #label(RefName,
			  [ #strong(Name), #embrace(#var(+Args))
			  ]),
		   #code(//)
		 ]),
	clean_name(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(directive({RawName}, {'0'}, {_}),
    #defitem(pubdef, #label(RefName,
			    [ ':- ', #strong(Name)
			    ]))) :- !,
	clean_name(RawName, Name),
	sformat(RefName, '~w/~w', [Name, 0]),
	add_to_index(RefName, +RefName).
cmd(directive({RawName}, {Arity}, {Args}),
    #defitem(pubdef, #label(RefName,
			    [ ':- ', #strong(Name), #embrace(#var(Args))
			    ]))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w/~w', [Name, Arity]),
	add_to_index(RefName, +RefName).
cmd(cfunction({RType}, {RawName}, {Args}),
    #defitem(pubdef, #label(RefName,
			    [ #var(RType), ' ', #strong(+RawName),
			      #embrace(#var(+Args))
			    ]))) :-
	clean_name(RawName, Name),
	sformat(RefName, '~w()', [Name]),
	add_to_index(RefName, +RefName).
cmd(cmacro({RType}, {Name}, {Args}),
    #defitem(pubdef, #label(RefName,
			    [ #var(RType), ' ', #strong(Name),
			      #embrace(#var(+Args))
			    ]))) :-
	sformat(RefName, '~w()', [Name]),
	add_to_index(RefName, +RefName).
cmd(resitem({Resource}),
    #defitem(pubdef, #label(Resource,
			    [ #strong(Resource)
			    ]))) :-
	add_to_index(Resource, +Resource).
cmd(prefixop(A, {RawName}, {Arg}),
    #defitem(pubdef, Content)) :-
	pred_tag(A, Content,
		 #label(RefName, [#strong(Name), ' ', #var(Arg)])),
	clean_name(RawName, Name),
	predicate_refname(Name, 1, RefName),
	add_to_index(RefName, +RefName).
cmd(infixop(A, {RawName}, {Arg1}, {Arg2}),
    #defitem(pubdef, Content)) :-
	pred_tag(A, Content,
		 #label(RefName,
			[ #var(Arg1), ' ', #strong(Name), ' ', #var(Arg2)
			])),
	clean_name(RawName, Name),
	predicate_refname(Name, 2, RefName),
	add_to_index(RefName, +RefName).
cmd(constitem({Name}), #defitem(#label(RefName, #strong(+Name)))) :-
	clean_name(Name, RefName),
	add_to_index(RefName, +RefName).
cmd(termitem({Name}, {[]}), #defitem(#strong(+Name))).
cmd(termitem({Name}, {Arg}),
    #defitem([#strong(+Name), #embrace(#var(+Arg))])).
cmd(infixtermitem({Name}, {Left}, {Right}),
    #defitem([#var(+Left), ' ', #strong(+Name), ' ', #var(+Right)])).
cmd(prologflagitem({Name}, {Type}, {Access}),
    #defitem(pubdef, #label(RefName, [#strong(Name), #embrace([#var(Type)|Change])]))) :-
	atom_concat('flag:', Name, RefName),
	(   Access == r
	->  Change = []
	;   Change = nospace(', changeable')
	).
cmd(fmtchar({Name}), [html('<LI>'), #code(+Name), html('<BR>')]).
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
cmd(menuitem({Name}, {[]}),
    #defitem(#label(RefName, #strong(+Name)))) :-
	clean_name(Name, RefName0),
	atom_concat('menu:', RefName0, RefName),
	concat_atom(Name, ' ', Atom),
	add_to_index(Atom, +RefName).
cmd(menuitem({Name}, {Arg}),
    #defitem([#label(RefName, #strong(+Name)), ' ', #embrace(#var(+Arg))])) :-
	clean_name(Name, RefName0),
	atom_concat('menu:', RefName0, RefName),
	concat_atom(Name, ' ', Atom),
	add_to_index(Atom, +RefName).
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
cmd(menuref({A1}),            #lref(RefName, Name)) :-
	clean_name(A1, RefName0),
	atom_concat('menu:', RefName0, RefName),
	concat_atom(A1, ' ', Name),
        add_to_index(Name).

% Glossary support

cmd(glossitem({Term}), #defitem(#label(RefName, #strong(Term)))) :-
	canonise_glossitem(Term, Ref),
	format(string(RefName), 'gloss:~w', [Ref]).
cmd(g({Term}),	#lref(gloss, RefName, Term)) :-
	canonise_glossitem(Term, Ref),
	format(string(RefName), 'gloss:~w', [Ref]).

% library stuff
cmd(libdoc({Name}, {Summary}),
    [HTML, #label(Name, [], Tag)]) :-
	filebase(Name, File),
	translate_section(2, -,
			  ['library(', Name, '): ', Summary],
			  HTML,
			  File),
	tex:label_tag(Name, Tag).

filebase(Name, File) :-
	atom_codes(Name, Codes),
	select_csym(Codes, Alnums),
	atom_codes(File, Alnums).

select_csym([], []).
select_csym([H|T0], [H|T]) :-
	code_type(H, csymf), !,
	select_csym(T0, T).
select_csym([_|T0], T) :-
	select_csym(T0, T).


pred_tag([], L, L).
pred_tag([Value], [#predtag(#embrace("[]", +Value))|L], L).

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
	maplist(=(0' ), T), !.		% '
remove_trailing_spaces([H|T0], [H|T]) :-
	remove_trailing_spaces(T0, T).


		 /*******************************
		 *	   PlDoc KEYWORDS	*
		 *******************************/

cmd(tag({Tag}),
    [ html('<DT><B>'), +Tag, html('</B><DD>') ]).


		 /*******************************
		 *               C		*
		 *******************************/

cmd(backslash, #code(\)).
cmd(bsl, #code(\)).
cmd(Cmd, HTML) :-
	urldef(Cmd, Atom), !,
	HTML = #code(Atom).


		 /*******************************
		 *    LATEX SPECIAL SEQUENCES	*
		 *******************************/

%	NOTE: This code is copied from doc_latex.pl from PlDoc.

%%	urldef(?DefName, ?String)
%
%	True if \DefName is  a  urldef   for  String.  UrlDefs are LaTeX
%	sequences that can be used to  represent strings with symbols in
%	fragile environments. Whenever a word can   be  expressed with a
%	urldef, we will  do  this  to   enhance  the  robustness  of the
%	generated LaTeX code.

:- dynamic
	urldef/2,
	urldefs_loaded/1.

%%	load_urldefs.
%%	load_urldefs(+File)
%
%	Load   =|\urldef|=   definitions   from    File   and   populate
%	urldef_name/2. See =|pldoc.sty|= for details.

load_urldefs :-
	urldefs_loaded(_), !.
load_urldefs :-
	absolute_file_name(library('pldoc/pldoc.sty'), File,
			   [ access(read) ]),
	load_urldefs(File).

load_urldefs(File) :-
	urldefs_loaded(File), !.
load_urldefs(File) :-
	open(File, read, In),
	call_cleanup((   read_line_to_codes(In, L0),
			 process_urldefs(L0, In)),
		     close(In)),
	assert(urldefs_loaded(File)).

process_urldefs(end_of_file, _) :- !.
process_urldefs(Line, In) :-
	(   phrase(urldef(Name, String), Line)
	->  assert(urldef(Name, String))
	;   true
	),
	read_line_to_codes(In, L2),
	process_urldefs(L2, In).

urldef(Name, String) -->
	"\\urldef{\\", string(NameS), "}\\satom{", string(StringS), "}",
	ws,
	(   "%"
	->  string(_)
	;   []
	),
	eol, !,
	{ atom_codes(Name, NameS),
	  atom_codes(String, StringS)
	}.

ws --> [C], { C =< 32 }, !, ws.
ws --> [].

string([]) --> [].
string([H|T]) --> [H], string(T).
	
eol([],[]).


clean_name([\Special], Out) :-
	urldef(Special, Out), !.
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

n_list(0, _, []) :- !.
n_list(N, X, [X|T]) :-
	N > 0,
	N2 is N - 1,
	n_list(N2, X, T).

:- load_urldefs.
