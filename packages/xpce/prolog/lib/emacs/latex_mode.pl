/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(emacs_latex_mode, []).
:- use_module(library(pce)).
:- require([ send_list/3
	   ]).
:- set_prolog_flag(character_escapes, false).

:- emacs_begin_mode(latex, outline,
		    "Mode for editing LaTeX documents",
		    [ insert_quote      = key('"'),
		      open_document     = key('\C-c\C-o') + button('LaTeX'),
		      close_environment = key('\C-c\C-f') + button('LaTeX'),
		      make_command      = key('\C-\') + button('LaTeX'),
		      make_environment  = key('\C-cRET') + button('LaTeX')
		    ],
		    [ \	   = symbol,
		      -    = symbol,
		      '`'  = open_bracket(''''),
		      '%'  = comment_start,
		      '\n' + comment_end,
		      paragraph_end(regex('\s *\(\sn\|%\|\\item\|\\tick\|\\begin\|\\end\|\\\(sub\)*section\)\|%$'))
		    ]).

:- send(@class, attribute, outline_regex_list,
	chain(regex('^\(\\\(sub\)*section{[^}]*}.*\n\)\(\(.*\n\)*\)\\\(sub\)*section{'))).

:- pce_global(@latex_env_regex, new(regex('\\\(begin\|end\){\(\w+\)}'))).

setup_mode(E) :->
	"Switch editor into fill-mode"::
	send(E, fill_mode, @on).

%	TBD: Nested environments?

close_environment(E) :->
	"Close the current LaTeX environment"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	new(Nesting, number(0)),
	new(Here, number(Caret)),
	repeat,
	(   get(@latex_env_regex, search, TB, Here, 0, Start)
	->  get(@latex_env_regex, register_value, TB, 1, BE),
	    (	send(BE, equal, end)
	    ->	send(Nesting, plus, 1),
		send(Here, value, Start),
		fail
	    ;	(   send(Nesting, equal, 0)
		->  !,
		    get(@latex_env_regex, register_value, TB, 2, Env),
		    get(E, column, Start, Col),
		    send(E, beginning_of_line),
		    (	send(E, looking_at, '\s *$')	% blank line
		    ->	true
		    ;	send(E, end_of_line),
			send(E, newline)
		    ),
		    send(E, align, Col),
		    send(E, insert, string('\\end{%s}\n', Env))
		;   send(Nesting, minus, 1),
		    send(Here, value, Start),
		    fail
		)
	    )
	;   !,
	    send(E, report, warning, 'No open LaTeX environment')
	).

		 /*******************************
		 *	      TYPING		*
		 *******************************/

insert_self(M, Times:[int], Id:[char]) :->
	"Insert, but donot warn on mismatched bracket"::
	pce_catch_error(chain(no_matching_bracket, mismatched_bracket),
			send(M?editor, insert_self, Times, Id)).


		 /*******************************
		 *	       QUOTE		*
		 *******************************/

insert_quote(M) :->
	"Insert `` or ''"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	get(M, syntax, S),
	get(TB, character, Caret-1, Before),
	(   \+ send(S, has_syntax, Before, layout)
	->  send(M, insert, '''''')
	;   send(M, insert, '``')
	).


		 /*******************************
		 *	      DOCUMENT		*
		 *******************************/

open_document(M, Style:'{article,report,book,letter}|name') :->
	"Insert document header"::
	send(M, format, '\\documentclass{%s}\n\n', Style),
	send(M, make_environment, document).


insert_section_header(E) :->
	"Insert LaTeX section header"::
	send(E, insert, 
'		 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		 %               C		%
		 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'),
	send(E, backward_word).

		 /*******************************
		 *	LATEX ENVIRONMENTS	*
		 *******************************/

:- pce_global(@latex_envionments,
	      new(chain('BIG', 'Big', abstract, array, big, center,
			description, displaymath, document, enumerate,
			eqnarray, 'eqnarray*', equation, figure, figure,
			float, flushleft, flushright, footnotesize, itemize,
			list, math, minipage, obeycr, picture, quotation,
			quote, sloppypar, small, tabbing, table, 'table*',
			tabular, 'tabular*', thebibliography, theindex,
			titlepage, trivlist, verbatim, 'verbatim*', verse,
			letter))).

:- pce_global(@latex_environment_name_type,
	      new(type(latex_environment_name, name_of, new(chain)))).

:- initialization
	send(@latex_environment_name_type, slot, context, @latex_envionments).

make_environment(M, Env:'latex_environment_name|name') :->
	"Insert \begin{env} ... \end{env} with completion"::
	send(M, format, '\\begin{%s}\n\n\\end{%s}\n', Env, Env),
	send(M, previous_line, 1),
	send(M, align_with_previous_line),
	send(M, previous_line, 1),
	send(M, align_with_previous_line).

		 /*******************************
		 *	  LATEX-COMMANDS	*
		 *******************************/

latex_lplain_commands(['[', ']', '!', '"', '#', '$', '%', '&', '''', '*',
			',', '.', ';', '=', '>', 'AA', 'AE', 'Arrowvert',
			'Big', 'Bigg', 'Biggl', 'Biggm', 'Biggr', 'Bigl',
			'Bigm', 'Bigr', 'Delta', 'Downarrow', 'Gamma', 'H',
			'Huge', 'Im', 'L', 'Lambda', 'LARGE', 'Large', left,
			right, 'Leftarrow', 'Leftrightarrow', 'Longleftarrow',
			'Longleftrightarrow', 'Longrightarrow', 'O', 'OE',
			'Omega', 'P', 'Phi', 'Pi', 'Pr', 'Psi', 'Re',
			'Rightarrow', 'S', 'Sigma', 'TeX', 'Theta',
			'Uparrow', 'Updownarrow', 'Upsilon', 'Vert',
			'Xi', '^', '_', '`', aa, active, acute, ae, aleph,
			allowbreak, alpha, amalg, angle, approx, arccos,
			arcsin, arctan, arg, arrowvert, ast, asymp, b,
			backslash, bar, beta, bgroup, big, bigbreak,
			bigcap, bigcirc, bigcup, bigg, biggl, biggm, biggr,
			bigl, bigm, bigodot, bigoplus, bigotimes, bigr,
			bigsqcup, bigtriangledown, bigtriangleup, biguplus,
			bigvee, bigwedge, bmod, body, bordermatrix, bot,
			bowtie, brace, braceld, bracelu, bracerd, braceru,
			bracevert, brack, break, breve, buildrel, bullet, c,
			cap, caption, cases, cdot, cdotp, cdots, check, chi,
			choose, circ, clubsuit, colon, cong, coprod,
			copyright, cos, cosh, cot, coth, cr, csc, cup, d,
			dag, dagger, dashv, ddag, ddagger, ddot, ddots,
			defaulthyphenchar, defaultskewchar, deg, delta, det,
			diamond, diamondsuit, dim, displaylines, div, dot,
			doteq, dotfill, dots, downarrow, downbracefill,
			eject, ell, empty, emptyset, endgraf, enskip,
			enspace, epsilon, equiv, eta, exists, exp, filbreak,
			flat, footnoterule, footnotesize, forall,
			frenchspacing, frac, frown, gamma, gcd, geq, gg,
			goodbreak, grave, hang, hat, hbar, heartsuit, hfill,
			hglue, hidewidth, hom, hookleftarrow, hookrightarrow,
			hphantom, hrulefill, huge, i, ialign, iff, imath,
			in, inf, infty, int, intop, iota, itemitem, j,
			jmath, jot, kappa, ker, l, lambda, langle, large,
			lbrace, lbrack, lceil, ldotp, ldots, leftarrow,
			leftarrowfill, leftharpoondown, leftharpoonup,
			leftline, leftrightarrow, leq, lfloor, lg, lgroup,
			lhook, lim, liminf, limsup, linepenalty, ll,
			lmoustache, ln, log, longleftarrow,
			longleftrightarrow, longmapsto, longrightarrow,
			loop, lq, mapsto, max, maxdepth, maxdimen, medbreak,
			medmuskip, mid, min, models, mp, mu, nabla, narrower,
			natural, nearrow, neg, negthinspace, neq, newtoks, ni,
			nonfrenchspacing, normalsize, normalbaselines, not,
			notin, nu, null, nwarrow, o, oalign, obeylines,
			obeyspaces, odot, oe, oint, ointop, omega, ominus,
			oplus, oslash, otimes, overbrace, overfullrule,
			overleftarrow, overrightarrow, parallel, parfillskip,
			parindent, parskip, partial, pb, pc, pd, perp, phi,
			pi, pm, pmod, prec, preceq, pretolerance, prime,
			prod, propto, psi, qquad, quad, rangle, rbrace,
			rbrack, rceil, rfloor, rgroup, rho, rhook,
			rightarrow, rightarrowfill, rightharpoondown,
			rightharpoonup, rightleftharpoons, rmoustache,
			rq, scriptspace, scriptsize, searrow, sec, setminus,
			sharp, showboxbreadth, showboxdepth, sigma, sim,
			simeq, sin, sinh, slash, small, smallbreak, smallint,
			smash, smile, sp, spadesuit, sqcap, sqcup, sqrt,
			sqsubseteq, sqsupseteq, ss, stackrel, star, strut,
			subset, subseteq, succ, succeq, sum, sup, supset,
			supseteq, surd, swarrow, t, tan, tanh, tau, theta,
			thickmuskip, thinmuskip, thinspace, tilde, tiny,
			times, top, topskip, triangle, triangleleft,
			triangleright, u, underbrace, uparrow, upbracefill,
			updownarrow, uplus, upsilon, v, varepsilon, varphi,
			varpi, varrho, varsigma, vartheta, vbadness, vdash,
			vdots, vec, vee, vert, wedge, widehat, widetilde,
			widowpenalty, wp, wr, xi, xpt, zeta, '~']).

latex_ordinary_commands(
	[ 'Alph', 'Roman', '\\', address, addtocounter, addtolength,
	  alph, appendix, arabic, bibliography, bibliographystyle,
	  bigskip, centering, chapter, circle, cite, cite, cleardoublepage,
	  clearpage, closing, dashbox, documentstyle, fbox, footnote,
	  footnotemark, footnotetext, framebox, glossary, hbox, hline,
	  hspace, include, includeonly, index, item, label, line, linebreak,
	  listoffigures, listoftables, makebox, makeglossary, makeindex,
	  maketitle, message, mbox, medskip, multicolumn, multiput,
	  newcommand, newcommand, newlength, newline, newpage, newtheorem,
	  noalign, nofiles, noindent, nolinebreak, nopagebreak, normalsize,
	  opening, oval, pagebreak, pagenumbering, pageref, pagestyle,
	  paragraph, parbox, protect, put, raggedleft, raggedright,
	  raggedright, raisebox, ref, renewcommand, roman, rule, savebox,
	  sbox, scriptscriptsize, scriptsize, section, setcounter,
	  setlength, settowidth, shortstack, shortstack, smallskip,
	  space, stop, subsection, subsubsection, tableentry,
	  tableofcontents, thicklines, thinlines, thispagestyle, today,
	  typein, typeout, usebox, vector, verb, vline, vspace]).

latex_parameters(
	[ columnsep, footinsertskip, intextsep, columnseprule,
	  oddsidemargin, columnwidth, textfloatsep, evensidemargin,
	  footsep, textheight, floatsep, headheight, textwidth, headsep,
	  topmargin]).

latex_tabbing_commands(
	[ kill, >, -, pushtab, <, =, poptab, +]).

:- pce_global(@latex_command_name_type,
	      new(type(latex_command_name, name_of, new(chain)))).

:- initialization
   get(@latex_command_name_type, context, Chain),
   send(Chain, clear),
   latex_lplain_commands(L1),   send_list(Chain, append, L1),
   latex_ordinary_commands(L2), send_list(Chain, append, L2),
   latex_parameters(L3),        send_list(Chain, append, L3),
   latex_tabbing_commands(L4),  send_list(Chain, append, L4),

   send(Chain, sort).

make_command(M, Cmd:'latex_command_name|name') :->
	"Insert a \command sequence with completion"::
	send(M, format, '\\%s', Cmd).

:- emacs_end_mode. 
