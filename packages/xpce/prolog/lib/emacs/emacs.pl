/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs, []).


:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

		/********************************
		*         DECLARE MODES		*
		********************************/

:- pce_global(@mode_name_type,
	      new(type(mode_name, name_of, new(chain)))).

:- initialization
   get(@mode_name_type, context, Ctx),
   send(Ctx, clear),
   send_list(Ctx, append,
	     [ fundamental
	     , prolog
	     , shell
	     ]).


		 /*******************************
		 *           PROLOG		*
		 *******************************/

:- initialization new(@loading_emacs, object).
					% SWI-Prolog extensions
pce_ifhostproperty(prolog(swi),
		   (:- initialization ensure_loaded(user:swi_prolog))). 


		 /*******************************
		 *	    LIBRARIES		*
		 *******************************/

:- pce_autoload(file_item, library(file_item)).


		 /*******************************
		 *          KERNEL FILES	*
		 *******************************/

:- initialization ensure_loaded(window).
:- initialization ensure_loaded(buffer).
:- initialization ensure_loaded(buffer_menu).
:- initialization ensure_loaded(server).
:- initialization ensure_loaded(fundamental_mode).


		 /*******************************
		 *       AUTOLOAD CLASSES	*
		 *******************************/

:- pce_autoload(emacs_hit_list,		hit_list).
:- pce_autoload(emacs_process_buffer,	shell).
:- pce_autoload(emacs_gdb_buffer,	gdb).
:- pce_autoload(emacs_annotate_buffer,  annotate_mode).


		 /*******************************
		 *	      MODES		*
		 *******************************/




:- declare_emacs_mode(language,		language_mode).
:- declare_emacs_mode(prolog,		prolog_mode).
:- declare_emacs_mode(latex,		latex_mode).
:- declare_emacs_mode(c,		c_mode).
:- declare_emacs_mode('c++',		cpp_mode).
:- declare_emacs_mode(script,		script_mode).
:- declare_emacs_mode(man,		man_mode).
:- declare_emacs_mode(text,		text_mode).
:- declare_emacs_mode(annotate,		annotate_mode).
:- declare_emacs_mode(gdb,		gdb).


		 /*******************************
		 *     EMACS GLOBAL OBJECTS	*
		 *******************************/

:- pce_global(@emacs_mark_list,
	      new(emacs_hit_list('Emacs Mark List'))).
:- pce_global(@emacs_base_names,
	      new(chain_table)).		  % file-base --> buffers
:- pce_global(@emacs_buffers,
	      new(dict)).			  % name --> buffer object
:- pce_global(@emacs_modes,
	      new(hash_table)).			  % name --> mode object
:- pce_global(@emacs,
	      new(emacs_buffer_menu(@emacs_buffers))).
:- pce_global(@emacs_comment_column, new(number(40))).
:- pce_global(@emacs_mode_list,			  % regex --> mode
	      new(sheet(attribute(regex('.*\.pl~?$'),   	prolog),
			attribute(regex('\.\(pl\|xpce\|pceemacs\)rc~?'),
				  prolog),
			attribute(regex('.*\.\(tex\|sty\)~?$'), latex),
			attribute(regex('.*\.doc~?$'),	 	latex),
			attribute(regex('.*\.ann~?$'),	 	annotate),
			attribute(regex('.*\.[ch]~?$'), 	c),
			attribute(regex('.*\.C$'),		'c++'),
			attribute(regex('.*\.cc$'),		'c++'),
			attribute(regex('.*\.cpp$'),		'c++'),
			attribute(regex('[Cc]ompose\|README'),	text)))).
:- pce_global(@emacs_default_mode, new(var(value := script))).

:- free(@loading_emacs).

