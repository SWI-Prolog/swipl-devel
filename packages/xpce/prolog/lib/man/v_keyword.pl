/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(man_keyword, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ ignore/1
	   , send_list/3
	   ]).

:- pce_begin_class(man_keyword_browser, man_frame,
		   "Keyword browser").

variable(keywords,	man_module,		get).
variable(current,	man_keyword_card*,	get).

initialise(KB, Manual:man_manual) :->
	"Create from Manual"::
	send(KB, send_super, initialise, Manual, 'Keyword Browser'),

	get(Manual, module, keywords, @on, Module),
	send(KB, slot, keywords, Module),
	
	browser(Browser, Module),
	dialog(Dialog),
	new(RelBrowser, man_summary_browser(man_summary, size(90, 15))),

	send(KB, append, Browser),
	send(RelBrowser, right, Browser),
	send(Dialog, below, Browser),
	send(KB, edit_mode, Manual?edit_mode),
	
	send(KB, open).


		/********************************
		*        DATA MANAGEMENT	*
		********************************/

create_keyword(KB, Name:string) :->
	"Add a new keyword"::
	send(Name, strip),
	(   get(Name, size, 0)
	->  send(@display, inform, 'Please enter a keyword first')
	;   get(KB, card, Name, _Kwd)
	->  send(@display, inform, 'Keyword %s already exists', Name)
	;   new(Kwd, man_keyword_card(KB?keywords, Name)),
	    append(KB?browser_member, Kwd),
	    send(KB, selected, Kwd)
	).		


card(KB, Kwd:name, Card) :<-
	"Find card for a named keyword"::
	get(KB?keywords?id_table, find_value, @arg2?name == Kwd, Card).


		/********************************
		*            DIALOG		*
		********************************/

dialog(D) :-
	new(D, dialog),
	new(KB, D?frame),
	ifmaintainer((
	  send(D, append, text_item(keyword, '', message(D?create_member,
							 execute))),
	  send(D, append,
	       button(create,
		      block(message(KB, create_keyword,
				    D?keyword_member?selection),
			    message(D?keyword_member, selection, ''))),
	       right),
	  send(D, append, button(sort,  message(KB?browser_member, sort))),
	  send(D, append, button(stars, message(KB, stars)))
	)),
	send(D, append, button(help,  message(KB, help))),
	send(D, append, button(quit,  message(KB, quit))).


		/********************************
		*          COMMUNICATION	*
		********************************/

edit_mode(KB, Val:bool) :->
	"Switch edit mode on/off"::
	get(KB, dialog_member, Dialog),
	ignore(send_list([ Dialog?create_member
			 , Dialog?keyword_member
			 , Dialog?sort_member
			 , Dialog?stars_member
			 ], active, Val)).
	

selected(CB, Obj:object*) :->
	"Set the selection"::
	get(CB, browser_member, Browser),
	(   Obj \== @nil,
	    send(Obj, instance_of, man_keyword_card)
	->  get(Browser?dict, find, @arg1?object == Obj, Di),
	    send(Browser, selection, Di),
	    send(Browser, normalise, Di),
	    send(CB, update_related, Obj)
	;   send(Browser, selection, @nil)
	),
	send(CB?man_summary_browser_member, selected, Obj).


release_selection(KB) :->
	"Clear the selection"::
	send(KB?man_summary_browser_member, release_selection),
	send(KB?browser_member, selection, @nil).


unrelated(KB, From:object*, Rel:name, To:object*) :->
	"Trap deleted relations"::
	send(KB, related, From, Rel, To).

related(KB, From:object*, Rel:name, _To:object*) :->
	"Trap added relations"::
	Rel == see_also,
	get(KB, current, From),
	send(KB, update_related).


		/********************************
		*        KEYWORD BROWSER	*
		********************************/

:- pce_global(@man_keyword_browser_popup, make_man_keyword_browser_popup).

make_man_keyword_browser_popup(P) :-
	new(P, popup),

	new(KB, @arg1?image?frame),
	new(Manual, KB?manual),
	new(Selection, Manual?selection),
	new(Obj, @arg1?object),
	new(KwdSelection, KB?dialog_member?keyword_member?selection),
	new(EditModeOn, Manual?edit_mode == @on),

	send_list(P, append,
		  [ menu_item(select,
			      message(KB, request_selection, Obj, @on),
			      @default, @on)
		  ]),
	ifmaintainer(send_list(P, append,
		  [ menu_item(relate,
			      message(KB, request_relate, Obj),
			      @default, @off,
			      and(EditModeOn,
				  Selection \== @nil,
				  Selection \== Obj,
				  not(message(Selection, man_related,
					      see_also, Obj))))
		  , menu_item(rename,
			      message(KB, rename, Obj, KwdSelection),
			      @default, @off,
			      and(EditModeOn, KwdSelection \== ''))
		  , menu_item(delete,
			      message(KB, delete, Obj),
			      @default, @off,
			      EditModeOn)
		  ])).

browser(Browser, KwdModule) :-
	new(Browser, browser),
	new(KB, Browser?frame),

	send(Browser, select_message,
	     message(KB, request_selection, @arg1?object, @off)),
	send(Browser, open_message,
	     message(KB, request_selection, @arg1?object, @on)),
	send(Browser, popup, @man_keyword_browser_popup),

	update_browser(Browser, KwdModule).

update_browser(Browser, Module) :-
	send(Browser, clear),
	send(Module?id_table, for_all,
	     message(@prolog, append, Browser, @arg2)),

	send(Browser, sort).
	

append(Br, Kwd) :-
	send(Br, append, dict_item(Kwd?name, @default, Kwd)).


delete(KB, Kwd:man_keyword_card) :->
	"Delete a keyword"::
	send(@display, confirm, 'Delete keyword %s', Kwd?name),
	get(KB?browser_member?dict, find, @arg1?object == Kwd, Di),
	send(KB?browser_member?dict, delete, Di),
	send(Kwd, free).
	

rename(KB, Kwd:man_keyword_card, Name:name) :->
	"Rename named keyword"::
	send(Kwd, store, name, Name),
	(   get(KB?browser_member?dict, find, @arg1?object == Kwd, Di)
	->  send(Di, key, Name)
	;   true
	).


stars(KB) :->
	"Update all `principle keyword' stars"::
	get(KB, keywords, Module),
	send(Module?id_table, for_all, message(KB, starify, @arg2)),
	get(KB, browser_member, Browser),
	update_browser(Browser, Module).


starify(_KB, Kwd:man_keyword_card) :->
	"Add/delete star for principle keyword"::
	(   (   get(Kwd, man_related, see_also, Chain)
	    ->  send(Chain, for_all, message(@arg1, instance_of,
					     man_keyword_card))
	    ;   true
	    )
	->  delete_star(Kwd)
	;   add_star(Kwd)
	).


delete_star(Kwd) :-
	get(Kwd, name, Name),
	new(S, string(Name)),
	send(S, suffix, '*'), !,
	send(S, delete, S?size - 1, 1),
	send(Kwd, store, name, S).
delete_star(_).


add_star(Kwd) :-
	get(Kwd, name, Name),
	new(S, string(Name)),
	(   send(S, suffix, '*')
	->  true
	;   send(S, append, '*'),
	    send(Kwd, store, name, S)
	).


update_related(KB, Keyword:[man_keyword_card]) :->
	"Select keyword and show related cards"::
	(   Keyword == @default
	->  get(KB, current, Kwd)
	;   Kwd = Keyword,
	    send(KB, slot, current, Kwd)
	),
	get(KB, member, man_summary_browser, Br),
	send(Br, clear),
	get(Kwd, man_related, see_also, Objects),
	send(Br, members, Objects).

:- pce_end_class.
