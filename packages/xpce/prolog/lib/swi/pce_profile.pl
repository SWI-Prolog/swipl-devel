:- module(pce_profile,
	  [ pce_show_profile/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(tabular)).
:- use_module(library(prolog_predicate)).

%	pce_show_profile/0
%	
%	Show already collected profile using a graphical browser.

pce_show_profile :-
	send(new(F, prof_frame), open),
	send(F, wait),
	send(F, load_profile).


		 /*******************************
		 *	       FRAME		*
		 *******************************/

:- pce_begin_class(prof_frame, persistent_frame,
		   "Show Prolog profile data").

variable(ticks,		   int,	 get, "Total # ticks").
variable(accounting_ticks, int,	 get, "# ticks while accounting").
variable(time,		   real, get, "Total time").
variable(nodes,		   int,	 get, "Nodes created").
variable(time_view,	   {ticks,percentage,seconds} := percentage,
				 get, "How time is displayed").

initialise(F) :->
	send_super(F, initialise, 'SWI-Prolog profiler'),
	send(F, append, new(TD, tool_dialog(F))),
	send(new(B, prof_browser), left, new(prof_details)),
	send(B, below, TD),
	send(new(report_dialog), below, B),
	send(F, fill_dialog, TD).

fill_dialog(F, TD:tool_dialog) :->
	send(TD, append, new(File, popup(file))),
	send(TD, append, new(Sort, popup(sort))),
	send(TD, append, new(Time, popup(time))),
	send(TD, append, new(Help, popup(help))),
	send_list(File, append,
		  [ menu_item(statistics,
			      message(F, show_statistics)),
		    gap,
		    menu_item(exit,
			      message(F, destroy))
		  ]),
	forall(sort_by(Label, Field, Order),
	       send(Sort, append,
		    menu_item(Label, message(F, sort_by, Field, Order)))),
	get(F?class, instance_variable, time_view, TV),
	get(TV, type, Type),
	get_chain(Type, value_set, Values),
	forall(member(TimeView, Values),
	       send(Time, append,
		    menu_item(TimeView, message(F, time_view, TimeView)))),
	send_list(Help, append,
		  [ menu_item(about,
			      message(F, about)),
		    menu_item(help,
			      message(F, help))
		  ]).
			      

load_profile(F) :->
	"Load stored profile from the Prolog database"::
	'$prof_statistics'(Ticks, Account, Time, Nodes),
	send(F, slot, ticks, Ticks),
	send(F, slot, accounting_ticks, Account),
	send(F, slot, time, Time),
	send(F, slot, nodes, Nodes),
	get(F, member, prof_browser, B),
	send(F, report, progress, 'Loading profile data ...'),
	send(B, load_profile),
	send(F, report, done),
	send(F, show_statistics),
	reset_profiler.			% Cleanup loaded data from the Prolog database


show_statistics(F) :->
	"Show basic statistics on profile"::
	get(F, ticks, Ticks),
	get(F, accounting_ticks, Account),
	get(F, time, Time),
	get(F, slot, nodes, Nodes),
	get(F, member, prof_browser, B),
	get(B?dict?members, size, Predicates),
	(   Ticks == 0
	->  Distortion = 0.0
	;   Distortion is 100*(Account/Ticks)
	),
	send(F, report, inform,
	     '%d samples in %.2f sec; %d predicates; \
	      %d nodes in call-graph; distortion %.0f%%',
	     Ticks, Time, Predicates, Nodes, Distortion).


details(F, From:prolog) :->
	"Show details on node or predicate"::
	get(F, member, prof_details, W),
	(   functor(From, node, 8)
	->  send(W, node, From)
	;   get(F, member, prof_browser, B),
	    get(B?dict, find,
		message(@arg1, has_predicate, prolog(From)),
		DI)
	->  get(DI, data, Node),
	    send(W, node, Node)
	).
	    
sort_by(F, SortBy:name, Order:[{normal,reverse}]) :->
	"Define the key for sorting the flat profile"::
	get(F, member, prof_browser, B),
	send(B, sort_by, SortBy, Order).

time_view(F, TV:name) :->
	send(F, slot, time_view, TV),
	get(F, member, prof_browser, B),
	get(F, member, prof_details, W),
	send(B, update_labels),
	send(W, refresh).

render_time(F, Ticks:int, Rendered:any) :<-
	"Render a time constant"::
	get(F, time_view, View),
	(   View == ticks
	->  Rendered = Ticks
	;   View == percentage
	->  get(F, ticks, Total),
	    get(F, accounting_ticks, Accounting),
	    (	Total == 0
	    ->	Rendered = '0.0%'
	    ;	Percentage is 100 * (Ticks/(Total-Accounting)),
		new(Rendered, string('%.1f%%', Percentage))
	    )
	;   View == seconds
	->  get(F, ticks, Total),
	    (	Total == 0
	    ->	Rendered = '0.0 s.'
	    ;   get(F, time, TotalTime),
	        Time is TotalTime*(Ticks/Total),
		new(Rendered, string('%.2f s.', Time))
	    )
	).

about(_F) :->
	send(@display, inform,
	     'SWI-Prolog execution profile viewer\n\
	     By Jan Wielemaker').

help(_F) :->
	send(@display, inform,
	     'No help yet').

:- pce_end_class(prof_frame).


		 /*******************************
		 *     FLAT PROFILE BROWSER	*
		 *******************************/

:- pce_begin_class(prof_browser, browser,
		   "Show flat profile in browser").

class_variable(size, size, size(40,20)).

variable(sort_by,  name := ticks_self, get, "How the items are sorted").

initialise(B) :->
	send_super(B, initialise),
	send(B, update_label),
	send(B, select_message, message(@arg1, details)).

resize(B) :->
	get(B?visible, width, W),
	send(B, tab_stops, vector(W-80)),
	send_super(B, resize).

load_profile(B) :->
	"Load stored profile from the Prolog database"::
	get(B, frame, Frame),
	get(B, sort_by, SortBy),
	forall(prof_node(Node),
	       send(B, append, prof_dict_item(Node, SortBy, Frame))),
	send(B, sort).
	
update_label(B) :->
	get(B, sort_by, Sort),
	sort_by(Human, Sort, _How),
	send(B, label, string('Flat profile (%s)', Human?label_name)).

sort_by(B, SortBy:name, Order:[{normal,reverse}]) :->
	"Define key on which to sort"::
	send(B, slot, sort_by, SortBy),
	send(B, update_label),
	send(B, sort, Order),
	send(B, update_labels).

sort(B, Order:[{normal,reverse}]) :->
	get(B, sort_by, Sort),
	(   Order == @default
	->  sort_by(_, Sort, TheOrder)
	;   TheOrder = Order
	),
	send_super(B, sort, ?(@arg1, compare, @arg2, Sort, TheOrder)).

update_labels(B) :->
	"Update labels of predicates"::
	get(B, sort_by, SortBy),
	get(B, frame, F),
	send(B?dict, for_all, message(@arg1, update_label, SortBy, F)).

:- pce_end_class(prof_browser).

:- pce_begin_class(prof_dict_item, dict_item,
		   "Show entry of Prolog flat profile").

variable(data,	       prolog, get, "Predicate data").

initialise(DI, Node:prolog, SortBy:name, F:prof_frame) :->
	"Create from predicate head"::
	send(DI, slot, data, Node),
	value(Node, predicate, Predicate),
	predicate_label(Predicate, Key),
	send_super(DI, initialise, Key),
	send(DI, update_label, SortBy, F).
	
value(DI, Name:name, Value:prolog) :<-
	"Get associated value"::
	get(DI, data, Data),
	value(Data, Name, Value).

has_predicate(DI, Pred:prolog) :->
	get(DI, data, Data),
	value(Data, predicate, Pred).

compare(DI, DI2:prof_dict_item,
	SortBy:name, Order:{normal,reverse},
	Result:name) :<-
	"Compare two predicate items on given key"::
	get(DI, value, SortBy, K1),
	get(DI2, value, SortBy, K2),
	(   Order == normal
	->  get(K1, compare, K2, Result)
	;   get(K2, compare, K1, Result)
	).

update_label(DI, SortBy:name, F:prof_frame) :->
	"Update label considering sort key and frame"::
	get(DI, key, Key),
	(   SortBy == name
	->  send(DI, label, Key)
	;   get(DI, value, SortBy, Value),
	    (	time_key(SortBy)
	    ->	get(F, render_time, Value, Rendered)
	    ;	Rendered = Value
	    ),
	    send(DI, label, string('%s\t%s', Key, Rendered))
	).

time_key(ticks).
time_key(ticks_self).
time_key(ticks_siblings).

details(DI) :->
	"Show details"::
	get(DI, data, Data),
	send(DI?dict?browser?frame, details, Data).

:- pce_end_class(prof_dict_item).


		 /*******************************
		 *	   DETAIL WINDOW	*
		 *******************************/

:- pce_begin_class(prof_details, window,
		   "Table showing profile details").

variable(tabular, tabular, get, "Displayed table").
variable(node,    prolog,  get, "Currently shown node").

initialise(W) :->
	send_super(W, initialise),
	send(W, label, 'Details'),
	send(W, background, colour(grey80)),
	send(W, scrollbars, vertical),
	send(W, display, new(T, tabular)),
	send(T, rules, all),
	send(T, cell_spacing, -1),
	send(W, slot, tabular, T).

resize(W) :->
	send_super(W, resize),
	get(W?visible, width, Width),
	send(W?tabular, table_width, Width-3).

title(W) :->
	"Show title-rows"::
	get(W, tabular, T),
	BG = (background := khaki1),
	send(T, append, 'Time',   bold, center, colspan := 2, BG),
	send(T, append, 'Access', bold, center, colspan := 2, BG),
	send(T, append, 'Predicate', bold, center,
	     valign := center, BG,
	     rowspan := 2),
	send(T, next_row),
	send(T, append, 'Self',   bold, center, BG),
	send(T, append, 'Siblings',   bold, center, BG),
	send(T, append, 'Call',   bold, center, BG),
	send(T, append, 'Redo',   bold, center, BG),
	send(T, next_row).

refresh(W) :->
	"Refresh to accomodate visualisation change"::
	(   get(W, node, Data),
	    Data \== @nil
	->  send(W, node, Data)
	;   true
	).

node(W, Data:prolog) :->
	"Visualise a node"::
	send(W, slot, node, Data),
	send(W?tabular, clear),
	send(W, title),
	value(Data, callers, Callers0),
	sort_relatives(Callers0, Callers),
	send(W, show_recursive, Data),
	show_relatives(Callers, parent, W),
	send(W, show_predicate, Data),
	value(Data, callees, Callees0),
	sort_relatives(Callees0, Callees1),
	reverse(Callees1, Callees),
	show_relatives(Callees, sibling, W).

sort_relatives(List, Sorted) :-
	key_with_calls(List, Keyed),
	keysort(Keyed, KeySorted),
	unkey(KeySorted, Sorted).

key_with_calls([], []).
key_with_calls([H|T0], [K-H|T]) :-
	arg(4, H, Calls),
	arg(5, H, Redos),
	K is Calls+Redos,
	key_with_calls(T0, T).

unkey([], []).
unkey([_-H|T0], [H|T]) :-
	unkey(T0, T).

show_relatives([], _, _) :- !.
show_relatives([H|T], Role, W) :-
	send(W, show_relative, H, Role),
	show_relatives(T, Role, W).

show_recursive(W, Data:prolog) :->
	"Show # resursive calls"::
	(   value(Data, recursive, Recursive),
	    Recursive \== 0
	->  get(W, tabular, T),
	    send(T, append, new(graphical), colspan := 2),
	    send(T, append, Recursive, halign := right),
	    send(T, append, new(graphical)),
	    send(T, append, '<recursive calls>', italic),
	    send(T, next_row)
	;   true
	).

show_predicate(W, Data:prolog) :->
	"Show the predicate we have details on"::
	value(Data, predicate, Pred),
	value(Data, ticks_self, Ticks),
	value(Data, ticks_siblings, SiblingTicks),
	value(Data, call, Call),
	value(Data, redo, Redo),
	get(W, frame, Frame),
	get(Frame, render_time, Ticks, Self),
	get(Frame, render_time, SiblingTicks, Siblings),
	get(W, tabular, T),
	BG = (background := khaki1),
	send(T, append, Self, halign := right, BG),
	send(T, append, Siblings, halign := right, BG),
	send(T, append, Call, halign := right, BG),
	send(T, append, Redo, halign := right, BG),
	send(T, append, new(Txt, prof_predicate_text(Pred, self)), BG),
	send(W, label, string('Details -- %s', Txt?string)),
	send(T, next_row).

show_relative(W, Caller:prolog, Role:name) :->
	Caller = node(Pred, Ticks, SiblingTicks, Calls, Redos),
	get(W, tabular, T),
	get(W, frame, Frame),
	get(Frame, render_time, Ticks, Self),
	get(Frame, render_time, SiblingTicks, Siblings),
	send(T, append, Self, halign := right),
	send(T, append, Siblings, halign := right),
	send(T, append, Calls, halign := right),
	send(T, append, Redos, halign := right),
	(   Pred == '<spontaneous>'
	->  send(T, append, Pred, italic)
	;   send(T, append, prof_predicate_text(Pred, Role))
	),
	send(T, next_row).


:- pce_end_class(prof_details).


:- pce_begin_class(prof_predicate_text, text,
		   "Show a predicate").

variable(predicate, prolog_predicate,	   get,	"Represented predicate").
variable(role,	    {parent,self,sibling}, get,	"Represented role").

initialise(T, Pred:prolog, Role:{parent,self,sibling}) :->
	send(T, slot, predicate, new(P, prolog_predicate(Pred))),
	send(T, slot, role, Role),
	get(P, print_name, Label),
	send_super(T, initialise, Label),
	send(T, colour, blue),
	send(T, underline, @on),
	(   Role == self
	->  send(T, font, bold)
	;   true
	).

:- free(@prof_predicate_text_recogniser).
:- pce_global(@prof_predicate_text_recogniser,
	      make_prof_predicate_text_recogniser).

make_prof_predicate_text_recogniser(G) :-
	Text = @arg1,
	Pred = @arg1?predicate,
	new(P, popup),
	send_list(P, append,
		  [ menu_item(details,
			      message(Text, details),
			      condition := Text?role \== self),
		    menu_item(edit,
			      message(Pred, edit),
			      condition := Pred?source),
		    menu_item(documentation,
			      message(Pred, help),
			      condition := message(Pred, has_help))
		  ]),
	new(C, click_gesture(left, '', double,
			     message(@receiver, details))),
	new(G, handler_group(C, popup_gesture(P))).


event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@prof_predicate_text_recogniser, event, Ev)
	).

details(T) :->
	"Show details of clicked predicate"::
	get(T?predicate, head, Head),
	send(T?frame, details, Head).

:- pce_end_class(prof_predicate_text).


		 /*******************************
		 *		UTIL		*
		 *******************************/

key(predicate,	    1).
key(ticks_self,	    2).
key(ticks_siblings, 3).
key(call,	    4).
key(redo,	    5).
key(recursive,	    6).
key(callers,	    7).
key(callees,	    8).

value(Data, name, Name) :- !,
	arg(1, Data, Pred),
	predicate_name(Pred, Name).
value(Data, label, Label) :- !,
	arg(1, Data, Pred),
	predicate_label(Pred, Label).
value(Data, ticks, Ticks) :- !,
	arg(2, Data, Self),
	arg(3, Data, Siblings),
	Ticks is Self + Siblings.
value(Data, Name, Value) :-
	key(Name, Arg),
	arg(Arg, Data, Value).

sort_by(by_name,	    name,	    normal).
sort_by(by_time,	    ticks,	    reverse).
sort_by(by_time_self,	    ticks_self,	    reverse).
sort_by(by_time_siblings,   ticks_siblings, reverse).
sort_by(by_number_of_calls, call,	    reverse).
sort_by(by_number_of_redos, redo,	    reverse).


%	predicate_label(+Head, -Label)
%	
%	Create a human-readable label for the given head

predicate_label(M:H, Label) :- !,
	functor(H, Name, Arity),
	(   hidden_module(M, H)
	->  concat_atom([Name, /, Arity], Label)
	;   concat_atom([M, :, Name, /, Arity], Label)
	).
predicate_label(H, Label) :- !,
	functor(H, Name, Arity),
	concat_atom([Name, /, Arity], Label).

hidden_module(system, _).
hidden_module(user, _).
hidden_module(M, H) :-
	predicate_property(system:H, imported_from(M)).

predicate_name(_:H, Name) :- !,
	predicate_name(H, Name).
predicate_name(H, Name) :-
	functor(H, Name, _Arity).

%	prof_node(node(Pred,
%		       TimeSelf, TimeSiblings,
%		       Calls, Redo, Recursive,
%		       Parents))
%
%	Collect data for each of the interesting predicates.

prof_node(Node) :-
	style_check(+dollar),
	call_cleanup(get_prof_node(Node), style_check(-dollar)).

get_prof_node(node(M:H,
		   TicksSelf, TicksSiblings,
		   Call, Redo, Recursive,
		   Parents, Siblings)) :-
	current_predicate(_, M:H),
	\+ predicate_property(M:H, imported_from(_)),
	'$prof_procedure_data'(M:H,
			       TicksSelf, TicksSiblings,
			       Call, Redo, Recursive,
			       Parents, Siblings).
