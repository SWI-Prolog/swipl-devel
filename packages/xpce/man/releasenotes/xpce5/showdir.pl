:- module(show_directory,
	  [ show_directory/1
	  ]).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define images we use as resources.  See \chapref{resources} for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resource(dir,	image,	image('16x16/closedir.xpm')).
resource(doc,	image,	image('16x16/doc.xpm')).
resource(pce,	image,	image('16x16/pce.xpm')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The toplevel of this module simply  creates   an  instance  of the class
dir_listing and opens it. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

show_directory(D) :-
	send(new(dir_listing(D)), open).

:- pce_begin_class(dir_listing, picture,
		   "Show contents of a directory").

variable(directory,	directory,	get, "Current directory").
variable(sort_column,	name,		get, "Name of column to sort on").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Change the ordinary picture window into  a   table.  First, the table is
associated using `device ->layout_manager'. Then   the parameters of the
table are filled. Note the similarity   between the HTML-3 definition of
terms and those used here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(DL, Dir:directory) :->
	send_super(DL, initialise),
	send(DL, layout_manager, new(T, table)),
	send(T, rules, groups),
	send(T, frame, box),
	send(T, border, 1),
	(   column(I, Name, Align, Label),
	    get(T, column, I, @on, Col),	% @on: create
	    send(Col, halign, Align),
	    send(Col, name, Name),
	    send(T, append, Label),
	    fail
	;   true
	),
	send(T, next_row, @on),			% @on: end group
	send(DL, sort_column, name),
	send(DL, directory, Dir).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Definition of the columns. This is easier   to  read, write and maintain
than long sequences of send-operation in  the ->initialise method above.
The class sortable_column_label is defined further down this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%      Index	Name,   Align,  Label
column(1,      image,	center, new(graphical)).
column(2,	size,	right,  sortable_column_label(size)).
column(3,   modified,	right,  sortable_column_label(modified)).
column(4,       name,	left,   sortable_column_label(name)).


clear(DL) :->
	"Remove all entries, except for the title-row"::
	get(DL, layout_manager, Table),
	send(Table, delete_rows, 2).

sort_column(DL, Col:name) :->
	"Switch sort column and underline proper label"::
	send(DL, slot, sort_column, Col),
	get(DL, layout_manager, Table),
	new(TitleCell, ?(@arg1, cell, 1)?image),
	send(Table?columns, for_all,
	     if(message(TitleCell, instance_of, sortable_column_label),
		if(TitleCell?name == Col,
		   message(TitleCell, underline, @on),
		   message(TitleCell, underline, @off)))).

sort(DL, On:[name]) :->
	(   On == @default
	->  true
	;   send(DL, sort_column, On)
	),
	get(DL, layout_manager, Table),
	send(Table, sort_rows, ?(DL, compare_rows, @arg1, @arg2), 2).

compare_rows(DL, R1:table_row, R2:table_row, Result) :<-
	"Compare two rows on <-sort_column"::
	get(DL, sort_column, ColName),
	get(R1, cell, ColName, C1),
	get(R2, cell, ColName, C2),
	get(C1, image, Gr1),
	get(C2, image, Gr2),
	get(Gr1, compare, Gr2, Result).

:- pce_group(event).

:- pce_global(@direcory_listing_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, clicked,
					?(@receiver, current, @event))))).

current(DL, Event:event, Current:'file|directory') :<-
	"Return pointed file/directory"::
	get(DL, layout_manager, Table),
	get(Table, cell_from_position, Event, Cell),
	get(Cell, row, RowN),
	get(Table, row, RowN, Row),
	get(Row, attribute, client, Current).

event(DL, Ev:event) :->
	(   send_super(DL, event(Ev))
	;   send(@direcory_listing_recogniser, event, Ev)
	).

clicked(DL, Clicked:'file|directory') :->
	(   send(Clicked, instance_of, directory)
	->  send(DL, directory, Clicked)
	;   true
	).
	    

:- pce_group(build).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build the contents of the table.  

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

directory(DL, Dir:directory) :->
	send(DL, slot, directory, Dir),
	send(DL, clear),
	new(Files, chain),
	new(Dirs, chain),
	send(Dir, scan, Files, Dirs),
	send(Dirs, for_all,
	     message(DL, append_directory,
		     ?(Dir, directory, @arg1))),
	send(Files, for_all,
	     message(DL, append_file,
		     ?(Dir, file, @arg1))),
	send(DL, sort).

append_directory(DL, Dir:directory) :->
	"Append a directory-row"::
	get(DL, layout_manager, Table),
	get(Table, row, Table?current?y, @on, Row),
	send(Row, attribute, client, Dir),
	send(Table, append, bitmap(resource(dir))),
	send(Table, append, dir_value_text(0)),
	send(Table, append, dir_value_text(Dir?time)),
	send(Table, append, dir_value_text(Dir?name)),
	send(Table, next_row).

append_file(DL, File:file) :->
	"Append a directory-row"::
	get(DL, layout_manager, Table),
	get(File, base_name, Name),
	file_image(Name, Image),
	get(Table, row, Table?current?y, @on, Row),
	send(Row, attribute, client, File),
	send(Table, append, bitmap(Image)),
	send(Table, append, dir_value_text(File?size)),
	send(Table, append, dir_value_text(File?time)),
	send(Table, append, dir_value_text(Name)),
	send(Table, next_row).

file_type('*.pl',	pce).
file_type(*,		doc).

file_image(File, image(resource(ResName))) :-
	new(Re, regex),
	file_type(Pattern, ResName),
	send(Re, file_pattern, Pattern),
	send(Re, match, File), !.

:- pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class sortable_column_label provides the label   displayed  in the first
(title) row. If the lable is clicked,  it   will  send  a message to the
window to sort the represented table on the named column.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(sortable_column_label, text).

initialise(L, Name:name) :->
	send_super(L, initialise(Name?label_name, font := bold)),
	send(L, name, Name).
	
:- pce_global(@sortable_column_label_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, clicked)))).

event(L, Ev:event) :->
	(   send(L, send_super, event, Ev)
	;   send(@sortable_column_label_recogniser, event, Ev)
	).

clicked(L) :->
	"Clicked.  Send ->sort to the window"::
	get(L, device, Window),
	send(Window, sort, L?name).
	
:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class dir_value_text is a simple  subclass   of  class text, providing a
generic <-compare method on the represented value, thus keeping the code
for sorting the table on a named column generic and simple.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(dir_value_text, text,
		   "Represent a value, providing <-compare").

variable(value, any, get, "Represented value").

initialise(C, Value:any) :->
	send(C, slot, value, Value),
	send_super(C, initialise(Value?print_name)).

compare(N1, N2:dir_value_text, Result) :<-
	get(N1?value, compare, N2?value, Result).

:- pce_end_class.


