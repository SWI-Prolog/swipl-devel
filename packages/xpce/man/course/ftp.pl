/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(ftp,
	  [ ftp/0,			% just start it
	    ftp/1,			% ... and connect as `ftp'
	    ftp/2			% ... and connect as user
	  ]).
:- use_module(library(pce)).
:- require([ concat_atom/2
	   , ignore/1
	   , maplist/3
	   , reverse/2
	   , send_list/3
	   ]).

ftp :-
	new(_, ftp_frame).
ftp(Address) :-
	new(_, ftp_frame(Address)).
ftp(Address, Login) :-
	new(_, ftp_frame(Address, Login)).


		 /*******************************
		 *	  CLASS FTP-FRAME	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Like most tools, the tool as a  whole   is  represented by a subclass of
class frame.  The major advantage of this   approach  is that all visual
parts are in a natural way  `part'  of   the  tool  and each of them can
easily find the overall tool using  <-frame,   which  is defined on most
visual classes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(ftp_frame, frame, "Ftp interaction").

variable(home,  name,		get, "The home directory").

initialise(F, Address:[name], Login:[name]) :->
	"Create frame [and connect]"::
	send(F, send_super, initialise, 'FTP Tool'),
	send(F, append, new(DT, dialog)),
	send(new(P, picture), below, DT),
	send(new(DB, dialog), below, P),

	fill_top_dialog(DT),
	fill_picture(P),
	fill_bottom_dialog(DB),
	send(F, open),
	(   Address == @default
	->  true
	;   send(F, connect, Address, Login)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fill_top_dialog/1 creates the top menu-bar.  There   is only one item in
this incomplete tool.  The ->pen and  ->gap   of  the  dialog window are
zeroed to make the menu-bar appear  nicely above the application window.
A label is put right to the menu-bar.  A `reporter' label is used by the
general ->report mechanism and will  display   all  errors  and warnings
produced by the tool.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_top_dialog(D) :-
	get(D, frame, Tool),

	send(D, pen, 0),
	send(D, gap, size(0,0)),
	send(D, append, new(MB, menu_bar)),
	send(D, append, graphical(width := 20), right), % add space
	send(D, append, label(reporter), right),
	send(MB, append, new(File, popup(file))),

	send_list(File, append,
		  [ menu_item(quit,
			      message(Tool, destroy))
		  ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The application window is a graphics window itself (picture) consists of
a tree holding `ftp_node' objects.  We turned  this into a class because
much of the basic functionality  of   the  tool  (expanding, collapsing,
viewing, etc.  are naturally defined as operations   on the nodes of the
tree.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_picture(P) :-
	send(P, display, new(T, tree(ftp_node(directory, /)))),
	send(T, node_handler, @ftp_node_recogniser).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  bottom-dialog  window  is  used  to    place   some  commonly  used
push-buttons.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_bottom_dialog(D) :-
	get(D, frame, Tool),
	send(D, append, button(abort, message(Tool, abort))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->unlink method is called when the  frame (= tool) is destroyed.  It
ensures that the ftp-connection is terminated before removing the frame.
->unlink *cannot* stop the unlinking  process:   it  must succeed and it
must call ->send_super: unlink.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unlink(F) :->
	"Make sure to kill the process"::
	ignore(send(F, disconnect)),
	send(F, send_super, unlink).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Part of the tool may be found  using   the  <-member method.  It is good
programming practice to make methods for  finding commonly used parts so
that the structure of the tool can remain hidden for the outside world.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tree(F, Tree:tree) :<-
	"Find the tree part of the interface"::
	get(F, member, picture, P),
	get(P, member, tree, Tree).


home(F, Home:name) :->
	"Set the home directory"::
	send(F?tree?root, string, Home),
	send(F, slot, home, Home).


		 /*******************************
		 *	  FTP OPERATIONS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ftp-process is connected using a `hyper' link.  Alternatives are the
use  of  attributes  or  instance-variables.   The  advantage  of  using
hyper-links is that  they  are  bi-directional,   safe  with  regard  to
destruction of either end-point and the semantics of destructions may be
defined on the link rather than on   the  each of the connected objects.
See `hyper ->unlink_to' `hyper ->unlink_from'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ftp(F, P:ftp_process*) :->
	(   P \== @nil
	->  new(_, hyper(F, P, ftp, tool))
	;   get(F, find_hyper, ftp, Hyper),
	    free(Hyper)
	).
ftp(F, P:ftp_process) :<-
	get(F, hypered, ftp, P).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Connect simply creates an `ftp_process' object, which automatically will
login on the remote machine.  After   the  connection is established, it
will obtain the root-directory using the   pwd  command.  See the method
`ftp_process ->pwd' below.  The message  argument   is  the message that
should be called when the ftp's  pwd command completes.  See description
on ftp_process class below for the communication details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

connect(F, Address:name, Login:[name]) :->
	"Connect to address"::
	send(F, disconnect),
	send(F, ftp, new(P, ftp_process(Address, Login))),
	send(P, pwd, message(F, home, @arg1)).


disconnect(F) :->
	"Close possible open connection"::
	(   get(F, ftp, Process)
	->  send(Process, kill),
	    send(F, ftp, @nil)
	;   true
	).

abort(F) :->
	"Send Control-C to the ftp process"::
	get(F, ftp, Process),
	send(F, report, status, 'Sending SIGINT to ftp'),
	send(Process, kill, int).

:- pce_end_class.

		 /*******************************
		 *	       NODE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Node in the ftp-directory/file hierarhy.    Directory  nodes are printed
bold, file nodes in normal roman font.  This class defines handling of a
selection  (inverted  item),  attaching  a  popup  menu  and  the  basic
operations (expand, collapse, view, etc.).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@ftp_node_recogniser, make_ftp_node_recogniser).

make_ftp_node_recogniser(R) :-
	Node = @receiver,
	new(S1, click_gesture(left, '', single,
			      and(message(Node?device, for_all,
					  message(@arg1, inverted, @off)),
				  message(Node, inverted, @on)))),
	new(S2, click_gesture(left, s, single,
			      message(Node, inverted,
				      Node?inverted?negate))),
	new(E1, click_gesture(left, '', double,
			      and(message(Node, expand),
				  message(Node, inverted, @off)))),

	PopNode = @arg1,
	new(P, popup_gesture(new(Pop, popup(options)))),
	send_list(Pop, append,
		  [ menu_item(expand,
			      message(PopNode, expand),
			      condition := PopNode?type == directory),
		    menu_item(collapse,
			      message(PopNode, collapse),
			      condition := not(message(PopNode?sons, empty)))
		  ]),
	new(R, handler_group(S1, S2, E1, P)).


:- pce_begin_class(ftp_node, node, "Node in the ftp-file-tree").

variable(type,	{file,directory},	get,	"Type of the node").


initialise(N, Type:{file,directory}, Name:name, Size:[int]) :->
	"Create from type, name and size"::
	(   Type == directory
	->  Font = font(helvetica, bold, 12)
	;   Font = font(helvetica, roman, 12)
	),
	new(T, text(Name, left, Font)),
	send(N, send_super, initialise, T),
	send(N, slot, type, Type),
	(   Size == @default
	->  true
	;   send(N, attribute, attribute(file_size, Size))
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deduce the path of the node by appending the components upto the root.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

path(N, Path:name) :<-
	"Get path-name"::
	node_path(N, L0),
	reverse(L0, L1),
	insert_separator(L1, /, L2),
	concat_atom(L2, Path).

node_path(N, [Me|Above]) :-
	get(N?image?string, value, Me),
	(   get(N?parents, head, Parent)
	->  node_path(Parent, Above)
	;   Above = []
	).

insert_separator([], _, []).
insert_separator([H], _, [H]) :- !.
insert_separator([H|T0], C, [H, C | T]) :-
	insert_separator(T0, C, T).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expand means: If it is a directory,   cd the ftp-server to the directory
of this node and issue an `ls'   command.  Parse the output line-by line
using the given message.

If the node is a file, `view' it: get   the file in a local tmp file and
start an XPCE view on it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

expand(N) :->
	"Get the sub-nodes"::
	(   get(N, type, file)
	->  send(N, view)
	;   get(N, path, Path),
	    get(N?frame, ftp, Process),
	    send(Process, cd, Path),
	    send(N, collapse),
	    send(Process, ls,
		 message(N, son, create(ftp_node, @arg1, @arg2, @arg3)))
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method ->son is redefined to make sure the new node is visible.  The
method `window ->normalise' ensures the visibility   of a graphical, set
of graphicals or area of the window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

son(N, Node:ftp_node) :->
	"Add a son and normalise"::
	send(N, send_super, son, Node),
	send(N?window, normalise, Node?image).

collapse(N) :->
	"Destroy all sons"::
	send(N?sons, for_all, message(@arg1, delete_tree)).

view(N) :->
	"Read the file"::
	get(N, path, Path),
	get(N?frame, ftp, Process),
	send(Process, view, Path).

:- pce_end_class.


		 /*******************************
		 *	    FTP PROCESS		*
		 *******************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The class ftp_process defines the interaction  with the ftp process.  It
is written such that it can easily be connected in another setting.

Communication is completely asynchronous.  This approach is desirable as
FTP servers sometimes have very  long   response-times  and  there is no
reason to block the interface in the mean-while.

In general, when an ftp-command is issued, it will:

	1) Wait for the ftp-process to get to the prompt.  It
	   dispatches events (keeping other applications happy)
	   while waiting.

	2) Set the `state' variable indicating the command and
	   the `action_message' indicating what to do with the
	   output and finally send the command to the server.

Subsequent data from the ftp-process  will   be  handled  by the ->input
method, which parses the  data  according   to  <-state  and invokes the
<-action_message on some patterns.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(ftp_process, process, "Wrapper around ftp").

variable(state,		name,	get,	"Current command state").
variable(login,		name,	get,	"Login name").
variable(action_message,[code],	get,	"Message to handle output").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the ftp_process.   Noteworthy   are  the  ->record_separator,
which will combine and/or break the   data-records  as received from the
process into lines.  It knows about the   two  prompts and will consider
those to be a separate record.  Finally it preperes the login command.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(P, Host:name, Login:[name]) :->
	"Create ftp process and connect"::
	send(P, send_super, initialise, ftp, Host),
	send(P, record_separator, string('^ftp> \\|^Password:\\|\n')),
	send(P, input_message, message(P, input, @arg1)),
	send(P, slot, action_message, @default),
	send(P, open),
	default(Login, ftp, TheLogin),
	send(P, slot, login, TheLogin),
	send(P, slot, state, login),
	send(P, format, '%s\n', TheLogin).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Password  stuff.   As  a  little  hack,   the  password  entry-field  is
unreadable by using a font that produces   no  readable output.  This is
the simplest solution.  A more elegant  solution   would  be  to use two
text-entry-fields: one that is visible and one that is not visible.  The
visible one could pass all editing commands   to the invisible one to do
the real work.  XPCE is not designed for this kind of things ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

give_pass(P) :->
	"Answer the password prompt"::
	get(P, login, Login),
	getpass(Login, Passwd),
	send(P, format, '%s\n', Passwd).


getpass(ftp, EMail) :- !,
	email(EMail).
getpass(anonymous, EMail) :- !,
	email(EMail).
getpass(User, Passwd) :-
	new(D, dialog('Enter Password')),
	send(D, append, new(T, text_item(User, ''))),
	send(T, value_font,
	     font(screen, roman, 2,
		  '-*-terminal-medium-r-normal-*-2-*-*-*-*-*-iso8859-*')),
	send(D, append, button(ok, message(D, return, T?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm_centered, RVal),
	send(D, destroy),
	Passwd = RVal.

email(EMail) :-
	get(@pce, user, User),
	get(@pce, hostname, Host),
	new(EMail, string('%s@%s', User, Host)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a case where we have  to   help  the reporting system a bit.  By
default, reports for non-visual objects  are   forwarded  to  the visual
object that handles the current event.    On call-backs from the process
input however, there is no current  event   and  messages will go to the
terminal.  The method <-report_to ensures  they   are  forwarded  to the
associated tool.  See also `ftp_frame ->ftp'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

report_to(P, Tool:frame) :<-
	"->report to the associated tool"::
	get(P, hypered, tool, Tool).


		 /*******************************
		 *	   INPUT HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling input from the process (ftp) is the central and most tricky bit
of this application.  The method ->input  will scan through the patterns
for one matching the input (which is broken  in lines).  When a match is
found, the action part is translated into a message:

	* The functor is the selector on this class
	
	* The arguments are arguments to the message.  Arguments of the
	form digit:type are replaced by the n-th register of the regular
	expression converted to the indicated type.

Thus, action(1:name) will invoked `ftp_process  ->action' with the first
register of the regular expression converted to a name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

input(P, Input:string) :->
	get(P, state, State),
	pattern(State, Pattern, Action),
	to_regex(Pattern, Regex),
	send(Regex, match, Input), !,
	Action =.. [Selector|Args],
	maplist(map_pattern_arg(Regex, Input), Args, NArgs),
	Message =.. [send, P, Selector | NArgs],
	Message.
	
pattern(_,
	'^ftp> $',
	prompt).
pattern(ls,
	'[-l].* \\(\\sd+\\) [A-Z][a-z][a-z].* \\([^ \n]+\\)$',
	action(file, 2:name, 1:int)).
pattern(ls,
	'd.* \\([^ \n]+\\)$',
	action(directory, 1:name)).
pattern(ls,
	'^total \\sd+$\\|^\\sd+ bytes received\\|remote: -l',
	succeed).
pattern(pwd,
	'257[^"]*"\\([^"]+\\)',
	action(1:name)).
pattern(login,
	'^Password:',
	give_pass).
pattern(view,
	'226 Transfer complete.',
	action).
pattern(_,
	'\\sd+.*\\.$',
	report(status, 0:string)).
pattern(_,
	'2[35]0-\\(.*\\)',
	message(1:string)).
pattern(_State,
	'.*',
	message(0:string)).
%	report(warning, 'Unrecognised (%s): %s', State, 0:string)).


:- dynamic mapped_to_regex/2.

to_regex(Pattern, Regex) :-
	mapped_to_regex(Pattern, Regex), !.
to_regex(Pattern, Regex) :-
	new(Regex, regex(string(Pattern))),
	send(Regex, lock_object, @on),
	asserta(mapped_to_regex(Pattern, Regex)).

map_pattern_arg(Regex, Input, Reg:Type, Value) :- !,
	get(Regex, register_value, Input, Reg, Type, Value).
map_pattern_arg(_, _, Value, Value).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method ->action is called from ->input   to have data handled by the
caller. Doing

	send(P, pwd, message(Tool, home, @arg1))

will, when the pwd-state  pattern   mathes,  call `ftp_process ->action:
PWD', which in turn  will  execute  the   given  message  using  the PWD
argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

action(P, Args:any...) :->
	"Perform action"::
	get(P, action_message, Msg),
	(   Msg == @default
	->  send(@pce, send_vector, write_ln, Args)
	;   send(Msg, forward_vector, Args)
	).


succeed(_P) :->
	"Just succeed"::
	true.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we get back to the prompt, all input is handled and we will clear the
action_message.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

prompt(P) :->
	"Has reverted to the prompt"::
	send(P, slot, state, prompt),
	send(P, slot, action_message, @default).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Informative messages from the ftp process  are processed by this method.
It pops up a view for displaying the messages.  Note once more the usage
of a hyper, which  ensures  the  user   can  discard  the  view  without
introducing inconsistencies.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

message(P, Message) :->
	"Handle informative messages"::
	(   get(P, hypered, message_view, V)
	->  View = V
	;   new(_, hyper(P, new(View, view('FTP message')),
			 message_view, ftp)),
	    send(View, confirm_done, @off),
	    send(new(D, dialog), below, View),
	    send(D, append, button(quit, message(View, destroy))),
	    send(D, append, button(clear, message(View, clear))),
	    send(View, open)
	),
	send(View, appendf, '%s\n', Message).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  method  ->wait_for_prompt  runs  the  main  XPCE  event-loop  using
`display->dispatch' until the ftp-process gets in the `prompt' <-state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

wait_for_prompt(P) :->
	"Process input till prompt"::
	send(P, report, progress, 'Waiting for ftp prompt'),
	repeat,
	(   get(P, state, prompt)
	->  !,
	    send(P, report, done)
	;   send(@display, dispatch),
	    fail
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The basic commands.  Given all  the   infra-structure  above,  these are
quite simple now!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ls(P, Msg:[code]) :->
	send(P, wait_for_prompt),
	send(P, slot, state, ls),
	send(P, slot, action_message, Msg),
	send(P, format, 'ls -l\n').


pwd(P, Msg:[code]) :->
	send(P, wait_for_prompt),
	send(P, slot, state, pwd),
	send(P, slot, action_message, Msg),
	send(P, format, 'pwd\n').


cd(P, Dir:char_array) :->
	send(P, wait_for_prompt),
	send(P, slot, state, cd),
	send(P, format, 'cd %s\n', Dir).


view(P, Path:char_array) :->
	"Start view on contents of file"::
	send(P, wait_for_prompt),
	new(Tmp, string('/tmp/xpce-ftp-%d', @pce?pid)),
	send(P, slot, action_message, message(P, make_view, Path, Tmp)),
	send(P, slot, state, view),
	send(P, format, 'get %s %s\n', Path, Tmp).


make_view(_P, RemoteFile:name, LocalFile:file) :->
	"Make a view for displaying file"::
	new(V, view(string('FTP: %s', RemoteFile))),
	send(V, load, LocalFile),
	send(LocalFile, remove),
	send(V, confirm_done, @off),
	send(V, open).

:- pce_end_class.
