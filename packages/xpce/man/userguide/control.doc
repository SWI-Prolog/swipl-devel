\section{Control-structure of \productpl{} applications} \label{sec:control}

\index{control,structure}%
This section deals with the control-structure of interactive
applications written in \productpl{}. Interactive graphical
applications are very different from terminal oriented applications.
Terminal oriented applications often have a top level control structure
of the form:

\begin{code}
go :-
	initialise,
	main_loop.

main_loop :-
	present_question,
	read_answer(Answer),
	process_answer(Answer),
	main_loop.
\end{code}

This schema is often refined with sub-loops dealing with
question/answers in a specific context.

Many interactive graphical applications present various UI components
simultaneously: the user is free on which component s/he wants to
operate next.  The users actions (keyboard-typing, mouse movement, and
mouse-clicking) must be related to the correct UI component and
interpreted accordingly in the application.  This interpretation is much
more complex than the interpretation of a stream of ASCII characters
typed by the user.

\subsection{Event-driven applications}

\index{event-driven}%
One approach is to write a main-loop that reads events, locates the
UI-component referred to and executes the appropriate actions.  This
loop, which must take care of repaint-requests, various local feedback
procedures (changing the mouse-cursor, inverting objects, etc.), is
complicated.  The approach taken by most graphical programming systems
including \plainproduct{}, is to move this loop into the infra-structure (i.e.\ into
the \plainproduct{} kernel).  The application programmer creates the desired
UI-components and supplies code fragments that will be called by the
main-loop when a certain event happens.  This control-structure is
called {\em event-driven} control.  Consider a button:

\begin{code}
1 ?- new(B, button(hello,
		   message(@pce, write_ln, hello))),
     send(B, open).
\end{code}

In this example the application creates and displays a button UI
component and associates a code fragment (the message) to be executed
when the button is pressed.  The \plainproduct{} kernel will loop through the main
event-loop.  For each event it will locate the UI component that should
handle the event.  When the button has recognised a `click' it will
execute the code fragment attached to it.  This behaviour is part of the
definition of class \class{button}.

It is clear that this approach relieves the application programmer of
many of the complications associated with event-processing.  As a
consequence, the `main-loop' of a \plainproduct{} application is no longer in the
application itself, but in the \plainproduct{} kernel.  Below is an outline of
the control structure of a \productpl{} application:

\begin{code}
go :-
	initialise_database,
	create_ui_components.

handle_help_pressed :-
	create_help_window.

handle_solve :-
	solve_the_problem,
	create_solution_window.

...
\end{code}

The predicate go will exit after it has initialised the application and
created the UI components.  Assuming the application window has a button
invoking the predicate handle_help_pressed, \plainproduct{} will call this
predicate when the user presses the help button.


\subsubsection{Keeping control}

The application code often wants to wait for the user to finish an
interaction. In \secref{modal}, we have seen a simple way of programming
this using `frame <-confirm'.  In this section, we will provide some
other options.

\paragraph{Message Queue} One possibility is to fall back to the \product{} 1
and 2 compatibility, where @prolog implements a queue of messages.
@prolog is an instance of class \class{host}. The relevant methods are:

\begin{description}
    \sendmethod{host}{call_back}{bool}
The default is @on. In this case, a message to @prolog is translated
into a predicate call on the Prolog engine.  If @off, a message is
appended to the `host <-messages' queue.
    \sendmethod{host}{catch_all}{Selector:name, Arg:any...}
If <-call_back equals @on, use the \var{Selector} to determine the
predicate to call, and the arguments to construct the argument vector
for the predicate.  Call the predicate and succeed or fail according to
success or failure of the Prolog predicate.

If <-call_back equals @off, create a \class{message} of the form

\begin{quote}
	message(@prolog, \var{Selector}, \var{Arg} ...)
\end{quote}

and append this message to the <-messages queue.
    \getmethod{host}{message}{}{message}
Return the <-head of the <-messages queue. If the queue is empty, ensure
<-call_back is (temporary) set to @off, and dispatch events using
`@display ->dispatch' as long as the <-messages queue is empty.
\end{description}

Note that it is possible to create multiple instances of class
\class{host}, to realise multiple message queues.  It is not
desirable to modify the @prolog host object, as other code may
rely on the <-call_back properties of @prolog.

\begin{quote}
{\bf Warning\hspace{10pt}} During normal operation, event processing
guards the objects created that are not assigned to any other object and
destroys all such objects after the event has completely been processed
(see \secref{memory}. Using the host message queue mechanism, the Prolog
programmer becomes responsible for such objects. For example, the
message object returned should be discarded using `object ->done' after
processing.
\end{quote}

\paragraph{Explicit dispatching} An alternative to the above, and the
`frame <-confirm' mechanism is to dispatch the events yourself.  This
is realised using \exam{send(@display, dispatch)}, described below.
This mechanism is the base of all the others.  It should be used to
realise different interaction schemas than the default callback
schema.

\begin{description}
    \sendmethod{display}{dispatch}{}
Process events and return on any of the following conditions
    \begin{itemlist}
	\item [event has been processed]
	Either a normal event, a timer or an input stream has been
	processed.  The method fails in this case.
        \item [timeout]
	The timeout (see `display_manager ->dispatch') has expired.
	The method fails in this case.
	\item [Input on the console]
	There is input from the Prolog window.  The message succeeds
	in this case.
    \end{itemlist}

For example, the following processes events in call-back style until
the fact quit/0 is in the Prolog database:

\begin{pcecode}
:- dynamic
	quit/0.

process_to_quit :-
	repeat,
	    send(@display, dispatch),
        quit, !.
\end{pcecode}
\end{description}


\subsection{\plainproduct{} and existing applications}

Due to the different control-regime described in the previous section,
traditional terminal oriented applications are not easily transformed
into \productpl{} graphical applications.  Depending on the application,
there are two ways to proceed.

The first is to keep the existing control-regime.  This implies that
the questions asked on the terminal will be replaced by \idx{modal}
dialog windows.  The main loop will be:

\begin{code}
go :-
	initialise_database,
	create_dialog(Dialog).

main_loop(Dialog) :-
	fill_dialog_with_next_question(Dialog),
	send(Dialog, fit),
	get(Dialog, confirm, Answer),
	process_answer(Answer),
	main_loop(Dialog).
\end{code}

This example reuses the same dialog window for all questions.  It is
trivial to change this loop to use a new dialog window for each
question. Output from the program may be presented in other windows.
The approach does not exploit the potentially larger freedom for the
user that is possible in graphical user interfaces.

If the application could be viewed as a number of commands operating
on some data-structure and this data-structure is stored on the Prolog
heap using assert/1 or recorda/2 one could consider rewriting the
toplevel control and provide a more flexible interface.


