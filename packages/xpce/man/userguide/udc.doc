\chapter{Defining classes}			\label{sec:udc}

The user defined class interface provides a natural way to define new
\product{} classes. It is both used to create higher level libraries that have
the same interface as the built-in \product{} classes as to define entire
applications. Many of the library modules and \productpl{} demo programs
are implemented as user-defined classes. The PceDraw demo is an
elaborate example defined entirely in user-defined classes.

A user defined class lives in \product{}, just as any other \product{} class. There
is no difference. Both use dynamic resolution of messages to method
objects and then execute the method object. Both use the same
object-management and storage facilities.

\productpl{} user-defined classes have their methods implemented
in Prolog.  This provides a neat and transparent interface between
the two systems.%
    \footnote{\product{} defines four implementation techniques for methods.
    {\em C-function pointers} are used for almost all the built-in
    behaviour. {\em C++-function pointers} are used when classes are
    defined in C++\ifpw{}{ (\cite{XPCE:cpp})}. Instances of
    \class{c_pointer} are left to the host object for interpretation and
    finally, \class{code} objects are executed.}

User defined classes are defined using Prolog syntax, where some
operators have special meaning. The definition of an \productpl{} class
is enclosed in

\begin{code}
:- pce_begin_class(<Class>, <Super> [, <Comment>]).
<Class definition>
:- pce_end_class.
\end{code}

Multiple classes may be defined in the same Prolog source file, but class
definitions may not be nested.


\section{The class definition skeleton}

We introduce the syntax for user-defined classes using a skeleton.
Except for the pce_begin_class/[2,3] and pce_end_class/0, everything in
the skeleton is optional and may be repeated multiple times. The order
of declarations is not important, but the order of the skeleton is the
proposed order.  An exception to this rule is the pce_group/1 directive,
that may be placed anywhere and defines the group-identifier for the
declarations that follow.  The skeleton is given in \figref{udcskeleton}.

\begin{figure}
\hr
\parindent 0pt
\parskip 1.5ex
\newcommand{\opt}[1]{{\it [#1]}}		% optional
\newcommand{\zom}[1]{{\it \{#1\}}}		% zero-or-more
\newcommand{\tab}{\makebox[6ex]{}}
:- pce_begin_class(\opt{<Meta>:}<Class>\opt{(\zom{<TermName>})}, <Super>\opt{, <Summary>}).

:- use_class_template(<TemplateClass>).\\
:- send(@class, <Selector>\zom{, <Arg>}).\\
:- pce_class_directive(<Goal>).

variable(<Name>, <Type>\opt{:= <Value>}, <Access> \opt{, <Summary>}).

delegate_to(<VarName>).

class_variable(<Name>, <Type>, <Default> \opt{, <Summary>}).

handle(<X>, <Y>, <Kind>, <Name>).

:- pce_group(<Group>).

<SendSelector>(<Receiver>\zom{, <Arg>\opt{:\opt{<AName>=}<Type>}}) \verb$:->$\\
	\tab\opt{<Summary>::}\\
	\tab<PrologBody>.

<GetSelector>(<Receiver>\zom{, <Arg>\opt{:\opt{<AName>=}<Type>}}, <RVal>\opt{:<Type>}) \verb$:<-$\\
	\tab\opt{<Summary>::}\\
	\tab<PrologBody>.

:- pce_end_class.
\hr
\caption{Skeleton for user-defined classes}
\label{fig:udcskeleton}
\end{figure}

\subsection{Definition of the template elements}

\begin{description}
    \directive{pce_begin_class}{[2,3]}{+[Meta:]Class, +Super, [+Summary]}
Start the definition of an \product{} user-defined class.  This directive can
appear anywhere in a Prolog source file.  The definition must be closed
using pce_end_class/0 and definitions may not be nested.  {\em Class}
describes the class to be created.  Besides giving the class-name, the
meta-class (class of the class) may be specified.  When omitted, 
the meta-class of the {\em Super} will be used, which is normally
class \class{class}.  An example of meta-class programming can be found
in PceDraw's file {\tt shape.pl}\ifpw{}{, see \cite{XPCE:draw}}.

The class-name may be followed by a list of {\em TermNames} that define
the result of object/2.  object/2 unifies its second argument
with a term whose functor is the name of the class and whose arguments
are the result of a `get' operation using the {\em TermName} as selector.
For example, \mbox{\tt point(x,y)} specifies that object(P,~T) unifies
{\em T} to a term point\mbox{}/2 with the <-x and <-y of the point instance
as arguments.  When omitted, the term-description of the super-class
is inherited.
    \directive{use_class_template}{1}{TemplateClass}%
Import a class template.  See \secref{template}.
    \directive{send}{[2-12]}{@class, ...}%
Directives like this may be used to invoke methods on the class under
construction. This can be used to modify the class in ways that are not
defined by this preprocessor. The following example tells the system
that the `visual' attribute of an imaginary user-defined class should
not be saved to file when the object is saved using
`object->_save_in_file'.

\begin{code}
:- send(@class, save_style_variable, nil).
\end{code}

See also pce_class_directive/1 and \secref{ucdimplementation}.
    \directive{pce_class_directive}{1}{+:Goal}%
Define {\em Goal} to be a goal that manipulates the class instance
directly.  See \secref{ucdimplementation}.

    \predicate{variable}{[3,4]}{Name, Type, Access, [Summary]}%
Define a new instance variable. {\em Name} is the name of the variable,
which is local to the class and its subclasses. {\em Type} defines the
type. See \secref{atype} and \secref{moretypes}. The type may be
postfixed with \mbox{{\tt :=} \arg{Value}} to specify an initial value.
If \arg{Value} can be modified (i.e. is not a \class{constant}, int or
\class{name}) it is often desirable to use \mbox{{\tt :=}
\term{new}{NewTerm}} to force each instance to create its own unique
copy of the initial value. {\em Access} defines which implicit {\em
universal} methods will be associated with the variable. A universal
method is defined to be a method that reads or writes the slot, without
performing any additional actions. See also \secref{slots}.

    \predicate{delegate_to}{1}{VariableName}%
Declares the variable named {\em VariableName} to be a candidate for
delegation.  See \secref{delegation}.

    \predicate{class_variable}{[3,4]}{Name, Type, Default, [Summary]}%
Declare a class-variable for the class. Class-variables describe common
properties for all instances of the class. The \arg{Default} value for a
class-variable can de defined in the \file{Defaults} file. See
\chapref{classvar} for details.

The {\em Default} entry describes the default value if there is no
value specified in the \file{Defaults} file.   Example:
\begin{code}
class_variable(size, size, size(400,200), "Default size of object").
\end{code}
    \predicate{handle}{3,4}{X, Y, Kind, Name}%
Equivalent to the expression below.  See also \secref{connection}.
\begin{code}
:- send(@class, handle, handle(X, Y, Kind, Name)).
\end{code}
    \directive{pce_group}{1}{GroupIdentifier}%
Sets the `behaviour <->group' attribute of any variable or method
definition following this directive.  Groups are used to organise
methods by the ClassBrowser.  Groups have no semantic implications.
\mbox{\tt :- pce_group(@default).} makes methods inherit their group
from the method that is re(de)fined.  If no method is re(de)fined,
the group will be {\tt miscellaneous}.
    \directive{pce_end_class}{1}{Class}
End the definition of the named \arg{Class}.  \arg{Class} must be the
same as the class-name used by the most recent pce_begin_class/[2,3].
This variation of pce_end_class/0 provides better documentation and
error correction.
    \directive{pce_begin_class}{0}{}
Close the definition of the most recently started class.  See also
pce_end_class/1.
\end{description}

\subsubsection{Syntax details}

Table \tabref{udcsyntax} describes the details of the non-terminals in
the above skeleton in more detail. The notation is an incomplete BNF
notation.

\begin{table}
\newcommand{\ttvbar}{\tt\string|}
\newlength{\dwidth}
\setlength{\dwidth}{\linewidth / 2}
\begin{tabular}{|lrl|p{\dwidth}|}
\hline
<Meta>		\isa <Name>	&    Name of the class this class
				     will be an instance of.  Default
				     is the meta-class of the
				     super-class \\
<Class>		\isa <Name>	&    Name of the class to be defined \\
<TermName>	\isa <Name>	&    Selector name to fetch object/2
				     argument.  For example, a point
				     is translated into point(<X>, <Y>) and
				     the description is {\tt point(x,y)} \\
<Super>		\isa <Name>	&    Name of the super-class.  {\tt object}
				     refers to the most general class \\
<Summary>	\isa {\tt "}\{<Char>\}{\tt "}
				&    Summary description as appearing in
				     the online manual.  $< 40$ characters,
				     no newlines, Prolog string \\
<TemplateClass>	\isa <Name>	&    Import a template class.  See
				     \secref{template} \\
<Selector>	\isa <Name>	&    Name of a method \\
<X>		\isa <IntExpr>	&    See class \class{handle} \\
<Y>		\isa <IntExpr>	&    See class \class{handle} \\
<Kind>		\isa <Name>	&    Category indicator.  See class
				     \class{handle} \\
<Access>	\isa \parbox[t]{4em}{\raggedright
				     \const{both} \ttvbar{}
				     \const{get} \ttvbar{}
				     \const{send} \ttvbar{}
				     \const{none}}
				&    Defines the access right to this
				     variable \\
<VarName>	\isa <Name>	&    Name of variable used for
				     delegation \\
<Group>		\isa <Name>	&    Functional group of the following
				     methods or variables.  Used to
				     organise the ClassBrowser \\
<SendSelector>	\isa <Name>	&    Name of send-method to define \\
<GetSelector>	\isa <Name>	&    Name of get-method to define \\
<Receiver>	\isa <Variable>	&    Prolog variable bound to the receiver \\
<Arg>		\isa <Variable> &    Prolog variable bound to argument \\
<RVal>		\isa <Variable>	&    Prolog variable that should be bound
				     to the return value \\
<AName>		\isa <Name>	&    \product{} name for named argument \\
<Type>		&&		&    See \secref{atype} and
				     \secref{moretypes} \\
<PrologBody>	&&		&    Ordinary Prolog code \\ 
<Value>		&&		&    Initial value for the instance variable.
				     At this moment, only using constants
				     is supported (int, name, bool) \\
\hline
\end{tabular}
\caption{Syntax details for User Defined Classes} \label{tab:udcsyntax}
\end{table}


\section{Accessing instance variables (slots)}	\label{sec:slots}

The method `object <->slot' is used to access slots directly, bypassing
possible methods with the same name. Normally, it should only be used in
->initialise (see below) and when defining a method with the same name
as a variable. Below is a fragment where a {\tt type} slot is displayed
by a text object named {\tt type} in a graphical object. This variable
has access {\tt get}, associating a universal method <-type that yields
the current value of the slot. The implementation of ->type uses the
->slot method to write the argument in the <-type slot and subsequently
performs the required side-effects. The ... indicate where the fragment
is incomplete.

\begin{code}
variable(type,	name,	get, "Epistemological type").

initialise(D, Type:name, ...) :->
	send_super(D, initialise),
	send(D, slot, type, Type),
	send(D, display, new(T, text(Type))),
	send(T, name, type),
	...

type(D, Type:type) :->
	"Modify the epistemological type"::
	send(D, slot, type, Type),
	get(D, member, type, Text),
	send(Text, string, Type).
\end{code}

\begin{description}
    \sendmethod{object}{slot}{name, unchecked}
    \getmethod*{object}{slot}{name}{unchecked}%
Read or write slot without side-effects.  The value will be converted to
the type of the instance variable addressed.  An error is raised if this
conversion is not defined or if the slot does not exist.
\end{description}


\section{Refining and redefining methods}

Re(de)fining methods is a common technique in object-oriented
programming. This section describes how methods can be re(de)fined and
what methods have special meaning in \product{} and are commonly redefined.

The method definition for a re(de)fined method is exactly the same
as for a new method.  The redefined method will inherit its group
(see pce_group/1) from the method of the super-class.

When refining a method we often want to call the method of our
super-class. For this reason there are two additional interface
predicates to access the behaviour of a specific class. In 99\% of the
cases we wish to invoke a method of the immediate super-class. For this
reason the class-compiler realises compile-time rewrite of
send_super/[2-12] and get_super/[3-13] to send_class/2 and get_class/3.

\begin{description}
    \predicate{send_class}{3}{+Object, +Class, +Message}
Invoke \arg{Message} on \arg{Object} using the implementation defined
with class \arg{Class}.  \arg{Class} must be the actual class of
\arg{Object} or one of its super-classes or an error is raised.

    \predicate{get_class}{4}{+Object, +Class, +Message, -Result}
This is the get-equivalent of send_class/3.

    \predicate{send_super}{2}{+Object, +Message}
The class-compiler converts goals of this format to an appropriate
send_class/3 call. Note that it is not possible to provide predicates as
an alternative to the compile-time expansion and therefore meta-calls
cannot use send_super/2.

    \predicate{get_super}{3}{+Object, +Message, -Result}
This is the get-equivalent of send_super/2.
\end{description}

Similar as the predicates send/2 and get/3 may be written as send/[3-12]
and get/[4-13] this is possible for send_super/2 and get_super/3.  In
addition the pre-5.0 `object->send_super' and `object<-get_super' are
expanded to send_class/2 and get_class/3.  The following calls are
all equivalent.  The last one should not be used by new code.

\begin{pcecode}
	send_super(Object, my_method(Arg1))
	send_super(Object, my_method, Arg1)
	send(Object, send_super, my_method, Arg1)
\end{pcecode}


\subsection{General redefinitions}	\label{sec:omsmethods}

The most commonly redefined methods are ->initialise and ->unlink to
redefine object creation and destruction.  {\bf Note that none of these
methods should ever be invoked directly on an object}, because the
implementation often makes assumptions that are only true in the context
they are normally invoked by the kernel.

\begin{description}
    \sendmethod{object}{initialise}{<Class-Defined>}
Initialise a new instance of the class. The initialisation is not
allowed to access behaviour or slots of the super-class without invoking
the ->initialise on th super-class. Omitting is a common source of
errors, often leading to crashes.

The initialise method should initialise all slots declared in this
class that have no specified value in the variable declaration and
cannot have the value @nil.  See also checkpce/0.

If ->initialise fails, the exception {\tt initialise_failed} will be
raised, passing the instance and the argument vector.  Afterwards, 
the (possible named) reference is destroyed and the object's slots are
reset to @nil. Finally, the instance is deallocated.  ->unlink (see below)
is not called.  In general, it is not good programming style to let
->initialise fail.
    \sendmethod{object}{unlink}{}%
Called from the object-management system if the object is to be
destroyed. This method {\em must} call ->unlink of the super-class
somewhere in the process. It is an error if ->unlink fails.

This method is normally used to unlink the object from related objects.
For example, \class{graphical} objects use it to remove themselves from
their device if they are displayed. There is no need to reset
slot-values as dereferencing the slot-values will be done by the
object-management system after ->unlink has finished.

->unlink is always called, whether the object was destroyed using ->free
or by the garbage-collector.
    \getmethod{object}{convert}{<Class-Defined>}{Instance}
This get method converts another object into an object of this class. It
is called by the type-checker.  Suppose an object {\em X} is handed to
the type checker for checking against this class.  If {\em X} is not
already an instance of this class or any of its subclasses, the type
checker will:
\begin{shortlist}
    \item Check {\em X} against the <Class-Defined> type.
    \item Run this method, passing the (possibly converted) {\em X}.
\end{shortlist}

The receiver is not defined during the execution of this method. The
method should either fail or succeed and return an instance of the
requested class or one of its super-classes. The argument vector
consists of a single argument. The type-conversion system guarantees the
argument is of the satisfied type. It is allowed, but not obligatory to
use the method of the super-class.

For example, suppose we are defining a class {\em person}, who has
a unique name.  There is a table @persons, that maps the name onto
the person.  We would like to be able to pass the name rather then
a person instance to a method argument with the type {\tt person}.
If no such person exist, a new person instance is created.  Below
is the implementation for this:
\begin{code}
convert(_, Name:name, P:person) :<-
	"Lookup from @persons or create a new one"::
	(   get(@persons, member, Name, P)
	->  true
	;   new(P, person(Name))
	).
\end{code}
See also <-lookup described below.
    \getmethod{object}{lookup}{<Class-Defined>}{Instance}
Called from the new() virtual machine operation to deal with {\em
reusable} objects before ->initialise is considered. The arguments are
normally the same as for ->initialise. If this method returns an instance,
this will be the value returned by new().  If it fails, a new instance
is allocated and ->initialise{}d.
\end{description}


\subsection{Redefinition in graphical classes}
\label{sec:graphmethods}

The generic graphical class \class{graphical} is prepared to have
several of its methods redefined in subclasses. This section describes
the most important of these methods.

\begin{description}
    \sendmethod{graphical}{event}{event}
Called when a user-event needs to be dispatched. This message is
initially sent to the window object receiving the event. Graphical
devices (and thus windows) collect all graphicals for which `graphical
->in_event_area' succeeds. These are normally all graphicals that overlap
with the current position of the pointer. It will sort these objects to
their stacking order, the topmost object first. See `device <-pointed'.
Next the device will use `event->post' to post the event to each of
these graphicals until one accepts the event, after which the method
immediately returns success. If none of the <-pointed objects is
prepared to accept the event, `graphical->event' will be invoked, trying
all he \class{recogniser} objects associated with this graphical.

Notably most subclasses of class \class{dialog_item}, the standard
controllers, refine ->event.

The method ->event is commonly redefined in user-defined graphicals to
make them sensitive to the mouse.  The following fragment of a class
definition makes it possible to resize and move instances.

\begin{code}
:- pce_global(@resize_and_move_recogniser,
	      new(handler_group(new(resize_gesture),
				new(move_gesture)))).

event(Gr, Ev:event) :->
	"Make the object re-sizeable and movable"::
	(   send_super(Gr, event, Ev)
	;   send(@resize_and_move_recogniser, event, Ev)
	).
\end{code}

Note that the implementation first tries the super-class. If the
super-class has no specific event-handling, this allows recognisers to
be attached that overrule the resize/move behaviour. Also, if it is a
\class{device}, invoking the super-class behaviour will test components
displayed on the device to be considered before the device as a whole.

It is not obligatory to use ->event on the super-class and if it is
used, no specific ordering is required. If there is no behaviour of the
super-class that conflicts with your extension we recommend to try
the super-class first, to ensure recognisers and local event-processing in
graphicals displayed on a device with redefined event-processing are
considered before your extensions.

Note the way recognisers are activated from event methods.  The
graphical object itself is not passed.  Instead, `recogniser->event'
reads the receiver from `event <-receiver' set by `event ->post'.

As a consequence, do not call `graphical ->event' directly.  An event
is directed to a graphical using `event ->post'.  For example, the
event-method of a device displaying an editable text object may decide
to forward all button and keyboard events to the text.  The following
accomplishes this:

\begin{code}
event(D, Ev:event) :->
	(   (   send(Ev, is_a, button)
	    ;   send(Ev, is_a, keyboard)
	    )
	->  % assumes text is named `text'
	    get(D, member, text, Text),	
	    send(Ev, post, Text)
	;   send_super(D, event, Ev)
	).
\end{code}
    \sendmethod{graphical}{geometry}{X:[int], Y:[int], W:[int], H:[int]}
Requests the receiver to position itself at the $X,Y$ and to be $W
\times H$ pixels in size. Any of these values may be @default,
indicating that the specific parameter is not to be changed.

Redefining ->geometry is the proper way to interfere with positioning or
resizing as this is the central method called by all move and resize
methods.

The example below takes the text-box to ensure proper geometry handling
by this class.  Note that (I) the size of a device is by definition the
bounding box of all displayed graphicals and (II) the text must be
centered again.

\begin{code}
geometry(D, X:[int], Y:[int], W:[int], H:[int]) :->
	get(D, member, box, B),
	get(D, member, text, T),
	send(B, set, @default, @default, W, H),
	send(T, center, B?center),
	send_super(D, geometry, X, Y).
\end{code}

\index{constraint,vs. method}
Note that the relation between the text and the box could also be
maintained using a \class{constraint} object.  The above implementation
however is only executed when the geometry of the device is changed,
while constraints will be executed whenever a message arrives on the
box or text.
    \sendmethod{graphical}{request_geometry}{X:[int], Y:[int], W:[int], H:[int]}
Is much like ->geometry, except that the interpretation of the units is
left to the graphical.  For example \class{editor} will use the current
font to translate $W$ and $H$ to pixels and then invoke ->geometry.  Not
used very often.
    \sendmethod{graphical}{compute}{}
This method cooperates with ->request_compute and may be used to delay
expensive graphical operations. Suppose we have a graphical
representation and a database object linked using a \class{hyper} like
this:

\begin{code}
new(_, hyper(Db, Gr, controller, model))
\end{code}

If the database object (model) is modified, it could use the following
to inform all associated controllers about the change:

\begin{code}
send(Db, send_hyper, controller, request_compute)
\end{code}

\product{} remembers that the state of this graphical is not consistent.  If
\product{} requires the graphical to be in a consistent state, either because
it needs to paint the graphical or because it requires information about
the geometry of the graphical, it will invoke the method ->compute on
the graphical.

This mechanism is used by graphicals that have a complicated structure
and are difficult to update. An example in the built-in classes is class
\class{text_image}, displaying the text of an \class{editor}. Any
modification to the text in the displayed region of the text_image
requires expensive computation to recompute the layout of the text.
Suppose the ->request_compute and ->compute mechanism is not available.
It this case, multiple modifications by the program to the text would
require this expensive process to run several times. Now, after
modifying the text, ->request_compute is invoked on the text_image.
Whenever \product{} has processed all pending events, it will invoke ->compute
to the text_image and then repaint it.

The method below is a far to simple example, where the ->compute method
simply copies the name of the represented object into the text object
displayed on the device ->compute is defined on.

\begin{code}
compute(C) :->
	"Update according to model"::
	get(C, get_hyper, model, name, Name),
	get(C, member, text, T),
	send(T, string, Name),
	send_super(C, compute).
\end{code}
    \sendmethod{graphical}{_redraw_area}{area}
Called by the graphical repaint thread.  Its task is to repaint itself.
{\em Area} indicates the area in the device coordinate system that needs
to be repainted.  This area overlaps with the <-area of the device.

Exploitation of this method to realise new graphical primitives is explained
in \secref{ugraphics}.
\end{description}


\section{Handling default arguments}			\label{sec:udcpreds}

The predicate default/3 provides a comfortable way to specify the
meaning of default arguments.  Future versions may incorporate the
default value in the \class{type} object itself.

\begin{description}
    \predicate{default}{3}{+Argument, +Default, -Value}
Used to specify and compute defaults for arguments.  {\em Argument}
is the actual argument passed to the method implementation, {\em
Default} is any valid \product{} object description (reference, integer,
real, atom or compound ground term describing an object, see
send/[2-12]).  {\em Default} can also be the term
\begin{code}
resource(<Object>, <Name>)
\end{code}
In which case the <-resource_value: <Name> from <Object> will be
used as default value.  {\em Value} is unified with {\em Argument}
if {\em Argument} is not @default and with {\em Default} otherwise.

The following is an example that sets the volume to a specified value or
the value of the resource `volume' if @default is passed as an argument.
\begin{code}
resource(volume, 0..130,  75, "Volume in decibels").

volume(X, Vol:[0..130]) :->
	default(Vol, resource(X, volume), V),
	<set the volume here>.
\end{code}
\end{description}


\section{Advanced topics}

\subsection{More on type declarations}		\label{sec:moretypes}

The basic \product{} type-syntax is described in \secref{atype} of
this manual.  Types are first-class reusable \product{} objects that are
created from the type-declaration in arguments and variables.  The
conversion from the textual representation to the object is performed
by \product{} itself (together with the resource syntax, one of the few
places where \product{} defines syntax).  All types can be specified as
Prolog quoted atoms.  For example:

\begin{code}
mymethod(Me, A:'graphical|dict_item|0..') :->
	...
\end{code}

For most cases however, this is not necessary.  If the type is not an
atom, the class-compiler will convert the Prolog term into an atom
suitable for \product{}'s type system.  Hence, {\tt [point]} will translate to
the atom '[point]', which is interpreted by \product{} as ``an instance of
class \class{point} or the constant @default''.  The atoms {\tt *} and
{\tt ...} are defined as postfix operators, while {\tt ..} is an infix
operator.  This makes `\mbox{\tt any ...}' a valid notation for ``any
number of anything'' (see \secref{varargs} below) and `{\tt
0..5}' a valid expression for ``an integer in the range 0 to 5
(including the boundaries).

Also, {\tt [box|circle]} is a valid description for ``an instance of
\class{box} or \class{circle} or the constant @default. Note however
that {\tt [box|circle|ellipse]} is {\em not} valid Prolog syntax and
should be written as {\tt '[box|circle|ellipse]'}. Whenever you are in
doubt, use quotes to prevent surprises.


\subsection{Methods with variable number of arguments} \label{sec:varargs}

\index{variable number of arguments}\index{vararg}\index{stdarg}%
Methods such as `chain->initialise' and `string->format' handle an
arbitrary number of arguments. The argument declaration for such a
method first defines a number (possibly zero) of `normal' arguments. The
last argument is postfixed with `{\tt ...}'.   The arguments assigned
to the `vararg' type are passed in a Prolog list.

Below is a refinement of `label ->report' that will colour the label
depending on the nature of the message. The ->report method takes two
obligatory arguments, the {\em kind} of the report and a {\em format}
string, as well as an undefined number of arguments required by the
format specification.

\begin{pcecode}
:- pce_begin_class(coloured_reporter, label,
		   "Coloured reporter label").

report(L, Kind:name, Format:char_array, Args:any ...) :->
	Msg =.. [report, Kind, Format | Args],
	send_super(L, Msg),
	get(L, colour_from_report_category, Kind, Colour),
	send(L, colour, Colour).

colour_from_report_category(L, Kind:name, Colour:colour) :<-
	<left to the user>.

:- pce_end_class.
\end{pcecode}


\subsubsection{Using class templates}		\label{sec:template}

\product{} provides two alternatives to multiple inheritance. Delegation is
discussed in \secref{delegation}. See also the directive
delegate_to/1 for user-defined class definitions.  The {\em template}
mechanism is much closer to real multiple inheritance.  A template
is a named partial class-definition that may be included in other
classes.  It behaves as if the source-code of the template definition
was literally included at the place of the use_class_template/1
directive.

In fact, the class-attributes (variables, method objects) are {\em
copied}, while the implementation (the Prolog clauses) are {\em shared}
between multiple references to the same template.

Templates itself form a hierarchy below class \class{template}, which
is an immediate subclass of \class{object}.  Including a template will
make all variables and methods defined between the template class and
class \class{template} available to the receiving class.

We illustrate the example below, making both editable boxes as editable
ellipses.  First we define the template class.

\begin{pcecode}
:- use_module(library(pce_template)).

:- pce_begin_class(editable_graphical, template).

:- pce_global(@editable_graphical_recogniser,
	      make_editable_graphical_recogniser).

make_editable_graphical_recogniser(G) :-
	Gr = @arg1,
	new(Dev, Gr?device),
	new(P, popup),
	send_list(P, append,
		  [ menu_item(cut, message(Gr, free)),
		    menu_item(duplicate,
			      message(Dev, display, Gr?clone,
				      ?(Gr?position, plus,
					point(10,10))))
		  ]),
	new(G, handler_group(new(resize_gesture),
			     new(move_gesture),
			     popup_gesture(P))).


event(G, Ev:event) :->
	(   send_super(G, event, Ev)
	;   send(@editable_graphical_recogniser, event, Ev)
	).
:- pce_end_class.
\end{pcecode}

The main program can now be defined as:

\begin{pcecode}
:- require([use_class_template/1]).

:- pce_begin_class(editable_box, box).
:- use_class_template(editable_graphical).
:- pce_end_class.

:- pce_begin_class(editable_ellipse, ellipse).
:- use_class_template(editable_graphical).
:- pce_end_class.

test :-
	send(new(P, picture('Template Demo')), open),
	send(P, display,
	     editable_box(100,50), point(20,20)),
	send(P, display,
	     editable_ellipse(100, 50), point(20, 90)).
\end{pcecode}


Note that use_class_template/1 {\em imports} the definitions from the
template in the current class.  Thus, the following {\bf will not 
extend} further on the `editable_graphical ->event' definition, but
instead {\bf replace} this definition.  Of course it is allowed to
subclass the definition of editable_box above and further refine the
event method in the subclass.

\begin{pcecode}
:- require([use_class_template/1]).

:- pce_begin_class(editable_box, box).
:- use_class_template(editable_graphical).

event(Gr, Ev:event) :->
	(   send_super(Gr, event, Ev)
	;   ...
	).
:- pce_end_class.
\end{pcecode}


\subsection{Implementation notes}	\label{sec:ucdimplementation}

The \productpl{} class-compilation is defined using the Prolog
preprocessing capabilities of term_expansion/2. While the class is
compiled, Prolog gathers the expressions belonging to the class. The
expansion of \mbox{\tt :- pce_end_class({\em Class})} emits the actual
code for the class.

The method implementation is realised by the predicates
pce_principal:send_implementation/3 and
pce_principal:get_implementation/4. that take the form:

\begin{description}
    \predicate{send_implementation}{3}{MethodId, Method(Arg...), Object}
Where \arg{MethodId} is unique identifier for the class and method,
\arg{Method} is the method implemented, \arg{Arg...}	are the
arguments accepted by the method and \arg{Object} is the receiving
object.
    \predicate{get_implementation}{4}{MethodId, Method(Arg...), Object, -Result}
This is the get-equivalent of send_implementation/3.
\end{description}

\begin{code}
:- pce_begin_class(gnus, ...
gnu(X, A:int) :-> ...
gnats(X, A:name, B:int) :-> ...
\end{code}

is translated into

\begin{code}
pce_principal:send_implementation('gnus$+$->gnu', gnu(A), O) :- ...
pce_principal:send_implementation('gnats$+$->gnu', gnats(A, B), O) :- ...
\end{code}

The remainder of the class specification is translated into a number of
Prolog clauses describing the class. No \product{} class is created. If
\product{} generates an {\tt undefined_class} exception, it will scan
for the class-description in the Prolog database and create the
\product{} \class{class} instance. No methods are associated with the
new class. Instead, all method binding is again based on exception
handling.

Modifications to the class beyond what is provided by the preprocessing
facilities (for example changing the `variable ->clone_style') cannot
be made by sending messages to the class inside the class definition
body as this would address the not-yet-existing class.  Instead, they
should be embedded in the pce_class_directive/1 directive.%
    \footnote{To facilitate the translation of old code, the construct
	      \mbox{\tt :- send(@class, ...} is treated automatically
	      as if it was embedded using pce_class_directive/1}.
The {\em Goal} argument of pce_class_directive/1 should refer to the
class using the \product{} \class{var} object @class.  When the class is
realised the exception system will bind @class to the current class
while running {\em Goal}.  {\em Goal} is called
from the Prolog module in which the class is defined.

\index{runtime generation}%
The main reason for the above approach is to exploit the
runtime-generation facilities of the hosting Prolog system to create
fast-starting portable and (depending on the hosting Prolog's
capabilities) stand-alone executables.

One of the consequences of the chosen approach is that the
class-building directives are not accessible as predicates.  There is
no preprocessing support for the dynamic creation of classes and the
programmer should thus fall back to raw manipulation of the \product{} class
objects.
