\section{Plotting graphs and barcharts}		\label{sec:libplot}

\index{axis}\index{plot}\index{diagram}\index{chart}\index{barchart}%
This section describes three libraries residing in
\metafile{<pcehome>/prolog/lib/plot} to deal with plotting graphs and
barcharts.

\subsection{Painting axis}

The library \pllib{plot/axis} defines the class \class{plot_axis} to
draw an X- or Y-axis. The class deals with computing the layout, placing
rule-marks, values and labels as well as translation between coordinates
and real values. Normally this class is used together with
\class{plotter}, \class{plot_axis} does not rely on other library
classes and may therefore be used independent of the remainder of the
plotting infrastructure.

We start with a small example from the library itself, creating the
picture below.

\begin{code}
?- [library('plot/axis')].
% library('plot/axis') compiled into plot_axis 0.03 sec, 27,012 bytes

?- send(new(P, picture), open),
	   send(P, display,
		plot_axis(x, 0, 100, @default, 400, point(40, 320))),
	   send(P, display,
		plot_axis(y, 0, 500, @default, 300, point(40, 320))).
\end{code}

\postscriptfig{axis}{A picture showing two axis}

Below is a reference to the important methods of this class.  The
sources to the class itself are a good example of complicated and
advanced layout computations and delaying of these until they are
really needed.

\begin{description}
    \sendmethod{plot_axis}{initialise}{%
	type={x,y},
        low=int{\tt|}real,
	high=int{\tt|}real,
        step=[int{\tt|}real],
	length=[int],
	origin=[point]}
Create a new axis.  \arg{type} defines whether it is an X- or Y-axis.
The axis represents values in the range [\arg{low}\ldots\arg{high}].
If \arg{step} is specified, a rule-mark with value is placed at
these intervals.  Otherwise the library computes its marking
dynamically.  The \arg{length} argument specifies the length of
the axis in pixels, the default is 200 and finally the \arg{origin}
defines the pixel-location of the origin.

    \sendmethod{plot_axis}{label}{graphical*}
Label to position near the end of the axis.  This is a graphical to
provide full flexibility.

    \sendmethod{plot_axis}{format}{[name]}
Define the printf()-format for rendering the values printed along the
axis.

    \getmethod{plot_axis}{location}{int{\tt|}real}{int}
Determine the coordinate in the device's coordinate system representing
the given value.  See also `plotter<-translate'.

    \getmethod{plot_axis}{value_from_coordinate}{int}{int{\tt|}real}
The inverse of <-location, returning the value along the axis from
a pixel coordinate.
\end{description}

Besides the principal methods below, the following methods are
available for changing attributes of an existing axis: ->origin,
->low, ->high, ->step, ->small_step (interval for rule-marks
without a value), ->length and ->type: \{x,y\}.


\subsection{Plotting graphs}

The library \pllib{plot/plotter} defines the classes \class{plotter}
and \class{plot_graph} for displaying graphs.  Class \class{plotter}
is a subclass of \class{device}.  The example below plots the function
$Y = sine(X)$

\begin{pcecode}
:- use_module(library('plot/plotter')).
:- use_module(library(autowin)).

plot_function :-
	plot_function(X:sin(X)).

plot_function(Template) :-
	To is 2*pi,
	PlotStep is To/100,
	Step is pi/4,
	new(W, auto_sized_picture('Plotter demo')),
	send(W, display, new(P, plotter)),
	send(P, axis, new(X, plot_axis(x, 0, To, Step, 300))),
	send(P, axis, plot_axis(y, -1, 1, @default, 200)),
	send(X, format, '%.2f'),
	send(P, graph, new(G, plot_graph)),
	plot_function(0, To, PlotStep, Template, G),
	send(W, open).

plot_function(X, To, _, _, _) :-
	X >= To, !.
plot_function(X, To, Step, Template, G) :-
	copy_term(Template, X:Func),
	Y is Func,
	send(G, append,	X, Y),
	NewX is X + Step,
	plot_function(NewX, To, Step, Template, G).
\end{pcecode}

\postscriptfig{plotsine}{Plotter showing sine function}

\begin{description}
    \sendmethod{plotter}{axis}{plot_axis}
Associate a \class{plot_axis}. Before using the plotter both an $X$ and
$Y$ axis must be associated. Associating an axis that already exists
causes the existing axis to be destroyed.

    \sendmethod{plotter}{graph}{plot_graph}
Append a graph.  Multiple graphs can be displayed on the same plotter.

    \sendmethod{plotter}{clear}{}
Remove all graphs.  The X- and Y-axis are not removed.

    \getmethod{plotter}{translate}{X:int{\tt|}real,
				   Y:int{\tt|}real}{point}
Translate a coordinate in the value-space to physical coordinates.

    \getmethod{plotter}{value_from_x}{int}{int{\tt|}real}
Translate an X-coordinate to a value.

    \getmethod{plotter}{value_from_y}{int}{int{\tt|}real}
Translate an Y-coordinate to a value.
\end{description}

Graphs themselves are instances of class \class{plot_graph}, a subclass
of \class{path}.  Instead of normal \class{point} objects, the points
are represented using the subclass \class{plot_point} that attaches the
real values to the physical coordinates.  Methods:

\begin{description}
    \sendmethod{plot_graph}{initialise}{%
	type=[\{poly,smooth,points_only\}],
	mark=[image]*}
The \arg{type} argument denotes the interpolation used.  Using
\const{poly} (default), straight lines are drawn between the points.
Using \const{smooth}, the curve is interpolated (see \class{path}
for details) and using \const{points_only}, no lines is painted,
just the marks.  Using the \arg{mark} argument the user may specify
marks to be drawn at each control-point.

    \sendmethod{plot_graph}{append}{x=int{\tt|}real, y=int{\tt|}real}
Append a control-point using the coordinate-system of the axis of the
plotter.
\end{description}

\subsection{Drawing barcharts using ``plot/barchart''}

\index{barchart}%
The \pllib{plot/barchart} library draws simple bar-charts.  It is based
on the \class{plotter} and \class{plot_axis} classes, adding simple
bars, grouped bars and stacked bars.  Below is an example from
\pllib{plot/demo} showing all active \product{}, classes, where active
is defined that more than 250 instances are created.  The code, except
for the calculation parts is show below.

\postscriptfig{plotclasses}{Classes of \product{} with $> 250$ instances
			    created}

\begin{pcecode}
barchart :-
	barchart(vertical).
barchart(HV) :-
	new(W, picture),
	active_classes(Classes),
	length(Classes, N),
	required_scale(Classes, Scale),
	send(W, display, new(BC, bar_chart(HV, 0, Scale, 200, N))),
	forall(member(class(Name, Created, Freed), Classes),
	       send(BC, append,
		    bar_group(Name,
			      bar(created, Created, green),
			      bar(freed, Freed, red)))),
	send(W, open).
\end{pcecode}

\begin{description}
    \sendmethod{bar_chart}{initialise}{%
	orientation=\{horizontal,vertical\},
	low=real,
	high=real,
	scale_length=[0..],
	nbars=[0..]}
Initialise a \class{bar_chart}, a subclass of \class{plotter} for
displaying bar-charts.  The \arg{orientation} indicates whether the
bars are vertical or horizontal.  The \arg{low} and \arg{high} arguments
are the scale arguments for the value-axis, while \arg{scale_length}
denotes the length of the axis.  The \arg{nbars} argument determines
the length of the axis on which the bars are footed.

    \sendmethod{bar_chart}{append}{bar{\tt|}bar_stack}
Append a single \class{bar}, \class{bar_stack} or \class{bar_group}
to the chart.  Bars and bar-stacks are named and can be addressed using
their name.

    \sendmethod{bar_chart}{delete}{member:bar{\tt|}bar_stack}
Remove the given bar.  The \exam{member:} construct makes the
type-conversion system translate a bar-name into a bar.  If the
bar is somewhere in the middle, the remaining bars are compacted
again.

    \sendmethod{bar_chart}{clear}{}
Removes all bars from the chart.

    \bothmethod{bar_chart}{value}{name}{real}
Modifies or requests the value of the named bar.  Fails if no bar
with this name is on the chart.

    \sendmethod{bar_chart}{event}{event}
Processes a single-click outside the bars to clear the selection.

    \sendmethod{bar_chart}{select}{bar{\tt|}bar_stack, [\{toggle,set\}]}
    \sendmethod{bar_chart}{selection}{bar{\tt|}bar_stack{\tt|}chain*}
    \getmethod{bar_stack}{selection}{}{chain}
Deal with the selection.  Selection is visualised by selecting the
labels, but communicated in terms of the bars themselves.
\end{description}


\subsubsection{Class \class{bar}}

Bars can either be displayed directly on a \class{bar_chart}, or as
part of a stack or group.  Stacked bars are used to indicate composition
of a value, while grouped bars often indicate development or otherwise
related values for the same object.

\begin{description}
    \sendmethod{bar}{initialise}{%
	name=name,
	value=real,
	colour=[colour],
	orientation=[\{horizontal,vertical\}]}
Create a \class{bar} from its \arg{name} and \arg{value}.  The bar
itself is a subclass of \class{box} and \arg{colour} is used to
fill the interior.  The \arg{orientation} needs only be specified
if the bar is not attached to a \class{bar_chart}.

    \sendmethod{bar}{value}{real}
Set the value of th bar.  This updates the bar-size automatically.

    \sendmethod{bar}{range}{low=real, high=real}
If the bar is editable (see also ->message and ->drag_message), these
are the lowest and highest values that can be set by the user.

    \sendmethod{bar}{message}{code*}
If not @nil, the bar may be modified by dragging it.  After releasing
the mouse-button, the new <-value is forwarded over the \class{code}.

    \sendmethod{bar}{drag_message}{code*}
If not @nil, the bar may be modified by dragging it.  While dragging,
the new value is forwarded on every change over the \class{code}.  It
is allowed to specify both ->message and ->drag_message.

    \sendmethod{bar_stack}{initialise}{name, bar ...}
Create a pile of bars representing a value composed of multiple
smaller values.

    \sendmethod{bar_group}{initialise}{name, bar ...}
Same as \class{bar_stack}, but places the bars next to each other
instead of stacked.
\end{description}


\subsubsection{Class \class{bar_button_group}}

A subclass of \class{dialog_group} that can be used to associate one
or more buttons or other controllers with a \class{bar} or
\class{bar_stack}.  This association is achieved by simply creating
an instance of this class.  \Figref{boot} shows both associated buttons
and a stacked bar.

\begin{description}
    \sendmethod{bar_button_group}{initialise}{bar{\tt|}bar_stack,
					      graphical ...}
Associate the given graphicals with given bar.

    \getmethod{bar_button_group}{bar}{}{bar{\tt|}bar_stack}
Return the bar or stack this group is connected too.  This behaviour
may be used to make to make the buttons less dependent on the bar
they are attached too.
\end{description}

\postscriptfig{boot}{Stacked bars with associated buttons}
