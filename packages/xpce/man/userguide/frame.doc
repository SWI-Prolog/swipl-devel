\section{Window layout in a frame}			\label{sec:framelayout}

\index{frame,layout}\index{window,layout in frame}\index{layout,window}%
A \class{frame} is the gateway between a collection of tiled
\class{window}{\em s} (graphics, dialog) and the Window System. Any
displayed window has a frame. The `window ->create' method (invoked by
`window ->open') will create a default frame for a window if the window
does not have a frame.

Windows in a frame are controlled by a hierarchy of \class{tile}
objects. The leaves of this hierarchy manage an individual window, while
the non-leaf tiles either manages a left-to-right list of sub-tiles or
a top-to-bottom list.  A non-leaf {\em horizontal} (left-to-right) tile
forces all windows to be as high as the highest wishes to be and
distributes the horizontal space over the sub-tiles, while a {\em
vertical} tile forces all members to the same (widest) member and
distributes the vertical space.

A tile hierarchy is built using the methods `window ->above', etc.\
described below.  Each window is born with a leaf-tile.  If two windows
are connected using ->left or ->right, a horizontal tile is created.
Any other window associated to the left or right will be added as a
member to this horizontal tile.  A analogous story applies to vertically
stacked windows.

If a window or collection of windows is placed left of a vertical tiled
stack of windows, a horizontal tile is created.

Whenever a window receives one of the ->above, ->left, etc. messages,
it will forward these messages to the root of the associated tile
hierarchy.  Assuming both windows are not yet related, the root tiles
negotiate the alignment (combine, become member, create a new root).

Suppose we want to realise the window layout shown in \figref{flayout1}.
Now, look for windows that have equal width or height.  This is the
starting point, {\bf W3} and {\bf W4} in this example.  So, the first
statement is:

\begin{code}
	send(W3, above, W4)
\end{code}

\postscriptfig[width=2in]{flayout1}{Demo window layout}

Note that `send(W4, below, W3)' is the same, except that if both {\bf W3}
and {\bf W4} have a \class{frame} attached, the frame of the {\em
argument} of the message will hold both windows, while the frame
of the {\em receiver} is destroyed.

Now simply continue inwards-out:

\begin{code}
	send(W2, left, W3),
	send(W1, above, W2),
	send(W5, below, W2)
\end{code}

Note that `send(W2, left, W4)' is {\em exactly} the same as positioning
{\bf W2} left of {\bf W3}.  The resulting tile hierarchy is shown in
\figref{tilehier1}.  The numbers indicate the order of creation of the
various objects.

\postscriptfig[width=2.5in]{tilehier1}{Tile hierarchy of example}

\subsection{Windows sizes and automatic adjustment}

\index{window,size specification}%
A \class{tile} defines six size parameters, described below.

\begin{description}
    \sendmethod{tile}{ideal_width}{int}%
    \sendmethod*{tile}{ideal_height}{int}%
These two parameters describe the {\em ideal} size of the tile. The
initial values for these parameters are read from the window for which
the tile is created. A non-leaf horizontal tile sets the ideal width to
the sum of its members and the ideal height to the maximum of its
members.
    \sendmethod{tile}{hor_stretch}{0..100}%
    \sendmethod*{tile}{ver_stretch}{0..100}%
These two parameters describe how easy the window stretches (gets
bigger).  A non-leaf horizontal tile sets hor_stretch to the maximum
of its members and ver_stretch to the minimum of its members.
    \sendmethod{tile}{hor_shrink}{0..100}%
    \sendmethod*{tile}{ver_shrink}{0..100}%
Same, but deals with making the window/tile smaller than its ideal size.
\end{description}

The various built-in window types have the following defaults shown in
\tabref{windowsizes}.

\begin{table}
\begin{minipage}{\textwidth}
\begin{center}
\begin{tabular}{|l|c|c|c|c|c|c|}
\hline
\bf class   & width & height & hor_shrink & ver_shrink & hor_stretch & ver_stretch \\
\hline
\hline
\bf window  & 200 & 100 & 100 & 100 & 100 & 100 \\
\bf picture & 400 & 200 & 100 & 100 & 100 & 100 \\
\bf dialog  & 200\fnm\alpha & 100\fnm\alpha &   0 &   0 &   0 &   0 \\
\bf browser & 25\fnm\beta &   10\fnm\beta &   0 &	100 &   0 & 100 \\
\bf view    & 80\fnm\beta &   20\fnm\beta & 100 & 100 & 100 & 100 \\
\hline
\end{tabular}
\footnotetext[1]{If the dialog is not empty, the bounding box of the
		 contents with the <-gap around it will be used.}
\footnotetext[2]{Interpreted as character units.}
\end{center}
\end{minipage}
\caption{The window types and their default sizes}
\label{tab:windowsizes}
\end{table}

These rules will often suffice for simple applications.  You may adjust
the stretch and shrink parameters after creating the window, but before
opening the window.


\subsection{Manipulating an open frame}

Windows may be added to an open frame using the ->above, etc. message,
specifying any of the member windows as argument.  Windows may be deleted
from a frame using `frame ->delete'.  If a window is added or deleted from
an open window, the message `frame ->fit' will be called to readjust
the contents.  This will normally change the size of the frame.  If this
is not desired, class \class{frame} must be sub-classed as below.  Note
that fixed_size should not be set to @on before the frame is open.

\begin{pcecode}
:- pce_begin_class(my_dynamic_frame, frame,
		   "Add fixed_size").

variable(fixed_size, bool := @off, both,
	 "Do not resize on ->fit").

fit(F) :->
	"Request to fit the contents"::
	(   get(F, fixed_size, @on)
	->  send(F, resize)
	;   send(F, send_super, fit)
	).
\end{pcecode}

% again :-)
