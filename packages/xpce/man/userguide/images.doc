\section{Using images and cursors}		\label{sec:images}

Many today graphical user interfaces extensively use (iconic) images.
There are many image formats, some for specific machines, some with
specific goals in mind, such as optimal compression at the loss of
accuracy or provide alternatives for different screen properties.

One of \product{}'s aim is to provide portability between the supported
platform. Therefore, we have chosen to support a few formats across all
platforms, in addition to the most popular formats for each individual
platform.


\subsection{Colour handling}			\label{sec:colour}

\index{colour,images}%
Colour handling is a hard task for todays computerprogrammer. There is a
large variety in techniques, each providing their own advantages and
disadvantages. \product{} doesn't aim for programmers that need to get
the best performance and best results rendering colours, but for the
programmer who wants a reasonable result at little effort.

As long as you are not using many colours, which is normally the case
as long as you do not handle full-colour images, there is no problem.
This is why this general topic is handled in the section on images.

Displays differ in the number of colours they can display simultaneously
and whether this set can be changed or not. X11 defines 6 types of
\idx{visuals}.  Luckily, these days only three models are popular.

\index{colour,256}\index{colour,16-bits}\index{colour,true}%
\begin{itemlist}
    \item [8-bit colour-mapped]
This is that hard one.  It allows displaying 256 colours at the same
time.  Applications have to negotiate with each others and the windowing
systems which colours are used at any given moment.

It is hard to do this without some advice from the user. On the other
hand, this format is popular because it leads to good graphical
performance.

    \item [16-bit `high-colour']
This schema is a low-colour-resolution version of true-colour, normally
using 5-bit on the red and blue channels and 6 on the green channel. It
is not very good showing perfect colours, nor showing colour gradients.

It is as easy for the programmer as true-colour and still fairly
efficient in terms of memory.

    \item [24/32 bit true-colour]
This uses the full 8-bit resolution supported by most hardware on all
three channels.  The 32-bit version wastes one byte for each pixel,
achieving comfortable alignment.  Depending on the hardware, 32 bit
colour is sometimes much faster.
\end{itemlist}


We will further discuss 8-bit colour below.  As handling this is totally
different in X11 and MS-Windows we do this in two separate sections.


\subsubsection{Colour-mapped displays on MS-Windows}

In MS-Windows one has the choice to stick with the reserved 20 colours
of the system palette or use a colourmap (palette, called by Microsoft).

If an application chooses to use a colourmap switching to this
application causes the entire screen to be repainted using the
application's colourmap.  The idea is that the active application looks
perfect and the other applications look a little distorted as they have
to do their job using an imperfect colourmap.

By default, \product{} makes a \class{colour_map} that holds a copy of
the reserved colours.  As colours are required they are added to this
map.  This schema is suitable for applications using (small) icons and
solid colours for graphics.  When loading large colourful images the
colourmap will get very big and optimising its mapping to the display
slow and poor.  In this case it is a good idea to use a fixed colourmap.
See class \class{colour_map} for details.

When using \product{} with many full-colour images it is advised to use
high-colour or true-colour modes.


\subsubsection{Colour-mapped displays on X11/Unix}

X11 provides colourmap sharing between applications. This avoids the
flickering when changing applications, but limits the number of
available colours.  Even worse, depending on the other applications
there can be a large difference in available colours.  The alternative
is to use a \idx{private colourmap}, but unlike MS-Windows the other
applications appear in totally random colours.  \product{} does not
support the use of private colourmaps therefore.

In practice, it is strongly advised to run X11 in 16, 24 or 32 bit mode
when running multiple applications presenting colourful images. For
example \idx{Netscape} insists creating its own colourmap and starting
Netscape after another application has consumed too many colours will
simply fail.


\subsection{Supported Image Formats}

The table below illustrates the image format capabilities of each of the
platforms. Shape support means that the format can indicate {\em
transparent} areas. If such an image file is loaded, the resulting
\class{image} object will have an `image <-mask' associated: a
monochrome image of the same side that indicates where paint is to be
applied. This is required for defining cursors (see `cursor
->initialise') from a single image file. {\em Hotspot} means the format
can specify a location.  If a Hotspot is found, the `image <-hot_spot'
attribute is filled with it.  A Hotspot is necessary for cursors, but
can also be useful for other images.

\begin{center}
\index{XPM,file format}%
\index{ICO,file format}%
\index{CUR,file format}%
\index{XBM,file format}%
\index{JPEG,file format}%
\index{GIF,file format}%
\index{BMP,file format}%
\index{PNM,file format}%
\index{image,file formats}%
\index{image,shape}%
\index{cursor}%
\index{icon}%
\begin{tabular}{|l|ccccccc|}
\hline
\bf Format & \bf Colour & \bf HotSpot & \bf Shape &
	     \multicolumn{2}{c}{\bf Unix/X11} &
	     \multicolumn{2}{c|}{\bf Win32} \\
	   & &&&   load	  & save	      & load & save \\
\hline
\multicolumn{8}{|c|}{Icons, Cursors and shaped images} \\
\hline
XPM	   & +&+&+&  +	  &  +		      &   +  &  +   \\
ICO	   & +&-&+&  -	  &  -		      &   +  &  -   \\
CUR	   & +&+&+&  -	  &  -		      &   +  &  -   \\
\hline
\multicolumn{8}{|c|}{Rectangular monochrome images} \\
\hline
XBM	   & -&-&-&  +	  &  +		      &   +  &  -   \\
\hline
\multicolumn{8}{|c|}{Large rectangular images} \\
\hline
JPEG	   & +&-&-&  +	  &  +		      &   +  &  +   \\
GIF	   & +&-&+&  +	  &  +		      &   +  &  +   \\
BMP	   & +&-&-&  -	  &  -		      &   +  &  -   \\
PNM	   & +&-&-&  +	  &  +		      &   +  &  +   \\
\hline
\end{tabular}
\end{center}

The XPM format ({\bf X} {\bf P}ix{\bf M}ap) is the preferred format for
platform-independent storage of images that are used by the application
for cursors, icons and other nice pictures. The XPM format and
supporting libraries are actively developed as a contributed package to
X11.  


\subsubsection{Creating XPM files}

\paragraph{Unix} There are two basic ways to create XPM files. One is to
convert from another format. On Unix, there are two popular conversion
tools. The \program{xv} program is a good interactive tool for format
conversion and applying graphical operations to images.

ImageMagic can be found at \url{http://www.simplesystems.org/ImageMagick/}
and provides a comprehensive toolkit for converting images.

The \program{pixmap} program is a comprehensive icon editor, supporting
all of XPM's features.  The image tools mentioned here, as well as the
XPM library sources and a FAQ dealing with XPM related issues can be
found at \url{ftp://swi.psy.uva.nl/xpce/util/images/}


\paragraph{Windows}

\product{} supports the Windows native \fileext{ICO}, \fileext{CUR} and
\fileext{BMP} formats. Any editor, such as the resource editors that
comes with most C(++) development environments can be used. When
portability of the application becomes an issue, simply load the icons
into \product{}, and write them in the XPM format using the `image
->save' method. See the skeleton below:

\begin{code}
to_xpm(In, Out) :-
	new(I, image(In)),
	send(I, save, Out, xpm),
	free(I).
\end{code}

Note that the above mentioned ImageMagick toolkit is also available for
MS-Windows.

\subsubsection{Using Images}

Images in any of the formats are recognised by many of \product{}'s GUI
classes.  Table \tabref{imageusage} provides a brief list:

\begin{table}
\begin{center}
\begin{tabularlp}{`menu_item ->selection'}
\hline
\class{bitmap}		& A \class{bitmap} converts an image into a first
			  class \class{graphical} object that can be
			  displayed anywhere. \\
\hline
\class{cursor}		& A \class{cursor} may be created of an image 
			  that has a mask and hot-spot. \\
\hline
`frame ->icon'		& Sets the icon of the frame. The visual result
			  depends on the window system and X11 window
			  manager used.  Using the Windows 95 or NT 4.0
			  shell, the image is displayed in the task-bar
			  and top-left of the window. \\
\hline
`dialog_item ->label'	& The label of all subclasses of class
			  \class{dialog_item} can be an image. \\
\hline
`label ->selection'	& A \class{label} can have an image as its
			  visualisation. \\
\hline
`menu_item ->selection'	& The items of a menu can be an image. \\
`style ->icon'		& Allows association of images to lines in
			  a \class{list_browser}, as well as marking
			  \classs{fragment} in an \class{editor}. \\
\hline
\end{tabularlp}
\end{center}
\caption{GUI classes using \class{image} objects}
\label{tab:imageusage}
\end{table}





