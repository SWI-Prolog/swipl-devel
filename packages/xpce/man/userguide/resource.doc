\chapter{Program resources}			\label{sec:resources}

Resources, in the new sense of the word is data that is required by
an application but cannot be expressed easily as program-code.  Examples
are image-files, help-files, and other files using non-Prolog syntax.
Such files are declared by defining clauses for the predicate
resource/3:

\begin{description}
    \predicate{resource}{3}{?Name, ?Class, ?PathSpec}
Define the file specified by \arg{PathSpec} to contain data for the
resource named \arg{Name} of resource-class \arg{Class}.

\arg{Name} refers to the logical name of the resource, which is
interpreted locally for a Prolog module.  Declarations in the module
\module{user} are visible as defaults from all other modules.
\arg{Class} defines the type of object to be expected in the file. Right
now, they provide an additional name-space for resources. \arg{PathSpec}
is a file specification as acceptable to absolute_file_name/[2,3].
\end{description}

Resources can be handled both from Prolog as for \product{}.  From
Prolog, this is achieved using open_resource/3:

\begin{description}
    \predicate{open_resource}{4}{+Name, ?Class, -Stream}
Opens the resource specified by \arg{Name} and \arg{Class}.  If the
latter is a variable, it will be unified to the class of the first
resource found that has the specified \arg{Name}.  If successful,
\arg{Stream} becomes a handle to a binary input stream, providing
access to the content of the resource.

The predicate open_resource/3 first checks resource/3.  If successful
it will open the returned resource source-file.  Otherwise it will look
in the programs resource database.  When creating a saved-state, the
system saves the resource contents into the resource archive,
but does not save the resource clauses.

This way, the development environment uses the files (and modifications
to the resource/3 declarations and/or files containing resource info
thus immediately affect the running environment, while the runtime
system quickly accesses the system resources.
\end{description}

From \product{}, resources are accessed using the class
\class{resource}, which is located next to \class{file} below the
common data-representation class \class{source_sink}.  Many of the
methods that require data accept instances of \class{source_sink},
making resources a suitable candidate.

Below is the preferred way to specify and use an icon.

\begin{code}
resource(my_icon,	image,	image('my_icon.xpm')).

	...,
	send(Button, label, image(resource(my_icon))),
	...,

\end{code}

The directive pce_image_directory/1 adds the provided directory to the
search-path for images (represented in the class-variable
\classvar{image}{path}), as well as to the \functor{image}{1} definition
of file_search_path/2.

Please note that MS-Windows formatted image files can currently not
be loaded through \class{resource} objects. The Windows API only
provides functions to extract these objects from a single file, or
nested as Windows resources in a \fileext{dll} or \fileext{exe} file.

Right now, it is advised to translate the images into \fileext{xpm}
format using the following simple command:

\begin{code}
?- send(image('myicon.ico'), save, 'myicon,xpm', xpm).
\end{code}

This transformation is complete as the \fileext{XPM} image format covers
all aspects of the Microsoft image formats.  For further details on
image formats and manipulation, see \secref{images}.
