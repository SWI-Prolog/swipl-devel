\section{Library ``find_file''}		\label{sec:findfile}

\index{file,prompting for}%
The library \pllib{find_file} defines the class \class{finder},
representing a modal dialog window for entering a filename. This class
defines the method `finder <-file':

\begin{description}
    \getmethod{finder}{file}{Exists:[bool], Ext:[name|chain],
			     Dir:[directory], Def:[file]}{name}
Ask the user for a file.  If \arg{Exists} is @on, only existing files
can be returned.  \arg{Ext} is either an atom denoting a single
extension or a chain of allowed extensions.  Extensions can be
specified with or without the leading dot.  I.e.\ both \const{pl}
and \const{'.pl'} are valid specifications of files ending in
\const{'.pl'}.  \arg{Dir} is the directory to start from.  When omitted,
this is the last directory visited by this instance of \class{finder}
or the current working directory.  \arg{Def} is the default file-name
returned.  When omitted there is no default.

Below is the typical declaration and usage.  In this example we ask
for an existing Prolog file.

\begin{code}
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

	...,
	get(@finder, file, @on, pl, PlFile),
	...,
\end{code}
\end{description}

\index{file,get name in Windows}%
The Windows version of \product{} implements the method
`display<-win_file_name', using the Win32 API standard functions
GetOpenFileName() or GetSaveFileName() to obtain a file-name for saving
or loading.  If this method is defined, `finder<-file' uses it, showing
the users familiar dialog.

Names for the extensions (as `Prolog file' rather than '*.pl') can be
defined by extending the multifile predicate pce_finder:file_type/2.  See
the library source for the standard definition.
