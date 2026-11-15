\chapter{Deploying applications}	\label{sec:runtime}

This chapter describes the features of SWI-Prolog for delivering
applications using \jargon{saved states}.

\section{Deployment options}
\label{sec:deployment-options}

There are several ways to make a Prolog application available to your
users. By far the easiest way is to require the user to install
SWI-Prolog and deliver the application as a directory holding source
files, other resources the application may need and a \jargon{Prolog
Script} file that provides the executable. See \secref{plscript}. The
two-step installation may be slightly less convenient for the end user,
but enables the end-user to conveniently run your program on a different
operating system or architecture. This mechanism is obviously not
suitable if you want to keep the source of your program secret.

Another solution is to use \jargon{saved states}, the main topic of this
chapter, together with the installed development system and disable
\jargon{autoloading} requirements into the state using
\cmdlineoption{--no-autoload} or the \term{autoload}{false} option of
qsave_program/2. This allows creating the application as a single file,
while avoiding the need to ensure that the state is self-contained. For
large programs this technique typically reduces startup time by an order
of magnitude. This mechanism is particularly suitable for in-house and
cloud deployment. It provides some protection against inspecting the
source. See \secref{protect-code} for details.

The final solution is to make sure all required resources are present in
the saved state. In this case the state may be added to the
\jargon{emulator} and the application consists of the emulator with
state and the shared objects/DLLs required to make the emulator work. If
the emulator can be statically linked for the target platform this
creates a single file executable that does not require SWI-Prolog
installed on the target computer.

\section{Understanding saved states}
\label{sec:saved-states}

A SWI-Prolog \jargon{saved state} is a \jargon{resource archive} that
contains the compiled program in a machine-independent
format,\footnote{Although the compiled code is independent from the CPU
and operating system, 32-bit compiled code does not run on the 64-bit
emulator, nor the other way around. Conditionally compiled code (see
if/1) may also reduce platform independence.} startup options,
optionally shared objects/DLLs and optionally additional
\jargon{resource} files. As of version 7.7.13, the resource archive
format is ZIP. A resource file is normally \textbf{created} using the
commandline option \cmdlineoption{-c}:

\begin{code}
swipl -o mystate option ... -c file.pl ...
\end{code}

The above causes SWI-Prolog to load the given Prolog files and call
qsave_program/2 using options created from the \textit{option \ldots} in
the command above.

A saved state may be \textbf{executed} in several ways. The basic
mechanism is to use the \cmdlineoption{-x}:

\begin{code}
swipl -x mystate app-arg ...
\end{code}

Saved states may have an arbitrary payload at the \emph{start}. This
allows combining a (shell) script or the emulator with the state to turn
the state into a single file executable. By default a state starts with
a shell script (Unix) or the emulator (Windows).\footnote{As the default
emulator is a short program while the true emulator is in a DLL this
keeps the state short.} The options \term{emulator}{File} and
\term{stand_alone}{Bool} control what is added at the start of the
state. Finally, C/C++ programs that embed Prolog may use a static C
string that embeds the state into the executable. See
PL_set_resource_db_mem().

\subsection{Creating a saved state}
\label{sec:create-saved-state}

The predicates in this section support creating a saved state. Note that
states are commonly created from the commandline using the
\cmdlineoption{-c}, for example:

\begin{code}
swipl -o mystate --foreign=save -c load.pl
\end{code}

Long (\const{--}) options are translated into options for
qsave_program/2. This transformation uses the same conventions as used
by argv_options/3, except that the transformation is guided by the
option type. This implies that integer and callable options need to have
valid syntax and boolean options may be abbreviated to simply
\exam{--autoload} or \exam{--no-autoload} as shorthands for
\exam{--autoload=true} and \exam{--autoload=false}.


\begin{description}
    \predicate{qsave_program}{2}{+File, +Options}
Saves the current state of the program to the file \arg{File}. The
result is a resource archive \arg{File} containing expresses all Prolog
data from the running program, all user-defined resources (see
resource/2 and open_resource/2) and optionally all shared objects/DLLs
required by the program for the current architecture. Depending on the
\const{stand_alone} option, the resource is headed by the emulator, a
Unix shell script or nothing. \arg{Options} is a list of additional
options:

    \begin{description}
	\termitem{stack_limit}{+Bytes}
Sets default stack limit for the new process. See the command line
option \cmdlineoption{--stack-limit} and the Prolog flag
\prologflag{stack_limit}.
	\termitem{goal}{:Callable}
Initialization goal for the new executable (see \cmdlineoption{-g}).
Two values have special meaning: \const{prolog} starts the Prolog
toplevel and \const{default} runs halt/0 if there are initialization
goals and the prolog/0 toplevel otherwise.
	\termitem{toplevel}{:Callable}
Top-level goal for the new executable (see \cmdlineoption{-t}).  Similar
to initialization/2 using \const{main}, the default toplevel is to enter
the Prolog interactive shell unless a goal has been specified using
\term{goal}{Callable}.
	\termitem{init_file}{+Atom}
Default initialization file for the new executable. See
\cmdlineoption{-f}.
	\termitem{class}{+Class}
If \const{runtime} (default), read resources from the state and
disconnect the code loaded into the state from the original source.  If
\const{development}, save the predicates in their current state and keep
reading resources from their source (if present). See also
open_resource/3.
	\termitem{autoload}{+Boolean}
If \const{true} (default), run autoload/0 first.  If the class is
\const{runtime} and \const{autoload} is \const{true}, the state is
supposed to be self contained and autoloading is disabled in the
restored state.
	\termitem{map}{+File}
Dump a human-readable trace of what has been saved in \arg{File}.
	\termitem{op}{+Action}
One of \const{save} (default) to save the current operator table
or \const{standard} to use the initial table of the emulator.
	\termitem{stand_alone}{+Boolean}
If \const{true}, the emulator is the first part of the state. If the
emulator is started it tests whether a saved state is attached
to itself and load this state. Provided the application has
all libraries loaded, the resulting executable is completely independent
from the runtime environment or location where it was built. See also
\secref{cmdlinecomp}.
	\termitem{emulator}{+File}
File to use for the emulator or executable used by the startup script.
Default is the running Prolog image \emph{after} following symbolic
links, e.g., \file{/usr/lib/swipl/lib/x86_64-linux/swipl}.  To create
a saved state based on the public executable such that it can run on
multiple architectures one can use e.g.

\begin{code}
$ swipl -o myexe --emulator=$(which swipl) -c myload.pl
\end{code}

	\termitem{foreign}{+Action}
If \const{save}, include shared objects (DLLs) for the current
architecture into the saved state. See current_foreign_library/2,
and current_prolog_flag(arch, Arch). If the program \program{strip}
is available, this is first used to reduce the size of the shared
object. If a state is started, use_foreign_library/1 first tries
to locate the foreign resource in the resource database. When
found it copies the content of the resource to a temporary file
and loads it. If possible (Unix), the temporary object is deleted
immediately after opening.\footnote{This option is experimental and
currently disabled by default. It will become the default if it proves
robust.}\footnote{Creating a temporary file is the most portable way
to load a shared object from a zip file but requires write access to
the file system. Future versions may provide shortcuts for specific
platforms that bypass the file system.}

If \arg{Action} is of the form \term{arch}{ListOfArches} then the
shared objects for the specified architectures are stored in
the saved state. On the command line, the list of architectures
can be passed as \const{--foreign=<CommaSepArchesList>}. In
order to obtain the shared object file for the specified
architectures, qsave_program/2 calls a user defined hook:
\term{qsave:arch_shlib}{+Arch, +FileSpec, -SoPath}. This hook
needs to unify \const{SoPath} with the absolute path to the
shared object for the specified architecture. \const{FileSpec} is
of the form \const{foreign(Name)}.

At runtime, SWI-Prolog will try to load the shared library which
is compatible with the current architecture, obtained by calling
\term{current_prolog_flag}{arch, Arch}. An architecture is
compatible if one of the two following conditions is true (tried
in order):

\begin{enumerate}
    \item{There is a shared object in the saved state file which
          matches the current architecture name (from
          current_prolog_flag/2) exactly.}
    \item{The user definable \term{qsave:compat_arch}{Arch1, Arch2}
          hook succeeds.}
\end{enumerate}

This last one is useful when one wants to produce one shared object file
that works for multiple architectures, usually compiling for the lowest
common denominator of a certain CPU type. For example, it is common to compile
for armv7 if even if the code will be running on newer arm CPUs. It
is also useful to provide highly-optimized shared objects for
particular architectures.

If \arg{Action} is \textbf{copy}, the foreign extensions are copied to
the installation location.  This feature is currently only supported
for Windows, where both DLLs required to run \program{swipl.exe} and
DLLs loaded through extensions are are copied to the same directory as
where the executable is saved.  Required DLLs are found using
win_process_modules/2.  Thus, we can create an executable using e.g.,

\begin{code}
swipl -o dist/myprog.exe --foreign=copy -c myprog.pl
\end{code}

The \exam{--foreign=copy} option is introduced in 9.3.6.

	\termitem{undefined}{+Value}
Defines what happens if an undefined predicate is found during the
code analysis.  Values are \const{ignore} (default) or \const{error}.
In the latter case creating the state is aborted with a message that
indicates the undefines predicates and from where they are called.
	\termitem{obfuscate}{+Boolean}
If \const{true} (default \const{false}), replace predicate names
with generated symbols to make the code harder to assess for
reverse engineering.  See \secref{obfuscate}.
	\termitem{verbose}{+Boolean}
If \const{true} (default \const{false}), report progress and status,
notably regarding auto loading.
    \end{description}

    \predicate{qsave_program}{1}{+File}
Equivalent to \exam{qsave_program(File, [])}.

    \predicate{autoload_all}{0}{}
Check the current Prolog program for predicates that are referred to,
are undefined, and have a definition in the Prolog library.  Load the
appropriate libraries.

This predicate is used by qsave_program/[1,2] to ensure the saved state
does not depend on availability of the libraries. The predicate
autoload_all/0 examines all clauses of the loaded program (obtained with
clause/2) and analyzes the body for referenced goals. Such an analysis
cannot be complete in Prolog, which allows for the creation of arbitrary terms at
runtime and the use of them as a goal. The current analysis is limited to the
following:

    \begin{itemize}
        \item Direct goals appearing in the body
	\item Arguments of declared meta-predicates that are marked
	      with an integer (0..9).  See meta_predicate/1.
    \end{itemize}

The analysis of meta-predicate arguments is limited to cases where the
argument appears literally in the clause or is assigned using =/2 before
the meta-call.  That is, the following fragment is processed correctly:

\begin{code}
	...,
	Goal = prove(Theory),
	forall(current_theory(Theory),
	       Goal)),
\end{code}

But, the calls to \nopredref{prove_simple}{1} and
\nopredref{prove_complex}{1} in the example below are \emph{not}
discovered by the analysis and therefore the modules that define these
predicates must be loaded explicitly using use_module/[1,2].

\begin{code}
	...,
	member(Goal, [ prove_simple(Theory),
		       prove_complex(Theory)
		     ]),
	forall(current_theory(Theory),
	       Goal)),
\end{code}

It is good practice to use gxref/0 to make sure that the program has
sufficient declarations such that the analysis tools can verify that
all required predicates can be resolved and that all code is called.
See meta_predicate/1, dynamic/1, public/1 and prolog:called_by/2.

    \prefixop{volatile}{+Name/Arity, \ldots}
Declare that the clauses of specified predicates should \strong{not} be
saved to the program.  The volatile declaration is normally used to
prevent the clauses of dynamic predicates that represent data for
the current session from being saved in the state file.
\end{description}


\subsection{Limitations of qsave_program}
\label{sec:qsavelimits}

There are three areas that require special attention when using
qsave_program/[1,2].

\begin{itemize}
    \item
If the program is an embedded Prolog application or uses the foreign
language interface, care has to be taken to restore the appropriate
foreign context. See \secref{qforeign} for details.

    \item
If the program uses directives (\exam{:- goal.} lines) that perform
other actions than setting predicate attributes (dynamic/1, volatile/1,
etc.) or loading files (use_module/1, etc.). Goals that need to be
executed when the state is started must use initialization/1 (ISO
standard) or initialization/2 (SWI extension that provides more control
over when the goal is executed). For example, initialization/2 can be
used to start the application:

\begin{code}
:- initialization(go, main).
\end{code}

    \item
\jargon{Blobs} used as references to the database (see clause/3,
recorded/3), streams, threads, etc.\ can not be saved. This implies that
(dynamic) clauses may not contain such references at the moment the
qsave_program/2 is called.  Note that the required foreign context
(stream, etc.) cannot be present in the state anyway, making it
pointless to save such references.  An attempt to save such objects
results in a warning.

The volatile/1 directive may be used to prevent saving the clauses of
predicates that hold such references. The saved program must
reinitialise such references using the normal program initialization
techniques: use initialization/1,2 directives, explicitly create them
by the entry point or make the various components recreate the context
lazily when required.

    \item
\jargon{Blobs} that properly implement the save() and load() callbacks
can be saved and restored. By default a blob is saved as an array of bytes,
of the internal form of the blob. This means that any saved program
using such a blob is probably not portable to a different architecture.

\end{itemize}


\subsection{Runtimes and Foreign Code}	\label{sec:qsaveforeign}
\label{sec:qforeign}

Many applications use packages that include foreign language components
compiled to shared objects or DLLs. This code is normally loaded using
use_foreign_library/1 and the \const{foreign} file search path.  Below
is an example from the \file{socket} library.

\begin{code}
:- use_foreign_library(foreign(socket)).
\end{code}

There are two options to handle shared objects in runtime applications.
The first is to use the \term{foreign}{save} option of qsave_program/2
or the \cmdlineoption{--foreign=save} commandline option. This causes
the dependent shared objects to be included into the resource archive.
The use_foreign_library/1 directive first attempts to find the foreign
file in the resource archive. Alternatively, the shared objects may be
placed in a directory that is distributed with the application. In this
cases the file search path \const{foreign} must be setup to point at
this directory. For example, we can place the shared objects in the same
directory as the executable using the definition below. This may be
refined further by adding subdirectories depending on the architecture
as available from the Prolog flag \prologflag{arch}.

\begin{code}
:- multifile user:file_search_path/2.

user:file_search_path(foreign, Dir) :-
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, Dir).
\end{code}


\section{State initialization}
\label{sec:state-initialization}

The initialization/1 and initialization/2 directive may be used to
register goals to be executed at various points in the life cycle of an
executable. Alternatively, one may consider \jargon{lazy
initialization} which typically follows the pattern below.  Single
threaded code can avoid using with_mutex/2.

\begin{code}
:- dynamic x_done/0.
:- volatile x_done/0.

x(X) :-
    x_done,
    !,
    use_x(X).
x(X) :-
    with_mutex(x, create_x),
    use_x(X).

create_x :-
    x_done,
    !.
create_x :-
    <create x>
    asserta(x_done).
\end{code}



\section{Using program resources}
\label{sec:program-resources}

A \jargon{resource} is similar to a file. Resources, however, can be
represented in two different formats: on files, as well as part of the
resource \jargon{archive} of a saved state (see qsave_program/2) that
acts as a \jargon{virtual file system} for the SWI-Prolog I/O predicates
(see open/4, register_iri_scheme/3).

A resource has a \jargon{name}. The \jargon{source} data of a resource
is a file. Resources are declared by adding clauses to the predicate
resource/2 or resource/3.  Resources can be accessed from Prolog as
files that start with \verb$res://$ or they can be opened using
open_resource/3.

\subsection{Resources as files}
\label{sec:res-files}

As of SWI-Prolog 7.7.13, resources that are compiled into the program
can be accessed using the normal file handling predicates. Currently the
following predicates transparently handle resources as read-only files:

\begin{shortlist}
    \item open/3, open/4
    \item access_file/2
    \item exists_file/1
    \item exists_directory/1
    \item time_file/2
    \item size_file/2
\end{shortlist}

In addition, open_shared_object/3, underlying use_foreign_library/1
handles \jargon{shared objects} or DLLs by copying them to a temporary
file and opening this file. If the OS allows for it, the copied file is
deleted immediately, otherwise it is deleted on program termination.

With the ability to open resources as if they were files we can use them
for many tasks without changing the source code as required when using
open_resource/2.  Below we describe a typical scenario.

\begin{itemize}
    \item Related resources are placed in one or more directories.
    Consider a web application where we have several directories
    holding icons.  Add clauses to file_search_path/2 that makes
    all icons accessible using the term \term{icon}{file}.

    \item Add a clause as below before creating the state. This
    causes all icons to be become available as
    \verb$res://app/icon/$\arg{file}.

\begin{code}
resource(app/icon, icon(.)).
\end{code}

    \item Add a clause to file_search_path/2 that make the icons
    available from the resource data.  For example using the
    code below.

\begin{code}
:- asserta(user:file_search_path(icon, 'res://app/icon').
\end{code}
\end{itemize}


\subsection{Access resources using open_resource}
\label{sec:res-resources}

Before the system had the ability to open resources as files, resources
were opened using the predicates open_resource/2 or open_resource/3.
These predicates provide somewhat better dynamic control over resources
depending on whether the code is running from files or from a saved
state.  The main disadvantage is that having a separate open call
requires rewriting code to make it work with resources rather than
files.

\begin{description}
    \predicate{open_resource}{2}{+Name, -Stream}
\nodescription
    \predicate{open_resource}{3}{+Name, -Stream, +Options}
Opens the resource specified by \arg{Name}. If successful, \arg{Stream}
is unified with an input stream that provides access to the resource.
The stream can be tuned using the \arg{Options}, which is a subset of
the options provided by open/4.

    \begin{description}
    \termitem{type}{Type}
    \nodescription
    \termitem{encoding}{Encoding}
    \nodescription
    \termitem{bom}{Bool}
Options that determine the binary/text type, encoding for text streams
and whether or not the content should be checked for a BOM marker.  The
options have the same meaning as the corresponding options for open/4.
    \end{description}

The predicate open_resource/3 first checks resource/2.  When successful
it will open the returned resource source file.  Otherwise it will look
in the program's resource database.  When creating a saved state, the
system normally saves the resource contents into the resource archive,
but does not save the resource clauses.

This way, the development environment uses the files (and modifications)
to the resource/3 declarations and/or files containing resource info,
thus immediately affecting the running environment, while the runtime
system quickly accesses the system resources.
\end{description}


\subsection{Declaring resources}
\label{sec:res-declare}

\begin{description}
    \predicate{resource}{2}{:Name, +FileSpec}
\nodescription
    \predicate{resource}{3}{:Name, +FileSpec, +Options}
These predicates are defined as dynamic predicates in the module
\const{user}. Clauses for them may be defined in any module, including
the user module. \arg{Name} is the name of the resource (an atom). A
resource name may contain any character, except for \$ and :, which are
reserved for internal usage by the resource library. \arg{FileSpec} is a
file specification that may exploit file_search_path/2 (see
absolute_file_name/2).

Often, resources are defined as unit clauses (facts), but the
definition of this predicate also allows for rules. For proper
generation of the saved state, it must be possible to enumerate the
available resources by calling this predicate with all its arguments
unbound.

If \arg{FileSpec} points at a directory, the content of the directory is
recursively added below \arg{Name}. If \arg{FileSpec} a term of the form
\term{Alias}{Name}, all directories that match this specification are
enumerated and their content is added to the resource database. If an
file appears in multiple results of this search path only the first file
is added.  Note that this is consistent with the normal behaviour where
absolute_file_name/3 returns the first match.  The \arg{Options} can
be used to control what is saved from a directory.

\begin{description}
    \termitem{include}{+Patterns}
Only include a file from a directory if it matches at least one of the
members of \arg{Patterns}.
    \termitem{exclude}{+Patterns}
Excludes a file from a directory if it matches at least one of the
members of \arg{Patterns}.
\end{description}
\end{description}

\subsection{Managing resource files}
\label{sec:swiplrc}

As of version 7.7.13, SWI-Prolog resource files are zip(1) files. Prolog
creates and accesses its resource files using the
\href{http://www.winimage.com/zLibDll/minizip.html}{minizip} project.
The resource files may be examined and modified using any tool that can
process zip files.

\section{Debugging and updating deployed systems}
\label{sec:debug-deployed-systems}

SWI-Prolog provides several facilities to debug and update running
(server) applications.  The core to these facilities are:

\begin{itemize}
    \item Hot-swap recompilation (\secref{loadrunningcode} and the library
\pllib{hotswap}) allow, with some limitation, making modifications to
running services. This includes adding debugging and logging statements.

    \item To make this useful some form of interaction is required. This
can be implemented using signal handlers (Unix), specific HTTP services,
generic HTTP services (e.g., \href{https://swish.swi-prolog.org}{SWISH})
or networked interaction using the library \pllib{prolog_server} that
allow interaction using netcat (\program{nc}) or \program{telnet}.
\end{itemize}


\section{Protecting your code}
\label{sec:protect-code}

Prolog in general, but SWI-Prolog in particular is an transparent
environment. Prolog's ``code is data'' point of view makes this natural
as it simplifies development and debugging.  Some users though want or
need to protect their code against copying or reverse engineering.

There are three ways to distribute code: as source, as \fileext{qlf}
file and in a saved state.  Both QLF files and saved states contain the
code as \jargon{virtual machine code}. QLF files capture the predicates
and directives, while saved state capture the current state of the
program. From the viewpoint of protecting code there is no significant
difference.

There are two aspects to protection. One is to make sure the attacker
has no access to the code in any format and the other is to provide
access to a non-human-readable version of the code. The second approach
is known as code obfuscation. Code obfuscation typically remove layout
and comments and rename all internal identifiers. If an attacker gets
access to the SWI-Prolog virtual machine code this can be
\jargon{decompiled}. The decompiled code does not include layout
information variable names and comments. Other identifiers, notably
predicate and module names are maintained. This provides some protection
against understanding the source as Prolog code without meaningful
variable names and comments is generally hard to follow.

For further protecting the code, there are several scenarios.

\begin{itemize}
    \item If the user has unrestricted access to the file system on
which the application is installed the user can always
access the state or QLF file. This data can be loaded into a
compatible emulator and be \jargon{decompiled}.

    \item If the user can run arbitrary Prolog code or shell commands
the state can be protected by embedding it as a string in the
executable deny read access to the executable.  This requires a
small C~program that includes the string and uses
PL_set_resource_db_mem() to register the string as the resource
database.  See PL_set_resource_db_mem() for details.  This protection
should be combined with the \prologflag{protect_static_code} described
below.

    \item Some extra protection can be provided using the Prolog
flag \prologflag{protect_static_code}, which disables decompilation of
\jargon{static} predicates. Note that most Prolog implementations cannot
decompile static code. Various SWI-Prolog tools depend on this
ability though. Examples are list_undefined/0, autoload/0,
show_coverage/1, etc.
\end{itemize}

\subsection{Obfuscating code in saved states}
\label{sec:obfuscate}

If the option \term{obfuscate}{true} is used with qsave_program/2,
certain atoms in the saved state are renamed. The renaming is performed
by library \pllib{obfuscate}. The current implementation is rather
conservative, renaming atoms that are used only to define the functor
that names a predicate. This is a safe operation, provided the
application does not create new references to renamed predicates by
reading additional source code or constructing the atom that names the
predicate dynamically in some other way such as using atom_concat/3.
Predicates that are called this way must be declared using public/1.

Note that more aggressive renaming is possible, but this requires more
detailed analysis of the various roles played by some atom.  Helpful
and descriptive predicate names tend to be unique and are thus subject
to this transformation.  More general names tend to collide with other
roles of the same atom and thus prevent renaming.

\section{Finding Application files}	\label{sec:findappfile}

If your application uses files that are not part of the saved program
such as database files, configuration files, etc., the runtime version
has to be able to locate these files. The file_search_path/2 mechanism
in combination with the \cmdlineoption{-p} \arg{alias} command line
argument provides a flexible mechanism for locating runtime files.
