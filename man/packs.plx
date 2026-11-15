\chapter{Packs: community add-ons}
\label{sec:packs}

\index{add-ons}%
\index{community, extensions}%
SWI-Prolog has a mechanism for incorporating community extensions
called \jargon{packs}. See the
\href{https://www.swi-prolog.org/pack/list}{pack landing page} for
details and available packs.  This chapter discusses how packages can
be attached to the current Prolog process, how they can be installed
as well as developing packages.

Packs are installed as self-containing directories that provide
additional Prolog libraries and \jargon{foreign modules}, compiled
native code plugins. In addition, a pack can define \jargon{apps},
command line tools that can be started using \exam{swipl app [args]}
(see \secref{swipl-app}). Packs are searched as sub-directories of the
Prolog search path \const{pack}. Initially, this search path is the
user's \jargon{App data}, followed by the system's \jargon{App data}.
The searched directories can be found using

\begin{code}
?- absolute_file_name(pack(.), Path, [solutions(all)]).
\end{code}

The search path can be managed using the environment variable
\const{SWIPL_PACK_PATH}, the \cmdlineoption{-p} command line option or
using attach_packs/2.


\section{Installing packs}
\label{sec:pack-install}

As of version 9.1.22, SWI-Prolog supports three models for managing
packs: \jargon{shared packages} are added to the user or system
environment, while \jargon{project specific packages} are added to a
particular project only.  Finally, project specific packages can be
managed as \jargon{git submodules}.  These three approaches are
discussed in more detail below.

Using pack_install/2 we can install a package either for the current
user or globally for all users.

\paragraph{Shared packages}

System-wide installations
is satisfactory as long as all projects can use the same version of a
pack, the packs required by all projects do not conflict, and
redistribution of the projects is not a primary concern.  For example,
if you frequently require RocksDB for several projects you are working
on, installing the \const{rocksdb} pack as user is appropriate.

The shared model is similar to e.g., Python's \program{pip} installer.
Python resolves dealing with packages for a specific project using
\jargon{virtual environments}, where each virtual environment provides
a selection of packages.  A Python virtual environment may be
\jargon{activated} for the current shell, which modifies the shell's
environment variables.


\paragraph{Project specific packages}

Alternatively, SWI-Prolog allows packs to be installed as part of a
project.  This approach is also found with \program{npm}, the Node.js
package manager.  Using project-specific packs with SWI-Prolog
requires calling attach_packs/2 before loading any library from a
pack.  To use (only) packs from the local sub directory \file{packs},
add this directive to the code that uses it:

\begin{code}
:- attach_packs(packs, [replace(true)]).
\end{code}

Packs can be installed into the \file{packs} directory directly using
pack_install/2 with the \term{pack_directory}{Dir} option or using
the \const{pack} \jargon{app} as

\begin{code}
swipl pack install --dir=packs <pack>
\end{code}

The preferred way is to use pack_install_local/3.  This predicate
takes a \jargon{closure} to collect the desired packages, creates an
installation plan and executes this.  This ensures a set of compatible
packs at their latest available version or explicitly specified
versions.  Typically, one would create a file \file{packs.pl}
according to the example below to install the packages required by
a project.  By using such a file it is easy to replicate a suitable
set of installed packs for anyone who wishes to use your application.

\begin{code}
:- module(packs, []).
:- use_module(library(prolog_pack)).
:- attach_packs(packs, [replace(true)]).

:- initialization(install, main).

pack(scasp, [commit('HEAD')]).
pack(environ, []).
pack(date_time, []).

install :-
    pack_install_local(pack, packs, []).
\end{code}

Here, the attach_packs/2 must be the same as used by the project.  The
first argument of pack_install_local/2 refers to \nopredref{pack}{2},
generating a list of target packages and options for each package.
The options for each pack are defined by pack_install/2.  They
typically refer to the download location and required version.  Given
the above, we can install the packages we need for a project using

\begin{code}
swipl packs.pl
\end{code}

\paragraph{Using GIT submodules}

Alternative to the above, if the desired packs are all available
as git repository, we can add packs to our git managed projects
by adding the packs as git submodules to our project.  For example,
we add a pack to the \file{packs} directory as

\begin{code}
mkdir packs
git submodule add https://github.com/SWI-Prolog/sCASP.git packs/scasp
git submodule add https://github.com/fnogatz/tap.git tap
\end{code}

As above, we can must make our project use the local packs by
calling pack_attach/2.  After fetching all submodules we can build
the foreign components and/or run the tests of the attached packs
using the steps below

\begin{code}
?- attach_packs(packs, [replace(true)]).
?- pack_rebuild.
\end{code}

Using git submodules gives full control of the pack versions you are
using.  It also makes you responsible of adding dependencies and
taking care of version dependencies between packs.  Finally, it
limits you to using git based packages.


\section{Built-in predicates for attaching packs}
\label{sec:pack-attach}

This section documents the built-in predicates to attach packs.
Predicates for creating, registering and installing packs are provided
by the library \pllib{prolog_pack}.

\begin{description}
    \predicate{attach_packs}{0}{}
Attaches all packs in subdirectories of directories that are accessible
through the \jargon{file search path} (see absolute_file_name/3)
\const{pack}.  The default for this search path is given below.  See
file_search_path/2 for the \const{app_data} search path.

\begin{code}
user:file_search_path(pack, app_data(pack)).
\end{code}

The default path may be overruled with the environment variable
\const{SWIPL_PACK_PATH}.  This variable must contain a list of
directories separated by the OS-specific \prologflag{path_sep}.

The predicate attach_packs/0 is called on startup of SWI-Prolog.

    \predicate{attach_packs}{1}{+Directory}
    \nodescription
    \predicate{attach_packs}{2}{+Directory, +Options}
Attach all packs that are subdirectories of \arg{Directory}.
\arg{Directory} is translated into a physical directory using
absolute_file_name/3.   This implies it can be a term \arg{Alias(SubDir)}
and the search is relative to the current source file if \arg{Directory}
is not an absolute path and these predicates are used as a directive.
Defined options are:

    \begin{description}
	\termitem{search}{+Where}
Determines the order in which pack library directories are searched.
Default is to add new packages at the end (\const{last}).  Using
\const{first}, new packages are added at the start.

	\termitem{duplicate}{+Action}
Determines what happens if a pack with the same name is already
attached.  Default is \const{warning}, which prints a warning and
ignores the new pack.  Other options are \const{keep}, which is
like \const{warning} but operates silently and \const{replace},
which detaches the old pack and attaches the new.

	\termitem{replace}{+Boolean}
If \const{true}, unregister all packs before registering the new
packs.
    \end{description}

The predicate attach_packs/2 can be used to attach packages that
are bundled with an application.  With the option \term{replace}{true},
attach_packs/2 ensures that the application only relies on bundled
packs.

    \predicate{pack_attach}{2}{+PackDir, +Options}
Attach a single package in \arg{PackDir}. \arg{PackDir} is expected to
contain a file `pack.pl` with the pack metadata and a `prolog`
directory. Options processed:

    \begin{description}
    \termitem{duplicate}{+Action}
What to do if the same package is already installed in a different
directory.  \arg{Action} is one of
        \begin{description}
	\termitem{warning}{}
	Warn and ignore the package.
	\termitem{keep}{}
	Silently ignore the package.
	\termitem{replace}{}
	Unregister the existing and insert the new package
	\end{description}
    \termitem{search}{+Where}
    Determines the order of searching package library directories.
    Default is \const{last}, alternative is \const{first}.
    \end{description}
\end{description}

\input{lib/prologpack}

\section{Structure of a pack}
\label{sec:pack-structure}

A \jargon{pack} is a directory that has two obligatory components:

\begin{enumerate}
\item A directory named \file{prolog}.   When the pack is attached,
  this directory is added to the \const{library} file search path.
  This implies that any \fileext{pl} file that appears in this
  directory can be loaded into Prolog using \exam{:- use_module(library(file)).}
  Alternatively, a file from a specific package can be loaded using e.g.,
  \exam{:- use_module(pack(environ/prolog/environ)).}
\item A file \file{pack.pl}.  This file provides the \jargon{meta data}
  for the pack.  See \secref{pack-metadata} for details.
\end{enumerate}

In addition, a pack may, and often does, include \jargon{foreign code}.
The current system provides support for classical Unix make files, GNU
autoconf/automake and CMake.  See \secref{pack-foreign} for details.
This build infrastructure is also used to test the package.

A pack can be made accessible in two ways

\begin{enumerate}
\item As an archive file.  This file must be named as below, where
  version is a dotted version number and <ext> is either \fileext{tgz}
  (gzipped tar archive) or \fileext{zip}.

\begin{code}
<pack>-<version>.<ext>
\end{code}

The pack contains the contents of the package.  The root of the
archive is identified by locating the file \file{pack.pl}.  Extraction
ignores the path leading to this file.  Typically, the archive
contains a single directory named after the package name without
version.

Installing packs from archives requires that SWI-Prolog has the
\const{archive} extension installed.  When a package is registered
with the central package server the server identifies it by the SHA1
hash of the archive.  It is therefore important that the archive is
never modified after registration.  If \emph{any} modification is
required (including comments, documentation, etc,) the user
\emph{must} create a new version.

\item A git repository.  This is now the preferred option because it
  provides a persistent location and easy version management.
\end{enumerate}

\section{Developing a pack}
\label{sec:pack-devel}

We recommend using GIT for developing SWI-Prolog packages.  To start a
new package, invent a name and verify that the name is not yet in use
at The \href{https://www.swi-prolog.org/pack/list}{pack landing page}.
Create a directory with this name, a sub-directory \file{prolog} and a
the metadata file \file{pack.pl} that contains at least the name and
version of the pack.  Below is a simple example.  See
\secref{pack-metadata} for all possible metadata fields.

\begin{code}
name(hello).
version('1.0.0').
title('Hello world').
keywords([demo]).
author( 'Bob Programmer, 'bob123@programmer.me. ).
download('https://github/bob123/hello.git').
\end{code}

Now, add the Prolog libraries provided to the \file{prolog} directory.
While doing so, please pay attention to the points below.  If your are
looking for examples of well structured libraries, please look at the
system libraries.

\begin{itemize}
\item Only add \jargon{module} files to the \file{prolog} directory.
\item Be aware that the modules your pack provides are globally
  accessible as \term{library}{File}.  Thus, make sure the name
  is fairly unique and the module name is typically the same as
  the \jargon{base name} of the file.
\item Modules that need not be immediately visible to the user
  should be placed in a subdirectory.  Typically one uses the
  pack name to name the subdirectory.\footnote{Using e.g.,
  \file{private} is not a good idea because the \file{private}
  directory of each pack using this would be available as
  \term{library}{Pack/private}.}  Use e.g., pack_\bnfmeta{name}
  for the module names of the private files.
\item Consider documenting the files using PlDoc.
\end{itemize}

Once the pack is ready for a very first test, we can make it
accessible using the command below.  On non-Windows systems,
this makes the pack accessible using a \jargon{symbolic link}
from your personal pack directory to this directory.

\begin{code}
swipl pack install .
\end{code}

After this command the new libraries should be available when
you start a new SWI-Prolog process.   Another way to make the
pack accessible is by using the \const{pack} search path (see
file_search_path/2).  The command (from the pack directory)
is

\begin{code}
swipl -p pack=..
\end{code}

\subsection{The pack meta data}
\label{sec:pack-metadata}

A pack must have a file \file{pack.pl} in its root directory.
The file contains Prolog terms.   Defined terms are below.  The
argument types are types registered with must_be/2 and described
in the running text.

\begin{description}
  \termitem{name}{atom}
  Name of the pack.  This should be the same as the directory
  name.  Names can be constructed from the ASCII letters,
  underscore and digits, e.g., \verb$[a-zA-Z9-0_]+$
  \termitem{title}{atom}
  Short summary of the package.  Do not use line breaks and
  limit respect at maximum length of about 40 characters.
  \termitem{keywords}{list(atom)}
  List of keywords that help finding your pack.  There is no
  fixed set of keywords to choose from.
  \termitem{description}{list(atom)}
  Longer description as a list of lines.
  \termitem{version}{version}
  Current version of the pack.  This is a list of integers
  separated by dots.  There is no limit to the number of
  sub revisions.
  \termitem{author}{atom, email_or_url_or_empty}
  Original author of the code.  If the contact address is
  unknown it may be omitted (empty atom).  Repeat this
  term for multiple authors.
  \termitem{maintainer}{atom, email_or_url}
  \nodescription
  \termitem{packager}{atom, email_or_url}
  As \const{author}, but the contact cannot be empty.  May
  be repeated.
  \termitem{pack_version}{nonneg}
  Package convention number.  Currently 1 (default) or 2.
  Version 2 provides better support for building foreign
  extensions.
  \termitem{home}{atom}
  Location of th home page.   This is typically a URL.
  \termitem{download}{atom}
  Location for downloading.   This is either the URL of
  the GIT repository or a wildcard URL for downloading
  the archive, e.g., \url{https://me.com/packs/mypack-*.zip}.
  An upgrade request fetches the \url{https://me.com/packs/},
  expecting an HTML page with links to the available versions.
  It then selects the latest version.
  \termitem{provides}{atom}
  Announce that the pack provides facilities identified by
  the given token.  Optionally, the token may be given a
  version using \term{@}{Token,Version}.  A pack implicitly
  provides \term{@}{PackName,PackVersion}.  The supplied
  tokens operate in the same \jargon{name space} as packages
  and thus the same care must be taken to select a name.  Multiple
  of these claims may be present.
  \termitem{requires}{dependency}
  The pack depends on the availability of \arg{Dependency}.  The
  \arg{Dependency} is a token, normally the name of another package.
  See \const{provides}.  The dependency may be further refined by
  writing \exam{Token Cmp Version}, where \arg{Cmp} is one of Prolog's
  standard numerical comparison operators.  See cmp_versions/3.
  This metadata is also used to state requirements on Prolog.  See
  \secref{pack-prolog-requires}.
  Multiple requirements are expressed with multiple claims.
  \termitem{conflicts}{dependency}
  The pack cannot be use together with the indicated \arg{Dependency}.
  This is the negation of \const{requires}.
  \termitem{replaces}{atom}
  This pack replaces some other pack.
  \termitem{autoload}{boolean}
  If \const{true}, add the library for the package as \jargon{autoload}
  library.   This implies that the exported predicates may be used
  without explicitly importing the library.   Use with care.
\end{description}

\subsubsection{Pack requirements on Prolog}
\label{sec:pack-prolog-requires}

The file \file{pack.pl} may contain \term{requires}{Requirement}
statements.  Normally, \arg{Requirement} is a pack or token,
optionally with a version requirement.  The requirement \const{prolog}
is reserved for requirements on the Prolog version while
\mbox{\const{prolog:}\arg{Feature}} may be used to demand specific
features.  Feature matching is described with
require_prolog_version/2.  Multiple requirements on Prolog must all be
true.  Below are some examples

\begin{code}
requires(prolog >= '9.2').	  % 9.2.0 or later
requires(prolog:threads).	  % flag threads = true
requires(prolog:library(socket)). % library(socket) exists
requires(prolog:bounded(false)).  % flag bounded = false
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Packs with foreign code}
\label{sec:pack-foreign}

Many packs include C or C++ resources.  Such packs include the C or
C++ resources in a subdirectory of the pack.  There are no restrictions
for naming this subdirectory or structuring the source files in this
directory.   The build process must create native \jargon{modules} in
the directory \file{lib/<arch>}, where <arch> is the architecture as
obtained by the Prolog flag \prologflag{arch}.

The build process identifies control files that tell the package
manager which build tool to use.  The package manager populates
the process environment with variables that provide details about
the running Prolog instance.   This environment is saved in a
file \file{buildenv.sh} in the pack root or build directory.  By
\jargon{sourcing} this file, the user may run the build tools by
hand for debugging purposes.

The build process consists of five steps that are described below

\begin{description}
  \definition{dependencies}
  This step currently only supports \const{conan}.  It is executed
  if either \file{conanfile.txt} or \file{conanfile.py} is found in
  the root directory of the pack.
  \definition{configure}
  This preparation step is executed if one of \file{CMakeLists.txt}
  (\program{cmake}), \file{configure}, \file{configure.in}
  (\program{autoconf}), \file{configure.ac} or \file{Makefile.am}
  (\program{automake}) are found.  The program to manage them is
  in parenthesis.
  \definition{build}
  Build the process.  When configured using (\program{cmake}) this
  will use (\program{cmake}).  Otherwise either \file{Makefile} or
  \file{makefile} is expected and Unix \program{make} is used to
  build the process.
  \definition{test}
  Test the project.  Either uses \program{cmake} or the GNU convention
  \exam{make check}.
  \definition{install}
  Install the project.  Either uses \program{cmake} or the GNU convention
  \exam{make install}.
\end{description}

While running the above tools, the environment is populated.  The names
of the variables provided depends on the \term{pack_version}{Version}
metadata.  We give the names for version~2, with the names for version~1
in parenthesis if this differs from the version~2 name.

\begin{description}
  \definition{\const{PATH}}
Contains the environment path with the directory holding the currently
running SWI-Prolog instance prepended in front of it.  As a result,
\program{swipl} is always present and runs the same SWI-Prolog instance as the
current Prolog process.
  \definition{\const{SWIPL}}
Contains the absolute file name of the running executable.
  \definition{\const{SWIPL_PACK_VERSION}}
Version of the pack system (1 or 2).  If not present we
must assume `1'.
  \definition{\const{SWIPL_VERSION} (\const{SWIPLVERSION}) }
Contains the numeric SWI-Prolog version defined as
$Major\times{}10000 + Minor\times{}100 + Patch$
  \definition{\const{SWIPL_HOME_DIR} (\const{SWIHOME})}
Contains the directory holding the SWI-Prolog home.
  \definition{\const{SWIPL_ARCH} (\const{SWIARCH})}
contains the machine architecture identifier.
  \definition{\const{SWIPL_MODULE_DIR} (\const{PACKSODIR})}
contains the destination directory for shared objects/DLLs
relative to a Prolog pack, i.e., \file{lib/\$SWIARCH}.
  \definition{\const{SWIPL_MODULE_LIB} (\const{SWISOLIB})}
The SWI-Prolog library or an empty string when it is not required
to link modules against this library (e.g., ELF systems)
  \definition{\const{SWIPL_LIB} (\const{SWILIB})}
The SWI-Prolog library we need to link to for programs that
\jargon{embed} SWI-Prolog (normally \exam{-lswipl})
  \definition{\const{SWIPL_INCLUDE_DIRS}}
CMake style variable that contains the directory holding
\file{SWI-Prolog.h}, \file{SWI-Stream.h} and
\file{SWI-cpp2.h}.
  \definition{\const{SWIPL_LIBRARIES_DIR}}
CMake style variable that contains the directory holding
\file{libswipl}
  \definition{\const{SWIPL_CC} (\const{CC})}
C compiler used to build SWI-Prolog.
  \definition{\const{SWIPL_CXX} (\const{CXX})}
C++ compiler used to build SWI-Prolog.
  \definition{\const{SWIPL_LD} (\const{LD})}
Linker used to link SWI-Prolog.
  \definition{\const{SWIPL_CFLAGS} (\const{CFLAGS})}
C-Flags for building extensions. Always contains \exam{-ISWIPL-INCLUDE-DIR}.
  \definition{\const{SWIPL_MODULE_LDFLAGS} (\const{LDSOFLAGS})}
Link flags for linking modules.
  \definition{\const{SWIPL_MODULE_EXT} (\const{SOEXT})}
File name extension for modules (e.g., \fileext{so} or \fileext{dll})
  \definition{\const{SWIPL_PREFIX} (\const{PREFIX})}
Install prefix for global binaries, libraries and include files.
\end{description}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Compiling a foreign extension using a simple Makefile}
\label{sec:pack-makefile}

If the package requires some C code to be compiled that has no
dependencies and needs no configuration it is probably easiest to use
a simple Unix make file.  We assume \term{pack_version}{2}.  Here is a
simple \file{Makefile}.  We assume the pack contains a file
\file{c/environ.c} that contains the C source.  Following the GNU
guidelines, the \file{Makefile} must define the following targets:

\begin{description}
  \definition{all (default)}
  Build the foreign extension.  In this very simple case we build
  the resulting module directly in the target directory.
  \definition{check}
  Test the package.  This is executed after the default build target.
  \definition{install}
  Install the package.  In this case this does nothing.
  \definition{clean}
  Clean the package.  This target disposes intermediate build products.
  \definition{distclean}
  Restore the package to its fully clean state.  This implies that
  all built products and intermediate build products are removed.
  The \const{distclean} target is used by pack_rebuild/1.
\end{description}

\begin{code}
MODULE= $(SWIPL_MODULE_DIR)/environ.$(SOEXT)
CFLAGS= $(SWIPL_CFLAGS)

all:    $(MODULE)

OBJ=c/environ.o

$(MODULE): $(OBJ)
        mkdir -p $(SWIPL_MODULE_DIR)
        $(SWIPL_LD) $(SWIPL_MODULE_LDFLAGS) -o $@ $(OBJ) $(SWIPL_MODULE_LIB)

check::
	$(SWIPL) -g run_tests -t halt test/test_environ.pl
install::
clean:
        rm -f $(OBJ)
distclean: clean
        rm -f $(MODULE)
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Publishing a pack}
\label{sec:pack-publish}

As described in \secref{pack-structure}, a pack is distributed either
as an archive file or as a GIT repository.  We strongly encourage
using a GIT repository as that gives good version and provenance
support.  Packs may be published by hand by making the archive or git
repository available from a globally accessible place on the internet
and installing the pack from this location.  This process is
streamlined, notably for GIT packs using pack_publish/2 and the
\jargon{app} \const{pack}.  To publish a pack a local GIT repository
that has publicly accessible \jargon{origin},

\begin{enumerate}
\item Update \term{version}{Version} in \file{pack.pl}
\item Commit all changes, make sure the the repository is clean.
\item Run
\begin{code}
swipl pack publish .
\end{code}
\end{enumerate}

This will

\begin{enumerate}
\item Verify the repository is clean and on the default branch.
\item \jargon{Tag} the repository with V\bnfmeta{version}.  By
  default, the tag will be \jargon{signed}.  Please setup signing
  for GIT or use the ``--no-sign`` option.
\item Push the repository and release tag.
\item Figure out the download location, either from the
  \term{download}{URL} metadata or the GIT remote information.
\item Install the package and its dependencies in a temporary
  isolated pack environment.
\item On success, register the pack with the server.
\item Delete the isolated pack environment.
\end{enumerate}

Similarly, a pack can be published from a public archive using
the command below.   When using an archive, \textbf{never} change
the content of the archive but, instead, create a new archive with
a new version.

\begin{code}
swipl pack publish URL
\end{code}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Compiling a foreign extension using CMake}
\label{sec:pack-cmake}

If the package is more complicated, a simple Makefile typically does
not suffice.  In this case we have two options.  One is to use the GNU
\program{autoconf} or \program{automake}.  However, \program{cmake} is
getting more popular and provides much better support for non-POSIX
platforms, e.g., Windows.  This section discusses building the same
package as \secref{pack-makefile} using \program{cmake}.

To use \program{cmake}, add the content below as the file
\file{CMakeLists.txt} to the root directory of the pack.  SWI-Prolog
ships with a \program{cmake} \jargon{include} file named
\file{swipl.cmake} that deals with most of the configuration issues.
Comments in the file below explain the various steps of the process.

\begin{code}
cmake_minimum_required(VERSION 3.10)
project(swipl-pack-environ)

# Include swipl.cmake from the running SWI-Prolog's home
list(INSERT CMAKE_MODULE_PATH 0 $ENV{SWIPL_HOME_DIR}/cmake)
include(swipl)

# Create the library as a CMake module
add_library(environ MODULE c/environ.c)

# Link the library to SWI-Prolog.  This also removes the `lib` prefix
# from the target on systems that define a common library file prefix
target_link_swipl(environ)

# Install the foreign target. `${swipl_module_dir}` contains the
# directory for installing modules for this architecture.

install(TARGETS environ
        DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/${swipl_module_dir})

# Run  tests.  This  is  executed   before    the   pack  is  installed.
# swipl_test(name) runs Prolog with the command line below.
#
#    swipl -p foreign=${CMAKE_CURRENT_SOURCE_DIR}/${swipl_module_dir} \
#          -p library=${CMAKE_CURRENT_SOURCE_DIR}/prolog \
#          --on-error=status \
#          -g test_${name} \
#          -t halt \
#          ${CMAKE_CURRENT_SOURCE_DIR}/test/test_${name}.pl
#
# This  implies  that  a  test  `name`  must    be  defined  in  a  file
# `test/test_${name}.pl`, which exports a  predicate `test_${name}`. The
# test succeeds if this predicate  succeeds   and  no error messages are
# printed.

enable_testing()
swipl_add_test(environ)
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Updating a package}
\label{sec:pack-update}

If a package needs a revision to fix bugs or add functionality it
needs to be updated.  First, we create a development environment
using

\begin{enumerate}
\item Clone the git repository that provides the pack.
\item Install the pack \emph{as a link} using the command below.  If
  the pack contains foreign build scripts, this creates a file
  \file{buildenv.sh} that contains the environment variables for
  building the pack.
\begin{code}
?- pack_install(.).
\end{code}
\end{enumerate}

Next, we can edit the pack sources and rebuild it the chosen build
tools after running \exam{source buildenv.sh} to set the appropriate
environment variables.  After validating that the pack works as
expected follow the instructions in \secref{pack-publish} to publish
the new version.
