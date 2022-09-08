# SWI-Prolog: A comprehensive Prolog implementation

SWI-Prolog is an open  source  (BSD-2)   implementation  of  the  Prolog
language with many extensions. It is implemented   in C (version 11) and
Prolog and is available for many platforms (Linux, practically any POSIX
like system, MacOS, Windows). All  CPUs   supported  by Debian Linux are
supported. A prototype running  in  your   browser  based  on  WASM (Web
Assembly) is available.

(SWI-)Prolog is a versatile language. It is being used for business rule
validation, natural language processing, software verification, software
refactoring, network configuration,  security,   robotics,  reasoning in
legal and medical domains,  graph   processing,  machine  learning (ILP,
PLP), linked data (RDF), mathematical proofs, and much more.

If you are interested in commercial   assistence to make SWI-Prolog work
in   your   organization,   please     contact   [SWI-Prolog   Solutions
b.v.](https://swi-prolog.com/).


## Forking, cloning and submitting patches

This           repository           uses             many           [Git
submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules).   This
causes the common issue that __fork   and clone doesn't work__. Instead,
_clone_  from  https://github.com/SWI-Prolog/swipl-devel.git   and  then
associate your clone with your  _fork_   (replace  `me` with your github
user name).

    git clone https://github.com/SWI-Prolog/swipl-devel.git
    cd swipl-devel
    git submodule update --init
    git remote add myfork git@github.com:me/swipl-devel.git

See [How to submit a patch](https://www.swi-prolog.org/howto/SubmitPatch.html)
for details.

See   also   the   discussion    at     [Being    friendly    to   quick
contributions](https://swi-prolog.discourse.group/t/being-friendly-to-quick-contributions/493/6)


## Building

See
[CMAKE.md](https://github.com/SWI-Prolog/swipl-devel/blob/master/CMAKE.md)
and [Build SWI-Prolog from source](https://www.swi-prolog.org/build/)


## Web home

Please   find   the   up-to-date   information     on    SWI-Prolog   at
https://www.swi-prolog.org.

## Trying SWI-Prolog online

An    online    version    of     SWI-Prolog      is     provided     by
[SWISH](https://swish.swi-prolog.org). Note that this version is subject
to sandbox restrictions and does not provide the features most valued in
SWI-Prolog such as its rich set of interfaces, multi-threading, modules,
etc.

## Forum/mailing list

Our       forum       is       hosted        at       a       [Discourse
site](https://swi-prolog.discourse.group/). The forum provides   a  mail
list interface.


## Documentation

Documentation is available from several locations and in several formats.

  - Several tutorials can be accessed from the __Tutorials__ menu on
    the [home page](https://www.swi-prolog.org)

  - A HTML version of the documentation is in the `doc/Manual` directory
    of the installation.  Note that some packagers put this documentation
    elsewhere or require it to be installed separately.

    These docs can be searched using `?- apropos("query").`.  Help on
    a predicate can be disaplayed using e.g., `?- help(append/3).`

  - A PDF version of the documentation is available from the [download
    page](https://www.swi-prolog.org/download/devel)

You  can  also  install  the  website    locally  to  use  its  complete
functionality   if   you   are    offline.     It    is   available   at
https://github.com/SWI-Prolog/plweb.
