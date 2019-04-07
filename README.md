# SWI-Prolog: A comprehensive Prolog implementation

## Getting the source code

This repositories uses many [GIT
submodules](https://git-scm.com/book/en/v1/Git-Tools-Submodules).
This causes the common __fork and clone not to work__.
Instead, _clone_ from
https://github.com/SWI-Prolog/swipl-devel.git and next associate your
clone with your _fork_.
Follow the steps below.

First, clone this Git repository:

    git clone https://github.com/SWI-Prolog/swipl-devel.git
    cd swipl-devel

Finally, fork this repository on GitHub, and associate your fork:

    # Replace "myfork" with any name you want except "origin".
    # Replace "me" with your github user name.

    git remote add myfork git@github.com:me/swipl-devel.git

Optional: Clone the package submodules you want to use/modify.
(If you don't know which packages you need, skip this step.
You can come back here later.)
You can specify individual package paths in the command line:

    git submodule update --init -- packages/jpl packages/clib packages/sgml

## Building from source

See [CMAKE.md](CMAKE.md)
and [Build SWI-Prolog from source](http://www.swi-prolog.org/build/)

## Contributing

There are several ways to contribute to SWI-Prolog:

- Write source code.
[Submit a patch](http://www.swi-prolog.org/howto/SubmitPatch.html).
- Write or improve documentation. (How?)
- Report an issue. (How?)
- Help answer questions in the [SWI-Prolog Discourse group](https://swi-prolog.discourse.group).
- Propose improvements in the SWI-Prolog Discourse group.
- Donate/sponsor/fund an improvement. (How?)

## Web home

Please find the up-to-date information on SWI-Prolog at
http://www.swi-prolog.org.

## Documentation

Documentation  is  available  from  several  locations  and  in  several
formats.

  - Paper format can be ordered as [print on
  demand](http://books.google.de/books?id=7AeiAwAAQBAJ&hl=en) and is
  available as PDF from the [download
  page](http://www.swi-prolog.org/download/devel).

  - Several tutorials can be accessed from the __Tutorials__ menu on
  the [home page](http://www.swi-prolog.org)

  - A HTML version of the documentation is in the `doc/Manual` directory
  of the installation.  Note that some packagers put this documentation
  elsewhere or require it to be installed seperately.

You can also install the website locally to  use its richness if you are
offline. It is available from https://github.com/SWI-Prolog/plweb.
