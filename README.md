# SWI-Prolog: A comprehensive Prolog implementation

## Forking, cloning and submitting patches

This repositories uses many [Git
submodules](https://git-scm.com/book/en/v1/Git-Tools-Submodules). This
causes the common issue that __fork and clone doesn't work__. Instead, _clone_ from
https://github.com/SWI-Prolog/swipl-devel.git and then associate your
clone with your _fork_ (replace `me` with your github user name).

    git clone https://github.com/SWI-Prolog/swipl-devel.git
    cd swipl-devel
    git submodule update --init
    git remote add myfork git@github.com:me/swipl-devel.git

See [How to submit a patch](http://www.swi-prolog.org/howto/SubmitPatch.html)
for details.

See also the discussion at
[Being friendly to quick contributions](https://swi-prolog.discourse.group/t/being-friendly-to-quick-contributions/493/6)

## Building

See
[CMAKE.md](https://github.com/SWI-Prolog/swipl-devel/blob/master/CMAKE.md)
and [Build SWI-Prolog from source](http://www.swi-prolog.org/build/)


## Web home

Please find the up-to-date information on SWI-Prolog at
http://www.swi-prolog.org.

## Documentation

Documentation is available from several locations and in several formats.

  - Paper format can be ordered as [print on
  demand](http://books.google.de/books?id=7AeiAwAAQBAJ&hl=en) and is
  available as PDF from the [download
  page](http://www.swi-prolog.org/download/devel).

  - Several tutorials can be accessed from the __Tutorials__ menu on
  the [home page](http://www.swi-prolog.org)

  - A HTML version of the documentation is in the `doc/Manual` directory
  of the installation.  Note that some packagers put this documentation
  elsewhere or require it to be installed separately.

You can also install the website locally to use its complete functionality if you are
offline. It is available at https://github.com/SWI-Prolog/plweb.
