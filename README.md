# SWI-Prolog: A comprehensive Prolog implementation

## Forking, cloning and submitting patches

This repositories uses many [GIT
submodules](https://git-scm.com/book/en/v1/Git-Tools-Submodules). This
cause the common __fork and clone not to work__. Instead, _clone_ from
https:github.com/SWI-Prolog/swipl-devel.git and next associate your
clone with your _fork_ using

   git remote add myfork git@github.com:me/clonedrepo.git

See [How to submit a patch](http://www.swi-prolog.org/howto/SubmitPatch.html)
for details.

## Building

See
[CMAKE.md](https://github.com/SWI-Prolog/swipl-devel/blob/master/CMAKE.md)
and [Build SWI-Prolog from source](http://www.swi-prolog.org/build/)


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
