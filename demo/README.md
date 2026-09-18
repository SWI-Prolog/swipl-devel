## Some simple Prolog demo programs

This directory contains some simple demos to   get  you started. You can
also use this directory to develop  your   own  programs but this is not
advised for serious usage. For normal usage,   choose  a location on the
filesystem that suits you best.

## Usage on Windows

On Windows platform use the   extension  associated to ``swipl-win.exe``
choosen at installation (default ``.pl``) for   Prolog  source files and
load the main file by double-clicking  it   in  the  Windows explorer or
dragging it onto ``swipl-win.exe`` or a shortcut to this executable (you
can put a shortcut to ``swipl-win.exe`` on your desktop).

## Usage from the commandline

Normally you create  your  Prolog  source   files  using  the  extension
``.pl``. You start a  program  using   one  of  the  following commands.
`swipl` is the commandline version and ``swipl-win`` opens a GUI.

    % swipl file.pl
    % swipl-win file.pl

or  ensuring  the  first  line  reads  as  below  and  make  ``file.pl``
executable using ``chmod +x file.pl``.

    #!/usr/bin/env swipl

## Demos

  - colours.pl <br>
    Shows semantic highlighting of the built-in editor.  Start using
    ``swipl-win colours.pl`` and type ``?- edit.``.
  - likes.pl <br>
    Very simply knowledge base about preferences.  See content on how to use it.
  - trace.pl <br>
    Simple path finding implementation with some excercises to use the debugger.
  - unicode.pl <br>
    Illustrates SWI-Prolog's support for Unicode source.


## Further information (manual, mailing list, user web)

  - https://www.swi-prolog.org

## License

All files in this demo directory are in the public domain.
