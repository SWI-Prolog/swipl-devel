# SWI-Prolog customization files

This directory contains example user-customisation   files. Please check
the files themselves for options and installation hints.

The file `init.pl` provides a template  for personalizing SWI-Prolog. On
Unix systems it is installed using

    mkdir -p ${XDG_DATA_HOME-$HOME/.config}/swi-prolog
    cp init.pl ${XDG_DATA_HOME-$HOME/.config}/swi-prolog

On Windows systems it is searched   for  in `swi-prolog` subdirectory of
the CSIDL folder CSIDL_APPDATA.

The `edit` script is for asking a PceEmacs server to edit files for you.
PceEmacs normally creates a unix  domain   socket  in  its configuration
directory. After starting PceEmacs, edit can be used as

    edit file
    edit file:line
    edit file:line:column

If PceEmacs cannot be contacted, `emacs` is called.
