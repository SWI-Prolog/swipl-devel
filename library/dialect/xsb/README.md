# XSB Prolog dialect support

This directory provides the library emulation   for  XSB. It is normally
activated using the directive below or by loading a file with the ``.P``
extension.

    :- expects_dialect(xsb).

The library files themselves are either   SWI-Prolog module files or XSB
module files. They are available through   the XSB directive below after
the xsb dialect is selected.

    :- import Pred, ... from Library.

## Providing a transparent user experience

The user may include the following   in ``<config>/init.pl`` to activate
XSB dialect support simply by loading ``.P`` files.

    :- use_module(library(dialect/xsb/source)).
