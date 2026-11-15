# Desktop integration

This  directory contains  files  for desktop  integration.  These  are
included either  with `swipl-win` or  if `xpce` is  included.  Desktop
files are installed  into the `desktop` directory in  the Prolog home.
This directory contains `swipl.png`, the default desktop icon.


## Freedesktop.org (Linux)

If  `swipl-win` is  installed,  this adds  `swipl-win.desktop` to  the
`desktop`   directory`.    If   `xpce`   is   installed,   this   adds
`swipl.desktop` to the `desktop` directory`.

The `.desktop` files must be installed in

  - For a user in `~/.local/share/applications`
  - For all users in `/usr/share/applications/myapp.desktop`

The desktop database is updated using

    update-mime-database ~/.local/share/mime`

To enable opening files, the MIME type   `x-prolog` needs to be defined.
To do this, copy `prolog-mime.xml` to

  - `~/.local/share/mime/packages`

To announce the new MIME type, use

    update-mime-database ~/.local/share/mime

## MacOS

The macOS application icon is `swipl.icns`, a prebuilt asset committed to
this directory.  It is derived from `swipl-256.png` and copied into the build
tree by `src/CMakeLists.txt` (it is *not* generated during a normal build, so
that builds work in restricted environments such as sandboxes or CI, where
`iconutil` is unavailable).

To refresh it after changing `swipl-256.png`, run on macOS (outside any
sandbox, as `iconutil` needs the IconServices system service):

    ninja regenerate_icns

then commit the updated `swipl.icns`.  Equivalently, run the generator
directly:

    sh make_icns.sh swipl-256.png swipl.icns

## Windows

To be done
