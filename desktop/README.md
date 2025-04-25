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

To be done

## Windows

To be done
