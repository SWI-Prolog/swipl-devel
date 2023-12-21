# SWI-Prolog maintenance scripts

This directory  contains various scripts used  for regular maintenance
and managing  releases.  Some are  old and merely kept  for reference.
See also `doc/Release.md`.  Some actively used scripts:

## Release management

  - `newversion` <br>
    Used to update the SWI-Prolog version.  Edit `VERSION` in the top-dir
	and run `scripts/newversion` to update `SWI-Prolog.h`,`man/main.doc`
	and set a GIT release tag on the repo and its submodules.

  - `mkchangelog` [--no-date] [version] <br>
    Generate the changelog.  Without arguments dump changes since the
	last release to stdout.   After generating a release, the changelog
	is generated using the command below.  Use the previous version.

		scripts/mkchangelog --no-date 9.1.20

  - `make-distribution` <br>
    Intended to be _sourced_.  Defines functions to build and upload
	a release.  Needs editing when used outside by build machines.

  - `make-src-tape` <br>
    Create a .tar.gz file from the GIT repo and its submodules.  See
	script for options.   Normally used through `make-distribution`


## Build management

 - `configure` <br>
   Sets CMake flags depending on the name of the build directory as
   `build.feature1-feature2-...`.  See script for details. For example:

       mkdir build.pgo
	   cd build.pgo
	   ../scripts/configure

 - `swipl-activate` [--installed] <br>
   Activate the Prolog from the current build directory in `$HOME/bin`.
   By default from the build dir, using `--installed` from the installed
   location.

 - `macos-deps.sh` <br>
   Build MacOS dependencies for releases.  See script for details.

## Helpers

 - `macosx_bundle_fixup.sh` <br>
   Used by `CPack` to finalize the dependencies in the MacOS bundle.
