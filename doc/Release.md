# Creating a SWI-Prolog release

This  document  summarises  the  steps  and  prerequisites  to  build  a
SWI-Prolog release. Most of the scripts  are in the `scripts` directory.
Many of these scripts contain paths  and   addresses  that relate to how
things are setup on my development machines.

## Machines

Releases are currently build using a  Linux machine running Ubuntu 20.04
and a Mac running MacOS 10.15.  For  a   full  release  you need a Linux
machine and a Mac. With minor adjustments, any Linux distro will do, but
the hardware must be amd64 to  be  able   to  run  Wine for building the
Windows releases.

## Preparing the build machines

This only needs to be done once.   One  must regularly check Windows and
MacOS dependencies for new releases, in particular for OpenSSL.

### Linux machine

  - Install the [dependencies](https://www.swi-prolog.org/build/Debian.txt),
    including those for building the PDF documentation

#### Preparing building Windows releases

  - Install wine, MinGW cross-compilers and the NSIS packager.  These
    are currently the following packages
    - mingw-w64
    - gcc-mingw-w64-i686
    - mingw-w64-x86-64-dev
    - wine
    - wine-binfmt
    - nsis
  - Install OpenJDK into the Wine environment
  - Install the prerequisites for Win32 in ~/mingw32 and for Win64 in
    ~/mingw64.  See README.mingw in the toplevel.

### Mac

  - Install Apple Xcode
  - Install Macports
    - Install these packages
      - gnutar
      - gsed
      - gcc-10
      - cmake
  - Install Java JDK
  - Build the SWI-Prolog dependencies according to `scripts/macos-deps.sh`

## Accounts, etc

  - Setup PGP signatures for git, so you can sign the release
  - Setup upload channels for rsync to the website.
  - Setup authentication for uploading to the Ubuntu PPA

## Preparing a release (on Linux, but could be adopted for Mac)

  - Edit `VERSION` to specify the new release

  - Run this command and tag+sign the release

	./scripts/newversion

## Build the releases

  - Create the releases.  Below is if all is setup correctly.  The
    script `make-distribution` defines a lot of useful functions.
    First time it may be wise to run the steps one-by-one.

	. scripts/make-distribution
        build

  - For the Mac
    - Pull the repo from the Linux machine, including tags and verify
      the directory is clean and in sync:

	git pull linux master
	git pull --tags linux master
	git describe

    - Build as for Linux:

	. scripts/make-distribution
        build

  - If there were no issues with any of the builds we can go to the
    next step.  If there are issues, fix them and repeat all steps,
    starting with `./scripts/newversion`

## Uploading the releases

  - Run the `upload` function as defined by `make-distribution`.
    This assumes the upload rsync channels are setup properly.
  - Upload the git repos:

	git push
	git push --tags
	git submodule foreach git push --tags

## Updating the website

  - Login to the webserver backends.  The basic sequence is below.
    Typically this also installs OS patches and reboot the server
    after the software upgrade instead of just the web server.

	cd src/swipl-devel
	git pull
	git describe		# Verify tag
	../scripts/pgo-compile.sh
	ctest
	ninja install
	sudo systemctl restart plweb

## Updating the Linux snaps

  - See snap/local/publish.md

## Updating Macports

  - Assumes a clone of https://github.com/macports/macports-ports under ~/src
    with a remote `fork` pointing at your fork of this repo at github.  I have
    this both on the Linux machine and Mac.

### Testing the Macports version

There is typically not that much  need   to  verify the whole thing also
builds under Macports if  you  already   built  and  tested  the current
version on the Mac. Still, if there are problems proceed as follows:

  - First time preparation
    - Setup a local repository according to
      https://guide.macports.org/chunked/development.local-repositories.html
    - Create directories `lang/swi-prolog` and `lang/swi-prolog-devel` under
      ~/ports and from these dirs create a symlink to the `Portfile` files
      in the `~/src/macports-ports` repo.
    - Run `portindex` in ~/ports

Now, to test your version run these commands. The first creates a source
tar archive and  the  second  publishes   this  in  the  local  Macports
distfiles store and updates the version and hashes of the `Portfile`.

    ./scripts/make-src-tape --tag=HEAD
    ./scripts/update-macports

Now you can run

    port lint swi-prolog-devel
    sudo port test swi-prolog-devel

### Creating a PR

Go back to the Linux machine and run

    ./scripts/update-macports

This   creates   a   branch   with   the    version   (say   8.3.1)   in
~/src/macports-ports with the correct version and   hashes.  This is why
you need to run this on the Linux   machine as you have uploaded the tar
archive from this machine. The one on  the   Mac  isn't the same as time
stamps vary.  Now

  - Go to ~/src/macports-ports
  - Verify that lang/swi-prolog/devel/Portfile looks good
  - Push the branch (assuming 8.3.1 is the version):

	git push fork 8.3.1:8.3.1

  - Open the link of the PR and complete it.

## Creating the Announce post

Create the basic ChangeLog, replacing  the   two  releases (previous and
new) and edit the result, highlighting the important aspects:

    mkdir -p ReleaseNotes
    ./scripts/mkchangelog --nodate 8.3.0 > ReleaseNotes/RELNOTES-8.3.1
    edit ReleaseNotes/RELNOTES-8.3.1

Finally, copy/paste into a new release topic in Discourse.
