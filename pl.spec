Summary:	SWI-Prolog - Edinburgh compatible Prolog compiler
Name:		pl
Version:	3.3.0
Release:	1
Copyright:	GPL-2
Source:		ftp://swi.psy.uva.nl/pub/SWI-Prolog/pl-%{version}.tar.gz
Vendor:		Jan Wielemaker <jan@swi.psy.uva.nl>
Url:		http://www.swi.psy.uva.nl/projects/SWI-Prolog/
Packager:	Tony Nugent <Tony.Nugent@usq.edu.au>
Group:		Development/Languages
Prefix:		/usr
BuildRoot:	/var/tmp/pl

%description
ISO/Edinburgh-style Prolog compiler including modules, autoload, libraries,
Garbage-collector, stack-expandor, C/C++-interface, GNU-readline interface,
very fast compiler, X11 interface using XPCE
(http://swi.psy.uva.nl/projects/xpce/)

%prep
%setup

%build
env CFLAGS="$RPM_OPT_FLAGS" \
  ./configure --prefix=/usr
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr
make install prefix=$RPM_BUILD_ROOT/usr bindir=$RPM_BUILD_ROOT/usr/bin \
	man_prefix=$RPM_BUILD_ROOT/usr/man
cp src/README.bin ..

# why are manpages installed twice?
rm -rf /usr/lib/pl-%{version}/man

# --- obsolated by rel-ln :
# # Make the package relocatable by using local links
# ARCH=`$RPM_BUILD_ROOT/usr/bin/pl -arch`
# for f in pl plrc plld; do
#       ln -sf ../lib/pl-%{version}/bin/$ARCH/$f $RPM_BUILD_ROOT/usr/bin
# done
# # or even:
# # ln -sf ../lib/pl-%{version}/bin/`$RPM_BUILD_ROOT/usr/bin/pl -arch`/pl{,rc,ld} \
# #	$RPM_BUILD_ROOT/usr/bin

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc ChangeLog README README.bin README.GUI COPYING
# not necessary in binary rpm, I think
# %doc INSTALL INSTALL.notes LSM PORTING VERSION
/usr/lib/pl-%{version}
/usr/man/man1/*
/usr/bin/pl*
