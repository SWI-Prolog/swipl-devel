Summary:	SWI-Prolog - Edinburgh compatible Prolog compiler
Name:		pl
Version:	3.2.9
Release:	1
Copyright:	Distributable, free for non-commercial use
Source:		ftp://swi.psy.uva.nl/pub/SWI-Prolog/pl-3.2.9.tar.gz
Vendor:		Jan Wielemaker <jan@swi.psy.uva.nl>
Url:		http://www.swi.psy.uva.nl/projects/SWI-Prolog/
Packager:	Tony Nugent <Tony.Nugent@usq.edu.au>
Group:		Development/Languages
Prefix:		/usr
BuildRoot:	/var/tmp/pl

%description
Edinburgh-style Prolog compiler including modules, autoload,
libraries, Garbage-collector, stack-expandor, C-interface,
GNU-readline and GNU-Emacs interface, very fast compiler,
X11 interface using XPCE (http://swi.psy.uva.nl/projects/xpce/)

%prep
%setup

%build
cd src
env CFLAGS="$RPM_OPT_FLAGS" \
  ./configure --prefix=/usr
make

%install
mkdir -p $RPM_BUILD_ROOT/usr
cd src
make install prefix=$RPM_BUILD_ROOT/usr
cp README.bin ..


%files
%doc ChangeLog INSTALL INSTALL.notes LICENSE LSM PORTING
%doc README README.bin README.GUI
%doc VERSION
%attr(755,root,root)/usr/lib/pl-3.2.9/bin/*
%attr(644,root,root)/usr/lib/pl-3.2.9/boot/*
%attr(644,root,root)/usr/lib/pl-3.2.9/include/*
%attr(-,root,root)/usr/lib/pl-3.2.9/lib/*
%attr(644,root,root)/usr/lib/pl-3.2.9/library/*
%attr(644,root,root)/usr/lib/pl-3.2.9/man/*
%attr(-,root,root)/usr/lib/pl-3.2.9/runtime/*
%attr(644,root,root)/usr/lib/pl-3.2.9/swipl
%attr(644,root,root)/usr/lib/pl-3.2.9/boot.prc
%attr(644,root,root)/usr/man/man1/pl.1
%attr(644,root,root)/usr/man/man1/plrc.1
%attr(644,root,root)/usr/man/man1/plld.1
%attr(-,root,root)/usr/bin/pl
%attr(-,root,root)/usr/bin/plrc
%attr(-,root,root)/usr/bin/plld
%attr(-,root,root)/usr/bin/pl-bite

%post
cd $RPM_INSTALL_PREFIX/bin
for f in pl plrc plld pl-bite; do
    rm -f $f
    ln -s $RPM_INSTALL_PREFIX/lib/pl-3.2.9/bin/i?86-linux/$f .
done

%postun
cd $RPM_INSTALL_PREFIX/bin
for f in pl plrc plld pl-bite; do
    rm -f $f
done

%clean
rm -rf $RPM_BUILD_ROOT
