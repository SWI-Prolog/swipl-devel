Summary:	SWI-Prolog - Edinburgh compatible Prolog compiler
Name:		pl
Version: 	5.0.8
Release:	1
License:	LGPL
Source:		ftp://swi.psy.uva.nl/pub/SWI-Prolog/pl-%{version}.tar.gz
Vendor:		Jan Wielemaker <jan@swi.psy.uva.nl>
Url:		http://www.swi-prolog.org/
Packager:	Michel Alexandre Salim <salimma1@yahoo.co.uk>
Group:		Development/Languages
Prefix:		/usr
BuildRoot:	/var/tmp/pl
Provides:	pl-%{version}

%description
ISO/Edinburgh-style Prolog compiler including modules, autoload, libraries,
Garbage-collector, stack-expandor, C/C++-interface, GNU-readline interface,
very fast compiler.  Including packages clib (Unix process control and
sockets), cpp (C++ interface), sgml (reading XML/SGML), sgml/RDF (reading
RDF into triples) and XPCE (Graphics UI toolkit, integrated editor
(Emacs-clone) and source-level debugger).

If you only want the plain compiler, there is also SWI-Prolog/lite.
%prep
%setup

%build
%configure
(cd packages && %configure)
make
(cd packages && make)

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr
make install \
	prefix=$RPM_BUILD_ROOT/usr \
	bindir=$RPM_BUILD_ROOT/usr/bin \
	mandir=$RPM_BUILD_ROOT%{_mandir}
( cd packages && \
  PATH=$RPM_BUILD_ROOT/usr/bin:$PATH make rpm-install \
	PLBASE=$RPM_BUILD_ROOT/usr/lib/pl-%{version} \
	prefix=$RPM_BUILD_ROOT/usr \
        bindir=$RPM_BUILD_ROOT/usr/bin \
	mandir=$RPM_BUILD_ROOT%{_mandir}/man1
)

# why are manpages installed twice?
rm -rf $RPM_BUILD_ROOT/usr/lib/pl-%{version}/man

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc ChangeLog README COPYING
%doc dotfiles/dot*
/usr/lib/pl-%{version}
%{_mandir}/man1/*
/usr/bin/pl*
/usr/bin/xpce
