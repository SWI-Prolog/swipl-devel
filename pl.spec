Name:		pl
Version:	5.4.3
Release:	200
Epoch:		0
Summary:	A Free Software Prolog compiler.
Group:		Development/Languages
License:	GPL/LGPL
URL:		http://www.swi-prolog.org/
Source0:	http://www.swi.psy.uva.nl/cgi-bin/nph-download/SWI-Prolog/pl-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

%description
ISO/Edinburgh-style Prolog compiler including modules, autoload,
libraries, Garbage-collector, stack-expandor, C/C++-interface,
Multiple threads, GNU-readline interface, very fast
compiler. Including packages clib (Unix process control, sockets,
MIME), cpp (C++ interface), sgml (reading XML/SGML), sgml/RDF (reading
RDF into triples), ODBC interface and XPCE (Graphics UI toolkit,
integrated editor (Emacs-clone) and graphical debugger).

If you only want the plain compiler, there is also SWI-Prolog/lite.

%prep
%setup -q

%build
%configure
make

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall
# We need to have the pl executable in the path to be able to
# build the add-on packackges. Hence we build them here and not
# in the build phase.
PATH=$RPM_BUILD_ROOT%{_bindir}:$PATH
pushd packages
%configure
make
%makeinstall PLBASE=$RPM_BUILD_ROOT%{_libdir}/%{name}-%{version}
popd

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc ChangeLog COPYING README VERSION README.GUI README.RT
%doc dotfiles/dot* dotfiles/README
%{_bindir}/*
%{_libdir}/pl-%{version}
%{_mandir}/man1/*
 
%changelog
* Thu Dec 5 2003 Carwyn Edwards <cedward1@inf.ed.ac.uk> - 0:5.2.11-1.inf.1
- Inspired by the specfile that comes with SWI-Prolog.
- Bascially re-wrote it.
