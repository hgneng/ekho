Name:		ekho
Version:	4.8
Release:	SUSE12%{?dist}
Summary:	Chinese (Cantonese/Mandarin) text-to-speech software

Group:		Applications/Communications
License:	GPL
URL:		http://www.eguidedog.net/ekho.php
Source0:	ekho-4.8.tar.bz2
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:	libsndfile-devel
BuildRequires:	ncurses-devel
BuildRequires:	gtk2-devel

# for Fedora
#BuildRequires:	pulseaudio-libs-devel

# for Open SUSE
BuildRequires:  libpulse-devel

Requires:	libsndfile
Requires:	pulseaudio-libs
Requires:	ncurses
Requires:	gtk2

# for Fedora
# Requires:	libXcursor

%description
Ekho is a Chinese (Cantonese/Mandarin/Hakka) text-to-speech software.

%prep
%setup -q


%build
%configure
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc
%{_bindir}/*
%{_includedir}/*
%{_datadir}/ekho-data/

%changelog

