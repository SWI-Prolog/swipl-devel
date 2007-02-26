Last updated:

	* Feb 26, 2007, Jan Wielemaker
		- Updated paths and copy commands for OpenSSL 0.9.8a


1. /usr/share/ssl/misc/CA.pl -newca

    apenoot
    NL
    Noord-Holland
    Amsterdam
    Diff Automatisering
    Support group
    J. van der Steen
    info@diff.nl

    creates: ./demoCA/

2. /usr/share/ssl/misc/CA.pl -newreq

    apenoot1                            apenoot2
    NL                                  NL
    Noord-Holland                       Noord-Holland
    Amsterdam                           Amsterdam
    Diff Automatisering                 Diff Automatisering
    Server                              Client
    J. van der Steen                    J. van der Steen
    J.van.der.Steen@diff.nl             J.van.der.Steen@diff.nl
    ----- extra -----                   ----- extra -----
    nootjes                             aapjes
    Diff Automatisering                 Diff Automatisering

    creates: newkey.pem and newreq.pem

3. /usr/share/ssl/misc/CA.pl -signreq

    apenoot

    creates: newcert.pem

4.  Move to server/client directory:

    mv newkey.pem server/server-key.pem
    mv newcert.pem server/server-cert.pem

    Repeat 2-4 for client site using these move commands:

    mv newkey.pem client/client-key.pem
    mv newcert.pem client/client-cert.pem
