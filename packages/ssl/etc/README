
1. /usr/local/ssl/misc/CA.pl -newca

    apenoot
    NL
    Noord-Holland
    Amsterdam
    Diff Automatisering
    Support group
    J. van der Steen
    info@diff.nl

    creates: ./demoCA/

2. /usr/local/ssl/misc/CA.pl -newreq

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

    creates: newreq.pem

3. /usr/local/ssl/misc/CA.pl -signreq

    apenoot

    creates: newcert.pem

4. /usr/local/ssl/misc/CA.pl -pkcs12 "Jan's Client Certificate"

    apenootX
    ----- extra -----
    aapjesX

    creates: newcert.p12
