/*  $Id$

    Part of SWI-Prolog

    Author:        Jan van der Steen and Jan Wielemaker
    E-mail:        J.van.der.Steen@diff.nl and jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, SWI-Prolog Foundation

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef UTILH__
#define UTILH__

#include <openssl/ssl.h>
#include <openssl/x509.h>

#if 0
#define TEST_HOST "10.0.2.100"  /* james */
#define TEST_HOST "10.0.2.18"   /* shuwa */
#endif
#define TEST_HOST "127.0.0.1"   /* localhost */
#define TEST_PORT 1111

/*
 * Location of server and client certificates, key's and authority
 */
#define HOME            "./"
#define CACERT          HOME "etc/demoCA/cacert.pem"
#define SERVER_CERTF    HOME "etc/server/server-cert.pem"
#define SERVER_KEYF     HOME "etc/server/server-key.pem"
#define SERVER_PASSWD   "apenoot1"
#define CLIENT_CERTF    HOME "etc/client/client-sign.pem"
#define CLIENT_KEYF     HOME "etc/client/client-sign-key.pem"
#define CLIENT_PASSWD   "apenoot2"

char *          util_cb_pem_passwd   ( PL_SSL *config
                                     , char *buf
                                     , int size
                                     ) ;
BOOL            util_cb_cert_verify  ( PL_SSL *config
                                     , const char *certificate
                                     , long nbytes
                                     , const char *error
                                     ) ;

void            util_run_test        (PL_SSL_INSTANCE *instance);
int             util_run_server      (PL_SSL_INSTANCE *instance);
int             util_run_client      (PL_SSL_INSTANCE *instance);

#endif
