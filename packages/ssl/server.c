/**********************************************************************
 * Filename:    server.c
 * Purpose:     Implementation of SSL server
 * Method:      OpenSSL
 * Author:      J.van.der.Steen@diff.nl
 * Date:        Thu May 27 11:11:06 CEST 2004
 **********************************************************************/

#include "ssllib.h"
#include "util.h"


int
main()
{
    PL_SSL          *config    = NULL;
    PL_SSL_INSTANCE *instance  = NULL;
    int              sock_inst = -1;

    /*
     * Initialize ssllib
     */
    (void) ssl_lib_init();

    /*
     * SSL preliminaries, creating context and handle for this session.
     */
    if ((config = ssl_init(TRUE)) == NULL) {
        exit(EXIT_FAILURE);
    }

    /*
     * Set some more parameters
     */
    ssl_set_cert       (config, SERVER_CERT_REQUIRED);
    ssl_set_certf      (config, SERVER_CERTF);
    ssl_set_keyf       (config, SERVER_KEYF);
    ssl_set_password   (config, SERVER_PASSWD);
#if 0
    ssl_set_cacert     (config, CACERT);
    ssl_set_cert       (config, SERVER_CERT_REQUIRED);
#endif
    ssl_set_peer_cert  (config, CLIENT_CERT_REQUIRED);

#if 1
    /*
     * Install some callback's
     */
    ssl_set_cb_cert_verify(config, util_cb_cert_verify, NULL);
    ssl_set_cb_pem_passwd (config, util_cb_pem_passwd, NULL);
#endif

    /*
     * Establish TCP layer with SSL layer on top of it
     */
    ssl_set_host       (config, NULL);
    ssl_set_port       (config, TEST_PORT);
    if ((config->sock = ssl_socket(config)) < 0) {
        exit(EXIT_FAILURE);
    }

    /*
     * Start up the server
     */
    while (1) {
        if ((sock_inst = ssl_accept(config, NULL, 0)) < 0) {
            exit(EXIT_FAILURE);
        }
        ssl_deb("ssl_accept() succeeded\n");
        if ((instance = ssl_ssl(config, sock_inst)) == NULL) {
            exit(EXIT_FAILURE);
        }
        ssl_deb("ssl_ssl() succeeded\n");

        util_run_test(instance);

        /*
         * Close down SSL, TCP and free all resources
         */
        if (ssl_close(instance) < 0) {
            ssl_err("ssl_close() failed\n");
        }
    }

    /*
     * Close down SSL, TCP and free all resources
     */
    ssl_exit(config);

    exit(EXIT_SUCCESS);
}
