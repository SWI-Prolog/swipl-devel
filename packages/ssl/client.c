/**********************************************************************
 * Filename:    client.c
 * Purpose:     Implementation of SSL client
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
    if ((config = ssl_init(FALSE)) == NULL) {
        exit(EXIT_FAILURE);
    }

    /*
     * Set some more parameters
     */
    ssl_set_cacert     (config, CACERT);
    ssl_set_certf      (config, CLIENT_CERTF);
    ssl_set_keyf       (config, CLIENT_KEYF);
    ssl_set_password   (config, CLIENT_PASSWD);
    ssl_set_cert       (config, CLIENT_CERT_REQUIRED);
#if 0
    ssl_set_peer_cert  (config, SERVER_CERT_REQUIRED);
#endif

#if 1
    /*
     * Install some callback's
     */
    ssl_set_cb_cert_verify(config, util_cb_cert_verify, NULL);
#endif

    /*
     * Establish TCP layer with SSL layer on top of it
     */
    ssl_set_host       (config, TEST_HOST);
    ssl_set_port       (config, TEST_PORT);
    if ((config->sock = ssl_socket(config)) < 0) {
        exit(EXIT_FAILURE);
    }

    /*
     * Start up the client
     */
    if ((sock_inst = ssl_connect(config)) < 0) {
        exit(EXIT_FAILURE);
    }
    if ((instance = ssl_ssl(config, sock_inst)) == NULL) {
        exit(EXIT_FAILURE);
    }
    util_run_test(instance);

    /*
     * Close down SSL, TCP and free all resources
     */
    ssl_close(instance);

    /*
     * Close down SSL, TCP and free all resources
     */
    ssl_exit(config);

    exit(EXIT_SUCCESS);
}
