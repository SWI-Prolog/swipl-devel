/*  $Id$

    Part of SWI-Prolog

    Author:        Jan van der Steen and Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
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


#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "ssllib.h"
#include <assert.h>
#include <string.h>
#ifdef WIN32
#  include <io.h>
#  include <winsock2.h>
#else
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <unistd.h>
#endif

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&mutex)
#define UNLOCK() pthread_mutex_unlock(&mutex)
#else
#define LOCK()
#define UNLOCK()
#endif

static atom_t ATOM_server;
static atom_t ATOM_client;
static atom_t ATOM_password;
static atom_t ATOM_host;
static atom_t ATOM_port;
static atom_t ATOM_cert;
static atom_t ATOM_peer_cert;
static atom_t ATOM_cacert_file;
static atom_t ATOM_certificate_file;
static atom_t ATOM_key_file;
static atom_t ATOM_pem_password_hook;
static atom_t ATOM_cert_verify_hook;

static functor_t FUNCTOR_ssl1;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_resource_error1;
static functor_t FUNCTOR_permission_error3;
static functor_t FUNCTOR_ip4;


static int
type_error(term_t val, const char *type)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex,
		PL_FUNCTOR, FUNCTOR_error2,
		  PL_FUNCTOR, FUNCTOR_type_error2,
		    PL_TERM, val,
		    PL_CHARS, type,
		  PL_VARIABLE); 

  return PL_raise_exception(ex);
}


static int
domain_error(term_t val, const char *type)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex,
		PL_FUNCTOR, FUNCTOR_error2,
		  PL_FUNCTOR, FUNCTOR_domain_error2,
		    PL_TERM, val,
		    PL_CHARS, type,
		  PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
resource_error(const char *resource)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex,
		PL_FUNCTOR, FUNCTOR_error2,
		  PL_FUNCTOR, FUNCTOR_resource_error1,
		    PL_CHARS, resource,
		  PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
permission_error(const char *action, const char *type, term_t obj)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex,
		PL_FUNCTOR, FUNCTOR_error2,
		  PL_FUNCTOR, FUNCTOR_permission_error3,
		    PL_CHARS, action,
		    PL_CHARS, type,
		    PL_TERM, obj,
		  PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
get_atom_ex(term_t t, atom_t *a)
{ if ( !PL_get_atom(t, a) )
    return type_error(t, "atom");

  return TRUE;
}


static int
get_char_arg(int a, term_t t, char **s)
{ term_t t2 = PL_new_term_ref();

  PL_get_arg(a, t, t2);
  if ( !PL_get_atom_chars(t2, s) )
    return type_error(t2, "atom");

  return TRUE;
}


#if 0					/* not used anymore */
static int
get_atom_arg(int a, term_t t, atom_t *n)
{ term_t t2 = PL_new_term_ref();

  PL_get_arg(a, t, t2);

  return get_atom_ex(t2, n);
}
#endif


static int
get_int_arg(int a, term_t t, int *i)
{ term_t t2 = PL_new_term_ref();

  PL_get_arg(a, t, t2);
  if ( !PL_get_integer(t2, i) )
    return type_error(t2, "integer");

  return TRUE;
}


static int
get_bool_arg(int a, term_t t, int *i)
{ term_t t2 = PL_new_term_ref();

  PL_get_arg(a, t, t2);
  if ( !PL_get_bool(t2, i) )
    return type_error(t2, "boolean");

  return TRUE;
}


static int
get_file_arg(int a, term_t t, char **f)
{ term_t t2 = PL_new_term_ref();

  PL_get_arg(a, t, t2);
  if ( !PL_get_file_name(t2, f, PL_FILE_EXIST) )
    return type_error(t2, "file");	/* TBD: check errors */

  return TRUE;
}


static int
get_predicate_arg(int a, term_t t, int arity, predicate_t *pred)
{ term_t t2 = PL_new_term_ref();
  module_t m = NULL;
  atom_t name;

  PL_get_arg(a, t, t2);
  PL_strip_module(t2, &m, t2);
  if ( !get_atom_ex(t2, &name) )
    return FALSE;

  *pred = PL_pred(PL_new_functor(name, arity), m);

  return TRUE;
}





static int
unify_conf(term_t config, PL_SSL *conf)
{ return PL_unify_term(config,
		       PL_FUNCTOR, FUNCTOR_ssl1,
		         PL_POINTER, conf);
}


static int
get_conf(term_t config, PL_SSL **conf)
{ term_t a = PL_new_term_ref();
  void *ptr;
  PL_SSL *ssl;

  if ( !PL_is_functor(config, FUNCTOR_ssl1) )
    return type_error(config, "ssl_config");
  PL_get_arg(1, config, a);
  if ( !PL_get_pointer(a, &ptr) )
    return type_error(config, "ssl_config");
  ssl = ptr;
  if ( ssl->magic != SSL_CONFIG_MAGIC )
    return type_error(config, "ssl_config");

  *conf = ssl;

  return TRUE;
}


static int
get_sock(term_t t, int *s)
{ if ( !PL_get_integer(t, s) )
    return type_error(t, "socket");

  return TRUE;
}


static int
tcp_unify_ip(term_t t, struct in_addr *ip)
{ unsigned long hip;

  hip = ntohl(ip->s_addr);

  return PL_unify_term(t,
		       PL_FUNCTOR, FUNCTOR_ip4,
		         PL_INTEGER, (hip >> 24) & 0xff,
		         PL_INTEGER, (hip >> 16) & 0xff,
		         PL_INTEGER, (hip >>  8) & 0xff,
		         PL_INTEGER, (hip >>  0) & 0xff);
}



		 /*******************************
		 *	      CALLBACK		*
		 *******************************/


static char *
pl_pem_passwd_hook(PL_SSL *config, char *buf, int size)
{ fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(2);
  predicate_t pred = (predicate_t) config->pl_ssl_cb_pem_passwd_data;
  char *passwd = NULL;
  unsigned int len;
    
  /*
   * hook(+SSL, -Passwd)
   */

  unify_conf(av+0, config);
  if ( PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) )
  { if ( PL_get_nchars(av+1, &len, &passwd, CVT_ALL) )
    { if ( len >= size )
	PL_warning("pem_passwd too long");
      else
	memcpy(buf, passwd, len);
    } else
      PL_warning("pem_passwd_hook returned wrong type");
  }

  PL_close_foreign_frame(fid);

  return passwd;
}


static BOOL
pl_cert_verify_hook(PL_SSL *config,
		    const char *certificate, long len,
		    const char *error)
{ fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(3);
  predicate_t pred = (predicate_t) config->pl_ssl_cb_cert_verify_data;
  int val;
    
  assert(pred);

  /*
   * hook(+SSL, +Certificate, +Error)
   */

  unify_conf(av+0, config);
  /*Sdprintf("\n---Certificate:'%s'---\n", certificate);*/
  PL_unify_atom_nchars(av+1, len, certificate);
  PL_unify_atom_chars(av+2, error);

  if ( PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) )
  { val = TRUE;
  } else
  { val = FALSE;			/* TBD: Handle exception? */
  }

  PL_close_foreign_frame(fid);

  return val;
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

static BOOL initialised  = FALSE;

static int
tcp_init()
{ LOCK();
  if ( initialised )
  { UNLOCK();
    return TRUE;
  }
  initialised = TRUE;

#ifdef WIN32
{ WSADATA WSAData;

  if ( WSAStartup(MAKEWORD(2,0), &WSAData) )
  { UNLOCK();
    return PL_warning("tcp_init() - WSAStartup failed.");
  }
  /*
   * startSocketThread();
   */
}
#endif /*WIN32*/

#ifdef _REENTRANT
  if ( !ssl_thread_setup() )
  { term_t o = PL_new_term_ref();

    PL_put_atom_chars(o, "ssl");
    return permission_error("setup_threads", "library", o);
  }
#endif

  UNLOCK();
  return TRUE;
}

static foreign_t
pl_ssl_init(term_t config, term_t role, term_t options)
{ atom_t a;
  PL_SSL *conf;
  int r;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  
  if ( !get_atom_ex(role, &a) )
    return FALSE;
  if ( a == ATOM_server )
    r = TRUE;
  else if ( a == ATOM_client )
    r = FALSE;
  else
    return domain_error(a, "ssl_role");

  /*
   * Initialize the (Windows) socket stream library
   */
  if ( !tcp_init() )
    return FALSE;

  if ( !(conf = ssl_init(r)) )
    return resource_error("memory");

  while( PL_get_list(tail, head, tail) )
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) )
      return type_error(head, "ssl_option");

    if ( name == ATOM_password && arity == 1 )
    { char *s;

      if ( !get_char_arg(1, head, &s) )
	return FALSE;

      ssl_set_password(conf, s);
    } else if ( name == ATOM_host && arity == 1 )
    { char *s;

      if ( !get_char_arg(1, head, &s) )
	return FALSE;

      ssl_set_host(conf, s);
    } else if ( name == ATOM_port && arity == 1 )
    { int p;

      if ( !get_int_arg(1, head, &p) )
	return FALSE;

      ssl_set_port(conf, p);
    } else if ( name == ATOM_cert && arity == 1 )
    { int val;

      if ( !get_bool_arg(1, head, &val) )
	return FALSE;
      
      ssl_set_cert(conf, val);
    } else if ( name == ATOM_peer_cert && arity == 1 )
    { int val;

      if ( !get_bool_arg(1, head, &val) )
	return FALSE;
      
      ssl_set_peer_cert(conf, val);
    } else if ( name == ATOM_cacert_file && arity == 1 )
    { char *file;

      if ( !get_file_arg(1, head, &file) )
	return FALSE;
      
      ssl_set_cacert(conf, file);
    } else if ( name == ATOM_certificate_file && arity == 1 )
    { char *file;

      if ( !get_file_arg(1, head, &file) )
	return FALSE;
      
      ssl_set_certf(conf, file);
    } else if ( name == ATOM_key_file && arity == 1 )
    { char *file;

      if ( !get_file_arg(1, head, &file) )
	return FALSE;
      
      ssl_set_keyf(conf, file);
    } else if ( name == ATOM_pem_password_hook && arity == 1 )
    { predicate_t hook;

      if ( !get_predicate_arg(1, head, 2, &hook) )
	return FALSE;

      ssl_set_cb_pem_passwd(conf, pl_pem_passwd_hook, (void *)hook);
    } else if ( name == ATOM_cert_verify_hook && arity == 1 )
    { predicate_t hook;

      if ( !get_predicate_arg(1, head, 3, &hook) )
	return FALSE;

      ssl_set_cb_cert_verify(conf, pl_cert_verify_hook, (void *)hook);
    } else
      return domain_error(head, "ssl_option");
  }

  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  if ( ssl_socket(conf) < 0 )
    return FALSE;			/* TBD: error */

  return unify_conf(config, conf);
}


static foreign_t
pl_ssl_accept(term_t config, term_t sock_inst, term_t peer)
{ PL_SSL *conf;
  int si;
  struct sockaddr_in sa_client;
  size_t             client_len = sizeof(sa_client);

  if ( !get_conf(config, &conf) )
    return FALSE;

  if ( (si = ssl_accept(conf, &sa_client, &client_len)) < 0 )
    return FALSE;			/* TBD: error */
  
  if ( PL_unify_integer(sock_inst, si) &&
       tcp_unify_ip(peer, &sa_client.sin_addr) )
    return TRUE;

  close(si);
  return FALSE;				/* exception? */
}


static int
pl_ssl_close(PL_SSL_INSTANCE *instance)
{ assert(instance->close_needed > 0);
  
  if ( --instance->close_needed == 0 )
    return ssl_close(instance);

  return 0;
}


static int
pl_ssl_control(PL_SSL_INSTANCE *instance, int action, void *data)
{ switch(action)
  { case SIO_GETFILENO:
    { int *p = data;

      *p = instance->sock;
      return 0;
    }
    default:
      return -1;
  }
}


static foreign_t
pl_ssl_exit(term_t config)
{ PL_SSL *conf;

  if ( !get_conf(config, &conf) )
    return FALSE;

  ssl_exit(conf);

  return TRUE;
}


static IOFUNCTIONS ssl_funcs =
{ (Sread_function) ssl_read,		/* read */
  (Swrite_function) ssl_write,		/* write */
  NULL,					/* seek */
  (Sclose_function) pl_ssl_close,	/* close */
  (Scontrol_function) pl_ssl_control	/* control */
};


static foreign_t
pl_ssl_open(term_t config, term_t socket, term_t in, term_t out)
{ PL_SSL *conf;
  PL_SSL_INSTANCE *instance;
  int si;
  IOSTREAM *i, *o;

  if ( !get_conf(config, &conf) )
    return FALSE;

  if ( conf->pl_ssl_role == PL_SSL_SERVER )
  { if ( !get_sock(socket, &si) )
      return FALSE;
  } else
  { if ( (si=ssl_connect(conf)) < 0 )
      return FALSE;			/* TBD: error */
  }
  if ( !(instance = ssl_ssl(conf, si)) )
    return FALSE;			/* TBD: error */

  if ( !(i=Snew(instance, SIO_INPUT, &ssl_funcs)) )
    return FALSE;
  instance->close_needed++;

  if ( !PL_unify_stream(in, i) )
  { Sclose(i);
    return FALSE;
  }
  
  if ( !(o=Snew(instance, SIO_OUTPUT, &ssl_funcs)) )
    return FALSE;
  instance->close_needed++;

  if ( !PL_unify_stream(out, o) )
  { Sclose(i);
    Sclose(o);
    return FALSE;
  }

  return TRUE;
}


static foreign_t
pl_ssl_debug(term_t level)
{ int l;

  if ( !PL_get_integer(level, &l) )
    return type_error(level, "integer");

  ssl_set_debug(l);

  return TRUE;
}



		 /*******************************
		 *	     INSTALL		*
		 *******************************/


install_t
install_ssl4pl()
{ ATOM_server             = PL_new_atom("server");
  ATOM_client             = PL_new_atom("client");
  ATOM_password           = PL_new_atom("password");
  ATOM_host               = PL_new_atom("host");
  ATOM_port               = PL_new_atom("port");
  ATOM_cert               = PL_new_atom("cert");
  ATOM_peer_cert          = PL_new_atom("peer_cert");
  ATOM_cacert_file        = PL_new_atom("cacert_file");
  ATOM_certificate_file   = PL_new_atom("certificate_file");
  ATOM_key_file           = PL_new_atom("key_file");
  ATOM_pem_password_hook  = PL_new_atom("pem_password_hook");
  ATOM_cert_verify_hook   = PL_new_atom("cert_verify_hook");

  FUNCTOR_ssl1            = PL_new_functor(PL_new_atom("$ssl"), 1);
  FUNCTOR_error2          = PL_new_functor(PL_new_atom("error"), 2);
  FUNCTOR_domain_error2   = PL_new_functor(PL_new_atom("domain_error"), 2);
  FUNCTOR_type_error2     = PL_new_functor(PL_new_atom("type_error"), 2);
  FUNCTOR_resource_error1 = PL_new_functor(PL_new_atom("resource_error"), 1);
  FUNCTOR_permission_error3=PL_new_functor(PL_new_atom("permission_error"), 3);
  FUNCTOR_ip4		  = PL_new_functor(PL_new_atom("ip"), 4);

  PL_register_foreign("ssl_init",       3, pl_ssl_init,    PL_FA_TRANSPARENT);
  PL_register_foreign("ssl_accept",     3, pl_ssl_accept,  0);
  PL_register_foreign("ssl_open",       4, pl_ssl_open,    0);
  PL_register_foreign("ssl_exit",       1, pl_ssl_exit,    0);
  PL_register_foreign_in_module("user", "ssl_debug", 1, pl_ssl_debug,   0);

  /*
   * Initialize ssllib
   */
  (void) ssl_lib_init();
}

install_t
uninstall_ssl4pl()
{ ssl_lib_exit();
}
