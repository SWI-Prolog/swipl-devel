/*  $Id$

    Part of SWI-Prolog

    Author:        Jan van der Steen and Jan Wielemaker
    E-mail:        wielemake@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, SWI-Prolog Foundation

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
#include <assert.h>
#include <string.h>
#include "ssllib.h"

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
static atom_t ATOM_close_parent;

static functor_t FUNCTOR_ssl1;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_resource_error1;
static functor_t FUNCTOR_ssl_error1;
static functor_t FUNCTOR_existence_error1;
static functor_t FUNCTOR_permission_error3;
static functor_t FUNCTOR_ip4;
static functor_t FUNCTOR_version1;
static functor_t FUNCTOR_notbefore1;
static functor_t FUNCTOR_notafter1;
static functor_t FUNCTOR_subject1;
static functor_t FUNCTOR_issuername1;
static functor_t FUNCTOR_serial1;
static functor_t FUNCTOR_public_key5;
static functor_t FUNCTOR_private_key8;
static functor_t FUNCTOR_key1;
static functor_t FUNCTOR_hash1;
static functor_t FUNCTOR_signature1;
static functor_t FUNCTOR_equals2;
static functor_t FUNCTOR_crl1;
static functor_t FUNCTOR_revocations1;
static functor_t FUNCTOR_revoked2;



static int
type_error(term_t val, const char *type)
{ term_t ex;

  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_type_error2,
		         PL_CHARS, type,
		         PL_TERM, val,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
domain_error(term_t val, const char *type)
{ term_t ex;

  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_domain_error2,
		         PL_TERM, val,
		         PL_CHARS, type,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
resource_error(const char *resource)
{ term_t ex;

  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_resource_error1,
		         PL_CHARS, resource,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}

static int
ssl_error(const char *id)
{ term_t ex;

  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_ssl_error1,
		         PL_CHARS, id,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}

static int
permission_error(const char *action, const char *type, term_t obj)
{ term_t ex;

  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_permission_error3,
		         PL_CHARS, action,
		         PL_CHARS, type,
		         PL_TERM, obj,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}

/* not used now
static int
existence_error(term_t resource)
{ term_t ex;

  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_existence_error1,
		         PL_TERM, resource,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}
*/

static int i2d_X509_CRL_INFO_wrapper(void* i, unsigned char** d)
{
   return i2d_X509_CRL_INFO(i, d);
}

static int i2d_X509_CINF_wrapper(void* i, unsigned char** d)
{
   return i2d_X509_CINF(i, d);
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

  _PL_get_arg(a, t, t2);
  if ( !PL_get_atom_chars(t2, s) )
    return type_error(t2, "atom");

  return TRUE;
}


static int
get_int_arg(int a, term_t t, int *i)
{ term_t t2 = PL_new_term_ref();

  _PL_get_arg(a, t, t2);
  if ( !PL_get_integer(t2, i) )
    return type_error(t2, "integer");

  return TRUE;
}


static int
get_bool_arg(int a, term_t t, int *i)
{ term_t t2 = PL_new_term_ref();

  _PL_get_arg(a, t, t2);
  if ( !PL_get_bool(t2, i) )
    return type_error(t2, "boolean");

  return TRUE;
}


static int
get_file_arg(int a, term_t t, char **f)
{ term_t t2 = PL_new_term_ref();

  _PL_get_arg(a, t, t2);
  if ( !PL_get_file_name(t2, f, PL_FILE_EXIST) )
    return type_error(t2, "file");	/* TBD: check errors */

  return TRUE;
}


static int
get_predicate_arg(int a, module_t m, term_t t, int arity, predicate_t *pred)
{ term_t t2 = PL_new_term_ref();
  atom_t name;

  _PL_get_arg(a, t, t2);
  PL_strip_module(t2, &m, t2);
  if ( !get_atom_ex(t2, &name) )
    return FALSE;

  *pred = PL_pred(PL_new_functor(name, arity), m);

  return TRUE;
}

static int
recover_public_key(term_t public_t, RSA** rsa)
{
   char *n, *e;
   term_t n_t, e_t;

   n_t = PL_new_term_ref();
   e_t = PL_new_term_ref();

   if(!(PL_get_arg(1, public_t, n_t) &&
        PL_get_arg(2, public_t, e_t)))
      return type_error(public_t, "public_key");

   if (!(PL_get_atom_chars(n_t, &n) &&
         PL_get_atom_chars(e_t, &e)))
      return type_error(public_t, "public_key");
   *rsa = RSA_new();
   BN_hex2bn(&((*rsa)->n), n);
   BN_hex2bn(&((*rsa)->e), e);
   return TRUE;
}

static int
recover_private_key(term_t private_t, RSA** rsa)
{
   char *n, *d, *e, *p, *q, *dmp1, *dmq1, *iqmp;
   term_t n_t, d_t, e_t, p_t, q_t, dmp1_t, dmq1_t, iqmp_t;

   n_t = PL_new_term_ref();
   e_t = PL_new_term_ref();
   d_t = PL_new_term_ref();
   p_t = PL_new_term_ref();
   q_t = PL_new_term_ref();
   dmp1_t = PL_new_term_ref();
   dmq1_t = PL_new_term_ref();
   iqmp_t = PL_new_term_ref();
   if(!(PL_get_arg(1, private_t, n_t) &&
        PL_get_arg(2, private_t, e_t) &&
        PL_get_arg(3, private_t, d_t) &&
        PL_get_arg(4, private_t, p_t) &&
        PL_get_arg(5, private_t, q_t) &&
        PL_get_arg(6, private_t, dmp1_t) &&
        PL_get_arg(7, private_t, dmq1_t) &&
        PL_get_arg(8, private_t, iqmp_t)))
      return type_error(private_t, "private_key");
   ssl_deb(1, "Dismantling key");
   if (!(PL_get_atom_chars(n_t, &n) &&
         PL_get_atom_chars(e_t, &e) &&
         PL_get_atom_chars(d_t, &d) &&
         PL_get_atom_chars(p_t, &p) &&
         PL_get_atom_chars(q_t, &q) &&
         PL_get_atom_chars(dmp1_t, &dmp1) &&
         PL_get_atom_chars(dmq1_t, &dmq1) &&
         PL_get_atom_chars(iqmp_t, &iqmp)))
      return type_error(private_t, "private_key");
   ssl_deb(1, "Assembling RSA");
   *rsa = RSA_new();

   BN_hex2bn(&((*rsa)->n), n);
   BN_hex2bn(&((*rsa)->d), d);
   BN_hex2bn(&((*rsa)->e), e);
   BN_hex2bn(&((*rsa)->p), p);
   BN_hex2bn(&((*rsa)->q), q);
   BN_hex2bn(&((*rsa)->dmp1), dmp1);
   BN_hex2bn(&((*rsa)->dmq1), dmq1);
   BN_hex2bn(&((*rsa)->iqmp), iqmp);
   return TRUE;
}



static int
unify_private_key(term_t item, RSA* rsa)
{ term_t n_t, d_t, e_t, p_t, q_t, dmp1_t, dmq1_t, iqmp_t;
  char* hex;
  int retval = 1;

  n_t = PL_new_term_ref();
  e_t = PL_new_term_ref();
  d_t = PL_new_term_ref();
  p_t = PL_new_term_ref();
  q_t = PL_new_term_ref();
  dmp1_t = PL_new_term_ref();
  dmq1_t = PL_new_term_ref();
  iqmp_t = PL_new_term_ref();

  hex = BN_bn2hex(rsa->n);
  retval = retval && (PL_unify_atom_nchars(n_t, strlen(hex), hex));
  OPENSSL_free(hex);

  hex = BN_bn2hex(rsa->e);
  retval = retval && (PL_unify_atom_nchars(e_t, strlen(hex), hex));
  OPENSSL_free(hex);

  if (rsa->d != NULL)
  { hex = BN_bn2hex(rsa->d);
    retval = retval && (PL_unify_atom_nchars(d_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(d_t, "-"));

  if (rsa->p != NULL)
  { hex = BN_bn2hex(rsa->p);
    retval = retval && (PL_unify_atom_nchars(p_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(p_t, "-"));

  if (rsa->q != NULL)
  { hex = BN_bn2hex(rsa->q);
    retval = retval && (PL_unify_atom_nchars(q_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(q_t, "-"));

  if (rsa->dmp1 != NULL)
  { hex = BN_bn2hex(rsa->dmp1);
    retval = retval && (PL_unify_atom_nchars(dmp1_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
      retval = retval && (PL_unify_atom_chars(dmp1_t, "-"));

  if (rsa->dmq1 != NULL)
  { hex = BN_bn2hex(rsa->dmq1);
    retval = retval && (PL_unify_atom_nchars(dmq1_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(dmq1_t, "-"));

  if (rsa->iqmp != NULL)
  { hex = BN_bn2hex(rsa->iqmp);
    retval = retval && (PL_unify_atom_nchars(iqmp_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(iqmp_t, "-"));

  return retval && PL_unify_term(item,
                                 PL_FUNCTOR, FUNCTOR_key1,
                                 PL_FUNCTOR, FUNCTOR_private_key8,
                                 PL_TERM, n_t,
                                 PL_TERM, e_t,
                                 PL_TERM, d_t,
                                 PL_TERM, p_t,
                                 PL_TERM, q_t,
                                 PL_TERM, dmp1_t,
                                 PL_TERM, dmq1_t,
                                 PL_TERM, iqmp_t);
}

static int
unify_public_key(term_t item, RSA* rsa)
{ term_t n_t, e_t, dmp1_t, dmq1_t, iqmp_t, key_t;
  char* hex;
  int retval = 1;

  n_t = PL_new_term_ref();
  e_t = PL_new_term_ref();
  dmp1_t = PL_new_term_ref();
  dmq1_t = PL_new_term_ref();
  iqmp_t = PL_new_term_ref();

  hex = BN_bn2hex(rsa->n);
  retval = retval && (PL_unify_atom_nchars(n_t, strlen(hex), hex));
  OPENSSL_free(hex);

  hex = BN_bn2hex(rsa->e);
  retval = retval && (PL_unify_atom_nchars(e_t, strlen(hex), hex));
  OPENSSL_free(hex);

  if (rsa->dmp1 != NULL)
  { hex = BN_bn2hex(rsa->dmp1);
    retval = retval && (PL_unify_atom_nchars(dmp1_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
      retval = retval && (PL_unify_atom_chars(dmp1_t, "-"));

  if (rsa->dmq1 != NULL)
  { hex = BN_bn2hex(rsa->dmq1);
    retval = retval && (PL_unify_atom_nchars(dmq1_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(dmq1_t, "-"));

  if (rsa->iqmp != NULL)
  { hex = BN_bn2hex(rsa->iqmp);
    retval = retval && (PL_unify_atom_nchars(iqmp_t, strlen(hex), hex));
    OPENSSL_free(hex);
  } else
     retval = retval && (PL_unify_atom_chars(iqmp_t, "-"));

  key_t = PL_new_term_ref();
  retval = retval && PL_unify_term(key_t,
                                   PL_FUNCTOR, FUNCTOR_public_key5,
                                   PL_TERM, n_t,
                                   PL_TERM, e_t,
                                   PL_TERM, dmp1_t,
                                   PL_TERM, dmq1_t,
                                   PL_TERM, iqmp_t);
  return retval && PL_unify_term(item,
                                 PL_FUNCTOR, FUNCTOR_key1,
                                 PL_TERM, key_t);
}


/* Note that while this might seem incredibly hacky, it is
   essentially the same algorithm used by X509_cmp_time to
   parse the date. Some
   Fractional seconds are ignored. This is also largely untested - there
   may be a lot of edge cases that dont work!
*/
static int
unify_asn1_time(term_t term, ASN1_TIME *time)
{ time_t result = 0;
  char buffer[24];
  char* pbuffer = buffer;
  size_t length = time->length;
  char * source = (char *)time->data;
  struct tm time_tm;
  time_t lSecondsFromUTC;

  if (time->type == V_ASN1_UTCTIME)
  {  if ((length < 11) || (length > 17))
     {  ssl_deb(2, "Unable to parse time - expected either 11 or 17 chars, not %d", length);
        return FALSE;
     }
     /* Otherwise just get the first 10 chars - ignore seconds */
     memcpy(pbuffer, source, 10);
     pbuffer += 10;
     source += 10;
  } else
  { if (length < 13)
     {  ssl_deb(2, "Unable to parse time - expected at least 13 chars, not %d", length);
        return FALSE;
     }
     /* Otherwise just get the first 12 chars - ignore seconds */
     memcpy(pbuffer, source, 10);
     pbuffer += 10;
     source += 10;
  }
  /* Next find end of string */
  if ((*source == 'Z') || (*source == '-') || (*source == '+'))
  { *(pbuffer++) = '0';
    *(pbuffer++) = '0';
  } else
  { *(pbuffer++) = *(source++);
    *(pbuffer++) = *(source++);
    if (*source == '.')
    { source++;
      while ((*source >= '0') && (*source <= '9'))
         source++;
    }
  }
  *(pbuffer++) = 'Z';
  *(pbuffer++) = '\0';

  /* If not UTC, calculate offset */
  if (*source == 'Z')
     lSecondsFromUTC = 0;
  else
  { if ((*source != '+') && (source[5] != '-'))
     {  ssl_deb(2, "Unable to parse time. Missing UTC offset");
        return FALSE;
     }
     lSecondsFromUTC = ((source[1]-'0') * 10 + (source[2]-'0')) * 60;
     lSecondsFromUTC += (source[3]-'0') * 10 + (source[4]-'0');
     if (*source == '-')
        lSecondsFromUTC = -lSecondsFromUTC;
  }
  /* Parse date */
  time_tm.tm_sec  = ((buffer[10] - '0') * 10) + (buffer[11] - '0');
  time_tm.tm_min  = ((buffer[8] - '0') * 10) + (buffer[9] - '0');
  time_tm.tm_hour = ((buffer[6] - '0') * 10) + (buffer[7] - '0');
  time_tm.tm_mday = ((buffer[4] - '0') * 10) + (buffer[5] - '0');
  time_tm.tm_mon  = (((buffer[2] - '0') * 10) + (buffer[3] - '0')) - 1;
  time_tm.tm_year = ((buffer[0] - '0') * 10) + (buffer[1] - '0');
  if (time_tm.tm_year < 50)
     time_tm.tm_year += 100; /* according to RFC 2459 */
  time_tm.tm_wday = 0;
  time_tm.tm_yday = 0;
  time_tm.tm_isdst = 0;  /* No DST adjustment requested, though mktime might do it anyway */
  result = mktime(&time_tm);
  if ((time_t)-1 != result)
  { if (time_tm.tm_isdst != 0)
        result -= 3600; /* if mktime has adjusted the time for DST, adjust it back 1 hour*/
     result += lSecondsFromUTC; /* Add in the UTC offset, or should we return UTC time? */
  } else
  { ssl_deb(2, "mktime() failed");
    return FALSE;
  }
  return PL_unify_integer(term, result);
}

static int
unify_hash(term_t hash, ASN1_OBJECT* algorithm, int (*i2d)(void*, unsigned char**), void * data)
{ const EVP_MD *type;
  EVP_MD_CTX ctx;
  int digestible_length;
  unsigned char* digest_buffer;
  unsigned char digest[EVP_MAX_MD_SIZE];
  unsigned int digest_length;
  unsigned char* p;

  /* Generate hash */
  type=EVP_get_digestbyname(OBJ_nid2sn(OBJ_obj2nid(algorithm)));
  if (type == NULL)
  { return ssl_error("digest_lookup");
  }
  EVP_MD_CTX_init(&ctx);
  digestible_length=i2d(data,NULL);
  digest_buffer = PL_malloc(digestible_length);
  if (digest_buffer == NULL)
     return resource_error("memory");

  /* i2d_X509_CINF will change the value of p. We need to pass in a copy */
  p = digest_buffer;
  i2d(data,&p);
  if (!EVP_DigestInit(&ctx, type))
  { EVP_MD_CTX_destroy(&ctx);
    PL_free(digest_buffer);
    return ssl_error("digest_initialize");
  }
  if (!EVP_DigestUpdate(&ctx, digest_buffer, digestible_length))
  { EVP_MD_CTX_destroy(&ctx);
    PL_free(digest_buffer);
    return ssl_error("digest_update");
  }
  if (!EVP_DigestFinal(&ctx, digest, &digest_length))
  { EVP_MD_CTX_destroy(&ctx);
    PL_free(digest_buffer);
    return ssl_error("digest_finalize");
  }
  EVP_MD_CTX_destroy(&ctx);
  PL_free(digest_buffer);
  return PL_unify_term(hash,
                       PL_NCHARS, digest_length, digest);
}

static int
unify_name(term_t term, X509_NAME* name)
{ int ni;
  term_t list = PL_copy_term_ref(term);
  term_t item = PL_new_term_ref();

  if (name == NULL)
     return PL_unify_term(term,
                          PL_CHARS, "<null>");
  for (ni = 0; ni < X509_NAME_entry_count(name); ni++)
  { X509_NAME_ENTRY* e = X509_NAME_get_entry(name, ni);
    ASN1_STRING* entry_data = X509_NAME_ENTRY_get_data(e);
    if (!(PL_unify_list(list, item, list) &&
          PL_unify_term(item,
                        PL_FUNCTOR, FUNCTOR_equals2,
                        PL_CHARS, OBJ_nid2sn(OBJ_obj2nid(e->object)),
                        PL_NCHARS, entry_data->length, entry_data->data)
           ))
       return FALSE;
  }
  return PL_unify_nil(list);
}

static int
unify_crl(term_t term, X509_CRL* crl)
{ X509_CRL_INFO* info = crl->crl;
  int i;
  term_t item = PL_new_term_ref();
  term_t hash = PL_new_term_ref();
  term_t issuer = PL_new_term_ref();
  term_t revocations = PL_new_term_ref();
  term_t list = PL_copy_term_ref(revocations);
  int result = 1;
  long n;
  unsigned char* p;
  term_t revocation_date;
  BIO* mem;

  mem = BIO_new(BIO_s_mem());
  if (mem == NULL)
     return resource_error("memory");

  i2a_ASN1_INTEGER(mem, crl->signature);
  if (!(unify_name(issuer, X509_CRL_get_issuer(crl)) &&
        unify_hash(hash, crl->sig_alg->algorithm, i2d_X509_CRL_INFO_wrapper, crl->crl) &&
        PL_unify_term(term,
                      PL_LIST, 4,
                      PL_FUNCTOR, FUNCTOR_issuername1,
                      PL_TERM, issuer,
                      PL_FUNCTOR, FUNCTOR_signature1,
                      PL_NCHARS, crl->signature->length, crl->signature->data,
                      PL_FUNCTOR, FUNCTOR_hash1,
                      PL_TERM, hash,
                      PL_FUNCTOR, FUNCTOR_revocations1,
                      PL_TERM, revocations)))
  {
     return FALSE;
  }

  for (i = 0; i < sk_X509_REVOKED_num(info->revoked); i++)
  {
     X509_REVOKED* revoked = sk_X509_REVOKED_value(info->revoked, i);
     i2a_ASN1_INTEGER(mem, revoked->serialNumber);
     result &= (((n = BIO_get_mem_data(mem, &p)) > 0) &&
                PL_unify_list(list, item, list) &&
                (revocation_date = PL_new_term_ref()) &&
                unify_asn1_time(revocation_date, revoked->revocationDate) &&
                PL_unify_term(item,
                              PL_FUNCTOR, FUNCTOR_revoked2,
                              PL_NCHARS, n, p,
                              PL_TERM, revocation_date));
     if (BIO_reset(mem) != 1)
     {
        BIO_free(mem);
        // The only reason I can imagine this would fail is out of memory
        return resource_error("memory");
     }
  }
  BIO_free(mem);
  return result && PL_unify_nil(list);
}

static int
unify_certificate(term_t cert, X509* data)
{ term_t list = PL_copy_term_ref(cert);
  term_t item = PL_new_term_ref();
  BIO * mem = NULL;
  long n;
  EVP_PKEY *key;
  RSA* rsa;
  term_t issuername;
  term_t subject;
  term_t hash;
  term_t not_before;
  term_t not_after;
  unsigned int crl_ext_id;
  unsigned char *p;
  X509_EXTENSION * crl_ext = NULL;


  if (!(PL_unify_list(list, item, list) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_version1,
                      PL_LONG, X509_get_version(data))
         ))
     return FALSE;
  if (!(PL_unify_list(list, item, list) &&
        (not_before = PL_new_term_ref()) &&
        unify_asn1_time(not_before, X509_get_notBefore(data)) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_notbefore1,
                      PL_TERM, not_before)))
     return FALSE;

  if (!(PL_unify_list(list, item, list) &&
        (not_after = PL_new_term_ref()) &&
        unify_asn1_time(not_after, X509_get_notAfter(data)) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_notafter1,
                      PL_TERM, not_after)))
     return FALSE;


  if ((mem = BIO_new(BIO_s_mem())) != NULL)
  { i2a_ASN1_INTEGER(mem, X509_get_serialNumber(data));
    if ((n = BIO_get_mem_data(mem, &p)) > 0)
    {  if (!(PL_unify_list(list, item, list) &&
             PL_unify_term(item,
                           PL_FUNCTOR, FUNCTOR_serial1,
                           PL_NCHARS, n, p)
              ))
          return FALSE;
    } else
      Sdprintf("Failed to print serial - continuing without serial\n");
  } else
    Sdprintf("Failed to allocate BIO for printing - continuing without serial\n");

  if (!(PL_unify_list(list, item, list) &&
        (subject = PL_new_term_ref()) &&
        unify_name(subject, X509_get_subject_name(data)) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_subject1,
                      PL_TERM, subject))
     )
     return FALSE;
  if (!((hash = PL_new_term_ref()) &&
        unify_hash(hash, data->sig_alg->algorithm, i2d_X509_CINF_wrapper, data->cert_info) &&
        PL_unify_list(list, item, list) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_hash1,
                      PL_TERM, hash)))
     return FALSE;
  if (!(PL_unify_list(list, item, list) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_signature1,
                      PL_NCHARS, data->signature->length, data->signature->data)
         ))
     return FALSE;

  if (!(PL_unify_list(list, item, list) &&
        (issuername = PL_new_term_ref()) &&
        unify_name(issuername, X509_get_issuer_name(data)) &&
        PL_unify_term(item,
                      PL_FUNCTOR, FUNCTOR_issuername1,
                      PL_TERM, issuername))
     )
     return FALSE;

  /* X509_extract_key returns a reference to the existing key, not a copy */
  key = X509_extract_key(data);

  /* EVP_PKEY_get1_RSA returns a reference to the existing key, not a copy */
  rsa = EVP_PKEY_get1_RSA(key);
  if (!(PL_unify_list(list, item, list) &&
        unify_public_key(item, rsa)))
     return FALSE;

  /* If the cert has a CRL distribution point, return that. If it does not,
     it is not an error
  */
  crl_ext_id = X509_get_ext_by_NID(data, NID_crl_distribution_points, -1);
  crl_ext = X509_get_ext(data, crl_ext_id);
  if (crl_ext != NULL)
  { STACK_OF(DIST_POINT) * distpoints;
    int i, j;
    term_t crl;
    term_t crl_list;
    term_t crl_item;

    distpoints = X509_get_ext_d2i(data, NID_crl_distribution_points, NULL, NULL);
    /* Loop through the CRL points, putting them into a list */
    if (!PL_unify_list(list, item, list))
       return FALSE;
    crl = PL_new_term_ref();
    crl_list = PL_copy_term_ref(crl);
    crl_item = PL_new_term_ref();

    for (i = 0; i < sk_DIST_POINT_num(distpoints); i++)
    { DIST_POINT *point;
      GENERAL_NAME *name;
      point = sk_DIST_POINT_value(distpoints, i);
      if (point->distpoint != NULL)
      { /* Each point may have several names? May as well put them all in */
        for (j = 0; j < sk_GENERAL_NAME_num(point->distpoint->name.fullname); j++)
        { name = sk_GENERAL_NAME_value(point->distpoint->name.fullname, j);
          if (name != NULL && name->type == GEN_URI)
          { if (!(PL_unify_list(crl_list, crl_item, crl_list) &&
                  PL_unify_atom_chars(crl_item, (const char *)name->d.ia5->data)))
                return FALSE;
          }
        }
      }
    }
    if (!PL_unify_nil(crl_list))
       return FALSE;
    if (!PL_unify_term(item,
                       PL_FUNCTOR, FUNCTOR_crl1,
                       PL_TERM, crl))
       return FALSE;
  }
  return PL_unify_nil(list);
}

static int
unify_certificates(term_t certs, term_t tail, STACK_OF(X509)* stack)
{ term_t item = PL_new_term_ref();
  term_t list = PL_copy_term_ref(certs);
  X509* cert = sk_X509_pop(stack);
  int retval = 1;

  while (cert != NULL && retval == 1)
  { retval &= PL_unify_list(list, item, list);
    retval &= unify_certificate(item, cert);
    X509_free(cert);
    cert = sk_X509_pop(stack);
    if (cert == NULL)
      return PL_unify(tail, item) && PL_unify_nil(list);
  }
  return retval && PL_unify_nil(list);
}

foreign_t
pl_load_public_key(term_t source, term_t key_t)
{ EVP_PKEY* key;
  RSA* rsa;
  BIO* bio;
  IOSTREAM* stream;
  int c;

  if ( !PL_get_stream_handle(source, &stream) )
     return type_error(source, "stream");
  bio = BIO_new(&bio_read_functions);
  BIO_set_ex_data(bio, 0, stream);

  /* Determine format */
  c = Sgetc(stream);
  if (c != EOF)
     Sungetc(c, stream);
  if (c == 0x30)  /* ASN.1 sequence, so assume DER */
     key = d2i_PUBKEY_bio(bio, NULL);
  else
     key = PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL);
  BIO_free(bio);
  PL_release_stream(stream);
  if (key == NULL)
     return permission_error("read", "key", source);
  rsa = EVP_PKEY_get1_RSA(key);
  EVP_PKEY_free(key);
  if (unify_public_key(key_t, rsa))
  { RSA_free(rsa);
    PL_succeed;
  } else
  { RSA_free(rsa);
    PL_fail;
  }
}

int private_password_callback(char *buf, int bufsiz, int verify, void* pw)
{ int res;
  char* password = (char*)pw;
  res = (int)strlen(password);
  if (res > bufsiz)
     res = bufsiz;
  memcpy(buf, password, res);
  return res;
}

foreign_t
pl_load_private_key(term_t source, term_t password, term_t key_t)
{ EVP_PKEY* key;
  RSA* rsa;
  BIO* bio;
  IOSTREAM* stream;
  char* password_chars;
  int c;

  if (!PL_get_atom_chars(password, &password_chars))
    return type_error(password, "atom");
  if ( !PL_get_stream_handle(source, &stream) )
     return type_error(source, "stream");
  bio = BIO_new(&bio_read_functions);
  BIO_set_ex_data(bio, 0, stream);

  /* Determine format */
  c = Sgetc(stream);
  if (c != EOF)
     Sungetc(c, stream);
  if (c == 0x30)  /* ASN.1 sequence, so assume DER */
     key = d2i_PrivateKey_bio(bio, NULL); /* TBD: Password! */
  else
     key = PEM_read_bio_PrivateKey(bio, NULL, &private_password_callback, (void*)password_chars);
  BIO_free(bio);
  PL_release_stream(stream);
  if (key == NULL)
     return permission_error("read", "key", source);
  rsa = EVP_PKEY_get1_RSA(key);
  EVP_PKEY_free(key);
  if (unify_private_key(key_t, rsa))
  { RSA_free(rsa);
    PL_succeed;
  } else
  { RSA_free(rsa);
    PL_fail;
  }
}

foreign_t
pl_load_crl(term_t source, term_t list)
{ X509_CRL* crl;
  BIO* bio;
  IOSTREAM* stream;
  int result;
  int c;

  if ( !PL_get_stream_handle(source, &stream) )
     return type_error(source, "stream");

  bio = BIO_new(&bio_read_functions);
  BIO_set_ex_data(bio, 0, stream);
  /* Determine the format of the CRL */
  c = Sgetc(stream);
  if (c != EOF)
     Sungetc(c, stream);
  if (c == 0x30)  /* ASN.1 sequence, so assume DER */
     crl = d2i_X509_CRL_bio(bio, NULL);
  else
     crl = PEM_read_bio_X509_CRL(bio, NULL, NULL, NULL);
  BIO_free(bio);
  PL_release_stream(stream);
  if (crl == NULL)
  { ssl_deb(2, "Failed to load CRL");
    PL_fail;
  }
  result = unify_crl(list, crl);
  X509_CRL_free(crl);
  return result;
}

foreign_t
pl_load_certificate(term_t source, term_t cert)
{ X509* x509;
  BIO* bio;
  IOSTREAM* stream;
  int c = 0;

  if ( !PL_get_stream_handle(source, &stream) )
     return type_error(source, "stream");
  bio = BIO_new(&bio_read_functions);
  BIO_set_ex_data(bio, 0, stream);
  /* Determine format */
  c = Sgetc(stream);
  if (c != EOF)
     Sungetc(c, stream);
  if (c == 0x30)  /* ASN.1 sequence, so assume DER */
     x509 = d2i_X509_bio(bio, NULL);
  else
     x509 = PEM_read_bio_X509(bio, NULL, 0, NULL);
  BIO_free(bio);
  PL_release_stream(stream);
  if (x509 == NULL)
     return ssl_error("read_x509");
  if (unify_certificate(cert, x509))
  { X509_free(x509);
    PL_succeed;
  } else
  { X509_free(x509);
    PL_fail;
  }
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
  _PL_get_arg(1, config, a);
  if ( !PL_get_pointer(a, &ptr) )
    return type_error(config, "ssl_config");
  ssl = ptr;
  if ( ssl->magic != SSL_CONFIG_MAGIC )
    return type_error(config, "ssl_config");

  *conf = ssl;

  return TRUE;
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
  size_t len;

  /*
   * hook(+SSL, -Passwd)
   */

  unify_conf(av+0, config);
  if ( PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) )
  { if ( PL_get_nchars(av+1, &len, &passwd, CVT_ALL) )
    { if ( len >= (unsigned int)size )
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
                    X509 * cert,
		    X509_STORE_CTX * ctx,
		    const char *error)
{ fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(5);
  predicate_t pred = (predicate_t) config->pl_ssl_cb_cert_verify_data;
  int val;
  STACK_OF(X509)* stack;

  assert(pred);

  stack = X509_STORE_CTX_get1_chain(ctx);


  /*
   * hook(+SSL, +Certificate, +Error)
   */

  unify_conf(av+0, config);
  /*Sdprintf("\n---Certificate:'%s'---\n", certificate);*/
  val = ( unify_certificate(av+1, cert) &&
          unify_certificates(av+2, av+3, stack) &&
	  PL_unify_atom_chars(av+4, error) &&
	  PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) );

  /* free any items still on stack, since X509_STORE_CTX_get1_chain returns a copy */
  sk_X509_pop_free(stack, X509_free);
  PL_close_foreign_frame(fid);

  return val;
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

static BOOL initialised  = FALSE;

static int
threads_init()
{ LOCK();
  if ( initialised )
  { UNLOCK();
    return TRUE;
  }
  initialised = TRUE;

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
pl_ssl_context(term_t role, term_t config, term_t options)
{ atom_t a;
  PL_SSL *conf;
  int r;
  term_t tail;
  term_t head = PL_new_term_ref();
  module_t module = NULL;

  PL_strip_module(options, &module, options);
  tail = PL_copy_term_ref(options);

  if ( !get_atom_ex(role, &a) )
    return FALSE;
  if ( a == ATOM_server )
    r = PL_SSL_SERVER;
  else if ( a == ATOM_client )
    r = PL_SSL_CLIENT;
  else
    return domain_error(a, "ssl_role");

 if ( !threads_init() )
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

      if ( !get_predicate_arg(1, module, head, 2, &hook) )
	return FALSE;

      ssl_set_cb_pem_passwd(conf, pl_pem_passwd_hook, (void *)hook);
    } else if ( name == ATOM_cert_verify_hook && arity == 1 )
    { predicate_t hook;

      if ( !get_predicate_arg(1, module, head, 5, &hook) )
	return FALSE;

      ssl_set_cb_cert_verify(conf, pl_cert_verify_hook, (void *)hook);
    } else if ( name == ATOM_close_parent && arity == 1 )
    { char* s;

      if ( !get_char_arg(1, head, &s) )
	return FALSE;

      ssl_set_close_parent(conf, strcmp(s, "true") == 0);
    } else
      continue;
  }

  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return unify_conf(config, conf);
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
    { if (instance->sread != NULL)
    {  SOCKET fd = Sfileno(instance->sread);
       SOCKET *fdp = data;
       *fdp = fd;
       return 0;
    } else if (instance->swrite != NULL)
    {  SOCKET fd = Sfileno(instance->swrite);
       SOCKET *fdp = data;
       *fdp = fd;
       return 0;
    }
    }
    return -1;
    case SIO_SETENCODING:
    case SIO_FLUSHOUTPUT:
      return 0;
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
pl_ssl_put_socket(term_t config, term_t data)
{ PL_SSL *conf;
  if ( !get_conf(config, &conf) )
    return FALSE;
  return PL_get_integer(data, &conf->sock);
}

static foreign_t
pl_ssl_get_socket(term_t config, term_t data)
{ PL_SSL *conf;
  if ( !get_conf(config, &conf) )
    return FALSE;
  return PL_unify_integer(data, conf->sock);
}

static foreign_t
pl_ssl_negotiate(term_t config, term_t org_in, term_t org_out, term_t in, term_t out)
{ PL_SSL *conf;
  IOSTREAM *sorg_in, *sorg_out;
  IOSTREAM *i, *o;
  PL_SSL_INSTANCE * instance = NULL;
  if ( !get_conf(config, &conf) )
    return FALSE;
  if ( !PL_get_stream_handle(org_in, &sorg_in) )
     return FALSE;
  if ( !PL_get_stream_handle(org_out, &sorg_out) )
     return FALSE;
  if ( !(instance = ssl_ssl_bio(conf, sorg_in, sorg_out)) )
  {  PL_release_stream(sorg_in);
     PL_release_stream(sorg_out);
     return FALSE;			/* TBD: error */
  }

  if ( !(i=Snew(instance, SIO_INPUT|SIO_RECORDPOS|SIO_FBUF, &ssl_funcs)) )
  {  PL_release_stream(sorg_in);
     PL_release_stream(sorg_out);
    return FALSE;
  }
  instance->close_needed++;
  if ( !PL_unify_stream(in, i) )
  { Sclose(i);
    PL_release_stream(sorg_in);
    PL_release_stream(sorg_out);
    return FALSE;
  }
  Sset_filter(sorg_in, i);
  PL_release_stream(sorg_in);
  if ( !(o=Snew(instance, SIO_OUTPUT|SIO_RECORDPOS|SIO_FBUF, &ssl_funcs)) )
  {  PL_release_stream(sorg_out);
    return FALSE;
  }
  instance->close_needed++;
  if ( !PL_unify_stream(out, o) )
  { Sclose(i);
    Sset_filter(sorg_in, NULL);
    PL_release_stream(sorg_out);
    Sclose(o);
    return FALSE;
  }
  Sset_filter(sorg_out, o);
  PL_release_stream(sorg_out);
  return TRUE;
}

foreign_t pl_rsa_private_decrypt(term_t private_t, term_t cipher_t, term_t plain_t)
{ size_t cipher_length;
  unsigned char* cipher;
  unsigned char* plain;
  int outsize;
  RSA* key;
  int retval;

  if(!PL_get_atom_nchars(cipher_t, &cipher_length, (char**)&cipher))
     return type_error(cipher_t, "atom");
  if (!recover_private_key(private_t, &key))
     return FALSE;

  outsize = RSA_size(key);
  ssl_deb(1, "Output size is going to be %d", outsize);
  plain = PL_malloc(outsize);
  ssl_deb(1, "Allocated %d bytes for plaintext", outsize);
  if ((outsize = RSA_private_decrypt((int)cipher_length, cipher, plain, key, RSA_PKCS1_PADDING)) <= 0)
  { ssl_deb(1, "Failure to decrypt!");
    RSA_free(key);
    PL_free(plain);
    PL_fail;
  }
  ssl_deb(1, "decrypted bytes: %d", outsize);
  ssl_deb(1, "Freeing RSA");
  RSA_free(key);
  ssl_deb(1, "Assembling plaintext");
  retval = PL_unify_atom_nchars(plain_t, outsize, (char*)plain);
  ssl_deb(1, "Freeing plaintext");
  PL_free(plain);
  ssl_deb(1, "Done");
  return retval;
}

foreign_t pl_rsa_public_decrypt(term_t public_t, term_t cipher_t, term_t plain_t)
{ size_t cipher_length;
  unsigned char* cipher;
  unsigned char* plain;
  int outsize;
  RSA* key;
  int retval;

  if(!PL_get_atom_nchars(cipher_t, &cipher_length, (char**)&cipher))
     return type_error(cipher_t, "atom");
  if (!recover_public_key(public_t, &key))
     return FALSE;

  outsize = RSA_size(key);
  ssl_deb(1, "Output size is going to be %d", outsize);
  plain = PL_malloc(outsize);
  ssl_deb(1, "Allocated %d bytes for plaintext", outsize);
  if ((outsize = RSA_public_decrypt((int)cipher_length, cipher, plain, key, RSA_PKCS1_PADDING)) <= 0)
  { ssl_deb(1, "Failure to decrypt!");
    RSA_free(key);
    PL_free(plain);
    return FALSE;
  }
  ssl_deb(1, "decrypted bytes: %d", outsize);
  ssl_deb(1, "Freeing RSA");
  RSA_free(key);
  ssl_deb(1, "Assembling plaintext");
  retval = PL_unify_atom_nchars(plain_t, outsize, (char*)plain);
  ssl_deb(1, "Freeing plaintext");
  PL_free(plain);
  ssl_deb(1, "Done");
  return retval;
}

foreign_t pl_rsa_public_encrypt(term_t public_t, term_t plain_t, term_t cipher_t)
{ size_t plain_length;
  unsigned char* cipher;
  unsigned char* plain;
  int outsize;
  RSA* key;
  int retval;

  ssl_deb(1, "Generating terms");
  ssl_deb(1, "Collecting plaintext");
  if(!PL_get_atom_nchars(plain_t, &plain_length, (char**)&plain))
     return type_error(plain_t, "atom");
  if (!recover_public_key(public_t, &key))
     return FALSE;

  outsize = RSA_size(key);
  ssl_deb(1, "Output size is going to be %d\n", outsize);
  cipher = PL_malloc(outsize);
  ssl_deb(1, "Allocated %d bytes for ciphertext\n", outsize);
  if ((outsize = RSA_public_encrypt((int)plain_length, plain, cipher, key, RSA_PKCS1_PADDING)) <= 0)
  { ssl_deb(1, "Failure to encrypt!");
    PL_free(plain);
    RSA_free(key);
    return FALSE;
  }
  ssl_deb(1, "encrypted bytes: %d\n", outsize);
  ssl_deb(1, "Freeing RSA");
  RSA_free(key);
  ssl_deb(1, "Assembling plaintext");
  retval = PL_unify_atom_nchars(cipher_t, outsize, (char*)cipher);
  ssl_deb(1, "Freeing plaintext");
  PL_free(cipher);
  ssl_deb(1, "Done");
  return retval;
}


foreign_t pl_rsa_private_encrypt(term_t private_t, term_t plain_t, term_t cipher_t)
{ size_t plain_length;
  unsigned char* cipher;
  unsigned char* plain;
  int outsize;
  RSA* key;
  int retval;
  if(!PL_get_atom_nchars(plain_t, &plain_length, (char**)&plain))
     return type_error(plain_t, "atom");
  if (!recover_private_key(private_t, &key))
     return FALSE;

  outsize = RSA_size(key);
  ssl_deb(1, "Output size is going to be %d", outsize);
  cipher = PL_malloc(outsize);
  ssl_deb(1, "Allocated %d bytes for ciphertext", outsize);
  if ((outsize = RSA_public_encrypt((int)plain_length, plain, cipher, key, RSA_PKCS1_PADDING)) <= 0)
  { ssl_deb(1, "Failure to encrypt!");
    PL_free(plain);
    RSA_free(key);
    return FALSE;
  }
  ssl_deb(1, "encrypted bytes: %d", outsize);
  ssl_deb(1, "Freeing RSA");
  RSA_free(key);
  ssl_deb(1, "Assembling plaintext");
  retval = PL_unify_atom_nchars(cipher_t, outsize, (char*)cipher);
  ssl_deb(1, "Freeing plaintext");
  PL_free(cipher);
  ssl_deb(1, "Done");
  return retval;
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
  ATOM_close_parent       = PL_new_atom("close_parent");

  FUNCTOR_ssl1            = PL_new_functor(PL_new_atom("$ssl"), 1);
  FUNCTOR_error2          = PL_new_functor(PL_new_atom("error"), 2);
  FUNCTOR_domain_error2   = PL_new_functor(PL_new_atom("domain_error"), 2);
  FUNCTOR_type_error2     = PL_new_functor(PL_new_atom("type_error"), 2);
  FUNCTOR_resource_error1 = PL_new_functor(PL_new_atom("resource_error"), 1);
  FUNCTOR_ssl_error1      = PL_new_functor(PL_new_atom("ssl_error"), 1);
  FUNCTOR_existence_error1 =PL_new_functor(PL_new_atom("existence_error"), 1);
  FUNCTOR_permission_error3=PL_new_functor(PL_new_atom("permission_error"), 3);
  FUNCTOR_ip4		  = PL_new_functor(PL_new_atom("ip"), 4);
  FUNCTOR_version1	  = PL_new_functor(PL_new_atom("version"), 1);
  FUNCTOR_notbefore1	  = PL_new_functor(PL_new_atom("notbefore"), 1);
  FUNCTOR_notafter1	  = PL_new_functor(PL_new_atom("notafter"), 1);
  FUNCTOR_subject1	  = PL_new_functor(PL_new_atom("subject"), 1);
  FUNCTOR_issuername1	  = PL_new_functor(PL_new_atom("issuer_name"), 1);
  FUNCTOR_serial1	  = PL_new_functor(PL_new_atom("serial"), 1);
  FUNCTOR_key1	          = PL_new_functor(PL_new_atom("key"), 1);
  FUNCTOR_public_key5     = PL_new_functor(PL_new_atom("public_key"), 5);
  FUNCTOR_private_key8    = PL_new_functor(PL_new_atom("private_key"), 8);
  FUNCTOR_hash1	          = PL_new_functor(PL_new_atom("hash"), 1);
  FUNCTOR_signature1      = PL_new_functor(PL_new_atom("signature"), 1);
  FUNCTOR_equals2         = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_crl1            = PL_new_functor(PL_new_atom("crl"), 1);
  FUNCTOR_revoked2        = PL_new_functor(PL_new_atom("revoked"), 2);
  FUNCTOR_revocations1    = PL_new_functor(PL_new_atom("revocations"), 1);

  PL_register_foreign("_ssl_context",	3, pl_ssl_context,    0);
  PL_register_foreign("ssl_exit",	1, pl_ssl_exit,	      0);
  PL_register_foreign("ssl_put_socket",	2, pl_ssl_put_socket, 0);
  PL_register_foreign("ssl_get_socket",	2, pl_ssl_get_socket, 0);
  PL_register_foreign("ssl_negotiate",	5, pl_ssl_negotiate,  0);
  PL_register_foreign("ssl_debug",	1, pl_ssl_debug,      0);
  PL_register_foreign("load_crl",       2, pl_load_crl,      0);
  PL_register_foreign("load_certificate",2,pl_load_certificate,      0);
  PL_register_foreign("load_private_key",3,pl_load_private_key,      0);
  PL_register_foreign("load_public_key", 2,pl_load_public_key,      0);
  PL_register_foreign("rsa_private_decrypt", 3, pl_rsa_private_decrypt, 0);
  PL_register_foreign("rsa_private_encrypt", 3, pl_rsa_private_encrypt, 0);
  PL_register_foreign("rsa_public_decrypt", 3, pl_rsa_public_decrypt, 0);
  PL_register_foreign("rsa_public_encrypt", 3, pl_rsa_public_encrypt, 0);

  /*
   * Initialize ssllib
   */
  (void) ssl_lib_init();
}

install_t
uninstall_ssl4pl()
{ ssl_lib_exit();
}
