/* Prolog -> C linkage table (generated) */

extern int pl_new();
extern int pl_send0();
extern int pl_send1();
extern int pl_send2();
extern int pl_send3();
extern int pl_sendn();
extern int pl_get0();
extern int pl_get1();
extern int pl_get2();
extern int pl_get3();
extern int pl_getn();
extern int pl_object1();
extern int pl_object2();
extern int pl_pce_init();
extern int pl_pce_predicate_reference();
extern int xt_create_app_context();
extern int pce_xt_appcontext();
extern int qp_pce_predicate_reference();
extern int setup_input();
extern int qp_pce_reset();
extern int qp_pce_exit();
extern int qp_pce_open();

static char *QP_table_s[] = {
        "pl_new",
        "pl_send0",
        "pl_send1",
        "pl_send2",
        "pl_send3",
        "pl_sendn",
        "pl_get0",
        "pl_get1",
        "pl_get2",
        "pl_get3",
        "pl_getn",
        "pl_object1",
        "pl_object2",
        "pl_pce_init",
        "pl_pce_predicate_reference",
        "xt_create_app_context",
        "pce_xt_appcontext",
        "qp_pce_predicate_reference",
        "setup_input",
        "qp_pce_reset",
        "qp_pce_exit",
        "qp_pce_open",
        0 };


static int (*(QP_table_f[]))() = {
        pl_new,
        pl_send0,
        pl_send1,
        pl_send2,
        pl_send3,
        pl_sendn,
        pl_get0,
        pl_get1,
        pl_get2,
        pl_get3,
        pl_getn,
        pl_object1,
        pl_object2,
        pl_pce_init,
        pl_pce_predicate_reference,
        xt_create_app_context,
        pce_xt_appcontext,
        qp_pce_predicate_reference,
        setup_input,
        qp_pce_reset,
        qp_pce_exit,
        qp_pce_open,
        0 };

/* entry function - returns addresses of functions */
QP_entry(sym)
char *sym;
{
  register int i;

  for (i=0; QP_table_s[i]; i++)
    if (strcmp(sym, QP_table_s[i]) == 0)
      return (int) QP_table_f[i];
  return 0;
}
