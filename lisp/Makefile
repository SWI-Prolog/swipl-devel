# SCCS: @(#)91/01/14 Makefile    1.3
# make gnu to create all .elc files. 
GNUEMACS = gnuemacs

STARTFILE = qprolog-mode.el

ELFILES =                       \
	qpaux.el                \
	qpcommands.el           \
	qpfile-compl.el 	\
	qpfindpred.el 		\
	qphelp-functions.el 	\
	qphelp.el 		\
	qprolog-indent.el 	\
	qprolog-mode.el 	\
	qpdelete.el 		\
	qprocess.el 		\
	qprolog.el 		\
	quintaux.el 		\
	qpshell-command-ring.el \
	qptokens.el 		\
	qui.el  		\
	qui_cmds.el		\
	qui_aux.el		\
	qui_filter.el		



gnu:
	$(GNUEMACS) -batch -q -l $(STARTFILE) -f batch-byte-compile $(ELFILES)
