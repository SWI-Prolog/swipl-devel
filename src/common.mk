# Declarations that can be shared between the Unix and
# Windows Makefiles

# C-Sources

OSOBJ=	os/pl-buffer.o os/pl-ctype.o os/pl-file.o os/pl-files.o \
	os/pl-glob.o os/pl-os.o os/pl-stream.o os/pl-string.o os/pl-table.o \
	os/pl-text.o os/pl-utf8.o os/pl-fmt.o os/pl-dtoa.o os/pl-option.o

OBJ=	pl-atom.o pl-wam.o pl-arith.o pl-bag.o pl-error.o \
	pl-comp.o pl-rc.o pl-dwim.o pl-ext.o pl-flag.o \
	pl-funct.o pl-gc.o pl-privitf.o pl-list.o \
	pl-load.o pl-modul.o pl-op.o pl-prims.o pl-pro.o \
	pl-proc.o pl-prof.o pl-read.o pl-rec.o pl-rl.o pl-setup.o \
	pl-sys.o pl-trace.o pl-util.o pl-wic.o pl-write.o \
	pl-term.o pl-thread.o pl-xterm.o pl-prologflag.o \
	pl-beos.o pl-attvar.o pl-gvar.o pl-btree.o \
	pl-main.o pl-gmp.o pl-tai.o pl-segstack.o pl-hash.o \
	pl-version.o pl-codetable.o pl-supervisor.o \
	pl-dbref.o pl-termhash.o pl-codelist.o

# Prolog library

PLSRC=	../boot/syspred.pl ../boot/toplevel.pl ../boot/license.pl \
	../boot/bags.pl ../boot/apply.pl ../boot/expand.pl ../boot/dcg.pl \
	../boot/writef.pl ../boot/history.pl ../boot/attvar.pl \
	../boot/dwim.pl ../boot/rc.pl \
	../boot/parms.pl ../boot/autoload.pl ../boot/qlf.pl \
	../boot/topvars.pl ../boot/messages.pl ../boot/load.pl

PLLIBS= MANUAL helpidx.pl help.pl explain.pl sort.pl \
	qsave.pl shlib.pl statistics.pl system.pl error.pl \
	backcomp.pl gensym.pl listing.pl debug.pl vm.pl \
	bim.pl quintus.pl edinburgh.pl ctypes.pl files.pl \
	edit.pl emacs_interface.pl shell.pl check.pl heaps.pl \
	tty.pl readln.pl readutil.pl make.pl hotfix.pl option.pl \
	am_match.pl oset.pl ordsets.pl occurs.pl lists.pl \
	www_browser.pl url.pl utf8.pl main.pl assoc.pl nb_set.pl \
	threadutil.pl qpforeign.pl dif.pl when.pl ugraphs.pl \
	checklast.pl checkselect.pl operators.pl date.pl \
	prolog_stack.pl prolog_clause.pl prolog_xref.pl prolog_source.pl \
	broadcast.pl pairs.pl base64.pl record.pl rbtrees.pl settings.pl \
	thread.pl dialect.pl apply_macros.pl apply.pl nb_rbtrees.pl \
	aggregate.pl pure_input.pl pio.pl thread_pool.pl terms.pl \
	charsio.pl portray_text.pl csv.pl persistency.pl fastrw.pl \
	coinduction.pl ansi_term.pl base32.pl prolog_history.pl

# Dialect library files

DIALECT=yap.pl hprolog.pl commons.pl ciao.pl sicstus.pl
YAP=	README.TXT
SICSTUS=block.pl timeout.pl system.pl arrays.pl lists.pl \
	sockets.pl swipl-lfr.pl
CIAO=	assertions.pl isomodes.pl regtypes.pl sockets.pl \
	read.pl write.pl strings.pl format.pl lists.pl \
	terms.pl system.pl iso_misc.pl aggregates.pl \
	classic.pl
CIAO_ENGINE=internals.pl hiord_rt.pl
ISO=	iso_predicates.pl
