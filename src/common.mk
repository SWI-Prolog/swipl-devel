# Declarations that can be shared between the Unix and
# Windows Makefiles

# C-Sources

OSOBJ=	os/pl-buffer.o os/pl-ctype.o os/pl-file.o os/pl-files.o \
	os/pl-glob.o os/pl-os.o os/pl-stream.o os/pl-string.o os/pl-table.o \
	os/pl-text.o os/pl-utf8.o os/pl-fmt.o os/pl-dtoa.o os/pl-option.o \
	os/pl-cstack.o os/pl-codelist.o os/pl-prologflag.o os/pl-rl.o \
	os/pl-tai.o os/pl-locale.o

OBJ=	pl-atom.o pl-wam.o pl-arith.o pl-bag.o pl-error.o \
	pl-comp.o pl-rc.o pl-dwim.o pl-ext.o pl-flag.o \
	pl-funct.o pl-gc.o pl-privitf.o pl-list.o pl-string.o \
	pl-load.o pl-modul.o pl-op.o pl-prims.o pl-pro.o \
	pl-proc.o pl-prof.o pl-read.o pl-rec.o pl-setup.o \
	pl-sys.o pl-trace.o pl-util.o pl-wic.o pl-write.o \
	pl-term.o pl-thread.o pl-xterm.o pl-srcfile.o \
	pl-beos.o pl-attvar.o pl-gvar.o pl-btree.o \
	pl-init.o pl-gmp.o pl-segstack.o pl-hash.o \
	pl-version.o pl-codetable.o pl-supervisor.o \
	pl-dbref.o pl-termhash.o pl-variant.o \
	pl-copyterm.o pl-debug.o pl-cont.o pl-ressymbol.o pl-dict.o

# Prolog library

PLSRC=	../boot/syspred.pl ../boot/toplevel.pl ../boot/license.pl \
	../boot/bags.pl ../boot/apply.pl ../boot/expand.pl ../boot/dcg.pl \
	../boot/history.pl ../boot/attvar.pl ../boot/packs.pl \
	../boot/dwim.pl ../boot/rc.pl ../boot/predopts.pl \
	../boot/parms.pl ../boot/autoload.pl ../boot/qlf.pl \
	../boot/topvars.pl ../boot/messages.pl ../boot/load.pl \
	../boot/dicts.pl

PLLIBS= MANUAL helpidx.pl help.pl explain.pl sort.pl \
	qsave.pl shlib.pl statistics.pl system.pl error.pl \
	backcomp.pl gensym.pl listing.pl debug.pl vm.pl \
	quintus.pl edinburgh.pl ctypes.pl files.pl modules.pl \
	edit.pl shell.pl check.pl heaps.pl win_menu.pl console_input.pl \
	tty.pl readln.pl readutil.pl make.pl hotfix.pl option.pl \
	oset.pl ordsets.pl occurs.pl lists.pl pprint.pl \
	www_browser.pl url.pl utf8.pl main.pl assoc.pl nb_set.pl \
	threadutil.pl qpforeign.pl dif.pl when.pl ugraphs.pl \
	checklast.pl checkselect.pl operators.pl date.pl \
	prolog_stack.pl prolog_clause.pl prolog_xref.pl prolog_source.pl \
	broadcast.pl pairs.pl base64.pl record.pl rbtrees.pl settings.pl \
	thread.pl dialect.pl apply_macros.pl apply.pl nb_rbtrees.pl \
	aggregate.pl pure_input.pl pio.pl thread_pool.pl terms.pl \
	charsio.pl portray_text.pl csv.pl persistency.pl fastrw.pl \
	coinduction.pl ansi_term.pl base32.pl prolog_history.pl \
	optparse.pl arithmetic.pl writef.pl predicate_options.pl \
	random.pl prolog_breakpoints.pl prolog_autoload.pl \
	prolog_colour.pl varnumbers.pl codesio.pl prolog_codewalk.pl \
	prolog_pack.pl git.pl prolog_metainference.pl quasi_quotations.pl \
	sandbox.pl prolog_format.pl prolog_install.pl check_installation.pl \
	solution_sequences.pl iostream.pl dicts.pl yall.pl

CLP=	bounds.pl clp_events.pl clp_distinct.pl simplex.pl clpfd.pl clpb.pl
DCG=	basics.pl

# Dialect library files

DIALECT=yap.pl hprolog.pl commons.pl ciao.pl sicstus.pl bim.pl ifprolog.pl
YAP=	README.TXT
SICSTUS=block.pl timeout.pl system.pl arrays.pl lists.pl terms.pl \
	sockets.pl swipl-lfr.pl
CIAO=	assertions.pl basicmodes.pl isomodes.pl regtypes.pl sockets.pl \
	read.pl write.pl strings.pl format.pl lists.pl dcg.pl freeze.pl \
	terms.pl system.pl iso_misc.pl aggregates.pl hiordlib.pl \
	hiord.pl classic.pl llists.pl nativeprops.pl nortchecks.pl \
	terms_check.pl terms_vars.pl unittestdecls.pl unittestprops.pl \
	sort.pl streams.pl file_utils.pl odd.pl prolog_flags.pl
CIAO_REGTYPES=regtypes_tr.pl
CIAO_ENGINE=attributes.pl basic_props.pl exceptions_db.pl exceptions.pl \
	internals.pl hiord_rt.pl basiccontrol.pl io_aux.pl
HPROLOG=format.pl
ECLIPSE=test_util_iso.pl
ISO=	iso_predicates.pl
SWI=	syspred_options.pl
IFPROLOG=
