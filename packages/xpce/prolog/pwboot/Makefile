# Generated automatically from Makefile.in by configure.
#
#  Makefile to make a release. Requires GNU make to execute
#

# QUINTUS binary identifier: `machine`
# To change PWVERSION, also modify pwversion/1 in pce_qp.pl!

MAKE=make
SHELL=bash
BIN=sun4-5
SO=so
builddir=linux
PCEMAJOR=5
PCEMINOR=0
PCEPATCH=0

PWVERSION=3.2
QPVERSION=3.4
QPC=qpc
QLD=qld

CP=cp -p
LN=ln -s
SUM=sum -r

#Deliver Version
#DELFLAGS = -h
DELFLAGS =
CDEBUGFLAGS=
#Debug/Test version
#DELFLAGS = 
#CDEBUGFLAGS= -g

PCEHOME=../../prowindows$(PWVERSION)
LINSTALLD=installd$(QPVERSION)/pw3
INSTALLD=../../$(LINSTALLD)
PCEHLP=$(PCEHOME)/appl-help
PCELIB=$(PCEHOME)/library
PCEDEMO=$(PCEHOME)/demo
PCECONTRIB=$(PCEHOME)/contrib

.SUFFIXES: .qof .pl

.pl.qof:
	$(QPC) -L . -i pce_qpc.qof -N $(DELFLAGS) -cH $*.pl -o $*.qof

RELDIRS=	$(PCEHOME) \
		$(PCEHLP) \
		$(PCEHLP)/lib \
		$(PCELIB) \
		$(PCELIB)/$(BIN) \
		$(PCELIB)/dialog \
		$(PCELIB)/dialog/lib \
		$(PCELIB)/draw \
		$(PCELIB)/emacs \
		$(PCELIB)/english \
		$(PCELIB)/man \
		$(PCELIB)/xref \
		$(PCEDEMO) \
		$(PCECONTRIB) \
		$(PCECONTRIB)/rubik \
		$(PCEDEMO)/dialog \
		$(PCEDEMO)/pw2

PCESHARE=	pce_editor.pl \
		pce_realise.pl \
		pce_expansion.pl \
		pce_error.pl \
		pce_expand.pl \
		pce_global.pl \
		pce_operator.pl \
		pce_autoload.pl


PCEQOFS=	pce_boot.qof \
		pce_qp.qof \
		pce_principal.qof \
		pce_error.qof \
		pce_autoload.qof \
		pce_global.qof \
		pce_realise.qof \
		pce_expansion.qof \
		pce_operator.qof \
		pce_editor.qof \
		pce_utils.qof \
		require.qof

OBJS=		../../quintus/src/$(BIN)/libpce.$(SO)

BINFILES=	libpce.$(SO) \
		libpce.a \
		libjpeg.a \
		libXpm.a

#		libXPCEmain.a
#		libXPCE.$(SO) libXPCE.$(SO).$(PCEMAJOR) \
#		libXPCE.$(SO).$(PCEMAJOR).$(PCEMINOR).$(PCEPATCH)

BOOTQOFS=	pce_qpc.qof \
		qp_pce.qof \
		pce.qof
BOOTPLS=	pce_qux.pl \
		QPINDEX.pl \
		english/pce_messages.pl

LIBPLS=		area.pl \
		broadcast.pl \
		dragdict.pl \
		dragdrop.pl \
		edit_dialog.pl \
		emacs_extend.pl \
		emacs_tags.pl \
		file_item.pl \
		find_file.pl \
		help_message.pl \
		keybinding.pl \
		make_dialog.pl \
		pce.pl \
		pce_arrow_item.pl \
		pce_config.pl \
		pce_configeditor.pl \
		pce_colour_item.pl \
		pce_cxx_headers.pl \
		pce_debug.pl \
		pce_drag_and_drop.pl \
		pce_drag_and_drop_dict_item.pl \
		pce_edit.pl \
		pce_editable_text.pl \
		pce_emacs.pl \
		pce_font_item.pl \
		pce_graphical_browser.pl \
		pce_help_file.pl \
		pce_helper.pl \
		pce_image.pl \
		pce_image_browser.pl \
		pce_image_item.pl \
		pce_loadcxx.pl \
		pce_main.pl \
		pce_manual.pl \
		pce_objects.pl \
		pce_portray.pl \
		pce_postscript.pl \
		pce_prompter.pl \
		pce_renew.pl \
		pce_require.pl \
		pce_selection.pl \
		pce_server.pl \
		pce_set_item.pl \
		pce_shell.pl \
		pce_style_item.pl \
		pce_tagged_connection.pl \
		pce_template.pl \
		pce_tick_box.pl \
		pce_toc.pl \
		pce_util.pl \
		pcedraw.pl \
		portray_object.pl \
		pw3.pl \
		scan_arguments.pl \
		stayup_popup.pl \
		subframe.pl \
		twm_geometry_box.pl \
		twm_resize_button.pl

MANPLS= 	man/p_card.pl \
		man/p_data.pl \
		man/util.pl \
		man/v_card.pl \
		man/v_class.pl \
		man/v_editor.pl \
		man/v_error.pl \
		man/v_global.pl \
		man/v_group.pl \
		man/v_hierarchy.pl \
		man/v_inherit.pl \
		man/v_inspector.pl \
		man/v_instance.pl \
		man/v_keyword.pl \
		man/v_manual.pl \
		man/v_module.pl \
		man/v_statistics.pl \
		man/v_summary.pl \
		man/v_tile.pl \
		man/v_topic.pl \
		man/v_visual.pl \
		man/v_search.pl \
		man/man_index.pl \
		man/behaviour_item.pl \
		man/classmap.pl \
		man/classification.obj

CTBPLS=		contrib.pl \
		README
RBIKPLS=	rubik/maplist.pl \
		rubik/rubik.pl \
		rubik/rubikpce.pl \
		rubik/README

DRAWPLS=	draw/align.pl \
		draw/attribute.pl \
		draw/canvas.pl \
		draw/config.pl \
		draw/draw.pl \
		draw/exportpl.pl \
		draw/gesture.pl \
		draw/menu.pl \
		draw/shapes.pl \
		draw/undo.pl

EMACSPLS=	emacs/application.pl \
		emacs/buffer.pl \
		emacs/buffer_menu.pl \
		emacs/c_mode.pl \
		emacs/cpp_mode.pl \
		emacs/fundamental_mode.pl \
		emacs/gdb.pl \
		emacs/hit_list.pl \
		emacs/language_mode.pl \
		emacs/latex_mode.pl \
		emacs/man_mode.pl \
		emacs/prolog_mode.pl \
		emacs/script_mode.pl \
		emacs/server.pl \
		emacs/shell.pl \
		emacs/text_mode.pl \
		emacs/emacs.pl \
		emacs/window.pl \
		emacs/outline_mode.pl \
		emacs/annotate_mode.pl

DIALOGPLS=	dialog/attribute.pl \
		dialog/behaviour.pl \
		dialog/dialog.pl \
		dialog/generate.pl \
		dialog/image_item.pl \
		dialog/label.pl \
		dialog/layout.pl \
		dialog/load.pl \
		dialog/menuitem.pl \
		dialog/meta.pl \
		dialog/pretty_print.pl \
		dialog/proto.pl \
		dialog/util.pl \
		dialog/sub_dialog.pl

LANGPLS=	english/pce_messages.pl

DEMOPLS= 	pce_demo.pl \
		chess.pl \
		constraint.pl \
		cursor.pl \
		event_hierarchy.pl \
		fontviewer.pl \
		graph.pl \
		imageviewer.pl \
		ispell.pl \
		juggler.pl \
		colour.pl \
		kangaroo.pl

PW2DEMOS=	pw2/README \
		pw2/adjective \
		pw2/determiner \
		pw2/dialog_new.pl \
		pw2/noun \
		pw2/parse_tree.help \
		pw2/parse_tree.pl \
		pw2/parse_tree_alt.pl \
		pw2/terminator \
		pw2/thermo.icon \
		pw2/thermo.pl \
		pw2/verb

HLPFILES=	help.hlp dialog.hlp customise.hlp emacs.hlp pcedraw.hlp
LIBHLPFILES=	dragdrop.hlp findfile.hlp overview.hlp

INFOFILES=	ChangeLog README

all:	binfiles qofs
	$(MAKE) release

binfiles:
	(cd ../../quintus/src; $(MAKE))

qofs:	pceshare pce_qpc.qof pce.qof

rttest: $(OBJS) rte_test.qof
	$(QLD) -dv -L `pwd` -L `pwd`/../boot \
		-L `pwd`/../lib rte_test.qof

pw.qui: $(OBJS) 
	$(QLD) -Ddv -L `pwd` -L `pwd`/../boot \
		-L `pwd`/../lib 'library(qui)' \
		$(EXTRALIBS)

pce_qpc.qof: pce_qpc.pl expandgoal.pl expandmath.pl
	$(QPC) -L . -c pce_qux.pl
	$(QPC) -L . -i pce_qpc.pl -c pce_qpc.pl -o pce_qpc0.qof
	$(QLD) -L . -dr pce_qpc0.qof -o $@
	rm -f pce_qpc0.qof

qp_pce.qof: $(PCEQOFS)
	$(QLD) -ro $@ $(PCEQOFS)

pce.qof:	qp_pce.qof

require.qof: require.pl
	$(QPC) -c require.pl

$(PCEQOFS):	pce_qpc.qof

release:	release_directories \
		release_lib \
		release_demo \
		release_contrib \
		release_bin \
		release_makes \
		release_misc \
		info_files \
		help_files \
		lib_qof \
		demo_qof \
		installd

lib_qof:
	cd $(PCELIB); $(MAKE)
demo_qof:
	cd $(PCEDEMO); $(MAKE)

installd:	$(INSTALLD) \
		$(INSTALLD)/postinstall \
		$(INSTALLD)/info \
		$(INSTALLD)/map

release_directories:	$(RELDIRS)

$(PCEHOME):
	mkdir $@
$(PCEHLP):
	mkdir $@
$(PCEHLP)/lib:
	mkdir $@
$(PCELIB):
	mkdir $@
$(PCELIB)/$(BIN):
	mkdir $@
$(PCELIB)/dialog:
	mkdir $@
$(PCELIB)/dialog/lib:
	mkdir $@
$(PCELIB)/draw:
	mkdir $@
$(PCELIB)/emacs:
	mkdir $@
$(PCELIB)/english:
	mkdir $@
$(PCELIB)/man:
	mkdir $@
$(PCELIB)/xref:
	mkdir $@
$(PCEDEMO):
	mkdir $@
$(PCECONTRIB):
	mkdir $@
$(PCECONTRIB)/rubik:
	mkdir $@
$(PCEDEMO)/dialog:
	mkdir $@
$(PCEDEMO)/pw2:
	mkdir $@
$(INSTALLD):
	mkdir -p $@


release_lib:		$(addprefix $(PCELIB)/, $(BOOTQOFS)) \
			$(addprefix $(PCELIB)/, $(BOOTPLS)) \
			$(addprefix $(PCELIB)/, $(LIBPLS)) \
			$(addprefix $(PCELIB)/, $(DRAWPLS)) \
			$(addprefix $(PCELIB)/, $(DIALOGPLS)) \
			$(addprefix $(PCELIB)/, dialog/bitmaps) \
			$(addprefix $(PCELIB)/, $(EMACSPLS)) \
			$(addprefix $(PCELIB)/, $(MANPLS)) \
			$(addprefix $(PCELIB)/, $(LANGPLS))

release_demo:		$(addprefix $(PCEDEMO)/, $(DEMOPLS)) 
#			$(addprefix $(PCEDEMO)/, $(PW2DEMOS))

release_contrib:	$(addprefix $(PCECONTRIB)/, $(CTBPLS)) \
			$(addprefix $(PCECONTRIB)/, $(RBIKPLS))

info_files:		$(addprefix $(PCEHOME)/, $(INFOFILES))

help_files:		$(addprefix $(PCEHLP)/, $(HLPFILES)) \
			$(addprefix $(PCEHLP)/lib/, $(LIBHLPFILES))

release_bin:	$(addprefix $(PCELIB)/$(BIN)/, $(BINFILES))

release_makes:	$(PCELIB)/Makefile $(PCEDEMO)/Makefile

release_misc:	$(PCEHOME)/Pce $(PCEHOME)/bitmaps $(PCELIB)/dialog/bitmaps \
		$(PCEHOME)/man $(PCEHOME)/postscript $(PCEHOME)/makepw3 \
		$(PCELIB)/xref/common.pl $(PCELIB)/xref/quintus.pl \
		$(PCEHOME)/include $(PCEHOME)/CXX

pceshare:	$(PCESHARE) english

pce_editor.pl: ../boot/pce_editor.pl
	$(LN) $< $@
pce_realise.pl: ../boot/pce_realise.pl
	$(LN) $< $@
pce_expansion.pl: ../boot/pce_expansion.pl
	$(LN) $< $@
pce_error.pl: ../boot/pce_error.pl
	$(LN) $< $@
pce_expand.pl: ../boot/pce_expand.pl
	$(LN) $< $@
pce_global.pl: ../boot/pce_global.pl
	$(LN) $< $@
pce_operator.pl: ../boot/pce_operator.pl
	$(LN) $< $@
pce_autoload.pl: ../boot/pce_autoload.pl
	$(LN) $< $@
english:
	$(LN) ../lib/english $@

$(PCELIB)/pce.pl:	../pwboot/pce.pl
	$(CP) ../pwboot/pce.pl $(PCELIB)/pce.pl
$(PCELIB)/pce_loadcxx.pl: pce_loadcxx.pl
	$(CP) pce_loadcxx.pl $(PCELIB)/pce_loadcxx.pl

# copy - implicit rules

$(PCELIB)/%: ../pwboot/%
	$(CP) $< $@

$(PCELIB)/%: ../lib/%
	$(CP) $< $@

$(PCEDEMO)/dialog/%: ../lib/dialog/demo/%
	$(CP) $< $@

$(PCECONTRIB)/%: ../contrib/%
	$(CP) $< $@

$(PCEDEMO)/pw2/%: ../../PWdemo/%
	$(CP) $< $@

$(PCEDEMO)/%: ../demo/%
	$(CP) $< $@

$(PCEHLP)/%: ../../appl-help/%
	$(CP) $< $@

# copy specific files

$(PCEHOME)/%:	../../%
	$(CP) $< $@

$(PCEHOME)/Makefile: install_make
	$(CP) $< $@

$(PCEHOME)/README: ../../README.pw3
	$(CP) $< $@

$(PCEHOME)/makepw3: ../../make_pw3
	$(CP) $< $@

$(PCELIB)/Makefile:  lib_make
	$(CP) $< $@

$(PCEDEMO)/Makefile: demo_make
	$(CP) $< $@

$(PCELIB)/$(BIN)/libpce.$(SO):	../../quintus/src/$(BIN)/libpce.$(SO)
	$(CP) $< $@

$(PCELIB)/$(BIN)/%:	../../quintus/src/$(BIN)/%
	$(CP) $< $@

$(PCELIB)/$(BIN)/%:	../../$(builddir)/%
	$(CP) $< $@
$(PCELIB)/$(BIN)/libXPCE.$(SO).$(PCEMAJOR):
	$(LN) libXPCE.$(SO) $@
$(PCELIB)/$(BIN)/libXPCE.$(SO).$(PCEMAJOR).$(PCEMINOR).$(PCEPATCH):
	$(LN) libXPCE.$(SO) $@

# copy directories

$(PCEHOME)/bitmaps:
	mkdir $@
	$(CP) -r ../../bitmaps/* $@

$(PCELIB)/dialog/bitmaps:
	mkdir $@
	$(CP) ../lib/dialog/bitmaps/*.bm $@

$(PCEHOME)/include:
	mkdir $@
	$(CP) -r ../../include/* $@
	rm -fr $@/CVS $@/pce/CVS

$(PCEHOME)/CXX:
	mkdir $@
	$(CP) -r ../../CXX/* $@
	rm -fr $@/CVS $@/demo/CVS

$(PCEHOME)/man:
	mkdir $@
	$(CP) -r ../../man/reference $@
	$(CP) -r ../../man/xpce-client.1 $@
	rm -fr $@/CVS $@/*/CVS $@/*/*/CVS $@/*/*/*/CVS

$(PCEHOME)/postscript:
	mkdir $@
	$(CP) -r ../../postscript/* $@
	rm -fr $@/CVS

$(INSTALLD)/postinstall:	postinstall
	$(CP) $< $@
$(INSTALLD)/info:	info
	$(CP) $< $@
$(INSTALLD)/map:
	cd ../.. && \
	rm -f $(LINSTALLD)/map && \
	for f in `find prowindows$(PWVERSION) -type f`; do \
	    echo "`$(SUM) $$f` $$f" >> $(LINSTALLD)/map; \
	done

clean:
	rm -fr $(PCEHOME)
	rm -f $(PCEQOFS) $(BOOTQOFS)
	rm -f $(PCESHARE)
	rm -f pce_qpc0.qof pce_qux.qof
