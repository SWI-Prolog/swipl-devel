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
PCEMAJOR=4
PCEMINOR=9
PCEPATCH=7

PWVERSION=3.1
QPC=qpc
QLD=qld

CP=cp -p
LN=ln -s

#Deliver Version
#DELFLAGS = -h
DELFLAGS =
CDEBUGFLAGS=
#Debug/Test version
#DELFLAGS = 
#CDEBUGFLAGS= -g

PCEHOME=../../prowindows$(PWVERSION)
PCEHLP=$(PCEHOME)/appl-help
PCELIB=$(PCEHOME)/library
PCEDEMO=$(PCEHOME)/demo
PCECONTRIB=$(PCEHOME)/contrib

.SUFFIXES: .qof .pl

.pl.qof:
	$(QPC) -L . -i pce_qpc.qof -N $(DELFLAGS) -cH $*.pl -o $*.qof


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
		libpce.a
#		libXPCEmain.a
#		libXPCE.$(SO) libXPCE.$(SO).$(PCEMAJOR) \
#		libXPCE.$(SO).$(PCEMAJOR).$(PCEMINOR).$(PCEPATCH)

BOOTQOFS=	pce_qpc.qof \
		qp_pce.qof \
		pce.qof
BOOTPLS=	pce_qux.pl \
		QPINDEX.pl \
		english/pce_messages.pl

LIBPLS=		pce.pl \
		pce_util.pl \
		area.pl \
		pce_emacs.pl \
		emacs_tags.pl \
		emacs_extend.pl \
		file_item.pl \
		find_file.pl \
		keybinding.pl \
		pce_debug.pl \
		pce_edit.pl \
		pce_manual.pl \
		pce_objects.pl \
		pce_portray.pl \
		pce_postscript.pl \
		pce_server.pl \
		pcedraw.pl \
		portray_object.pl \
		scan_arguments.pl \
		stayup_popup.pl \
		twm_resize_button.pl \
		twm_geometry_box.pl \
		subframe.pl \
		pce_renew.pl \
		pce_font_item.pl \
		pce_style_item.pl \
		pce_cxx_headers.pl \
		pce_selection.pl \
		pce_drag_and_drop.pl \
		dragdrop.pl \
		pce_drag_and_drop_dict_item.pl \
		dragdict.pl \
		pce_require.pl \
		pce_shell.pl \
		pce_main.pl \
		pce_toc.pl \
		pce_tick_box.pl \
		edit_dialog.pl \
		make_dialog.pl \
		pce_editable_text.pl \
		pce_helper.pl \
		pce_help_file.pl \
		help_message.pl \
		pce_image.pl \
		pce_prompter.pl \
		pce_template.pl \
		pce_tagged_connection.pl \
		pce_loadcxx.pl

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
		draw/draw.pl \
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

DEMOPLS= 	biff.pl \
		pce_demo.pl \
		chess.pl \
		constraint.pl \
		cursor.pl \
		dialog.pl \
		event_hierarchy.pl \
		fontviewer.pl \
		ftplog.pl \
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

HLPFILES=	help.hlp dialog.hlp customise.hlp emacs.hlp pcedraw.hlp \
		pcefaq.hlp
LIBHLPFILES=	dragdrop.hlp findfile.hlp overview.hlp

INFOFILES=	ChangeLog README.customise README.font

all:	binfiles qofs release

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

pce_qpc.qof: pce_qpc.pl
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

release: directories release_lib release_demo release_contrib \
	 release_bin release_makes release_misc info_files help_files
	cd $(PCELIB); $(MAKE)
	cd $(PCEDEMO); $(MAKE)

directories:
	@[ -d $(PCEHOME) ] || mkdir $(PCEHOME)
	@[ -d $(PCEHLP) ] || mkdir $(PCEHLP)
	@[ -d $(PCEHLP)/lib ] || mkdir $(PCEHLP)/lib
	@[ -d $(PCELIB) ] || mkdir $(PCELIB)
	@[ -d $(PCELIB)/$(BIN) ] || mkdir $(PCELIB)/$(BIN)
	@[ -d $(PCELIB)/dialog ] || mkdir $(PCELIB)/dialog
	@[ -d $(PCELIB)/dialog/lib ] || mkdir $(PCELIB)/dialog/lib
	@[ -d $(PCELIB)/draw ] || mkdir $(PCELIB)/draw
	@[ -d $(PCELIB)/emacs ] || mkdir $(PCELIB)/emacs
	@[ -d $(PCELIB)/english ] || mkdir $(PCELIB)/english
	@[ -d $(PCELIB)/man ] || mkdir $(PCELIB)/man
	@[ -d $(PCELIB)/xref ] || mkdir $(PCELIB)/xref
	@[ -d $(PCEDEMO) ] || mkdir $(PCEDEMO)
	@[ -d $(PCECONTRIB) ] || mkdir $(PCECONTRIB)
	@[ -d $(PCECONTRIB)/rubik ] || mkdir $(PCECONTRIB)/rubik
	@[ -d $(PCEDEMO)/dialog ] || mkdir $(PCEDEMO)/dialog
	@[ -d $(PCEDEMO)/pw2 ] || mkdir $(PCEDEMO)/pw2


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

release_bin:	$(addprefix $(PCELIB)/$(BIN)/, $(BINFILES)) \
		$(PCELIB)/$(BIN)/libXt.so.4.0

release_makes:	$(PCEHOME)/Makefile $(PCELIB)/Makefile $(PCEDEMO)/Makefile

release_misc:	$(PCEHOME)/Pce $(PCEHOME)/bitmaps $(PCELIB)/dialog/bitmaps \
		$(PCEHOME)/man $(PCEHOME)/postscript \
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

$(PCELIB)/Makefile:  lib_make
	$(CP) $< $@

$(PCEDEMO)/Makefile: demo_make
	$(CP) $< $@

$(PCELIB)/$(BIN)/libpce.$(SO):	../../quintus/src/$(BIN)/libpce.$(SO)
	$(CP) $< $@

$(PCELIB)/$(BIN)/libpce.a:	../../quintus/src/$(BIN)/libpce.a
	$(CP) $< $@

$(PCELIB)/$(BIN)/%:	../../$(builddir)/%
	$(CP) $< $@
$(PCELIB)/$(BIN)/libXPCE.$(SO).$(PCEMAJOR):
	$(LN) libXPCE.$(SO) $@
$(PCELIB)/$(BIN)/libXPCE.$(SO).$(PCEMAJOR).$(PCEMINOR).$(PCEPATCH):
	$(LN) libXPCE.$(SO) $@

ifeq "$(BIN)" "sun4-4"
$(PCELIB)/$(BIN)/libXt.so.4.0:	../../quintus/src/$(BIN)/libXt.so.4.0
	$(CP) $< $@
else
.PHONY: $(PCELIB)/$(BIN)/libXt.so.4.0
endif


# copy directories

$(PCEHOME)/bitmaps:
	mkdir $@
	$(CP) ../../bitmaps/*.bm $@

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
	$(CP) -r ../../man/faq $@
	$(CP) -r ../../man/xpce.1 $@
	$(CP) -r ../../man/xpce-client.1 $@
	rm -fr $@/CVS $@/*/CVS $@/*/*/CVS $@/*/*/*/CVS
	rm -f $@/releasenotes/*.fm*

$(PCEHOME)/postscript:
	mkdir $@
	$(CP) -r ../../postscript/* $@
	rm -fr $@/CVS

clean:
	rm -fr $(PCEHOME)
	rm -f $(PCEQOFS) $(BOOTQOFS)
	rm -f $(PCESHARE)
	rm -f pce_qpc0.qof pce_qux.qof
