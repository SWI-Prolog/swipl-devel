LIBPL=		doc_html.pl doc_wiki.pl doc_modes.pl doc_register.pl \
		doc_process.pl doc_index.pl doc_search.pl doc_man.pl \
		doc_library.pl hooks.pl doc_htmlsrc.pl doc_colour.pl \
		doc_util.pl doc_access.pl
SUPPORT=	pldoc.css pldoc.js pllisting.css pldoc.sty \
		edit.gif private.png public.png reload.gif favicon.ico \
		up.gif source.png h1-bg.png pub-bg.png multi-bg.png \
		priv-bg.png
DOCALL=		$(LIBPL) $(SUPPORT)
PUBPL=		pldoc.pl doc_http.pl doc_latex.pl
EXAMPLES=	README
EXAMPLEEXE=	man_server.pl
