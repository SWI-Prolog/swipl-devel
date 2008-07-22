TEXMF=/usr/share/texmf
DOCFONT=$(TEXMF)/fonts/tfm/doc
DOCTEX=$(TEXMF)/tex/latex/doc

install: install-fonts install-tex rehash
	
install-tex:
	if [ ! -d $(DOCTEX) ]; then mkdir $(DOCTEX); fi
	cp txt.sty rfc.sty i-d.sty $(DOCTEX)

install-fonts:
	if [ ! -d $(DOCFONT) ]; then mkdir $(DOCFONT); fi
	cp doc.tfm $(DOCFONT)
	(cd $(DOCFONT) && ln doc.tfm docbf.tfm)
	(cd $(DOCFONT) && ln doc.tfm docul.tfm)

rehash:
	texconfig rehash
