TEXMF=/usr/share/texmf
DOCFONT=$(TEXMF)/fonts/tfm/doc

install::
	if [ ! -d $(DOCFONT) ]; then mkdir $(DOCFONT); fi
	cp doc.tfm $(DOCFONT)
	(cd $(DOCFONT) && ln doc.tfm docbf.tfm)
	(cd $(DOCFONT) && ln doc.tfm docul.tfm)
	texconfig rehash
