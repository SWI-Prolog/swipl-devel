SHELL=/bin/csh

psfiles:
	dvips -t 0 refman.dvi | gzip > ~ftp/pub/xpce/doc/refman/refman--30-0.ps.gz 
	tpr -f  ~ftp/pub/xpce/doc/refman/refman refman

clean:
	rm -f *~ refman.?? refman.???
