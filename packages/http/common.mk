# Makefile fragments to are updated frequently and can be shared

LIBPL=		html_write.pl http_client.pl http_header.pl \
		http_mime_plugin.pl http_sgml_plugin.pl \
		mimepack.pl mimetype.pl dcg_basics.pl \
		thread_httpd.pl xpce_httpd.pl inetd_httpd.pl \
		http_wrapper.pl http_open.pl http_session.pl \
		http_error.pl http_parameters.pl http_dispatch.pl \
		http_authenticate.pl http_stream.pl http_log.pl \
		http_path.pl http_hook.pl html_head.pl http_exception.pl \
		json.pl http_json.pl json_convert.pl http_dirindex.pl \
		http_server_files.pl http_pwp.pl http_host.pl
EXAMPLES=	demo_body.pl demo_client.pl demo_threads.pl demo_xpce.pl \
		calc.pl demo_files.pl demo_pwp.pl
EXAMPLEEXE=	demo_inetd
XPCEPL=		http_image.pl
