JPL
	is a simple Servlet 2.2 (or later) web application
	containing its own copy of jpl.jar and a couple of
	servlets which call SWI-Prolog (see JPL/WEB-INF/classes)

JPL.war
	is JPL in the form of a "web archive"

To deploy under Tomcat, copy either JPL or JPL.war into
Tomcat's webapps folder and restart Tomcat.

Then visit the application's default ("welcome") page, e.g.

	http://localhost:8080/JPL

with a web browser.

----
Paul Singleton
February 2004

