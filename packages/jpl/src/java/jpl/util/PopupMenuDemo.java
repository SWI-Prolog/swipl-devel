package jpl.util;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


/*
 * Adapted from a Swing Connection demo
 * see pcm's jpl_demo:jpl_popup_demo/0
 */
public class PopupMenuDemo extends JFrame 
                           implements ActionListener {
	private static final long	serialVersionUID	= 1L;
	// JTextArea output;
    public JPopupMenu popup;
	JMenuItem source;
	int mi;

    public PopupMenuDemo() {

     // Add regular components to the window, using the default BorderLayout.
     // output = new JTextArea(5, 30);
     // output.setEditable(false);
     // getContentPane().add(new JScrollPane(output), BorderLayout.CENTER);
    }

/*	JPopupMenu
	 +-	JMenuItem
	 +-	JMenuItem
	 +-	JMenu -----	JPopupMenu
	 |				 +-	JMenuItem
	 |				 +-	JMenuItem
	 +-	JMenuItem
	 +-	JMenuItem
 */
    public boolean search(JPopupMenu p) {
		Object[] mes = p.getSubElements();		// array of JMenuItem or JMenu (see diagram)
		int i;

		for ( i=0 ; i<mes.length ; i++ ) {
			if ( mes[i] == source ) {			// it's the clicked JMenuItem
				return true;
			}
			else
			if ( mes[i] instanceof JMenu ) {	// it's a submenu
				if ( search((JPopupMenu)(((JMenu)mes[i]).getSubElements())[0]) ) {
					return true;				// clicked JMenuItem was within this submenu tree
				}
			}
			else {								// it's a non-matching leaf element
				mi++;							// next JMenuItemm will be the mi-th
			}
		}
		return false;
	}

	public void actionPerformed(ActionEvent e) {
        source = (JMenuItem)(e.getSource());
     // output.append("action event source: " + source.getText()
     //          + " (an instance of " + getClassName(source) + ")\n");
		mi = 1;
		if ( search(popup) ) {
	 //		output.append("clicked " + mi + "\n");
			if ( (new jpl.Query("jpl_popup_demo_callback", new jpl.Term[] {new jpl.Integer(mi)})).hasSolution() ) {
		 //		output.append("succeeded\n");
			}
			else {
		 //		output.append("failed\n");
			}
		}
	 //	output.append("jpl_popup_demo_callback(" + mi + ")\n");
    }

 // protected String getClassName(Object o) {    		// Returns just the class name -- no package info.
 //     String classString = o.getClass().getName();
 //     int dotIndex = classString.lastIndexOf(".");
 //     return classString.substring(dotIndex+1);
 // }

	public JPopupMenu buildPopupMenu(Object[] mis) {
		int i;
		JPopupMenu m = new JPopupMenu((String)mis[0]);
		JMenuItem mi;

		for ( i=1 ; i<mis.length ; i++ ) {
			if ( mis[i] instanceof String ) {
				mi = new JMenuItem((String)mis[i]);
				mi.addActionListener(this);
				m.add(mi);
			}
			else
			if ( mis[i] instanceof Object[] ) {
				m.add(buildSubMenu((Object[])mis[i]));
			}
			else {
				return null;							// bad menuitems array
			}
		}
		return m;
	}

	public JMenu buildSubMenu(Object[] mis) {
		int i;
		JMenu m = new JMenu((String)mis[0]);
		JMenuItem mi;

		for ( i=1 ; i<mis.length ; i++ ) {
			if ( mis[i] instanceof String ) {
				mi = new JMenuItem((String)mis[i]);
				mi.addActionListener(this);
				m.add(mi);
			}
			else
			if ( mis[i] instanceof Object[] ) {
				m.add(buildSubMenu((Object[])mis[i]));
			}
			else {
				return null;							// bad menuitems array
			}
		}
		return m;
	}

	public void showPopup(Object[] mis, int x, int y) {
		Point p = getLocationOnScreen();		// on-screen location of origin of this Frame

		popup = buildPopupMenu(mis);			// discarding any previous one...
		setVisible(true);						// ensure this Frame thingy is visible (else we get an error)
		popup.show(this,x-p.x,y-p.y);			// must show over some Component (e.g. this)
	}

}
