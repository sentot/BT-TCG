package core;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.util.*;
import java.io.IOException;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import core.Main;

// dummies serve as example tab add-ons, delete them later when we have real add-ons
//import dummies.DummyPanel;
//import dummies.DummyTextArea;

import excel.SaveExcel;
import excel.SavePathExcel;
import tree.Node;


public class Launcher extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private static JPopupMenu popupForOR = new JPopupMenu();
	private static JPopupMenu popupForUA = new JPopupMenu();

	private static void addToORPopup (String label, String entry) {
		JMenuItem menuItem = new JMenuItem(label);
		final String str = entry;
		menuItem.addActionListener(new java.awt.event.ActionListener(){
  		  public void actionPerformed(java.awt.event.ActionEvent evt) {
  			  Main.insertORTextInput(str);
  		  }
		});
		popupForOR.add(menuItem);
	}

	private static void addToUAPopup (String label, String entry) {
		JMenuItem menuItem = new JMenuItem(label);
		final String str = entry;
		menuItem.addActionListener(new java.awt.event.ActionListener(){
  		  public void actionPerformed(java.awt.event.ActionEvent evt) {
  			  Main.insertUATextInput(str);
  		  }
		});
		popupForUA.add(menuItem);
	}
	
	public static void clear() {
		System.out.println("STUFF CLEARED");
		// Insert your reset code here.
		// This is called e.g., when a new BT or configuration file is loaded.
	}
	
	public static ArrayList<JComponent> fileMenuItems () {
		
		ArrayList<JComponent> result = new ArrayList<JComponent>();
		
		// For saveExcel, the menu item is itself a menu (cascading menu)
		JMenu saveExcel = new JMenu("Save Test Cases as Excel");
		saveExcel.add(new SaveExcel(false, false));
		saveExcel.addSeparator();
		saveExcel.add(new SaveExcel(false, true));
		saveExcel.addSeparator();
		saveExcel.add(new SaveExcel(true, false));
		result.add(saveExcel);

		JMenu savePathExcel = new JMenu("Save Test Plan as Excel");
		savePathExcel.add(new SavePathExcel(false));
		savePathExcel.addSeparator();
		savePathExcel.add(new SavePathExcel(true));
		result.add(savePathExcel);
		
		return result;
		
	}
	
	public static void main(String[] args) {
		Boolean success = false;
	    try {
	      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
	      success = true;
	    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
	        | UnsupportedLookAndFeelException e1) {
	      System.err
	          .println("System 'Look and Feel' could not be found. Will try Windows 'Look and Feel'");
	    }
	    if (!success) {
	    	try {
	    		UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
	    	} catch (ClassNotFoundException | InstantiationException | IllegalAccessException
	    			| UnsupportedLookAndFeelException e1) {
	    		System.err.println("Windows 'Look and Feel' could not be found. Using default 'Look and Feel'");
	    	}
	    }
	    Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
	      public void run() {
	      	  Main.closeSBCL();
	      }
	    }));
	    
	    ArrayList<JComponent> comps = new ArrayList<JComponent>();
	    ArrayList<String> labels = new ArrayList<String>();
	    
	    // Tab add-ons added here using comps.add and labels.add
	    
//	    comps.add(new DummyPanel());
//	    labels.add("Dummy 1");
//	    
//	    comps.add(new DummyTextArea());
//	    labels.add("Dummy 2");
	    
	    // End of tab add-ons
	    
	    final ArrayList<JComponent> cmps = comps;
	    final ArrayList<String> lbls = labels;
	    
	    EventQueue.invokeLater(new Runnable() {
	      public void run() {
	    	  Main.createFrame(cmps,lbls);
	    	  
	    	  // popup menu items added here
	    	  
	    	  addToORPopup("wait","time.sleep(secs)\n");
	    	  addToORPopup("value","print(driver.find_element_by_id(\"id\").get_attribute(\"value\"))\n");  
	    	  addToORPopup("check value","if driver.find_element_by_id(\"id\").get_attribute(\"value\") == value:\n  success\nelse:\n  fail\n");

	    	  addToUAPopup("wait","time.sleep(secs)\n");
	    	  
	    	  // you can do fancier things in popup items using code similar to, e.g.:
	    	  
	    	  JMenuItem menuItem = new JMenuItem("check component value");
	    	  menuItem.addActionListener(new java.awt.event.ActionListener(){
	    		  public void actionPerformed(java.awt.event.ActionEvent evt) {
	    			  if (Main.getSelectedORProfile() != null) {
		    			  Main.insertORTextInput("if driver.find_element_by_id(\""
		    					  + Main.getSelectedORProfile().getComponent()
		    					  + "\") == \""
		    					  + Main.getSelectedORProfile().getBehaviour()
		    					  + "\" then: \n  success\nelse:\n  fail\n");
	    			  }
	    		  }
	    		  });
	    	  popupForOR.add(menuItem);

	    	  JMenuItem menuItem2 = new JMenuItem("default action");
	    	  menuItem2.addActionListener(new java.awt.event.ActionListener(){
	    		  public void actionPerformed(java.awt.event.ActionEvent evt) {
	    			  if (Main.getSelectedUAProfile() != null) {
	    				  Main.insertUATextInput("driver.find_element_by_id(\""
	    		    			  + Main.getSelectedUAProfile().getComponent()
	    		    					  + "\")." + Main.getSelectedUAProfile().getBehaviour() + "()\n");
	    			  }
	    		  }
	    		  });
	    	  popupForUA.add(menuItem2); 

	    	  JMenuItem menuItem3 = new JMenuItem("click");
	    	  menuItem3.addActionListener(new java.awt.event.ActionListener(){
	    		  public void actionPerformed(java.awt.event.ActionEvent evt) {
	    			  if (Main.getSelectedUAProfile() != null) {
	    				  Main.insertUATextInput("driver.find_element_by_id(\""
	    		    			  + Main.getSelectedUAProfile().getComponent()
	    		    					  + "\").click()\n");
	    			  }
	    		  }
	    		  });
	    	  popupForUA.add(menuItem3); 
	    	  
	    	  // end of adding popup menu items
	    	  
	    	  Main.setORTextInputPopup(popupForOR);  
	    	  Main.setUATextInputPopup(popupForUA);
	    	  
	    	  
	    	  Main.revalidateNow();
	    	  
	      }
	    });
	  }
	  
}
