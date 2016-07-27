package core;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.jdom2.Attribute;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;
import org.jdom2.output.Format;
import org.jdom2.output.XMLOutputter;

import com.SBCLPipe;

import other.Constants;
import other.TestCase;
import other.TestSegment;
import renderers.CPCell;
import renderers.NOICell;
import renderers.NodeProfileTableModel;
import renderers.ORCell;
import renderers.TestCaseCell;
import renderers.TestCasesModel;
import renderers.TestPathCell;
import renderers.TestPathModel;
import renderers.UACell;
import tree.Block;
import tree.Node;
import tree.NodeProfile;
import util.BTModelReader;

import core.Launcher;

public class Main {

  /**
   * Application
   */
  private static JFrame frame = null;

  /**
   * Main Panel
   */
  private static JPanel contentPane;

  /**
   * Components on Main Panel
   */
  private static JTabbedPane tabbedPane;
  private static JButton btnGenerateTestCases;
  private static JButton btnSpare;

  // ===== NEW CONFIGURATION
  
  // GUI components
  
  private static JTable tableCPs;
  private static JComboBox<NodeProfile> comboInitial = new JComboBox<NodeProfile>();
  private static JComboBox<Block> comboInitialNode = new JComboBox<Block>();
  private static JTable tableNOIs;
  private static NodeProfile initialNodeProfile;
  private static Block initialBlock;
  
  // Data structures

  static TreeSet<NodeProfile> chosenCPSET = new TreeSet<NodeProfile>();
  static TreeSet<NodeProfile> chosenNOISET = new TreeSet<NodeProfile>();
  
 
  // =====
  
  // ===== NEW DICTIONARIES
  
  // GUI components

  private static JTable tableActions;
  private static JTable tableObservables;
  private static final JTextArea textareaAction = new JTextArea();
  private static final JTextArea textareaObservable = new JTextArea();
  private static final JCheckBox checkboxAction = new JCheckBox("Action needs preparation");

  // Data structures

  static LinkedHashMap<NodeProfile, String> observables = new LinkedHashMap<NodeProfile, String>();
  static LinkedHashMap<NodeProfile, String[]> actions = new LinkedHashMap<NodeProfile, String[]>();
  
 
  // =====
  
  
  
  /**
   * Components on the Test Planner tab
   */
  private static JTable tblTCs;
  private static JTable tblTP;

  static JLabel lblCurrentTestPath = new JLabel("Current Test Path (0 of 0 cases)");
  
  private static ArrayList<TestCase> allTestCases = new ArrayList<TestCase>();
  private static ArrayList<TestSegment> testPath = new ArrayList<TestSegment>();
  
  /**
   * Maps to store info about BT Model
   */
  
  // Keep indexToNodesMap
  private static TreeMap<Integer, Block> indexToNodesMap = new TreeMap<Integer, Block>();
  
  
  private static HashMap<String, ArrayList<Node>> compToBehaviourMap =
      new HashMap<String, ArrayList<Node>>();

  /**
   * Variables that must be saved
   */
  private static String btXML;
  private static String btFilePath;
  private static Integer btFileType = 0;
  
  
  /**
   * Other
   */
  private static SBCLPipe sbcl = new SBCLPipe();
  private static Boolean isLoaded = false;

  /**
   * Launching has been factored out.
   */
  
  public static void closeSBCL () {
	  try {
		  sbcl.close();
	  } catch (IOException e) {
		  printErrorMessage("error|3|");
	  }
  }
  
  public static SBCLPipe getSBCL() {
	  return sbcl;
  }
  
  public static void createFrame (ArrayList<JComponent> comps, ArrayList<String> labels) {
      try {
    	  frame = new JFrame();
    	  frame.setTitle("BT-TCG");
    	  
    	  
    	  fillFrame();
          frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
          for (Integer i = 0; i < comps.size(); i++) {
	    	  tabbedPane.addTab(labels.get(i), null, comps.get(i), null);
          }
          frame.setLocationRelativeTo(null);
          frame.pack();
          frame.setVisible(true);
          frame.setSize(800,600);
        } catch (Exception e) {
          e.printStackTrace();
        }
  }
  
  public static void validateNow() {
	  frame.validate();
  }

  public static void revalidateNow() {
	  frame.revalidate();
  }
  
  public static void setVisibleNow() {
	  frame.setVisible(true);
  }
  
  public static void setTitle(String title) {
	  frame.setTitle(title);
  }
  
  public static void packNow() {
	  frame.pack();
  }
  
  public static Boolean modelIsLoaded () {
	  return isLoaded;
  }
  
  public static String getBTFilePath () {
	  return btFilePath;
  }
  
  public static Block getBlock(Integer i) {
	  return indexToNodesMap.get(i);
  }
  
  public static Integer numberOfBlocks () {
	  return indexToNodesMap.size();
  }
  
  public static Boolean testCasesGenerated () {
	  return (allTestCases.size() > 0);
  }
  
  public static Boolean testPathGenerated () {
	  return (testPath.size() > 0);
  }
  
  public static Boolean isChosenCP(NodeProfile nodeProfile) {
	  return chosenCPSET.contains(nodeProfile);
  }
  
  public static Block getInitialBlock() {
	  return initialBlock;
  }

  public static Boolean isChosenNOI(NodeProfile nodeProfile) {
	  return chosenNOISET.contains(nodeProfile);
  }
  
  public static String getAction(NodeProfile prof) {
	  String result = "";
	  if (actions.containsKey(prof)) {
		  result = actions.get(prof)[0];
	  }
	  return result;
  }
  
  public static String getPrepAsString(NodeProfile prof) {
	  String result = "false";
	  if (actions.containsKey(prof)) {
		  result = actions.get(prof)[1];
	  }
	  return result;
  }

  public static Boolean getPrepAsBoolean(NodeProfile prof) {
	  Boolean result = false;
	  if (actions.containsKey(prof)) {
		  if (actions.get(prof)[1].equals("true")) {
			  result = true;
		  }
	  }
	  return result;
  }
  
  public static String getObservable(NodeProfile prof) {
	  String result = "";
	  if (observables.containsKey(prof)) {
		  result = observables.get(prof);
	  }
	  return result;
  }
  
  private static void setAction(NodeProfile prof, String value, Boolean preamble) {
	  String[] data = new String[] {value, Boolean.toString(preamble)};
	  actions.put(prof, data);
  }
  
  private static void setObservable(NodeProfile prof, String value) {
	  observables.put(prof, value);
  }

  public static String observableResponse (Node node) {
	  if (observables.containsKey(node.getNodeProfile())) {
		  return observables.get(node.getNodeProfile());
	  } else {
		  return "";
	  }
  }
  
  public static String userAction (Node node) {
	  if (actions.containsKey(node.getNodeProfile())) {
		  return actions.get(node.getNodeProfile())[0];
	  } else {
		  return "";
	  }
  }
  
  public static Boolean needsPreparation (Node node) {
	  String entry = "";
	  NodeProfile prof = internNodeProfile(node);
	  if (actions.containsKey(prof)) {
		  entry = actions.get(prof)[1];
	  }
	  return (entry == "true");
  }
  
  public static ArrayList<TestCase> getTestCases() {
	  return allTestCases;
  }
  
  public static ArrayList<TestSegment> getTestPath() {
	  return testPath;
  }
  
  public static void insertORTextInput(String text) {
	  textareaObservable.insert(text, textareaObservable.getCaretPosition());
	  for (int i : tableObservables.getSelectedRows()) {
		  setObservable((NodeProfile) tableObservables.getValueAt(i, 0), textareaObservable.getText());;
          tableObservables.repaint();
        }
  }
  
  public static void setORTextInput(String text) {
	  textareaObservable.setText(text);
	  for (int i : tableObservables.getSelectedRows()) {
		  setObservable((NodeProfile) tableObservables.getValueAt(i, 0), textareaObservable.getText());
          tableObservables.repaint();
        }
  }

  public static void appendORTextInput(String text) {
	  textareaObservable.append(text);
	  for (int i : tableObservables.getSelectedRows()) {
		  setObservable((NodeProfile) tableObservables.getValueAt(i, 0), textareaObservable.getText());
          tableObservables.repaint();
        }
  }
  
  public static void insertUATextInput(String text) {
	  textareaAction.insert(text, textareaAction.getCaretPosition());
	  for (int i : tableActions.getSelectedRows()) {
		  setAction((NodeProfile) tableActions.getValueAt(i, 0),textareaAction.getText(),checkboxAction.isSelected());
          tableActions.repaint();
        }
  }

  public static void setUATextInput(String text) {
	  textareaAction.setText(text);
	  for (int i : tableActions.getSelectedRows()) {
		  setAction((NodeProfile) tableActions.getValueAt(i, 0),textareaAction.getText(),checkboxAction.isSelected());
          tableActions.repaint();
        }
  }

  public static void appendUATextInput(String text) {
	  textareaAction.append(text);
	  for (int i : tableActions.getSelectedRows()) {
		  setAction((NodeProfile) tableActions.getValueAt(i, 0),textareaAction.getText(),checkboxAction.isSelected());
          tableActions.repaint();
        }
  }
  
  public static NodeProfile getSelectedORProfile() {
	  if (tableObservables.getSelectedRows().length > 0) {
		  return ((NodeProfile) tableObservables.getValueAt(tableObservables.getSelectedRows()[0], 0));
	  }
	  return null;
  }
  
  public static NodeProfile getSelectedUAProfile() {
	  if (tableActions.getSelectedRows().length > 0) {
		  return ((NodeProfile) tableActions.getValueAt(tableActions.getSelectedRows()[0], 0));
	  }
	  return null;
  }
  
  public static void setORTextInputPopup (JPopupMenu popup) {
	  textareaObservable.setComponentPopupMenu(popup);
  }

  public static void setUATextInputPopup (JPopupMenu popup) {
	  textareaAction.setComponentPopupMenu(popup);
  }
  
  // Node profiles are interned
  
  private static ArrayList<NodeProfile> nodeProfiles = new ArrayList<NodeProfile>();
  
  public static ArrayList<NodeProfile> getNodeProfiles() {
	  return nodeProfiles;
  }
  
  private static NodeProfile internNodeProfile(Node node) {
	  NodeProfile profile = node.getNodeProfile();
	  Boolean found = false;
	  for (Integer i=0;i<nodeProfiles.size();i++) {
		  if (profile.equals(nodeProfiles.get(i))) {
			  found = true;
			  profile = nodeProfiles.get(i);
			  break;
		  }
	  }
	  if (found == false) {
		  nodeProfiles.add(profile);
	  }
	  node.setNodeProfile(profile);
	  return profile;
  }
  
  private static NodeProfile internProfile(NodeProfile prof) {
	  NodeProfile result = prof;
	  for (Integer i=0;i<nodeProfiles.size();i++) {
		  if (prof.equals(nodeProfiles.get(i))) {
			  result = nodeProfiles.get(i);
			  break;
		  }
	  }
	  return result;
  }
  
  private static void internNodeProfiles() {
	  for (Integer i=0;i<indexToNodesMap.size();i++) {
		  for (Node node:indexToNodesMap.get(i).getNodes()) {
			  internNodeProfile(node);
		  }
	  }
  }
  
  public static Boolean blockContainsProfile(Block block, NodeProfile profile) {
	  Boolean result = false;
	  for (Node node: block.getNodes()) {
		  if (profile == internNodeProfile(node)) {
			  result = true;
		  }
	  }
	  return result;
  }
  
  public static ArrayList<Block> blocksMatching(NodeProfile profile) {
	  ArrayList<Block> result = new ArrayList<Block>();
	  for (Integer i: indexToNodesMap.keySet()) {
		  Block block = getBlock(i);
		  if (blockContainsProfile(block,profile)) {
			  result.add(block);
		  }
	  }
	  return result;
  }

  /**
   * Clear all previous configurations from memory.
   */
  private static void clearEverything() {
	  
    chosenCPSET.clear();
    chosenNOISET.clear();
    initialNodeProfile = null;
    initialBlock = null;
    observables.clear();
    actions.clear();
    
    nodeProfiles.clear();

    indexToNodesMap.clear();
    compToBehaviourMap.clear();
    
    allTestCases.clear();
    testPath.clear();
    
    // Clear the test planner tab
    populateTPTab(new ArrayList<ArrayList<Integer>>());
    
    // Clear add-ons
    Launcher.clear();
    
    updateDisplay();
    
    btFilePath = "";
  }

  private static void loadBTModel(File f) {
    System.out.println("FILE: " + f.getAbsolutePath());
    System.out.println("loading model");

    // load bt-file into BTAnalyser
    String result;
    if (btFileType == 0) {
    	System.out.println("Processing BT file");
    	result = sbcl.sendCommand("(process-bt-file \"" + (f.getPath()).replace("\\", "/") + "\")");
    } else {
    	System.out.println("Processing BESE file");
    	result = sbcl.sendCommand("(process-bese-file \"" + (f.getPath()).replace("\\", "/") + "\")");
    }
    if (!result.contains("</result>")) {
    	printErrorMessage("error|6|");
    	return;
    }
    
    // Now BT Analyser has the BT file processed
    // (i.e., if a previous BT file was processed, it has now been replaced)
	clearEverything();
    btFilePath = f.getAbsolutePath();
	
    // important to ensure resulting TCPs are reachable/valid
    System.out.println("Ensure TCPs are reachable");
    result = sbcl.sendCommand("(reachable-states)");
    if (!result.contains("</result>")) {
    	printErrorMessage("error|6|");
    	return;
    }

    System.out.println("Building BT");
    btXML = sbcl.sendCommand("(print-bt)");
    if (!btXML.contains("</result>")) {
    	printErrorMessage("error|6|");
    	return;
    }
    
    if (!btXML.equals("<result><error>No Behavior Tree loaded.</error></result>")) {
      readBTXML(btXML);
    } else {
      printErrorMessage("error|6|");
      return;
    }

    populateData();

    isLoaded = true;
    frame.setTitle("BT-TCG - " + f.getName());
  }

  private static void loadTCC(File f) {
    clearEverything();
    String file = null;
    try {
      file = new String(Files.readAllBytes(Paths.get(f.getPath())));
    } catch (IOException e) {
      e.printStackTrace();
    }
    if (file != null) {
      readConfig(file);
    }
  }

  private static void readBTXML(String btXML) {
    BTModelReader modelReader = new BTModelReader(btXML);
    
    indexToNodesMap = modelReader.getIndexToNodeMap();
    
    internNodeProfiles();
    
  }

  private static void processOldStyleConfig (Element config) {

	  // process NOIS
	  List<Element> nodesOfInterest = config.getChild("NOI").getChildren();
	  for (Element noi : nodesOfInterest) {
		  if (checkElementsAttributes(noi, "noi", true)) {
	          Node xmlNode = new Node(noi.getAttributeValue("tag"),
	              noi.getAttributeValue("component"), noi.getAttributeValue("behaviour-type"),
	              noi.getAttributeValue("behaviour"), noi.getAttributeValue("flag"), null, "");
	          NodeProfile prof = internNodeProfile(xmlNode);
	          if (!isChosenNOI(prof)) {
	        	  chosenNOISET.add(prof);
	          }
		  }
	  }
	  
	  // process CPs
	  List<Element> checkpoints = config.getChild("CP").getChildren("node");
	  for (Element cp : checkpoints) {
	      if (checkElementsAttributes(cp, "cp", true)) {
	          Node xmlNode = new Node(cp.getAttributeValue("tag"),
	        		  cp.getAttributeValue("component"), cp.getAttributeValue("behaviour-type"),
		              cp.getAttributeValue("behaviour"), cp.getAttributeValue("flag"), null, "");
	          NodeProfile prof = internNodeProfile(xmlNode);
	          if (!isChosenCP(prof)) {
	        	  chosenCPSET.add(prof);
	          }
	      }
	  }
	  
	  // process Initial CP
	  Element initialCp = config.getChild("CP").getChild("initial");
	  if (initialCp != null) {
	      if (checkElementsAttributes(initialCp, "initial", true)) {
	        Node xmlNode = new Node(initialCp.getAttributeValue("tag"),
	            initialCp.getAttributeValue("component"), initialCp.getAttributeValue("behaviour-type"),
	            initialCp.getAttributeValue("behaviour"), initialCp.getAttributeValue("flag"), null, "");
	        initialNodeProfile = internNodeProfile(xmlNode);
	        //updateCPInitialCombo();
	        System.out.println("initial state: " + initialNodeProfile.toString());
	      } else {
	      	System.out.println("No initial state!!!");
	      }
	    }

	  // process Observable Responses
	  List<Element> observables = config.getChild("OR").getChildren();
	  for (Element or : observables) {
		  if (checkElementsAttributes(or, "or", true)) {
			  Node xmlNode = new Node(or.getAttributeValue("tag"),
	                or.getAttributeValue("component"), or.getAttributeValue("behaviour-type"),
	                or.getAttributeValue("behaviour"), or.getAttributeValue("flag"), null, "");
			  NodeProfile prof = internNodeProfile(xmlNode);
			  setObservable(prof, or.getAttributeValue("observation"));
	          }
	        }
	  
	  // process User Actions
	  List<Element> actions = config.getChild("UA").getChildren();
	  for (Element ua : actions) {
	      if (checkElementsAttributes(ua, "ua", true)) {
	    	  Node xmlNode = new Node(ua.getAttributeValue("tag"),
	                ua.getAttributeValue("component"), ua.getAttributeValue("behaviour-type"),
	                ua.getAttributeValue("behaviour"), ua.getAttributeValue("flag"), null, "");
	    	  NodeProfile prof = internNodeProfile(xmlNode);
	    	  Boolean preamble = false;
	    	  if (ua.getAttributeValue("preamble") == "true") {
	    		  preamble = true;
	    		  }
	    	  setAction(prof, ua.getAttributeValue("action"), preamble);
	      }
	  }
	  }

  private static void processNewStyleConfig (Element config) {

	  // process NOIS
	  List<Element> nodesOfInterest = config.getChild("NOI").getChildren();
	  for (Element noi : nodesOfInterest) {
		  if (checkElementsAttributes(noi, "noi", false)) {
			  NodeProfile prof = internProfile(new NodeProfile(noi.getAttributeValue("component"), noi.getAttributeValue("behaviour-type"),
	              noi.getAttributeValue("behaviour"), Boolean.valueOf(noi.getAttributeValue("kill-flag"))));
			  if (!isChosenNOI(prof)) {
				  chosenNOISET.add(prof);
				  }
		  }
	  }
	  
	  // process CPs
	  List<Element> checkpoints = config.getChild("CP").getChildren("node-profile");
	  for (Element cp : checkpoints) {
		  if (checkElementsAttributes(cp, "cp", false)) {
			  NodeProfile prof = internProfile(new NodeProfile(cp.getAttributeValue("component"), cp.getAttributeValue("behaviour-type"),
	              cp.getAttributeValue("behaviour"), Boolean.valueOf(cp.getAttributeValue("kill-flag"))));
			  if (!isChosenCP(prof)) {
				  chosenCPSET.add(prof);
			  }
		  }
	  }
	  
	  // process Initial CP
	  Element init = config.getChild("CP").getChild("initial");
	  if (init != null) {
		  if (checkElementsAttributes(init, "initial" , false)) {
			  NodeProfile prof = internProfile(new NodeProfile(init.getAttributeValue("component"), init.getAttributeValue("behaviour-type"),
	              init.getAttributeValue("behaviour"), Boolean.valueOf(init.getAttributeValue("kill-flag"))));
			  initialNodeProfile = prof;
			  System.out.println("initial state: " + initialNodeProfile.toString());
			  } else {
				  System.out.println("No initial state!!!");
				  }
	  }

	  // process Observable Responses
	  List<Element> observables = config.getChild("OR").getChildren();
	  for (Element or : observables) {
		  if (checkElementsAttributes(or, "or", false)) {
			  NodeProfile prof = internProfile(new NodeProfile(or.getAttributeValue("component"), or.getAttributeValue("behaviour-type"),
	              or.getAttributeValue("behaviour"), Boolean.valueOf(or.getAttributeValue("kill-flag"))));
			  setObservable(prof, or.getAttributeValue("observation"));
			  }
	  }
	  
	  // process User Actions
	  List<Element> actions = config.getChild("UA").getChildren();
	  for (Element ua : actions) {
		  if (checkElementsAttributes(ua, "ua", false)) {
			  NodeProfile prof = internProfile(new NodeProfile(ua.getAttributeValue("component"), ua.getAttributeValue("behaviour-type"),
	              ua.getAttributeValue("behaviour"), Boolean.valueOf(ua.getAttributeValue("kill-flag"))));
			  Boolean preamble = false;
			  if (ua.getAttributeValue("preamble").equals("true")) {
				  preamble = true;
			  }

			  setAction(prof, ua.getAttributeValue("action"), preamble);
			  }
		  }
	  }
  
  private static void readConfig(String configXML) {
    org.jdom2.input.SAXBuilder saxBuilder = new SAXBuilder();

    org.jdom2.Document doc = null;
    try {
      doc = saxBuilder.build(new StringReader(configXML));
    } catch (JDOMException | IOException e) {
      e.printStackTrace();
    }

    Element config = doc.getRootElement();
    
    btFilePath = config.getAttributeValue("filepath");
    File f = new File(btFilePath);
    if (btFilePath.endsWith("xml")) {
    	btFileType = 1;
    } else {
    	btFileType = 0;
    }
    loadBTModel(f);
    
    if (config.getAttributeValue("version").equals(Constants.oldConfigVersion)) {
    	processOldStyleConfig (config);
    } else {
    	processNewStyleConfig (config);
    }

    updateDisplay();
    
  }

  private static boolean checkElementsAttributes(Element element, String elementType, Boolean oldStyle) {
    boolean valid = element.hasAttributes();
    switch (elementType) {
      case "noi":
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("tag") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("component") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour-type") != null);
        }
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("flag") != null);
        }
        if (valid && !oldStyle) {
        	valid = (element.getAttributeValue("kill-flag") != null);
        }
        return valid;
      case "cp":
        if (valid) {
          valid = (element.getAttributeValue("component") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour") != null);
        }
        if (valid && !oldStyle) {
        	valid = (element.getAttributeValue("behaviour-type") != null);
        }
        if (valid && !oldStyle) {
        	valid = (element.getAttributeValue("kill-flag") != null);
        }
        return valid;
      case "initial":
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("tag") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("component") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour-type") != null);
        }
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("flag") != null);
        }
        if (valid && !oldStyle) {
        	valid = (element.getAttributeValue("kill-flag") != null);
        }
        return valid;
      case "or":
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("tag") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("component") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour-type") != null);
        }
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("flag") != null);
        }
        if (valid && !oldStyle) {
        	valid = (element.getAttributeValue("kill-flag") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("observation") != null);
        }
        return valid;
      case "ua":
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("tag") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("component") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("behaviour-type") != null);
        }
        if (valid && oldStyle) {
          valid = (element.getAttributeValue("flag") != null);
        }
        if (valid && !oldStyle) {
        	valid = (element.getAttributeValue("kill-flag") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("action") != null);
        }
        if (valid) {
          valid = (element.getAttributeValue("preamble") != null);
        }
        return valid;
      default:
        return false;
    }
  }

  /**
   * Populate configuration in BT-TCG
   */
  private static void populateData() {

    populateNodeMaps();

    updateDisplay();

    btnGenerateTestCases.setEnabled(true);
    tabbedPane.setEnabledAt(tabbedPane.indexOfTab(Constants.configTabName), true);
    
  }

  private static void populateNodeMaps() {
    List<Node> nodes = new ArrayList<Node>();
    for (int key : indexToNodesMap.keySet()) {
      for (Node node : indexToNodesMap.get(key).getNodes()) {
        if (Constants.acceptedUABehaviourTypes.contains(node.getBehaviourType())) {
          actions.put(node.getNodeProfile(), new String[] {"", ""});
        }
        if (Constants.acceptedORBehaviourTypes.contains(node.getBehaviourType())) {
          observables.put(node.getNodeProfile(), "");
        }
        if (!compToBehaviourMap.containsKey(node.getComponent())) {
          compToBehaviourMap.put(node.getComponent(), new ArrayList<Node>());
        }
        compToBehaviourMap.get(node.getComponent()).add(node);
        nodes.add(node);
      }
    }

    populateTableCPs();
    populateTableActions();
    populateTableObservables();
    tableNOIs.setModel(new NodeProfileTableModel(nodeProfiles));
  }

  // Also need to update initial state/node? (modify/reset the combo box entries) *****
  
  private static void populateTableCPs() {
    ArrayList<NodeProfile> profiles = new ArrayList<NodeProfile>();
    for (NodeProfile prof: nodeProfiles) {
    	if (prof.getBehaviourType().equals("STATE-REALISATION")) {
    		profiles.add(prof);
    	}
    }
    tableCPs.setModel(new NodeProfileTableModel(profiles));
    }

  private static void updateDisplay() {
	  // update other stuff in configuration tab as well?
	  updateCPInitialCombo();
	  updateActionTab();
	  updateObservableTab();
	  revalidateNow();
  }

  private static void updateCPInitialCombo () {
	  TreeSet<NodeProfile>potentialInitialCPs = new TreeSet<NodeProfile>();
	    for (NodeProfile nodeProfile : chosenCPSET) {
	        potentialInitialCPs.add(nodeProfile);
	      }
	      comboInitial
	          .setModel(new DefaultComboBoxModel<NodeProfile>(potentialInitialCPs.toArray(new NodeProfile[] {})));
	      comboInitial.setSelectedIndex(-1);
	      Boolean found = false;
	      if (initialNodeProfile != null) {
	      	for (int i = 0; i < comboInitial.getItemCount(); i++) {
	      		if (initialNodeProfile.equals(comboInitial.getItemAt(i))) {
	      			comboInitial.setSelectedItem(comboInitial.getItemAt(i));
	      			initialNodeProfile = comboInitial.getItemAt(i);
	      			found = true;
	      			break;
	      		}
	      		}
	      	if (found == false) {
	      		// initial was deleted
	      		if (0 < comboInitial.getItemCount()) {
	      			initialNodeProfile = comboInitial.getItemAt(0);
	      			comboInitial.setSelectedIndex(0);
	      		} else {
	      			initialNodeProfile = null;
	      		}
	      		}
	      	} else {
	      		// initial was not set
	      		if (0 < comboInitial.getItemCount()) {
	      			initialNodeProfile = comboInitial.getItemAt(0);
	      			comboInitial.setSelectedIndex(0);
	      			}
	      		}
	      updateCPInitialBlockCombo();
	    }
	
  private static void updateCPInitialBlockCombo () {
	  ArrayList<Block> potentialInitialBlocks = new  ArrayList<Block>();
	  if (initialNodeProfile != null) {
		  for (Block block: blocksMatching(initialNodeProfile)) {
			  if (!block.containsReversion() && !block.containsReference()) {
				  potentialInitialBlocks.add(block);
			  }
		  }
	  }
	  Block[] arr = new Block[potentialInitialBlocks.size()];
	  arr = (Block[])potentialInitialBlocks.toArray(arr);
	  comboInitialNode.setModel(new DefaultComboBoxModel<Block>(arr));
	  if (initialNodeProfile == null) {
		  initialBlock = null;
	  } else if ((initialBlock != null) && blockContainsProfile(initialBlock,initialNodeProfile)) {
		  comboInitialNode.setSelectedItem(initialBlock);
	  } else {
		  for (Integer i = 0; i < comboInitialNode.getItemCount(); i++) {
			  Block block = comboInitialNode.getItemAt(i);
			  if (blockContainsProfile(block,initialNodeProfile)) {
				  initialBlock = block;
				  comboInitialNode.setSelectedItem(block);
				  break;
			  }
		  }
	  }
  }
  
  private static void updateObservableTab() {
	  populateTableObservables();
	  updateObservableInput();
  }
  
  private static void populateTableObservables() {
	  Set<NodeProfile> potentialObservables = new TreeSet<NodeProfile>();
	  for (NodeProfile prof: nodeProfiles) {
		  if (Constants.acceptedORBehaviourTypes.contains(prof.getBehaviourType())) {
			  potentialObservables.add(prof);
		  }
	  }
	  tableObservables.setModel(new NodeProfileTableModel(potentialObservables));
  }

  private static void updateObservableInput() {
	  if (tableObservables.getSelectedRow() != -1) {
		  NodeProfile selectedProf = (NodeProfile) tableObservables.getValueAt(tableObservables.getSelectedRow(),0);
		  if (!getObservable(selectedProf).equals("")) {
			  textareaObservable.setText(getObservable(selectedProf));
		  } else {
			  textareaObservable.setText("");
		  }
	  } else {
		  textareaObservable.setText("");
	  }
  }

  private static void updateActionTab() {
	  populateTableActions();
	  updateActionInput();
  }

  private static void populateTableActions() {
	  Set<NodeProfile> potentialActions = new TreeSet<NodeProfile>();
	  for (NodeProfile prof: nodeProfiles) {
		  if (Constants.acceptedUABehaviourTypes.contains(prof.getBehaviourType())) {
			  potentialActions.add(prof);
		  }
	  }
	  tableActions.setModel(new NodeProfileTableModel(potentialActions));
  }

  private static void updateActionCheckbox() {
	  if (textareaAction.getText().equals("")) {
		  checkboxAction.setEnabled(false);
		  checkboxAction.setSelected(false);
	  } else {
		  checkboxAction.setEnabled(true);
	  }
  }
  
  /**
   * Update Actions text area and checkbox GUI components from data structures
   */
  
  private static void updateActionInput() {
	  if (tableActions.getSelectedRow() != -1) {
		  NodeProfile selectedProf = (NodeProfile) tableActions.getValueAt(tableActions.getSelectedRow(),0);
		  if (!getAction(selectedProf).equals("")) {
			  textareaAction.setText(getAction(selectedProf));
			  checkboxAction.setSelected(getPrepAsBoolean(selectedProf));
		  } else {
			  textareaAction.setText("");
		  }
	  } else {
		  textareaAction.setText("");
	  }
	  updateActionCheckbox();
  }

  private static Boolean selectedTestCase(TestCase tc) {
	  Boolean result = false;
	  for (TestSegment segment: testPath) {
		  if (segment.getTestCase().equals(tc)) {
			  result = true;
			  break;
		  }
	  }
	  return result;
  }
  
  private static void updateTblTCs() {
	  Block realCurrentBlock;
	  Block currentBlock;
	  if (testPath.size() == 0) {
//		  realCurrentBlock = initialBlock;
//		  currentBlock = initialBlock;
		  realCurrentBlock = getBlock(0);
		  currentBlock = realCurrentBlock;
	  } else {
		  realCurrentBlock = getBlock(testPath.get(testPath.size()-1).getTestCase().getEnd());
		  currentBlock = equivalentStartingBlock(realCurrentBlock);
	  }
	  for (TestCase tc: allTestCases) {
		  tc.setSelected(selectedTestCase(tc));
	  }
	  for (TestCase tc: allTestCases) {
		  Block block = getBlock(tc.getStart());
		  if (block.equals(currentBlock)) {
			  if (testPath.size() == 0) {
				  tc.setReachable(true);
				  tc.setPreamble(false);
			  } else {
				  ArrayList<Integer> prefix = findPrefix();
				  ArrayList<Integer> tcSteps = tc.getSteps();
				  if (pathIsFeasible(mergePaths(prefix, tcSteps))) {
					  tc.setReachable(true);
					  tc.setPreamble(false);
				  } else {
					  ArrayList<Integer> gap = getGap(findPrefix(), tc.getSteps());
					  if (gap != null) {
						  if (gap.size() == 0) {
							  tc.setReachable(true);
							  tc.setPreamble(false);
						  } else {
							  tc.setReachable(true);
							  tc.setPreamble(true);
						  }
					  } else {
						  tc.setReachable(false);
						  tc.setPreamble(true);
					  }
				  }
			  }
		  } else {
			  ArrayList<Integer> gap = getGap(findPrefix(), tc.getSteps());
			  if (gap == null) {
				  // needs further check if tc starts at a reversion target
				  if (block.containsReversion() || block.containsReference()) {
					  tc.setReachable(false);
					  tc.setPreamble(true);
				  } else {
					  System.out.println("CHECKING FOR REVERSION/REFERENCE TARGET");
					  tc.setReachable(false);
					  tc.setPreamble(true);
					  ArrayList<Integer> tcSteps = tc.getSteps();
					  for (Integer i = 0; i < numberOfBlocks(); i++) {
						  Block b = getBlock(i);
						  if ((b.containsReversion() || b.containsReference()) && equivalentStartingBlock(b).equals(block)) {
							  ArrayList<Integer> steps = new ArrayList<Integer>();
							  steps.add(i);
							  for (Integer j = 1; j < tcSteps.size(); j++) {
								  steps.add(getBlock(tcSteps.get(j)).getIndex());
							  }
							  gap = getGap(findPrefix(), steps);
							  if (gap != null) {
								  tc.setReachable(true);
								  tc.setPreamble(true);
								  System.out.println("SUCCESS");
								  break;
							  }
						  }
					  }
				  }
			  } else {
				  tc.setReachable(true);
				  tc.setPreamble(true);
			  }
		  }
	  }
	  tblTCs.revalidate();
	  tblTCs.repaint();
  }
  
  private static void updateTblTP() {
    ((TestPathModel) tblTP.getModel()).addData(testPath);
    tblTP.revalidate();
    tblTP.repaint();
    Integer i = allTestCases.size();
    Integer j = testPath.size();
    lblCurrentTestPath.setText("Current Test Path (" + j + " of " + i + " cases):");
    lblCurrentTestPath.repaint();
  }

  private static void populateTPTab(ArrayList<ArrayList<Integer>> rawTestCases) {
    allTestCases = new ArrayList<TestCase>();
    testPath = new ArrayList<TestSegment>();
    for (int i = 0; i < rawTestCases.size(); i++) {
      ArrayList<Integer> testCase = rawTestCases.get(i);
      TestCase tc = constructTestCase(testCase);
      tc.setIndex(i+1);
      allTestCases.add(tc);
      ArrayList<Integer> blocks = getPreamble(tc.getSteps());
      tc.setStepsBefore(blocks, getNodeList(blocks));
      if (!(tc.getStartNode().getBlockIndex() == initialBlock.getIndex())) {
    	  tc.setPreamble(true);
        } else {
        	tc.setPreamble(false);
        }
      blocks = getPostamble(tc.getSteps());
      tc.setStepsAfter(blocks, getNodeList(blocks));
      tc.setLooping(testCaseLoops(tc));
    }
    System.out.println("TEST CASES: " + allTestCases.size());
    tblTCs.setModel(new TestCasesModel(allTestCases));
    updateTblTCs();
    updateTblTP();
  }

  /**
   * Open folder containing test cases.
   */
  private static void openFolder() {
    String path =
        System.getProperty("user.dir") + System.getProperty("file.separator") + "test-cases";
    Path thePath = Paths.get(path);
    if (Files.exists(thePath)) {
      try {
        Desktop.getDesktop().open(new File(path));
      } catch (IOException e) {
        printErrorMessage("error|8");
        e.printStackTrace();
      }
    } else {
      boolean success = (new File(path)).mkdirs();
      if (!success) {
        // Directory creation failed
        printErrorMessage("error|7");
      } else {
        try {
          Desktop.getDesktop().open(new File(System.getProperty("user.dir")
              + System.getProperty("file.separator") + "test-cases"));
        } catch (IOException e1) {
          printErrorMessage("error|8");
          e1.printStackTrace();
        }
      }
    }
  }

  /**
   * Populate test case configuration in TCC file format.
   */

  private static void SaveConfig() {
    JFrame parentFrame = new JFrame();

    JFileChooser fileChooser = new JFileChooser() {
      private static final long serialVersionUID = 1L;

      @Override
      public void approveSelection() {
        File f = getSelectedFile();
        if (f.exists()) {
          int result = JOptionPane.showConfirmDialog(this, "The file exists, overwrite?",
              "Existing file", JOptionPane.YES_NO_CANCEL_OPTION);
          switch (result) {
            case JOptionPane.YES_OPTION:
              super.approveSelection();
              return;
            case JOptionPane.NO_OPTION:
              return;
            case JOptionPane.CLOSED_OPTION:
              return;
            case JOptionPane.CANCEL_OPTION:
              cancelSelection();
              return;
          }
        }
        super.approveSelection();
      }
    };

    javax.swing.filechooser.FileFilter filter =
        new FileNameExtensionFilter("Test Case Config file (.tcc.xml)", new String[] {"xml"});
    fileChooser.addChoosableFileFilter(filter);
    fileChooser.setFileFilter(filter);

    fileChooser.setDialogTitle("Specify a file to save");

    int userSelection = fileChooser.showSaveDialog(parentFrame);

    if (userSelection == JFileChooser.APPROVE_OPTION) {

      File fileToSave = fileChooser.getSelectedFile();
      String filename = fileToSave.getAbsolutePath();
      if (fileToSave.getAbsolutePath().endsWith(".tcc.xml")) {
        filename = filename.substring(0, filename.length() - 8);
      }
      System.out.println("Save as file: " + filename + ".tcc.xml");
      XMLOutputter xmlOut = new XMLOutputter(Format.getPrettyFormat());
      FileOutputStream fos;
      try {
        fos = new FileOutputStream(filename + ".tcc.xml");
        fos.write(xmlOut.outputString(constructTCC()).getBytes());
        fos.close();
      } catch (Exception e) {

      }
    }
  }

  private static Document constructTCC() {
    Document doc = new Document();
    Element config = new Element("config");
    config.setAttribute(new Attribute("version", Constants.version));
    config.setAttribute(new Attribute("filepath", btFilePath));
    Element noi = new Element("NOI");
    for (NodeProfile nodeProfile : chosenNOISET) {
      Element nodeToAdd = new Element("node-profile");
      nodeToAdd.setAttribute("component", nodeProfile.getComponent());
      nodeToAdd.setAttribute("behaviour", nodeProfile.getBehaviour());
      nodeToAdd.setAttribute("behaviour-type", nodeProfile.getBehaviourType());
      nodeToAdd.setAttribute("kill-flag", nodeProfile.getKillFlag().toString());
      noi.addContent(nodeToAdd);
    }
    Element cp = new Element("CP");
    for (NodeProfile nodeProfile : chosenCPSET) {
      Element nodeToAdd = new Element("node-profile");
      nodeToAdd.setAttribute("component", nodeProfile.getComponent());
      nodeToAdd.setAttribute("behaviour", nodeProfile.getBehaviour());
      nodeToAdd.setAttribute("behaviour-type", nodeProfile.getBehaviourType());
      nodeToAdd.setAttribute("kill-flag", nodeProfile.getKillFlag().toString());
      cp.addContent(nodeToAdd);
    }
    if (initialNodeProfile != null) {
      Element initialCP = new Element("initial");
      initialCP.setAttribute("component", initialNodeProfile.getComponent());
      initialCP.setAttribute("behaviour", initialNodeProfile.getBehaviour());
      initialCP.setAttribute("behaviour-type", initialNodeProfile.getBehaviourType());
      initialCP.setAttribute("kill-flag", initialNodeProfile.getKillFlag().toString());
      cp.addContent(initialCP);
    }

    Element or = new Element("OR");
    for (Entry<NodeProfile, String> observation : observables.entrySet()) {
      if (!observation.getValue().equals("")) {
        NodeProfile nodeProfile = observation.getKey();
        Element nodeToAdd = new Element("node-profile");
        nodeToAdd.setAttribute("component", nodeProfile.getComponent());
        nodeToAdd.setAttribute("behaviour", nodeProfile.getBehaviour());
        nodeToAdd.setAttribute("behaviour-type", nodeProfile.getBehaviourType());
        nodeToAdd.setAttribute("kill-flag", nodeProfile.getKillFlag().toString());
        nodeToAdd.setAttribute("observation", observation.getValue());
        or.addContent(nodeToAdd);
      }
    }

    Element ua = new Element("UA");
    for (Entry<NodeProfile, String[]> action : actions.entrySet()) {
      if (!action.getValue()[0].equals("")) {
        NodeProfile nodeProfile = action.getKey();
        Element nodeToAdd = new Element("node-profile");
        nodeToAdd.setAttribute("component", nodeProfile.getComponent());
        nodeToAdd.setAttribute("behaviour", nodeProfile.getBehaviour());
        nodeToAdd.setAttribute("behaviour-type", nodeProfile.getBehaviourType());
        nodeToAdd.setAttribute("kill-flag", nodeProfile.getKillFlag().toString());
        nodeToAdd.setAttribute("action", action.getValue()[0]);
        nodeToAdd.setAttribute("preamble", action.getValue()[1]);
        ua.addContent(nodeToAdd);
      }
    }

    config.addContent(noi);
    config.addContent(cp);
    config.addContent(or);
    config.addContent(ua);
    doc.addContent(config);
    return doc;
  }

  private static void fillFrame() {

	    contentPane = new JPanel(new GridBagLayout());
	    
	    frame.setContentPane(contentPane);

	    JMenuBar menubar = createMenuBar();
	    frame.setJMenuBar(menubar);

	    createMainButtons();

	    tabbedPane = new JTabbedPane(JTabbedPane.TOP);
	    
	    JPanel configTab = createConfigTab();
	    tabbedPane.addTab("Test Configuration",null,configTab,null);
	    JPanel actionsTab = createActionsTab();
	    tabbedPane.addTab("Actions", null, actionsTab,null);
	    JPanel observablesTab = createObservablesTab();
	    tabbedPane.addTab("Observables", null, observablesTab, null);
	    JPanel testPlannerTab = createTPTab();
	    tabbedPane.addTab("Test Planner", null, testPlannerTab, null);
	    

	    GridBagConstraints paneConstraints = new GridBagConstraints();
	    paneConstraints.gridx = 0;
	    paneConstraints.gridy = 1;
	    paneConstraints.gridwidth = 4;
	    paneConstraints.gridheight = 2;
	    paneConstraints.weightx = 1.0;
	    paneConstraints.weighty = 1.0;
	    paneConstraints.fill = GridBagConstraints.BOTH;
	    
	    contentPane.add(tabbedPane, paneConstraints);

	    
//	    for (int i = 0; i < tabbedPane.getTabCount(); i++) {
//	      tabbedPane.setEnabledAt(i, false);
//	    }
  }

  private static JMenuBar createMenuBar() {
    final JMenuBar menuBar = new JMenuBar();
    
    final JMenu mnFile = new JMenu("File");
    menuBar.add(mnFile);
    mnFile.setMnemonic(KeyEvent.VK_F);

    /**
     * Load BT File.
     */
    final JMenuItem mntmLoad = new JMenuItem("Load BTModel or Test Case Config");
    mntmLoad.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
        JFileChooser chooser = new JFileChooser(
            System.getProperty("user.dir") + System.getProperty("file.separator") + "models");
        FileNameExtensionFilter filter =
            new FileNameExtensionFilter("BT Model", "bt", "btc", "xml");
        chooser.setFileFilter(filter);
        chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        chooser.setMultiSelectionEnabled(false);
        int retVal = chooser.showOpenDialog(frame);

        if (retVal == JFileChooser.APPROVE_OPTION) {
          File f = chooser.getSelectedFile();
          System.out.println("You chose " + f.getPath());
          btFilePath = (f.getPath()).replace("\\", "/");
          if (btFilePath.endsWith("tcc.xml")) {
            String result = connectToServer();
            if (!(result.equals("") || result.contains("error"))) {
              System.out.println("loading config");
              loadTCC(f);
            } else {
              printErrorMessage("error|4|");
            }
          } else if (btFilePath.endsWith("btc") || btFilePath.endsWith("bt")) {
            String result = connectToServer();
            if (!(result.equals("") || result.contains("error"))) {
              System.out.println("loading model");
              btFileType = 0;
              loadBTModel(f);
            } else {
              printErrorMessage("error|4|");
            }
          } else if (btFilePath.endsWith("xml")) {
        	  String result = connectToServer();
              if (!(result.equals("") || result.contains("error"))) {
                System.out.println("loading model");
                btFileType = 1;
                loadBTModel(f);
              } else {
                printErrorMessage("error|4|");
              }
          } else {
            printErrorMessage("error|9|");
          }
        } else if (retVal == JFileChooser.ERROR_OPTION) {
          printErrorMessage("error|6|");
        }
      }
    });
    mnFile.add(mntmLoad);


    final JMenuItem mntmSave = new JMenuItem("Save Test Case Config");
    mntmSave.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (isLoaded) {
          SaveConfig();
        } else {
          printErrorMessage("error|11|");
        }
      }
    });
    mnFile.add(mntmSave);

    // File menu add-ons
    final ArrayList<JComponent> menuItems = Launcher.fileMenuItems();
    for (int i = 0; i < menuItems.size(); i++) {
    	mnFile.add(menuItems.get(i));
    }

    final JMenuItem mntmExit = new JMenuItem("Exit");
    mntmExit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
        if (isLoaded == false) {
          System.exit(0);
        } else {
          int dialogButton = JOptionPane.YES_NO_OPTION;
          int dialogResult = JOptionPane.showConfirmDialog(null,
              "Would you like to save your configurations first?", "Warning", dialogButton);
          if (dialogResult == JOptionPane.YES_OPTION) {
            SaveConfig();
            System.exit(0);
          }
          System.exit(0);
        }
      }
    });
    mnFile.add(mntmExit);
    
    final JMenu mnHelp = new JMenu("Help");
    menuBar.add(mnHelp);

    final JMenuItem mntmTips = new JMenuItem("Tips");
    mntmTips.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
        JOptionPane.showMessageDialog(frame,
            "Instructions on generating test cases: \n\n1) File > Load BT > Choose BT file \n2) Fill in 'Nodes of Interest', 'CheckPoints' \n3) Click on Generate Test Paths \n4) Fill in Observable Responses and User Actions \n5) Click Generate Test Cases \n6) Save Config by File > Save Test Case Config\n\nDo note that you do not need to fill in 'Observable Responses' and 'User Actions' to generate test paths, but that is required for generating test cases.",
            "Tips", JOptionPane.INFORMATION_MESSAGE);
      }
    });
    mnHelp.add(mntmTips);

    final JMenuItem mntmAbout = new JMenuItem("About");
    mntmAbout.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
        JOptionPane.showMessageDialog(frame,
            "BT-TCG\r\nBy Mitchell Savell et al\r\nmitchellsavell@gmail.com\r\n\r\nBased on TCGen-UI\r\nBy Soh Wei Yu",
            "About", JOptionPane.PLAIN_MESSAGE);

      }
    });
    mnHelp.add(mntmAbout);
    return menuBar;
  }

  private static void createMainButtons() {
    /**
     * Call API and generate TCPs
     */
	  

	    GridBagConstraints btn1Constraints = new GridBagConstraints();
	    btn1Constraints.gridx = 0;
	    btn1Constraints.gridy = 0;
	    
	    
    btnGenerateTestCases = new JButton("Generate Test Cases");
    btnGenerateTestCases.setEnabled(false);
    btnGenerateTestCases.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
    	allTestCases.clear();
    	if ((initialNodeProfile != null)) {
          ArrayList<ArrayList<Integer>> testCases = generateTestCases();
          System.out.println("RAW TEST CASES: " + testCases.size());
          if (testCases.size() > 0) {
              populateTPTab(testCases);
        	  tabbedPane.setEnabledAt(tabbedPane.indexOfTab(Constants.testPlannerTabName), true);
          } else {
            JOptionPane.showMessageDialog(null, "No test cases generated.");
          }
        } else {
          JOptionPane.showMessageDialog(null,
              "Invalid config. Make sure you have selected some checkpoints and an initial state.");
        }
      }
    });
    contentPane.add(btnGenerateTestCases,btn1Constraints);


    GridBagConstraints btn2Constraints = new GridBagConstraints();
    btn2Constraints.gridx = 1;
    btn2Constraints.gridy = 0;
    
    
    btnSpare = new JButton("Spare Button (Does Nothing)");
    btnSpare.setEnabled(false);
    btnSpare.setEnabled(false);
    btnSpare.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
        // TODO Put stuff here
      }
    });
    contentPane.add(btnSpare,btn2Constraints);

    GridBagConstraints btn3Constraints = new GridBagConstraints();
    btn3Constraints.gridx = 2;
    btn3Constraints.gridy = 0;
    
    
    
    JButton btnOpenTestCase = new JButton("Open Test Case Folder");
    btnOpenTestCase.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent arg0) {
        openFolder();
      }
    });
    contentPane.add(btnOpenTestCase,btn3Constraints);
  }

  
  
  private static JPanel createConfigTab() {
	  JPanel panelConfig = new JPanel(new GridBagLayout());
	  
	  GridBagConstraints cpLabelConstraints = new GridBagConstraints();
	  cpLabelConstraints.gridx = 0;
	  cpLabelConstraints.gridy = 0;
	  cpLabelConstraints.anchor = GridBagConstraints.LINE_START;
	  
	  JLabel cpLabel = new JLabel("Check Points");
	  cpLabel.setFont(new Font("Tahoma", Font.PLAIN, 18));
	  panelConfig.add(cpLabel,cpLabelConstraints);

	  tableCPs = new JTable(new NodeProfileTableModel(null));

	  tableCPs.setDefaultRenderer(NodeProfile.class, new CPCell());
	  tableCPs.setDefaultEditor(NodeProfile.class, new CPCell());
	  tableCPs.setRowHeight(30);
	  tableCPs.setBorder(null);
	  tableCPs.setFillsViewportHeight(true);
	  tableCPs.setCellSelectionEnabled(true);
	  tableCPs.setTableHeader(null);

	  GridBagConstraints scrollPane1Constraints = new GridBagConstraints();
	  scrollPane1Constraints.gridx = 0;
	  scrollPane1Constraints.gridy = 1;
	  scrollPane1Constraints.gridwidth = 2;
	  scrollPane1Constraints.weightx = 1.0;
	  scrollPane1Constraints.weighty = 1.0;
	  scrollPane1Constraints.anchor = GridBagConstraints.PAGE_START;
	  scrollPane1Constraints.fill = GridBagConstraints.BOTH;
	  
	  JScrollPane scrollPane1 = new JScrollPane(tableCPs);
	  scrollPane1.setViewportBorder(null);
	  scrollPane1.setPreferredSize(new Dimension(380,380));
	  // scrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	  panelConfig.add(scrollPane1,scrollPane1Constraints);
	  
	  JPopupMenu cpPopupMenu = new JPopupMenu();
	  JMenuItem cpMenuItemAdd = new JMenuItem("Add");
	  cpMenuItemAdd.addActionListener(new java.awt.event.ActionListener(){
	  		  public void actionPerformed(java.awt.event.ActionEvent evt) {
	  			  for (Integer i : tableCPs.getSelectedRows()) {
	  				chosenCPSET.add((NodeProfile) tableCPs.getValueAt(i,0));
	  				}
	  			updateCPInitialCombo();
	  			tableCPs.repaint();
	  		  }
	  		  });
	  JMenuItem cpMenuItemRemove = new JMenuItem("Remove");
	  cpMenuItemRemove.addActionListener(new java.awt.event.ActionListener(){
  		  public void actionPerformed(java.awt.event.ActionEvent evt) {
  			  for (Integer i : tableCPs.getSelectedRows()) {
  				chosenCPSET.remove((NodeProfile) tableCPs.getValueAt(i,0));
  				}
	  			updateCPInitialCombo();
	  			tableCPs.repaint();
  		  }
  		  });
	  
	  cpPopupMenu.add(cpMenuItemAdd);
	  cpPopupMenu.add(cpMenuItemRemove);
	  tableCPs.setComponentPopupMenu(cpPopupMenu);

	  GridBagConstraints noiLabelConstraints = new GridBagConstraints();
	  noiLabelConstraints.gridx = 2;
	  noiLabelConstraints.gridy = 0;
	  noiLabelConstraints.anchor = GridBagConstraints.LINE_START;
	  
	  JLabel noiLabel = new JLabel("Nodes of Interest");
	  noiLabel.setFont(new Font("Tahoma", Font.PLAIN, 18));
	  panelConfig.add(noiLabel,noiLabelConstraints);

	  tableNOIs = new JTable(new NodeProfileTableModel(null));

	  tableNOIs.setDefaultRenderer(NodeProfile.class, new NOICell());
	  tableNOIs.setDefaultEditor(NodeProfile.class, new NOICell());
	  tableNOIs.setRowHeight(30);
	  tableNOIs.setBorder(null);
	  tableNOIs.setFillsViewportHeight(true);
	  tableNOIs.setCellSelectionEnabled(true);
	  tableNOIs.setTableHeader(null);

	  GridBagConstraints scrollPane2Constraints = new GridBagConstraints();
	  scrollPane2Constraints.gridx = 2;
	  scrollPane2Constraints.gridy = 1;
	  scrollPane2Constraints.gridwidth = 2;
	  scrollPane2Constraints.weightx = 1.0;
	  scrollPane2Constraints.weighty = 1.0;
	  scrollPane2Constraints.anchor = GridBagConstraints.PAGE_START;
	  scrollPane2Constraints.fill = GridBagConstraints.BOTH;
	  
	  JScrollPane scrollPane2 = new JScrollPane(tableNOIs);
	  scrollPane2.setViewportBorder(null);
	  scrollPane2.setPreferredSize(new Dimension(380,380));
	  //scrollPane2.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	  panelConfig.add(scrollPane2,scrollPane2Constraints);

	  JPopupMenu noiPopupMenu = new JPopupMenu();
	  JMenuItem noiMenuItemAdd = new JMenuItem("Add");
	  noiMenuItemAdd.addActionListener(new java.awt.event.ActionListener(){
	  		  public void actionPerformed(java.awt.event.ActionEvent evt) {
	  			  for (Integer i : tableNOIs.getSelectedRows()) {
	  				chosenNOISET.add((NodeProfile) tableNOIs.getValueAt(i,0));
	  				}
	  			tableNOIs.repaint();
	  		  }
	  		  });
	  JMenuItem noiMenuItemRemove = new JMenuItem("Remove");
	  noiMenuItemRemove.addActionListener(new java.awt.event.ActionListener(){
  		  public void actionPerformed(java.awt.event.ActionEvent evt) {
  			  for (Integer i : tableNOIs.getSelectedRows()) {
  				chosenNOISET.remove((NodeProfile) tableNOIs.getValueAt(i,0));
  				}
	  			tableNOIs.repaint();
  		  }
  		  });
	  
	  noiPopupMenu.add(noiMenuItemAdd);
	  noiPopupMenu.add(noiMenuItemRemove);
	  tableNOIs.setComponentPopupMenu(noiPopupMenu);


	  /* Select Initial CheckPoint Label */
	  GridBagConstraints labelInitConstraints = new GridBagConstraints();
	  labelInitConstraints.gridx = 0;
	  labelInitConstraints.gridy = 2;
	  labelInitConstraints.anchor = GridBagConstraints.LINE_START;
	  JLabel lblInitialState = new JLabel("Initial State:");
	  lblInitialState.setHorizontalAlignment(SwingConstants.RIGHT);
	  panelConfig.add(lblInitialState,labelInitConstraints);
	    
	  /* Initial Checkpoint ComboBox */
	  GridBagConstraints comboInitConstraints = new GridBagConstraints();
	  comboInitConstraints.gridx = 1;
	  comboInitConstraints.gridy = 2;
	  comboInitConstraints.gridwidth = 3;
	  comboInitConstraints.anchor = GridBagConstraints.LINE_START;
	  comboInitial.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent arg0) {
			  if (comboInitial.getSelectedItem() != null) {
				  initialNodeProfile = (NodeProfile) comboInitial.getSelectedItem();
				  // update the other combo box
				  updateCPInitialBlockCombo();
				  }
			  }
		  });
	  panelConfig.add(comboInitial,comboInitConstraints);

	  /* Select Initial CheckPoint BT Label */
	  GridBagConstraints labelInitNodeConstraints = new GridBagConstraints();
	  labelInitNodeConstraints.gridx = 0;
	  labelInitNodeConstraints.gridy = 3;
	  labelInitNodeConstraints.anchor = GridBagConstraints.LINE_START;
	  JLabel lblInitialNode = new JLabel("Initial BT Node:");
	  lblInitialNode.setHorizontalAlignment(SwingConstants.RIGHT);
	  panelConfig.add(lblInitialNode,labelInitNodeConstraints);
		    
	  /* Initial Checkpoint BT ComboBox */
	  GridBagConstraints comboInitNodeConstraints = new GridBagConstraints();
	  comboInitNodeConstraints.gridx = 1;
	  comboInitNodeConstraints.gridy = 3;
	  comboInitNodeConstraints.gridwidth = 3;
	  comboInitNodeConstraints.anchor = GridBagConstraints.LINE_START;
	  comboInitialNode.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent arg0) {
				  if (comboInitialNode.getSelectedItem() != null) {
					  initialBlock = (Block) comboInitialNode.getSelectedItem();
					  } else {
						  updateCPInitialBlockCombo();
					  }
				  }
			  });
	  panelConfig.add(comboInitialNode,comboInitNodeConstraints);
		  
	  return panelConfig;
  }
  
  private static JPanel createActionsTab () {
	  JPanel panelActions = new JPanel(new GridBagLayout());

	  tableActions = new JTable(new NodeProfileTableModel(null));

	  tableActions.setDefaultRenderer(NodeProfile.class, new UACell());
	  tableActions.setDefaultEditor(NodeProfile.class, new UACell());
	  tableActions.setRowHeight(30);
	  tableActions.setBorder(null);
	  tableActions.setFillsViewportHeight(true);
	  tableActions.setCellSelectionEnabled(true);
	  tableActions.setTableHeader(null);
	  tableActions.setTableHeader(null);
	  tableActions.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	  tableActions.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
	      @Override
	      public void valueChanged(ListSelectionEvent arg0) {
	        updateActionInput();
	      }
	    });
	  tableActions.setFillsViewportHeight(true);
	  tableActions.setCellSelectionEnabled(true);
	  tableActions.setBorder(null);

	  textareaAction.setWrapStyleWord(true);
	  textareaAction.setLineWrap(true);
	  textareaAction.addKeyListener(new KeyAdapter() {
	      @Override
	      public void keyReleased(KeyEvent e) {
	        //updateActionCheckbox();
	        for (Integer i : tableActions.getSelectedRows()) {
	        	NodeProfile prof = (NodeProfile) tableActions.getValueAt(i,0);
	        	setAction(prof,textareaAction.getText(),checkboxAction.isSelected());
	        	tableActions.repaint();
	        }
	        updateActionCheckbox();
	      }
	    });
	  
	  GridBagConstraints scrollPane1Constraints = new GridBagConstraints();
	  scrollPane1Constraints.gridx = 0;
	  scrollPane1Constraints.gridy = 0;
	  scrollPane1Constraints.gridheight = 2;
	  scrollPane1Constraints.weightx = 1.0;
	  scrollPane1Constraints.weighty = 1.0;
	  scrollPane1Constraints.anchor = GridBagConstraints.PAGE_START;
	  scrollPane1Constraints.fill = GridBagConstraints.BOTH;
	  
	  JScrollPane scrollPane1 = new JScrollPane(tableActions);
	  scrollPane1.setViewportBorder(null);
	  scrollPane1.setPreferredSize(new Dimension(380,380));
	  // scrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	  panelActions.add(scrollPane1,scrollPane1Constraints);
	  
	  GridBagConstraints checkboxConstraints = new GridBagConstraints();
	  checkboxConstraints.gridx = 1;
	  checkboxConstraints.gridy = 0;
	  checkboxConstraints.anchor = GridBagConstraints.LINE_START;

	  /* Action Checkbox */
	  checkboxAction.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent arg0) {
			  for (int i : tableActions.getSelectedRows()) {
				  setAction((NodeProfile)tableActions.getValueAt(i, 0),textareaAction.getText(),checkboxAction.isSelected());
			  }
			  tableActions.repaint();
			  updateActionCheckbox();
			  }
		  });
	  panelActions.add(checkboxAction,checkboxConstraints);

	  GridBagConstraints scrollPane2Constraints = new GridBagConstraints();
	  scrollPane2Constraints.gridx = 1;
	  scrollPane2Constraints.gridy = 1;
	  scrollPane2Constraints.weightx = 1.0;
	  scrollPane2Constraints.weighty = 1.0;
	  scrollPane2Constraints.anchor = GridBagConstraints.PAGE_START;
	  scrollPane2Constraints.fill = GridBagConstraints.BOTH;
	  
	  JScrollPane scrollPane2 = new JScrollPane(textareaAction);
	  scrollPane2.setPreferredSize(new Dimension(380,380));
	  panelActions.add(scrollPane2,scrollPane2Constraints);
	  
	  return panelActions;
  }
  

  private static JPanel createObservablesTab () {
	  JPanel panelObservables = new JPanel(new GridBagLayout());

	  tableObservables = new JTable(new NodeProfileTableModel(null));

	  tableObservables.setDefaultRenderer(NodeProfile.class, new ORCell());
	  tableObservables.setDefaultEditor(NodeProfile.class, new ORCell());
	  tableObservables.setRowHeight(30);
	  tableObservables.setBorder(null);
	  tableObservables.setFillsViewportHeight(true);
	  tableObservables.setCellSelectionEnabled(true);
	  tableObservables.setTableHeader(null);
	  tableObservables.setTableHeader(null);
	  tableObservables.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	  tableObservables.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
	      @Override
	      public void valueChanged(ListSelectionEvent arg0) {
	        updateObservableInput();
	      }
	    });
	  tableObservables.setFillsViewportHeight(true);
	  tableObservables.setCellSelectionEnabled(true);
	  tableObservables.setBorder(null);

	  textareaObservable.setWrapStyleWord(true);
	  textareaObservable.setLineWrap(true);
	  textareaObservable.addKeyListener(new KeyAdapter() {
	      @Override
	      public void keyReleased(KeyEvent e) {
	        for (Integer i : tableObservables.getSelectedRows()) {
	        	NodeProfile prof = (NodeProfile) tableObservables.getValueAt(i,0);
	        	setObservable(prof,textareaObservable.getText());
	        	tableObservables.repaint();
	        }
	      }
	    });
	  
	  GridBagConstraints scrollPane1Constraints = new GridBagConstraints();
	  scrollPane1Constraints.gridx = 0;
	  scrollPane1Constraints.gridy = 0;
	  scrollPane1Constraints.gridheight = 2;
	  scrollPane1Constraints.weightx = 1.0;
	  scrollPane1Constraints.weighty = 1.0;
	  scrollPane1Constraints.anchor = GridBagConstraints.PAGE_START;
	  scrollPane1Constraints.fill = GridBagConstraints.BOTH;
	  
	  JScrollPane scrollPane1 = new JScrollPane(tableObservables);
	  scrollPane1.setViewportBorder(null);
	  scrollPane1.setPreferredSize(new Dimension(380,380));
	  // scrollPane1.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	  panelObservables.add(scrollPane1,scrollPane1Constraints);
	  

	  GridBagConstraints scrollPane2Constraints = new GridBagConstraints();
	  scrollPane2Constraints.gridx = 1;
	  scrollPane2Constraints.gridy = 0;
	  scrollPane2Constraints.weightx = 1.0;
	  scrollPane2Constraints.weighty = 1.0;
	  scrollPane2Constraints.anchor = GridBagConstraints.PAGE_START;
	  scrollPane2Constraints.fill = GridBagConstraints.BOTH;
	  
	  JScrollPane scrollPane2 = new JScrollPane(textareaObservable);
	  scrollPane2.setPreferredSize(new Dimension(380,380));
	  panelObservables.add(scrollPane2,scrollPane2Constraints);
	  
	  
	  return panelObservables;
  }
  
	  // Utility functions for test path generation
    
  private static ArrayList<Integer> findTPSources() {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  if (initialNodeProfile != null) {
		  for (Integer i = 0; i < indexToNodesMap.size(); i++) {
			  Block block = getBlock(i);
			  for (NodeProfile prof: chosenCPSET) {
				  if (blockContainsProfile(block,prof) && !block.containsReversion() && !block.containsReference()) {
					  result.add(i);
					  break;
				  }
			  }
		  }
	  }
	  return result;
  }
  
  private static ArrayList<Integer> findTPTargets() {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  if (initialNodeProfile != null) {
		  for (Integer i = 0; i < indexToNodesMap.size(); i++) {
			  Block block = getBlock(i);
			  for (NodeProfile prof: chosenCPSET) {
				  if (blockContainsProfile(block,prof)) {
					  result.add(i);
					  break;
				  }
			  }
		  }
	  }
	  return result;
  }

  public static ArrayList<Integer> findPreambleStarts() {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  // initialBlock and all reversion blocks to the initial block
	  if (initialNodeProfile != null) {
		  for (Integer i = 0; i < indexToNodesMap.size(); i++) {
			  Block block = equivalentStartingBlock(getBlock(i));
			  if (block == initialBlock) {
				  result.add(i);
			  }
		  }
	  }
	  return result;
  }
  
  private static ArrayList<Integer> findNOIs(ArrayList<Integer> CPs) {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  if (initialNodeProfile != null) {
		  for (Integer i = 0; i < indexToNodesMap.size(); i++) {
			  Boolean inCPs = false;
			  for (Integer j = 0; j < CPs.size(); j++) {
				  if (i == CPs.get(j)) {
					  inCPs = true;
					  break;
				  }
			  }
			  // Only include NOI if not also a CP
			  if (!inCPs) {
				  Block block = getBlock(i);
				  
				  if (block.containsReference() || block.containsReversion()) {
					  result.add(i);
				  } else {
					  
					  for (NodeProfile prof: chosenNOISET) {
						  if (blockContainsProfile(block,prof)) {
							  result.add(i);
							  break;
						  }
					  }
					  
				  }
				  
			  }
		  }
	  }
	  return result;
  }
  
  private static ArrayList<Integer> mergePaths (ArrayList<Integer> path1, ArrayList<Integer> path2) {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  if (path1 == null || path1.size() == 0) {
		  if (path2 != null) {
			  for (Integer i = 0; i < path2.size(); i++) {
				  result.add(path2.get(i));
			  }
		  }
	  } else if (path2 == null) {
		  for (Integer i = 0; i < path1.size(); i++) {
			  result.add(path1.get(i));
		  }
	  } else {
		  for (Integer i = 0; i < path1.size(); i++) {
			  result.add(path1.get(i));
		  }
		  for (Integer i = 1; i < path2.size(); i++) {
			  result.add(path2.get(i));
		  }
	  }
	  return result;
  }
  
  private static ArrayList<Integer> appendPaths (ArrayList<Integer> path1, ArrayList<Integer> path2) {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  if (path1 == null || path1.size() == 0) {
		  if (path2 != null) {
			  for (Integer i = 0; i < path2.size(); i++) {
				  result.add(path2.get(i));
			  }
		  }
	  } else if (path2 == null) {
		  for (Integer i = 0; i < path1.size(); i++) {
			  result.add(path1.get(i));
		  }
	  } else {
		  for (Integer i = 0; i < path1.size(); i++) {
			  result.add(path1.get(i));
		  }
		  for (Integer i = 0; i < path2.size(); i++) {
			  result.add(path2.get(i));
		  }
	  }
	  return result;
  }
  
  
  private static Boolean pathIsFeasible (ArrayList<Integer> path) {
	  Boolean result = false;
	  String command = "(check-test-path " + pathToString(path) + ")";
	  String xmlResult = getSBCL().sendCommand(command);
	  if (xmlResult.matches("<result>FEASIBLE<\\/result>")) {
		  result = true;
		  }
	  return result;
  }
  
  private static Boolean testCaseLoops (TestCase tc) {
	  return getBlock(tc.getStart()).equals(reversionOrReferenceTarget(getBlock(tc.getEnd())));
  }
  
  private static ArrayList<Integer> testCaseStepsModifiedForLooping (TestCase tc) {
	  if (tc.isLooping()) {
		  ArrayList<Integer> result = new ArrayList<Integer>();
		  result.add(tc.getEnd());
		  Integer size = tc.getSteps().size();
		  for (Integer i = 1; i < size; i++) {
			  result.add(tc.getSteps().get(i));
		  }
		  return result;
	  } else {
		  return tc.getSteps();
	  }
  }
  
  private static ArrayList<Integer> testCaseStepsForSegment(TestCase tc) {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  ArrayList<Integer> tcSteps = tc.getSteps();
	  if (testPath.size() > 0) {
		  result.add(testPath.get(testPath.size() - 1).getTestCase().getEnd());
		  for (Integer i = 1; i < tcSteps.size(); i++) {
			  result.add(tcSteps.get(i));
		  }
	  } else {
		  for (Integer i = 0; i < tcSteps.size(); i++) {
			  result.add(tcSteps.get(i));
		  }
	  }
	  return result;
  }
  
  private static String pathToString (ArrayList<Integer> path) {
	  if (path == null || path.size() == 0) {
		  return "()";
	  } else {
		  StringBuilder sb = new StringBuilder();
		  sb.append("(");
		  for (Integer n: path) {
			  sb.append(n + " ");
		  }
		  sb.setLength(sb.length() - 1);
		  sb.append(")");
		  return sb.toString();
	  }
  }

  private static Boolean isStrongMatch(ArrayList<Integer> gap, TestCase tc, Integer goal) {
    ArrayList<Integer> testBlocks = tc.getSteps();
    if (testPath.size() == 0) {
    	testBlocks = appendPaths(tc.getBlocksBefore(), testBlocks);
    }
    Integer currentBlock = null;
    if (testPath.size() > 0) {
    	currentBlock = testPath.get(testPath.size() - 1).getTestCase().getEnd();
    }
    if (gap.size() == (testBlocks.size() - 1)) {
      if (testBlocks.get(gap.size()) != goal) {
    	  return false;
      }
      if (currentBlock == null) {
    	  for (Integer i = 0; i < gap.size() - 1; i++) {
    	        if (gap.get(i) != testBlocks.get(i)) {
    	          return false;
    	        }
    	      }
      } else {
    	  if (!reversionOrReferenceTarget(getBlock(currentBlock)).equals(reversionOrReferenceTarget(getBlock(testBlocks.get(0))))) {
    		  return false;
    	  } else {
    		  for (Integer i = 1; i < gap.size() - 1; i++) {
    		        if (gap.get(i) != testBlocks.get(i)) {
    		          return false;
    		        }
    		      }
    	  }
      }
      System.out.println("Strong match successful");
      return true;
    } else {
      return false;
    }
  }

  private static Boolean isPartMatch(ArrayList<Integer> gap, TestCase tc) {
    ArrayList<Integer> testBlocks = tc.getSteps();
    if (testPath.size() == 0) {
    	testBlocks = appendPaths(tc.getBlocksBefore(), testBlocks);
    }
    Integer currentBlock = null;
    if (testPath.size() > 0) {
    	currentBlock = testPath.get(testPath.size() - 1).getTestCase().getEnd();
    }
    if (gap.size() >= testBlocks.size()) {
      if (currentBlock == null) {
    	  for (Integer i = 0; i < testBlocks.size(); i++) {
    	        if (gap.get(i) != testBlocks.get(i)) {
    	          return false;
    	        }
    	      }
      } else {
    	  if (!reversionOrReferenceTarget(getBlock(currentBlock)).equals(reversionOrReferenceTarget(getBlock(testBlocks.get(0))))) {
    		  return false;
    	  } else {
    		  for (Integer i = 1; i < testBlocks.size(); i++) {
    		        if (gap.get(i) != testBlocks.get(i)) {
    		          return false;
    		        }
    		      }
    	  }
      }
      return true;
    } else {
      return false;
    }
  }

  private static void addTestSegment(TestSegment segment) {
	  if (testPath.size() > 0) {
		  segment.setRealStart(testPath.get(testPath.size()-1).getTestCase().getEnd());
	  }
	  testPath.add(segment);
	  segment.getTestCase().setSelected(true);
	  updateTblTCs();
  }
  
  private static void fillTestPathGap(ArrayList<Integer> gap, TestCase tc, ArrayList<Integer> tcSteps) {
	  Boolean change = true;
	  Integer tcStart = tcSteps.get(0);
	  ArrayList<Integer> prefix = findPrefix();
	  while (change) {
		  change = false;
		  for (TestCase nextTc: allTestCases) {
			  if (!nextTc.isSelected() && !nextTc.equals(tc)) {
				  if (isStrongMatch(gap, nextTc, tcStart)) {
					  if (testPath.size() == 0) {
						  ArrayList<Integer> preamble = new ArrayList<Integer>();
						  if (nextTc.needsPreamble()) {
							  preamble.add(nextTc.getStart());
							  preamble = appendPaths(nextTc.getBlocksBefore(), preamble);
							  insertLoopingTestCases(tc, preamble, mergePaths(nextTc.getSteps(), tcSteps));
						  } else {
							  insertLoopingTestCases(tc, preamble, mergePaths(nextTc.getSteps(), tcSteps));
						  }
					  }
					  ArrayList<Integer> preamble = new ArrayList<Integer>();
					  if (testPath.size() == 0) {
						  preamble = nextTc.getBlocksBefore();
					  }
					  if (!nextTc.isSelected() && nextTc.isReachable()
							  && (!nextTc.needsPreamble() || testPath.size() == 0)) {
						  addTestSegment(new TestSegment(preamble, nextTc));
						  prefix = findPrefix();
						  insertLoopingTestCases(tc, prefix, tcSteps);
					  }
					  gap = new ArrayList<Integer>();
					  break;
				  } else if (isPartMatch(gap, nextTc)) {
					  if (testPath.size() == 0) {
						  ArrayList<Integer> preamble = new ArrayList<Integer>();
						  ArrayList<Integer> suffix = appendPaths(gap, tcSteps);
						  suffix.subList(0, nextTc.getBlocksBefore().size()).clear();
						  if (nextTc.needsPreamble()) {
							  preamble.add(nextTc.getStart());
							  preamble = appendPaths(nextTc.getBlocksBefore(), preamble);
							  insertLoopingTestCases(tc, preamble, suffix);
						  } else {
							  insertLoopingTestCases(tc, preamble, suffix);
						  }
					  }
					  if (!nextTc.isSelected() && nextTc.isReachable() && !nextTc.needsPreamble()) {
						  addTestSegment(new TestSegment(new ArrayList<Integer>(), nextTc));
						  prefix = findPrefix();
						  insertLoopingTestCases(tc, prefix, tcSteps);
					  }
					  ArrayList<Integer> preamble = new ArrayList<Integer>();
					  if (testPath.size() == 0) {
						  preamble = nextTc.getBlocksBefore();
					  }
					  if (!nextTc.isSelected() && nextTc.isReachable()
							  && (!nextTc.needsPreamble() || testPath.size() == 0)) {
						  addTestSegment(new TestSegment(preamble, nextTc));
						  prefix = findPrefix();
						  insertLoopingTestCases(tc, prefix, tcSteps);
					  }
					  prefix = findPrefix();
					  gap = getGap(prefix, tcSteps);
					  ArrayList<Integer> suffix = appendPaths(gap, tcSteps);
					  insertLoopingTestCases(tc, prefix, suffix);
					  change = true;
					  break;
				  }
			  }
		  }
	  }
	  if (gap.size() > 0) {
		  // Want insert looping with gap
		  Integer pathSize = testPath.size();
		  insertLoopingTestCasesWithGap(tc, prefix, tcSteps, gap);
		  if (testPath.size() > pathSize) {
			  gap = new ArrayList<Integer>();
		  }
	  }
	  TestSegment segment = new TestSegment(gap, tc);
	  addTestSegment(segment);
  }
  
  private static ArrayList<Integer> findPrefix() {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  // We assume that testPath is not empty. Ensure this in the call(s)!!!
	  // Otherwise the result may be wrong.
	  Boolean isFirst = true;
	  for (TestSegment segment: testPath) {
		  Boolean hasPreamble = false;
		  for (Integer i = 0; i < segment.getPreamble().size(); i++) {
			  if (i > 0 || isFirst) {
				  result.add(segment.getPreamble().get(i));
			  }		  
			  hasPreamble = true;
		  }
		  for (Integer i = 0; i < segment.getTestCase().getSteps().size(); i++) {
			  if (i > 0 || hasPreamble || isFirst) {
				  result.add(segment.getTestCase().getSteps().get(i));
			  }		  
		  }
		  isFirst = false;
	  }
	  return result;
  }
  
  private static void insertLoopingTestCases(TestCase refTc, ArrayList<Integer> prefix, ArrayList<Integer> suffix) {
	  ArrayList<Integer> pref = prefix;
	  for (TestCase tc: allTestCases) {
		  if (!tc.isSelected() && !tc.equals(refTc) && tc.isLooping()
				  && (pref.size() == 0 || reversionOrReferenceTarget(getBlock(tc.getStart())).equals(reversionOrReferenceTarget(getBlock(pref.get(pref.size() - 1)))))) {
			  if (pathIsFeasible(mergePaths(mergePaths(pref, tc.getSteps()), suffix))) {
				  if (testPath.size() == 0) {
					  addTestSegment(new TestSegment(tc.getBlocksBefore(), tc));
					  pref = appendPaths(tc.getBlocksBefore(), tc.getSteps());
				  } else {
					  addTestSegment(new TestSegment(new ArrayList<Integer>(), tc));
					  pref = mergePaths(pref, tc.getSteps());
				  }
			  }
		  }
	  }
  }

  private static void insertLoopingTestCasesWithGap(TestCase refTc, ArrayList<Integer> prefix, ArrayList<Integer> suffix, ArrayList<Integer> gap) {
	  ArrayList<Integer> pref = mergePaths(prefix, gap);
	  pref.add(refTc.getStart());
	  ArrayList<Integer> newGap = gap;
	  for (TestCase tc: allTestCases) {
		  if (!tc.isSelected() && !tc.equals(refTc) && tc.isLooping()
				  && reversionOrReferenceTarget(getBlock(tc.getStart())).equals(reversionOrReferenceTarget(getBlock(refTc.getStart())))) {
			  if (pathIsFeasible(mergePaths(mergePaths(pref, tc.getSteps()), suffix))) {
				  addTestSegment(new TestSegment(newGap, tc));
				  newGap = new ArrayList<Integer>();
				  pref = mergePaths(pref, tc.getSteps());
			  }
		  }
	  }
  }
  

  private static JPanel createTPTab() {
    JPanel panelTP = new JPanel(new GridBagLayout());

    GridBagConstraints label1Constraints = new GridBagConstraints();
    label1Constraints.gridx = 0;
    label1Constraints.gridy = 0;
	  
    /* Generated Test Cases Label */
    JLabel lblGeneratedTestCases = new JLabel("Generated Test Cases:");
    lblGeneratedTestCases.setFont(new Font("Tahoma", Font.PLAIN, 18));
    panelTP.add(lblGeneratedTestCases,label1Constraints);

    GridBagConstraints scrollPane1Constraints = new GridBagConstraints();
    scrollPane1Constraints.gridx = 0;
    scrollPane1Constraints.gridy = 1;
    scrollPane1Constraints.gridwidth = 2;
    scrollPane1Constraints.gridheight = 2;
    scrollPane1Constraints.weightx = 1.0;
    scrollPane1Constraints.weighty = 1.0;
    scrollPane1Constraints.anchor = GridBagConstraints.PAGE_START;
    scrollPane1Constraints.fill = GridBagConstraints.BOTH;
	  
    /* Generated Test Cases Table */
    tblTCs = new JTable(new TestCasesModel(null));
    tblTCs.setDefaultRenderer(TestCase.class, new TestCaseCell());
    tblTCs.setDefaultEditor(TestCase.class, new TestCaseCell());
    tblTCs.setRowHeight(100);
    tblTCs.setTableHeader(null);
    tblTCs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane scrollPane = new JScrollPane(tblTCs);
    scrollPane.setPreferredSize(new Dimension(280,380));
    panelTP.add(scrollPane,scrollPane1Constraints);

    GridBagConstraints label2Constraints = new GridBagConstraints();
    label2Constraints.gridx = 2;
    label2Constraints.gridy = 0;
	  
    /* Current Test Path Label */
    //JLabel lblCurrentTestPath = new JLabel("Current Test Path");
    lblCurrentTestPath.setFont(new Font("Tahoma", Font.PLAIN, 18));
    panelTP.add(lblCurrentTestPath,label2Constraints);

    GridBagConstraints scrollPane2Constraints = new GridBagConstraints();
    scrollPane2Constraints.gridx = 2;
    scrollPane2Constraints.gridy = 1;
    scrollPane2Constraints.gridwidth = 2;
    scrollPane2Constraints.gridheight = 2;
    scrollPane2Constraints.weightx = 1.0;
    scrollPane2Constraints.weighty = 1.0;
    scrollPane2Constraints.anchor = GridBagConstraints.PAGE_START;
    scrollPane2Constraints.fill = GridBagConstraints.BOTH;
	  
    /* Current Test Path Table */
    tblTP = new JTable();
    tblTP.setModel(new TestPathModel(null));
    tblTP.setDefaultRenderer(TestSegment.class, new TestPathCell());
    tblTP.setDefaultEditor(TestSegment.class, new TestPathCell());
    tblTP.setRowHeight(80);
    tblTP.setTableHeader(null);
    tblTP.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane scrollPane2 = new JScrollPane(tblTP);
    scrollPane2.setPreferredSize(new Dimension(460,380));
    panelTP.add(scrollPane2,scrollPane2Constraints);

    GridBagConstraints button1Constraints = new GridBagConstraints();
    button1Constraints.gridx = 0;
    button1Constraints.gridy = 3;

    GridBagConstraints button2Constraints = new GridBagConstraints();
    button2Constraints.gridx = 1;
    button2Constraints.gridy = 3;

    GridBagConstraints button3Constraints = new GridBagConstraints();
    button3Constraints.gridx = 2;
    button3Constraints.gridy = 3;
	  
    /* Add Test Case Button */
    JButton btnAddTestCase = new JButton("Add Test Case ->");
    btnAddTestCase.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
    	  if (tblTCs.getSelectedRow() >= 0) {
    		  TestCase tc = (TestCase) tblTCs.getValueAt(tblTCs.getSelectedRow(), 0);
    		  if (getBlock(tc.getStart()).equals(initialBlock) && testPath.size() == 0
    				  && !tc.needsPreamble()) {
    			  insertLoopingTestCases(tc, new ArrayList<Integer>(), testCaseStepsForSegment(tc));
    			  if (testPath.size() == 0) {
    				  addTestSegment(new TestSegment(tc.getBlocksBefore(), tc));
    			  } else {
    				  addTestSegment(new TestSegment(new ArrayList<Integer>(), tc));
    			  }
    			  }
    		  else if (tc.isReachable()) {
    			  if (!tc.needsPreamble()) {
    				  insertLoopingTestCases(tc, findPrefix(), testCaseStepsForSegment(tc));
    				  addTestSegment(new TestSegment(new ArrayList<Integer>(), tc));
    				  } else {
    					  ArrayList<Integer> tcSteps = tc.getSteps();
    					  ArrayList<Integer> prefix = findPrefix();
    					  ArrayList<Integer> gap = getGap(prefix, tcSteps);
    					  if (gap == null) {
    						  if (tc.isLooping()) {
    							  tcSteps = testCaseStepsModifiedForLooping(tc);
    							  gap = getGap(prefix, tcSteps);
    							  if (gap == null) {
    								  System.out.println("Problem with " + tcSteps.toString());
    							  } else {
    								  // Need to change starting block, BUT ONLY for the segment.
    								  ArrayList<Integer> suffix = appendPaths(gap, tcSteps);
    								  insertLoopingTestCases(tc, prefix, suffix);
    								  fillTestPathGap(gap, tc, tcSteps);
    							  }
    						  } else {
    							  System.out.println("Problem with " + tcSteps.toString());
    						  } 						  
    					  } else {
    						  ArrayList<Integer> suffix = appendPaths(gap, tcSteps);
							  insertLoopingTestCases(tc, prefix, suffix);
    						  fillTestPathGap(gap, tc, tcSteps);
    					  }
    				  } 
    			  }
    		  updateTblTCs();
    		  updateTblTP();
    		  }
      }
    });
    panelTP.add(btnAddTestCase,button1Constraints);
    
    /* Undo Button */
    JButton btnUndo = new JButton("Undo");
    btnUndo.addActionListener(new ActionListener () {
    	public void actionPerformed(ActionEvent e) {
    		if (testPath.size() > 0) {
    			Integer selectedRow = tblTP.getSelectedRow();
    			if (selectedRow >= 0) {
    				testPath.subList(selectedRow, testPath.size()).clear();
    				testPath.trimToSize();
    				tblTP.clearSelection();
    			} else {
    				testPath.remove(testPath.size() - 1);
    			}
    			updateTblTCs();
    			updateTblTP();
    		}
    	}
    });
    panelTP.add(btnUndo,button2Constraints);
    
    /* Export to HTML Button */
//    JButton btnExportToHtml = new JButton("Export to HTML");
//    panelTP.add(btnExportToHtml,button3Constraints);
    
    return panelTP;
  }

  private static ArrayList<Integer> getPreamble(ArrayList<Integer> testPath) {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  String command = "(test-path-preamble 0 " + pathToString(testPath) + ")";
	  String xmlResult = getSBCL().sendCommand(command);
	  if (xmlResult.matches("<result><path>(<block-index>\\d+<\\/block-index>)+<\\/path><\\/result>")) {
		  org.jdom2.input.SAXBuilder saxBuilder = new SAXBuilder();
		  org.jdom2.Document doc = null;
		  try {
			  doc = saxBuilder.build(new StringReader(xmlResult));
			  } catch (JDOMException | IOException e) {
				  e.printStackTrace();
				  }
		  Element root = doc.getRootElement();
		  Element path = root.getChild("path");
		  List<Element> blocks = path.getChildren("block-index");
		  for (Element block: blocks) {
			  result.add(Integer.parseInt(block.getValue()));
			  }
		  }
	  return result;
  }

  private static ArrayList<Integer> getGap(ArrayList<Integer> prefix, ArrayList<Integer> testPath) {
	  ArrayList<Integer> result = new ArrayList<Integer>();
	  String command = "(test-path-gap " + pathToString(prefix) + " " + pathToString(testPath) + ")";
	  String xmlResult = getSBCL().sendCommand(command);
	  if (xmlResult.matches("<result><path>(<block-index>\\d+<\\/block-index>)+<\\/path><\\/result>")) {
		  org.jdom2.input.SAXBuilder saxBuilder = new SAXBuilder();
		  org.jdom2.Document doc = null;
		  try {
			  doc = saxBuilder.build(new StringReader(xmlResult));
			  } catch (JDOMException | IOException e) {
				  e.printStackTrace();
				  }
		  Element root = doc.getRootElement();
		  Element path = root.getChild("path");
		  List<Element> blocks = path.getChildren("block-index");
		  for (Element block: blocks) {
			  result.add(Integer.parseInt(block.getValue()));
			  }
		  } else if (xmlResult.matches("<result>FAILED<\\/result>")) {
			  result = null;
		  }
	  return result;
  }
  
  
  // testPath is raw test path
  
  private static ArrayList<Integer> getPostamble(ArrayList<Integer> testPath) {
		ArrayList<Integer> result = new ArrayList<Integer>();
		ArrayList<Integer> startingBlocks = findPreambleStarts();
		Integer startingBlock = testPath.get(testPath.size()-1);
		Boolean success = false;
		String testPathString = pathToString(testPath);
		for (Integer s:startingBlocks) {
			
			if (s == startingBlock) {
				success = true;
				// already at starting block
				result = new ArrayList<Integer>();
			} else {
				ArrayList<Integer> newResult = new ArrayList<Integer>();
				String xmlResult = getSBCL().sendCommand("(test-path-postamble " + s + " " + testPathString + ")");			
				if (xmlResult.matches("<result><path>(<block-index>\\d+<\\/block-index>)+<\\/path><\\/result>")) {
			        org.jdom2.input.SAXBuilder saxBuilder = new SAXBuilder();
			        org.jdom2.Document doc = null;
			        try {
			          doc = saxBuilder.build(new StringReader(xmlResult));
			        } catch (JDOMException | IOException e) {
			          e.printStackTrace();
			        }
			        
			        Element root = doc.getRootElement();
			        Element path = root.getChild("path");
			        List<Element> blocks = path.getChildren("block-index");
			        for (Element block: blocks) {
			        	newResult.add(Integer.parseInt(block.getValue()));
			        	}
			        if (success == false) {
			        	result = newResult;
			        	success = true;
			        } else if (newResult.size() < result.size()) {
			        	result = newResult;
			        }			
			        }
				}
      }
		return result;
	}
	

  public static ArrayList<TestCase> sendTestCaseCommand(Integer start, ArrayList<Integer> nois, Integer end) {
	  ArrayList<TestCase> result = new ArrayList<TestCase>();
	  ArrayList<Integer> cpBlocks = findTPTargets();
	  Boolean ok = false;
	  for (Integer i: cpBlocks) {
		  if (start == i) {
			  ok = true;
			  break;
		  }
	  }
	  if (ok) {
		  ok = false;
		  for (Integer i: cpBlocks) {
			  if (end == i) {
				  ok = true;
				  break;
			  }
		  }
	  } else {
		  System.out.println("Bad starting block index");
	  }
	  if (ok) {
		  ArrayList<Integer> filteredNOIs = new ArrayList<Integer>();
		  for (Integer i: nois) {
			  if (i >= 0 && i < numberOfBlocks()) {
				  ok = true;
				  for (Integer j: cpBlocks) {
					  if (i == j) {
						  ok = false;
						  break;
					  }
				  }
				  if (ok) {
					  filteredNOIs.add(i);
				  }
			  }
		  }
		  String command = "(find-test-paths " + start + " " + pathToString(filteredNOIs) + " "
		  + pathToString(cpBlocks) + ")";
		  String cmdResult = sbcl.sendCommand(command);
		  if (cmdResult.matches("<result>(<path>(<block-index>\\d+<\\/block-index>)+<\\/path>)+<\\/result>")) {
			  org.jdom2.input.SAXBuilder saxBuilder = new SAXBuilder();
			  org.jdom2.Document doc = null;
			  try {
				  doc = saxBuilder.build(new StringReader(cmdResult));
			  } catch (JDOMException | IOException e) {
		          e.printStackTrace();
		          }
			  Element root = doc.getRootElement();
			  List<Element> paths = root.getChildren("path");
			  for (Element path: paths) {
				  List<Element> blocks = path.getChildren("block-index");
				  ArrayList<Integer> pathList = new ArrayList<Integer>();
				  for (Element block: blocks) {
					  pathList.add(Integer.parseInt(block.getValue()));
					  }
				  // pathlist is raw (block indices)
				  TestCase tc = constructTestCase(pathList);
				  if (!(tc.getStartNode().getBlockIndex() == initialBlock.getIndex())) {
				        ArrayList<Integer> blks = getPreamble(tc.getSteps());
				        System.out.println("BLOCKS: " + blks);
				        tc.setStepsBefore(blks, getNodeList(blks));
				      }
				      ArrayList<Integer> blks = getPostamble(tc.getSteps());
				      tc.setStepsAfter(blks, getNodeList(blks));
				  result.add(tc);
		        }
		  }
	  } else {
		  System.out.println("Bad ending block index");
	  }
	  return result;
  }
  
  private static ArrayList<ArrayList<Integer>> xmlStringToRawPath (String str) {
	  org.jdom2.input.SAXBuilder saxBuilder = new SAXBuilder();
	  org.jdom2.Document doc = null;
	  ArrayList<ArrayList<Integer>> result = new ArrayList<ArrayList<Integer>>();
      try {
        doc = saxBuilder.build(new StringReader(str));
      } catch (JDOMException | IOException e) {
        e.printStackTrace();
      }
      Element root = doc.getRootElement();
      List<Element> paths = root.getChildren("path");
      for (Element path: paths) {
      	List<Element> blocks = path.getChildren("block-index");
      	ArrayList<Integer> pathList = new ArrayList<Integer>();
      	for (Element block: blocks) {
      		pathList.add(Integer.parseInt(block.getValue()));
      	}
      	result.add(pathList);
      }
      return result;
  }
  
  private static Boolean equivalentCases (ArrayList<Integer> case1, ArrayList<Integer> case2) {
	  Boolean result = (case1.size() == case2.size());
	  if (result && case1.size() > 0) {
		  Block start1 = equivalentStartingBlock(getBlock(case1.get(0)));
		  Block start2 = equivalentStartingBlock(getBlock(case2.get(0)));
		  if (start1 == start2) {
			  for (Integer i = 1; i < case1.size(); i++) {
				  if (case1.get(i) != case2.get(i)) {
					  result = false;
					  break;
				  }
			  }
		  } else {
			  result = false;
		  }
	  }
	  return result;
  }
  
  private static ArrayList<ArrayList<Integer>> generateTestCases() {
	  ArrayList<ArrayList<Integer>> result = new ArrayList<ArrayList<Integer>>();
	  ArrayList<Integer> startingBlocks = findTPSources();
	  ArrayList<Integer> endingBlocks = findTPTargets();
	  ArrayList<Integer> noiBlocks = findNOIs(endingBlocks);
	  String validPathsPattern = "<result>(<path>(<block-index>\\d+<\\/block-index>)+<\\/path>)+<\\/result>";
	  for (Integer i : startingBlocks) {
	      for (Integer j : endingBlocks) {
	    	  Boolean feasible = false;
	    	  ArrayList<ArrayList<Integer>> rawCases = new ArrayList<ArrayList<Integer>>();
	    	  ArrayList<Integer> independentNOIs = new ArrayList<Integer>();
	    	  // First do without NOIs
	    	  String commandResult = sbcl.sendCommand("(find-test-paths " + i + " () " + j
	    			  + " " + pathToString(endingBlocks) + ")");
	    	  if (commandResult.matches(validPathsPattern)) {
	    		  feasible = true;
	    		  rawCases = xmlStringToRawPath(commandResult);
	    	  }
	    	  if (feasible) {
	    		  // Now do with NOIs one by one
	    		  for (Integer k: noiBlocks) {
	    			  commandResult = sbcl.sendCommand("(find-test-paths " + i + " (" + k + ") " + j
	    	    			  + " " + pathToString(endingBlocks) + ")");
	    			  if (commandResult.matches(validPathsPattern)) {
	    				  ArrayList<ArrayList<Integer>> noiCases = xmlStringToRawPath(commandResult);
	    				  // Record if the NOI is "independent"
	    				  if (noiCases.size() > 1) {
	    					  independentNOIs.add(k);
	    				  }
	    				  for (ArrayList<Integer> noiCase: noiCases) {
	    					  Boolean alreadyCovered = false;
	    					  for (ArrayList<Integer> rawCase: rawCases) {
	    						  if (rawCase.equals(noiCase)) {
	    							  alreadyCovered = true;
	    							  break;
	    						  }
	    					  }
	    					  if (!alreadyCovered) {
	    						  rawCases.add(noiCase);
	    					  }
	    				  }
	    			  }
	    		  }
	    		  // Add pairwise "independent" NOIs here
//	    		  for (Integer ii = 0; ii < independentNOIs.size(); ii++) {
//	    			  for (Integer jj = ii + 1; jj < independentNOIs.size(); jj++) {
//	    				  commandResult = sbcl.sendCommand("(find-test-paths " + i + " (" + independentNOIs.get(ii)
//	    				  + " " + independentNOIs.get(jj) + ") " + j
//		    			  + " " + pathToString(endingBlocks) + ")");
//	    				  if (commandResult.matches(validPathsPattern)) {
//	    					  ArrayList<ArrayList<Integer>> noiCases = xmlStringToRawPath(commandResult);
//	    					  for (ArrayList<Integer> noiCase: noiCases) {
//		    					  Boolean alreadyCovered = false;
//		    					  for (ArrayList<Integer> rawCase: rawCases) {
//		    						  if (rawCase.equals(noiCase)) {
//		    							  alreadyCovered = true;
//		    							  break;
//		    						  }
//		    					  }
//		    					  if (!alreadyCovered) {
//		    						  rawCases.add(noiCase);
//		    					  }
//		    				  }
//	    				  }
//	    			  }
//	    		  }
	    	  }
	    	  for (ArrayList<Integer> rawCase: rawCases) {
	    		  result.add(rawCase);
	    	  }
	      }
	    }
	  System.out.println("Now do reversions and references");
	  // Add things from reversions/references not already covered
	  ArrayList<Integer> revBlocks = new ArrayList<Integer>();
	  for (Integer i: endingBlocks) {
		  Boolean found = false;
		  for (Integer j: startingBlocks) {
			  if (i == j) {
				  found = true;
				  break;
			  }
		  }
		  if (!found) {
			  revBlocks.add(i);
		  }
	  }
	  for (Integer i: revBlocks) {
		  for (Integer j: endingBlocks) {
			  Boolean feasible = false;
	    	  ArrayList<ArrayList<Integer>> rawCases = new ArrayList<ArrayList<Integer>>();
	    	  ArrayList<Integer> independentNOIs = new ArrayList<Integer>();
	    	  // First do without NOIs
	    	  String commandResult = sbcl.sendCommand("(find-test-paths " + i + " () " + j
	    			  + " " + pathToString(endingBlocks) + ")");
	    	  if (commandResult.matches(validPathsPattern)) {
	    		  feasible = true;
	    		  rawCases = xmlStringToRawPath(commandResult);
	    	  }
	    	  if (feasible) {
	    		// Now do with NOIs one by one
	    		  for (Integer k: noiBlocks) {
	    			  commandResult = sbcl.sendCommand("(find-test-paths " + i + " (" + k + ") " + j
	    	    			  + " " + pathToString(endingBlocks) + ")");
	    			  if (commandResult.matches(validPathsPattern)) {
	    				  ArrayList<ArrayList<Integer>> noiCases = xmlStringToRawPath(commandResult);
	    				  // Record if the NOI is "independent"
	    				  if (noiCases.size() > 1) {
	    					  independentNOIs.add(k);
	    				  }
	    				  for (ArrayList<Integer> noiCase: noiCases) {
	    					  Boolean alreadyCovered = false;
	    					  for (ArrayList<Integer> rawCase: rawCases) {
	    						  if (rawCase.equals(noiCase)) {
	    							  alreadyCovered = true;
	    							  break;
	    						  }
	    					  }
	    					  if (!alreadyCovered) {
	    						  rawCases.add(noiCase);
	    					  }
	    				  }
	    			  }
	    		  }
	    		  // Add pairwise "independent" NOIs here
//	    		  for (Integer ii = 0; ii < independentNOIs.size(); ii++) {
//	    			  for (Integer jj = ii + 1; jj < independentNOIs.size(); jj++) {
//	    				  commandResult = sbcl.sendCommand("(find-test-paths " + i + " (" + independentNOIs.get(ii)
//	    				  + " " + independentNOIs.get(jj) + ") " + j
//		    			  + " " + pathToString(endingBlocks) + ")");
//	    				  if (commandResult.matches(validPathsPattern)) {
//	    					  ArrayList<ArrayList<Integer>> noiCases = xmlStringToRawPath(commandResult);
//	    					  for (ArrayList<Integer> noiCase: noiCases) {
//		    					  Boolean alreadyCovered = false;
//		    					  for (ArrayList<Integer> rawCase: rawCases) {
//		    						  if (rawCase.equals(noiCase)) {
//		    							  alreadyCovered = true;
//		    							  break;
//		    						  }
//		    					  }
//		    					  if (!alreadyCovered) {
//		    						  System.out.println("***** Found test case with independent NOIs");
//		    						  rawCases.add(noiCase);
//		    					  }
//		    				  }
//	    				  }
//	    			  }
//	    		  }
		    	  for (ArrayList<Integer> rawCase: rawCases) {
		    		  Boolean found = false;
		    		  for (ArrayList<Integer> foundCase: result) {
		    			  if (equivalentCases(rawCase,foundCase)) {
		    				  found = true;
		    				  break;
		    			  }
		    		  }
		    		  if (!found) {
		    			  System.out.println("***** Added test case from reversion/reference");
		    			  result.add(rawCase);
		    		  }
		    		  
		    	  }
	    	  }
		  }
	  }
	  return result;
  }
 
  private static void printErrorMessage(String error) {
    int errorIndex =
        Integer.parseInt(error.substring(error.indexOf('|') + 1, error.lastIndexOf('|')));
    String errorMessage = error.substring(error.lastIndexOf('|') + 1);
    switch (errorIndex) {
      case 0:
        JOptionPane.showMessageDialog(null,
            "IP address name resolution failed.\r\n" + errorMessage);
        break;
      case 1:
        JOptionPane.showMessageDialog(null,
            "Unknown error while trying to connect to server.\r\n" + errorMessage);
        break;
      case 2:
        JOptionPane.showMessageDialog(null,
            "Message could not be sent. Connection may have been lost" + errorMessage);
        break;
      case 3:
        JOptionPane.showMessageDialog(null, "Unable to close lisp server.");
        break;
      case 4:
        JOptionPane.showMessageDialog(null, "Unable to start lisp server.");
        break;
      case 5:
        JOptionPane.showMessageDialog(null, "Communication with server timed out.");
        break;
      case 6:
        JOptionPane.showMessageDialog(null, "Unable to read Behaviour Tree Model");
        break;
      case 7:
        JOptionPane.showMessageDialog(null,
            "Unable to create folder. Please ensure write access is available in "
                + System.getProperty("user.dir"));
        break;
      case 8:
        JOptionPane.showMessageDialog(null,
            "An unknown error blocked this application from opening "
                + System.getProperty("user.dir") + System.getProperty("file.separator")
                + "test-cases");
        break;
      case 9:
        JOptionPane.showMessageDialog(null,
            "Selected file has invalid type. Application can read \"btc\" and \"tcc.xml\" files only.");
        break;
      case 10:
        JOptionPane.showMessageDialog(null,
            "Selected Test Case Configuration is using a different version to the current application.");
        break;
      case 11:
        JOptionPane.showMessageDialog(null, "There is nothing to save.");
        break;
      default:
        JOptionPane.showMessageDialog(null, "Other error.\r\n" + errorMessage);
        break;
    }
  }

  private static String connectToServer() {
    String connectionResult = "";
    try {
      connectionResult = sbcl.connect("localhost", 12);
      if (!connectionResult.equals("success")) {
        printErrorMessage(connectionResult);
        return connectionResult;
      }
    } catch (InterruptedException | IOException e) {
      // nop
    }
    return connectionResult;
  }

  private static TestCase constructTestCase(Collection<Integer> blocks) {
    ArrayList<Node> nodeList = getNodeList(blocks);
    return new TestCase(blocks, nodeList);
  }

  private static ArrayList<Node> getNodeList(Collection<Integer> blocks) {
    ArrayList<Node> nodeList = new ArrayList<Node>();
    for (int i : blocks) {
      nodeList.addAll(indexToNodesMap.get(i).getNodes());
    }
    return nodeList;
  }
  
  public static NodeProfile reversionProfile(Block block) {
	  NodeProfile result = null;
	  for (Node node: block.getNodes()) {
		  if (node.getFlag().equals("REVERSION")) {
			  result = node.getNodeProfile();
			  }
	  }
	  return result;
  }

  public static String reversionLabel(Block block) {
	  String result = null;
	  for (Node node: block.getNodes()) {
		  if (node.getFlag().equals("REVERSION")) {
			  result = node.getLabel();
			  }
	  }
	  return result;
  }

  public static NodeProfile referenceProfile(Block block) {
	  NodeProfile result = null;
	  for (Node node: block.getNodes()) {
		  if (node.getFlag().equals("REFERENCE")) {
			  result = node.getNodeProfile();
			  }
	  }
	  return result;
  }

  public static String referenceLabel(Block block) {
	  String result = null;
	  for (Node node: block.getNodes()) {
		  if (node.getFlag().equals("REFERENCE")) {
			  result = node.getLabel();
			  }
	  }
	  return result;
  }

  public static Boolean containsProfile(Block block, NodeProfile profile){
	  Boolean result = false;
	  for (Node node: block.getNodes()) {
		  if (node.getNodeProfile().equals(profile)) {
			  result = true;
			  break;
		  }
	  }
	  return result;
  }
  
  public static Boolean profileAndLabelMatch (Block block, NodeProfile profile, String label) {
	  Boolean result = false;
	  for (Node node: block.getNodes()) {
		  if (node.getNodeProfile().equals(profile) && node.getLabel().equals(label)) {
			  result = true;
			  break;
		  }
	  }
	  return result;
  }

  public static Boolean lastProfileAndLabelMatch (Block block, NodeProfile profile, String label) {
	  Node node = block.getNodes().get(block.getNodes().size() - 1);
	  if (node.getNodeProfile().equals(profile) && node.getLabel().equals(label)) {
		  return true;
	  } else {
		  return false;
	  }
  }
  
  private static Block findReferenceTarget (Block block) {
	  if (block.getParent() != null) {
		  return findReferenceTargetAux(referenceProfile(block), referenceLabel(block), block, getBlock(block.getParent()));
	  } else {
		  return null;
	  }
  }
  
  private static Block findReferenceTargetAux (NodeProfile profile, String label, Block childBlock, Block block) {
	  if (block == null || block.getBranchType().equals("PARALLEL")) {
		  return null;
	  } else if (lastProfileAndLabelMatch(block, profile, label)) {
		  return block;
	  } else {
		  Block result = null;
		  for (Integer i: block.getChildren()) {
			  if (!getBlock(i).equals(childBlock)) {
				  result = findReferenceTargetAuxAux(profile, label, getBlock(i));
				  if (result != null) {
					  break;
				  }
			  }
		  }
		  if (result == null) {
			  return findReferenceTargetAux(profile, label, block, getBlock(block.getParent()));
		  } else {
			  return result;
		  }
	  }
  }
  
  private static Block findReferenceTargetAuxAux (NodeProfile profile, String label, Block block) {
	  if (block.getChildren().size() == 0) {
		  return null;
	  } else if (lastProfileAndLabelMatch(block, profile, label)) {
		  return block;
	  } else if (block.getBranchType().equals("PARALLEL")) {
		  return null;
	  } else {
		  Block result = null;
		  for (Integer i: block.getChildren()) {
			  result = findReferenceTargetAuxAux(profile, label, getBlock(i));
			  if (result != null) {
				  break;
			  }
		  }
		  return result;
	  }
  }
  
  public static Block reversionOrReferenceTarget(Block block) {
	  Block result = block;
	  if (block.containsReversion()) {
		  // Need to take into account label
		  NodeProfile reversionProfile = reversionProfile(block);
		  String reversionLabel = reversionLabel(block);
		  for (Integer i = block.getParent(); i != null; i = getBlock(i).getParent()) {
			  if (profileAndLabelMatch(getBlock(i), reversionProfile, reversionLabel)) {
				  result = getBlock(i);
				  break;
			  }
		  }
	  } else if (block.containsReference()) {
		  // Need to find ref (left subtree)
		  result = findReferenceTarget(block);
		  if (result == null) {
			  result = block;
		  }
	  }
	  return result;
  }
  
  public static Boolean isChosenCPBlock (Block block) {
	  Boolean result = false;
	  for (Node node: block.getNodes()) {
		  if (isChosenCP(node.getNodeProfile())) {
			  result = true;
			  break;
		  }
	  }
	  return result;
  }
  
  public static Block equivalentStartingBlock (Block block) {
	  Block result = reversionOrReferenceTarget(block);
	  if (!isChosenCPBlock(result)) {
		  result = block;
	  }
	  return result;
  }
  
}
