package other;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;

public final class Constants {

  public static final String version = "0.2";
  
  public static final String oldConfigVersion = "0.1";

  public static final String configTabName = "Test Configuration";
  
  public static final String noiTabName = "Nodes of Interest";

  public static final String cpTabName = "CheckPoints";

  public static final String observablesTabName = "Observable Responses";

  public static final String userActionsTabName = "User Actions/External Inputs";

  public static final String testPlannerTabName = "Test Planner";

  public static final String htmlTabSpacing = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";

  public static final int timeout = 120000;

  public static final Color cellChosenBG = new Color(100, 215, 115);

  public static final Color cellNotChosenBG = new Color(255, 255, 255);

  public static final Color cellUnavailableBG = new Color(100, 215, 115);
  
  //public static final Color selectedTestCaseColour = new Color(50, 190, 235);
  // change to light blue-green
  public static final Color selectedTestCaseColour = new Color(204, 255, 255);
  public static final Color notSelectedTestCaseColour = new Color(255, 255, 255);
  public static final Color needsPreambleColour = new Color(235, 235, 60);
  public static final Color unavailableColour = new Color(225, 30, 75);
  public static final Color immediatelyAvailableColour = new Color(50, 235, 95);
  

  // 8 spaces
  public static final String tabSpacing = "        ";

  // INTERNAL-OUTPUT no longer allowed as OR
  public static final ArrayList<String> acceptedORBehaviourTypes = new ArrayList<String>(
      Arrays.asList("STATE-REALISATION", "EXTERNAL-OUTPUT"));

  public static final ArrayList<String> acceptedUABehaviourTypes =
      new ArrayList<String>(Arrays.asList("EXTERNAL-INPUT", "EVENT"));
  
  public static final ArrayList<String> acceptedUAFlags =
      new ArrayList<String>(Arrays.asList("REVERSION", "KILL", "SYNCHRONISATION", ""));
  
  public static final ArrayList<String> acceptedORFlags =
      new ArrayList<String>(Arrays.asList("REVERSION", "KILL", "SYNCHRONISATION", ""));

  public static final ArrayList<String> nodeFlags =
      new ArrayList<String>(Arrays.asList("REVERSION", "REFERENCE", "KILL", "SYNCHRONISATION"));

  public static final ArrayList<String> nodeFlagSymbols =
      new ArrayList<String>(Arrays.asList("^", "=>", "--", "="));
}
