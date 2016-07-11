package tree;

import other.Constants;

public class Node implements Comparable<Node> {
  //private Integer tag;
  private String tag;
  private String component;
  private String behaviourType;
  private String behaviour;
  private String flag;
  private String label;
  private Integer blockIndex;
  private Boolean noi = false;
  private Boolean cp = false;
  private String action = "";
  private Boolean preamble = false;
  private String observable = "";
  private NodeProfile profile;


  public Node(String tagParam, String componentParam, String behaviourTypeParam,
      String behaviourParam, String flagParam, Integer blockIndexParam, String labelParam) {
    tag = tagParam;
    label = labelParam;
    component = componentParam;
    behaviourType = behaviourTypeParam;
    behaviour = behaviourParam;
    if (flagParam == null) {
      flag = "";
    } else {
      flag = flagParam;
    }
    blockIndex = blockIndexParam;
  }

  public String getTag() {
    return tag;
  }

  public String getLabel() {
    return label;
  }

  public String getComponent() {
    return component;
  }

  public String getBehaviour() {
    return behaviour;
  }

  public String getBehaviourType() {
    return behaviourType;
  }

  public String getFlag() {
    return flag;
  }
  
  public Integer getBlockIndex() {
    return blockIndex;
  }

  public Boolean isNoi() {
    return noi;
  }

  public void setNoi(Boolean noi) {
    this.noi = noi;
  }

  public Boolean isCp() {
    return cp;
  }

  public void setCp(Boolean cp) {
    this.cp = cp;
  }

  @Override
  public int compareTo(Node otherNode) {
    int result = getTag().compareTo(otherNode.getTag());
    if (result == 0) {
    	result = getLabel().compareTo(otherNode.getLabel());
    }
    if (result == 0) {
      result = getComponent().compareTo(otherNode.getComponent());
    }
    if (result == 0) {
      result = getBehaviour().compareTo(otherNode.getBehaviour());
    }
    if (result == 0) {
      result = getBehaviourType().compareTo(otherNode.getBehaviourType());
    }
    if (result == 0) {
      result = getFlag().compareTo(otherNode.getFlag());
    }
    return result;
  }

  @Override
  public boolean equals(Object otherNode) {
    if (otherNode instanceof Node) {
      boolean result = getTag().equals(((Node) otherNode).getTag());
      if (result) {
    	  result = getLabel().equals(((Node) otherNode).getLabel());
      }
      if (result) {
        result = getComponent().equals(((Node) otherNode).getComponent());
      }
      if (result) {
        result = getBehaviour().equals(((Node) otherNode).getBehaviour());
      }
      if (result) {
        result = getBehaviourType().equals(((Node) otherNode).getBehaviourType());
      }
      if (result) {
        result = getFlag().equals(((Node) otherNode).getFlag());
      }
      return result;
    } else {
      return false;
    }
  }

  public boolean equalsSimple(Node otherNode) {
    boolean result = getComponent().equals(otherNode.getComponent());
    if (result) {
      result = getBehaviour().equals(otherNode.getBehaviour());
    }
    if (result) {
      result = getBehaviourType().equals(otherNode.getBehaviourType());
    }
    if (result) {
    	if (getFlag().equals("KILL")) {
    		result = otherNode.getFlag().equals("KILL");
    	} else {
    		result = !otherNode.getFlag().equals("KILL");
    	}
    }
    return result;
  }
  
  public String simpleToString() {
	  String openParen = "";
	  String closeParen = "";
	  if (getBehaviourType().equals("STATE-REALISATION")) {
		  openParen = "[";
		  closeParen = "]";
	  } else if (getBehaviourType().equals("SELECTION")) {
		  openParen = "?";
		  closeParen = "?";
	  } else if (getBehaviourType().equals("GUARD")) {
		  openParen = "???";
		  closeParen = "???";
	  } else if (getBehaviourType().equals("EVENT")) {
		  openParen = "??";
		  closeParen = "??";
	  } else if (getBehaviourType().equals("INTERNAL-INPUT")) {
		  openParen = ">";
		  closeParen = "<";
	  } else if (getBehaviourType().equals("INTERNAL-OUTPUT")) {
		  openParen = "<";
		  closeParen = ">";
	  } else if (getBehaviourType().equals("EXTERNAL-INPUT")) {
		  openParen = ">>";
		  closeParen = "<<";
	  } else if (getBehaviourType().equals("EXTERNAL-OUTPUT")) {
		  openParen = "<<";
		  closeParen = ">>";
		  }
	  return getComponent() + " " + openParen + getBehaviour() + closeParen;
  }

  public String toString() {
	  String flag = "";
	  if (getFlag().equals("REVERSION")) {
		  flag = " ^";
	  } else if (getFlag().equals("REFERENCE")) {
		  flag = " =>";
	  } else if (getFlag().equals("KILL")) {
		  flag = " --";
	  } else if (getFlag().equals("SYNCHRONISE")) {
		  flag = " =";
	  }
	  String label = "";
	  if (!getLabel().isEmpty()) {
		  label = " " + getLabel();
	  }
	  return getTag() + " | " + simpleToString() + flag + label;
  }
  
  public NodeProfile getNodeProfile() {
	  Boolean killFlag = false;
	  if (profile == null) {
		  if (getFlag().equals("KILL")) {
			  killFlag = true;
			  }
		  return new NodeProfile(getComponent(),getBehaviourType(),getBehaviour(),killFlag);
	  } else {
		  return profile;
	  }
  }

  public void setNodeProfile(NodeProfile prof) {
	  profile = prof;
  }

  public String getAction() {
    return action;
  }

  public void setAction(String action) {
    this.action = action;
  }

  public String getObservable() {
    return observable;
  }

  public void setObservable(String observable) {
    this.observable = observable;
  }

  public Boolean getPreamble() {
    return preamble;
  }

  public void setPreamble(Boolean preamble) {
    this.preamble = preamble;
  }
}
