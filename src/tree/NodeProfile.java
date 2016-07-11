package tree;

public class NodeProfile implements Comparable<NodeProfile> {
	
	  private String component;
	  private String behaviourType;
	  private String behaviour;
	  private Boolean killFlag;
	  
	  public NodeProfile(String comp, String behType, String beh, Boolean kFlag) {
		  component = comp;
		  behaviourType = behType;
		  behaviour = beh;
		  killFlag = kFlag;
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

	  public Boolean getKillFlag() {
	    return killFlag;
	  }

	  @Override
	  public int compareTo(NodeProfile otherNodeProfile) {
	    int result = getComponent().compareTo(otherNodeProfile.getComponent());
	    if (result == 0) {
	      result = getBehaviour().compareTo(otherNodeProfile.getBehaviour());
	    }
	    if (result == 0) {
	      result = getBehaviourType().compareTo(otherNodeProfile.getBehaviourType());
	    }
	    return result;
	  }

	  @Override
	  public boolean equals(Object otherNodeProfile) {
	    if (otherNodeProfile instanceof NodeProfile) {
	      boolean result = getKillFlag().equals(((NodeProfile) otherNodeProfile).getKillFlag());
	      if (result) {
	        result = getComponent().equals(((NodeProfile) otherNodeProfile).getComponent());
	      }
	      if (result) {
	        result = getBehaviour().equals(((NodeProfile) otherNodeProfile).getBehaviour());
	      }
	      if (result) {
	        result = getBehaviourType().equals(((NodeProfile) otherNodeProfile).getBehaviourType());
	      }
	      return result;
	    } else {
	      return false;
	    }
	  }
	  
	  public String toString() {
		  String openParen = "";
		  String closeParen = "";
		  String flag = "";
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
		  
		  if (getKillFlag()) {
			  flag = " --";
		  }
		  
		  return getComponent() + " " + openParen + getBehaviour() + closeParen + flag;
	  }

	  
}
