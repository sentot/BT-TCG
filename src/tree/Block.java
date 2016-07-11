package tree;

import java.util.List;

public class Block {

  private Integer parent;
  private List<Integer> children;
  private List<Node> nodes;
  private Integer index;
  private String branchType;


  public Block(Integer parentParam, List<Integer> childrenParam, Integer indexParam,
      String branchTypeParam, List<Node> nodesParam) {
    parent = parentParam;
    children = childrenParam;
    index = indexParam;
    branchType = branchTypeParam;
    nodes = nodesParam;
  }

  public Integer getParent() {
    return parent;
  }


  public List<Integer> getChildren() {
    return children;
  }

  public Integer getIndex() {
    return index;
  }
  
  public String getBranchType() {
    return branchType;
  }
  
  public List<Node> getNodes() {
    return nodes;
  }
  
  public Boolean containsReversion() {
	  Boolean result = false;
	  for (Node node: nodes) {
		  if (node.getFlag().equals("REVERSION")) {
			  result = true;
		  }
	  }
	  return result;
  }

  public Boolean containsReference() {
	  Boolean result = false;
	  if (children.size() == 0) {
		  for (Node node: nodes) {
			  if (node.getFlag().equals("REFERENCE")) {
				  result = true;
			  }
		  }
	  }
	  return result;
  }

  @Override
  public String toString() {
	  String result = "";
	  for (Node node: getNodes()) {
		  result = result + node.toString() + "\n";
	  }
	  return result;
  }
}
