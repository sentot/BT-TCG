package other;

import java.util.ArrayList;
import java.util.Collection;

import core.Main;
import tree.Block;
import tree.Node;

public class TestCase {
  private ArrayList<Integer> blockList;
  private Integer index;
  private ArrayList<Node> nodeList;
  private Node startNode;
  private Node endNode;
  private ArrayList<Integer> blocksBefore = new ArrayList<Integer>();
  private ArrayList<Node> nodesBefore = new ArrayList<Node>();
  private ArrayList<Integer> blocksAfter = new ArrayList<Integer>();
  private ArrayList<Node> nodesAfter = new ArrayList<Node>();
  private Boolean isSelected = false;
  private Boolean isReachable = true;
  private Boolean needsPreamble = true;
  private Boolean loops = false;

  public TestCase(Collection<Integer> blockListParam, Collection<Node> nodeListParam) {
    this.blockList = new ArrayList<Integer>(blockListParam);
    this.nodeList = new ArrayList<Node>(nodeListParam);
    this.startNode = nodeList.get(0);
    this.endNode = nodeList.get(nodeList.size() - 1);

  }

  public Integer getIndex() {
    return index;
  }
  
  public void setIndex(Integer value) {
	  index = value;
  }

  public ArrayList<Integer> getSteps() {
    return blockList;
  }

  public ArrayList<Node> getNodeSteps() {
    return nodeList;
  }

  public Integer getLength() {
    return blockList.size();
  }

  public Integer getNodeLength() {
    return nodeList.size();
  }

  public Integer getStart() {
	  return blockList.get(0);
  }
  
  public Integer getEnd() {
	  return blockList.get(blockList.size()-1);
  }
  
  public Node getStartNode() {
    return startNode;
  }
  
  public Node getCPStartNode() {
	  Block b = Main.getBlock(blockList.get(0));
	  Node result = b.getNodes().get(0);
	  for (Node n: b.getNodes()) {
		  if (Main.isChosenCP(n.getNodeProfile())) {
			  result = n;
			  break;
		  }
	  }
	  return result;
  }

  public Node getEndNode() {
    return endNode;
  }

  public Node getCPEndNode() {
	  Block b = Main.getBlock(blockList.get(blockList.size() - 1));
	  Node result = b.getNodes().get(0);
	  for (Node n: b.getNodes()) {
		  if (Main.isChosenCP(n.getNodeProfile())) {
			  result = n;
			  break;
		  }
	  }
	  return result;
  }

  public void setStepsBefore(ArrayList<Integer> blocksBeforeParam, ArrayList<Node> nodesBeforeParam) {
    if (blocksBeforeParam != null) {
      blocksBefore = blocksBeforeParam;
      nodesBefore = nodesBeforeParam;
    }
  }

  public ArrayList<Integer> getBlocksBefore() {
    return blocksBefore;
  }

  public Integer getBlocksBeforeLength() {
    return blocksBefore.size();
  }

  public ArrayList<Node> getNodesBefore() {
    return nodesBefore;
  }

  public Integer getNodesBeforeLength() {
    return nodesBefore.size();
  }

  public void setStepsAfter(ArrayList<Integer> blocksAfterParam, ArrayList<Node> nodesAfterParam) {
    if (blocksAfterParam != null) {
      blocksAfter = blocksAfterParam;
      nodesAfter = nodesAfterParam;
    }
  }

  public ArrayList<Integer> getBlocksAfter() {
    return blocksAfter;
  }

  public int getBlocksAfterLength() {
    return blocksAfter.size();
  }

  public ArrayList<Node> getNodesAfter() {
    return nodesAfter;
  }

  public int getNodesAfterLength() {
    return nodesAfter.size();
  }

  public String toString() {
    return endNode + "" + blockList + "(" + getLength() + ")";
  }

  public Boolean isSelected() {
    return isSelected;
  }

  @Override
  public boolean equals(Object otherTC) {
    if (otherTC instanceof TestCase) {
      return getSteps().equals(((TestCase) otherTC).getSteps());
    } else {
      return false;
    }
  }

  public void setSelected(Boolean isSelected) {
    this.isSelected = isSelected;
  }

  public Boolean isReachable() {
    return isReachable;
  }

  public void setReachable(Boolean isReachable) {
    this.isReachable = isReachable;
  }
  
  public Boolean needsPreamble() {
	  return needsPreamble;
  }
  
  public void setPreamble(Boolean needsPreamble) {
	  this.needsPreamble = needsPreamble;
  }

  public Boolean isLooping() {
	  return loops;
  }
  
  public void setLooping(Boolean value) {
	  this.loops = value;
  }

}
