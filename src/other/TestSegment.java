package other;

import java.util.ArrayList;

public class TestSegment {

	  private ArrayList<Integer> preamble;
	  private TestCase testCase;
	  private Integer realStart;
	  
	  public TestSegment(ArrayList<Integer> preambleParam, TestCase testCaseParam) {
		  this.preamble = new ArrayList<Integer>(preambleParam);
		  this.testCase = testCaseParam;
	      this.realStart = testCaseParam.getStart();
	  }
	  
	  public ArrayList<Integer> getPreamble() {
		  return preamble;
	  }
	  
	  public void setPreamble(ArrayList<Integer> preambleParam) {
		  preamble = new ArrayList<Integer>(preambleParam);
	  }
	  
	  public TestCase getTestCase() {
		  return testCase;
	  }
	  
	  public void setTestCase(TestCase testCaseParam) {
		  testCase = testCaseParam;
	  }
	  
	  public Integer getRealStart() {
		  return realStart;
	  }
	  
	  public void setRealStart(Integer value) {
		  realStart = value;
	  }
}
