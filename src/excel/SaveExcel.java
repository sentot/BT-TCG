package excel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Date;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileNameExtensionFilter;

import core.Main;
import jxl.SheetSettings;
import jxl.Workbook;
import jxl.format.Colour;
import jxl.format.PageOrientation;
import jxl.write.Label;
import jxl.write.WritableCellFormat;
import jxl.write.WritableFont;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;
import other.TestCase;
import tree.Block;
import tree.Node;
import tree.NodeProfile;

public class SaveExcel extends JMenuItem {
	
	private static final long serialVersionUID = 1L;
	
	private Boolean nodeIsPrecondition (Node node) {
		return node.getTag().endsWith("++");
	}

	private Boolean nodeIsIncluded (Node node) {
		return node.getTag().endsWith(" +");
	}
	
	private void saveDocumentation() {

	    JFrame parentFrame = new JFrame();

	    JFileChooser fileChooser = new JFileChooser() {
	        private static final long serialVersionUID = 1L;
	    	
	      @Override
	      public void approveSelection() {
	        File f = getSelectedFile();
	        if (f.exists()) {
	          Integer result = JOptionPane.showConfirmDialog(this, "The file exists, overwrite?",
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
	        new FileNameExtensionFilter("Excel file (.xls)", new String[] {"xls"});
	    fileChooser.addChoosableFileFilter(filter);
	    fileChooser.setFileFilter(filter);

	    fileChooser.setDialogTitle("Specify a file to save");

	    int userSelection = fileChooser.showSaveDialog(parentFrame);

	    if (userSelection == JFileChooser.APPROVE_OPTION) {

	      File fileToSave = fileChooser.getSelectedFile();
	      String filename = fileToSave.getAbsolutePath();
	      if (fileToSave.getAbsolutePath().endsWith(".xls") == false) {
	        filename = filename + ".xls";
	      }
	      System.out.println("Save as file: " + filename);
	      
	      try {
	        
	    	WritableWorkbook wb = Workbook.createWorkbook(new File(filename));
	    	
	    	WritableSheet sheet = wb.createSheet("Generated Use Case", 0); 

			wb.setColourRGB(Colour.LIGHT_TURQUOISE2, 208, 234, 255);

			// Test generation info
			
			WritableCellFormat infoFormat=new WritableCellFormat();
			infoFormat.setAlignment(jxl.format.Alignment.JUSTIFY);
			infoFormat.setWrap(true);
			sheet.mergeCells(0, 0, 2, 0);
			Date date = new Date();
			String btPathName = Main.getBTFilePath();
			sheet.addCell(new Label(0,0,"Use cases generated from BT source: " + btPathName + ".\n" +
			"Generated on " + date.toString() + ".\n",infoFormat));
			
			sheet.setColumnView(0,5);
			sheet.setColumnView(1,55);
			sheet.setColumnView(2,25);
			
			// Table headings
			
			WritableFont headerFont=new WritableFont(WritableFont.ARIAL,10,WritableFont.BOLD);
			WritableCellFormat headerFormat=new WritableCellFormat(headerFont);

			headerFormat.setAlignment(jxl.format.Alignment.CENTRE);
			headerFormat.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			headerFormat.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			headerFormat.setBackground(Colour.LIGHT_TURQUOISE2);
			headerFormat.setWrap(false);

			sheet.addCell(new Label(0,1,"ID",headerFormat));
			sheet.addCell(new Label(1,1,"Function",headerFormat));
			sheet.addCell(new Label(2,1,"Comments",headerFormat));

			// Entries
			

			WritableCellFormat cellFormat = new WritableCellFormat();

			cellFormat.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			cellFormat.setAlignment(jxl.format.Alignment.CENTRE);
			cellFormat.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			cellFormat.setWrap(false);

			WritableCellFormat cellFormat1 = new WritableCellFormat();

			cellFormat1.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			cellFormat1.setAlignment(jxl.format.Alignment.JUSTIFY);
			cellFormat1.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			cellFormat1.setWrap(false);

			WritableCellFormat cellFormat2 = new WritableCellFormat();

			cellFormat2.setVerticalAlignment(jxl.format.VerticalAlignment.TOP);
			cellFormat2.setAlignment(jxl.format.Alignment.JUSTIFY);
			cellFormat2.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			cellFormat2.setWrap(true);
			

			Integer nextRow = 2;
			Integer id = 1;
			for (TestCase tc: Main.getTestCases()) {
				
				Integer savedRow = nextRow;
				
				Boolean startFound = false;
				
				Integer n = nextRow;
								
				for (Integer i: (tc.getSteps())) {
					for (Node node: Main.getBlock(i).getNodes()) {
						if (startFound) {
						if (Main.userAction(node).equals("") == false) {
							if (nodeIsPrecondition(node)) {
								String ua = "To perform this function, first: " + Main.userAction(node);
								sheet.addCell(new Label(1,nextRow,ua,cellFormat2));
								nextRow++;
							} else if (nodeIsIncluded(node)) {
								String ua = Main.userAction(node);
								sheet.addCell(new Label(1,nextRow,ua,cellFormat2));
								nextRow++;
							}
						} else if (Main.observableResponse(node).equals("") == false) {
							if (nodeIsPrecondition(node)) {
								String or = "This function only applies if " + Main.observableResponse(node);
								sheet.addCell(new Label(1,nextRow,or,cellFormat2));
								nextRow++;
							} else if (nodeIsIncluded(node)) {
								String or = Main.observableResponse(node);
								sheet.addCell(new Label(1,nextRow,or,cellFormat2));
								nextRow++;
							}
						}
						}
					}
					startFound = true;
				}

				if (nextRow > savedRow) {

					sheet.addCell(new Label(0,n,id + "",cellFormat));
					
					sheet.mergeCells(0, n, 0, nextRow-1);
				
					sheet.addCell(new Label(2,n,"",cellFormat1));
					sheet.mergeCells(2, n, 2, nextRow-1);
				
				}
				
				id++;
				
			}

	        
	        // prepare to close
			SheetSettings setting = sheet.getSettings();  
			setting.setOrientation(PageOrientation.LANDSCAPE);  
			wb.write();
			wb.close(); 

			
	      } catch (Exception e) {

	      }
	    }
		
	}
	
	private void save (Boolean extraInformationFlag) {
		
	    JFrame parentFrame = new JFrame();

	    JFileChooser fileChooser = new JFileChooser() {
	        private static final long serialVersionUID = 1L;
	    	
	      @Override
	      public void approveSelection() {
	        File f = getSelectedFile();
	        if (f.exists()) {
	          Integer result = JOptionPane.showConfirmDialog(this, "The file exists, overwrite?",
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
	        new FileNameExtensionFilter("Excel file (.xls)", new String[] {"xls"});
	    fileChooser.addChoosableFileFilter(filter);
	    fileChooser.setFileFilter(filter);

	    fileChooser.setDialogTitle("Specify a file to save");

	    int userSelection = fileChooser.showSaveDialog(parentFrame);

	    if (userSelection == JFileChooser.APPROVE_OPTION) {

	      File fileToSave = fileChooser.getSelectedFile();
	      String filename = fileToSave.getAbsolutePath();
	      if (fileToSave.getAbsolutePath().endsWith(".xls") == false) {
	        filename = filename + ".xls";
	      }
	      System.out.println("Save as file: " + filename);
	      
	      try {
	        
	    	WritableWorkbook wb = Workbook.createWorkbook(new File(filename));
	    	
	    	WritableSheet sheet = wb.createSheet("Generated Test Case", 0); 

			wb.setColourRGB(Colour.LIGHT_TURQUOISE2, 208, 234, 255);

			// Test generation info
			
			WritableCellFormat infoFormat=new WritableCellFormat();
			infoFormat.setAlignment(jxl.format.Alignment.JUSTIFY);
			infoFormat.setWrap(true);
			sheet.mergeCells(0, 0, 15, 0);
			Date date = new Date();
			String btPathName = Main.getBTFilePath();
			sheet.addCell(new Label(0,0,"Test cases generated from BT source: " + btPathName + ".\n" +
			"Generated on " + date.toString() + ".\n",infoFormat));
			
			sheet.setColumnView(0,5);
			sheet.setColumnView(1,20);
			sheet.setColumnView(2,20);
			
			sheet.setColumnView(3,5);
			sheet.setColumnView(4,10);
			sheet.setColumnView(5,10);
			sheet.setColumnView(6,5);
			
			sheet.setColumnView(7,5);
			sheet.setColumnView(8,10);
			sheet.setColumnView(9,10);
			sheet.setColumnView(10,5);

			sheet.setColumnView(11,5);
			sheet.setColumnView(12,10);
			sheet.setColumnView(13,10);
			sheet.setColumnView(14,5);
			
			sheet.setColumnView(15,25);
			
			// Table headings
			
			WritableFont headerFont=new WritableFont(WritableFont.ARIAL,10,WritableFont.BOLD);
			WritableCellFormat headerFormat=new WritableCellFormat(headerFont);

			headerFormat.setAlignment(jxl.format.Alignment.CENTRE);
			headerFormat.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			headerFormat.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			headerFormat.setBackground(Colour.LIGHT_TURQUOISE2);
			headerFormat.setWrap(false);

			sheet.addCell(new Label(0,1,"ID",headerFormat));
			sheet.addCell(new Label(1,1,"Start",headerFormat));
			sheet.addCell(new Label(2,1,"End",headerFormat));
			sheet.addCell(new Label(3,1,"Preamble",headerFormat));
			sheet.mergeCells(3, 1, 6, 1);
			sheet.addCell(new Label(7,1,"Main Test",headerFormat));
			sheet.mergeCells(7, 1, 10, 1);
			sheet.addCell(new Label(11,1,"Postamble",headerFormat));
			sheet.mergeCells(11, 1, 14, 1);
			sheet.addCell(new Label(15,1,"Comments",headerFormat));

			sheet.mergeCells(0, 1, 0, 2);
			sheet.mergeCells(1, 1, 1, 2);
			sheet.mergeCells(2, 1, 2, 2);

			sheet.addCell(new Label(3,2,"Action (A)",headerFormat));
			sheet.mergeCells(3, 2, 4, 2);
			sheet.addCell(new Label(5,2,"Observable (O)",headerFormat));
			sheet.mergeCells(5, 2, 6, 2);
			sheet.addCell(new Label(7,2,"Action (A)",headerFormat));
			sheet.mergeCells(7, 2, 8, 2);
			sheet.addCell(new Label(9,2,"Observable (O)",headerFormat));
			sheet.mergeCells(9, 2, 10, 2);
			sheet.addCell(new Label(11,2,"Action (A)",headerFormat));
			sheet.mergeCells(11, 2, 12, 2);
			sheet.addCell(new Label(13,2,"Observable (O)",headerFormat));
			sheet.mergeCells(13, 2, 14, 2);

			sheet.mergeCells(15, 1, 15, 2);
			
			// Entries
			

			WritableCellFormat cellFormat = new WritableCellFormat();

			cellFormat.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			cellFormat.setAlignment(jxl.format.Alignment.CENTRE);
			cellFormat.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			cellFormat.setWrap(false);

			WritableCellFormat cellFormat1 = new WritableCellFormat();

			cellFormat1.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			cellFormat1.setAlignment(jxl.format.Alignment.JUSTIFY);
			cellFormat1.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			cellFormat1.setWrap(false);

			WritableCellFormat cellFormat2 = new WritableCellFormat();

			cellFormat2.setVerticalAlignment(jxl.format.VerticalAlignment.TOP);
			cellFormat2.setAlignment(jxl.format.Alignment.JUSTIFY);
			cellFormat2.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			cellFormat2.setWrap(true);
			

			Integer nextRow = 3;
			Integer id = 1;
			for (TestCase tc: Main.getTestCases()) {
				
				String startBlock = Main.getBlock(tc.getStart()).toString();
				String endBlock = Main.getBlock(tc.getEnd()).toString();
				
				Integer savedRow = nextRow;
				
				Boolean startFound = false;
				
				Integer nextPreamble = nextRow;
				Integer nextPostamble = nextRow;
				Integer n = nextRow;
				
				for (Integer i: (tc.getBlocksBefore())) {
					for (Node node: Main.getBlock(i).getNodes()) {
						if (Main.userAction(node).equals("") == false) {
							sheet.addCell(new Label(3,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(4,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(5,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(6,nextPreamble,"A",headerFormat));
							String ua = Main.userAction(node);
							sheet.mergeCells(3, nextPreamble, 5, nextPreamble);
							sheet.addCell(new Label(3,nextPreamble,ua,cellFormat2));
							nextPreamble++;
						} else if (extraInformationFlag && Main.observableResponse(node).equals("") == false) {
							sheet.addCell(new Label(3,nextPreamble,"O",headerFormat));
							sheet.addCell(new Label(4,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(5,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(6,nextPreamble,"",infoFormat));
							String or = Main.observableResponse(node);
							sheet.mergeCells(4, nextPreamble, 6, nextPreamble);
							sheet.addCell(new Label(4,nextPreamble,or,cellFormat2));
							nextPreamble++;
						}
					}
				}
				if (!extraInformationFlag) {
					for (Node node: Main.getBlock(tc.getSteps().get(0)).getNodes()) {
						if (Main.userAction(node).equals("") == false) {
							sheet.addCell(new Label(3,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(4,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(5,nextPreamble,"",infoFormat));
							sheet.addCell(new Label(6,nextPreamble,"A",headerFormat));
							String ua = Main.userAction(node);
							sheet.mergeCells(3, nextPreamble, 5, nextPreamble);
							sheet.addCell(new Label(3,nextPreamble,ua,cellFormat2));
							nextPreamble++;
						}
					}
				}
				
				for (Integer i: (tc.getSteps())) {
					for (Node node: Main.getBlock(i).getNodes()) {
						if (startFound || extraInformationFlag) {
						if (Main.userAction(node).equals("") == false) {
							sheet.addCell(new Label(7,nextRow,"",infoFormat));
							sheet.addCell(new Label(8,nextRow,"",infoFormat));
							sheet.addCell(new Label(9,nextRow,"",infoFormat));
							sheet.addCell(new Label(10,nextRow,"A",headerFormat));
							String ua = Main.userAction(node);
							sheet.mergeCells(7, nextRow, 9, nextRow);
							sheet.addCell(new Label(7,nextRow,ua,cellFormat2));
							nextRow++;
						} else if (Main.observableResponse(node).equals("") == false) {
							sheet.addCell(new Label(7,nextRow,"O",headerFormat));
							sheet.addCell(new Label(8,nextRow,"",infoFormat));
							sheet.addCell(new Label(9,nextRow,"",infoFormat));
							sheet.addCell(new Label(10,nextRow,"",infoFormat));
							String or = Main.observableResponse(node);
							sheet.mergeCells(8, nextRow, 10, nextRow);
							sheet.addCell(new Label(8,nextRow,or,cellFormat2));	
							nextRow++;
						}
						}
//						if (node.getNodeProfile().equals(startProfile)) {
//							startFound = true;
//						}
					}
					startFound = true;
				}

				for (Integer i: tc.getBlocksAfter()) {
					for (Node node: Main.getBlock(i).getNodes()) {
						if (Main.userAction(node).equals("") == false) {
							sheet.addCell(new Label(11,nextPostamble,"",infoFormat));
							sheet.addCell(new Label(12,nextPostamble,"",infoFormat));
							sheet.addCell(new Label(13,nextPostamble,"",infoFormat));
							sheet.addCell(new Label(14,nextPostamble,"A",headerFormat));
							String ua = Main.userAction(node);
							sheet.mergeCells(11, nextPostamble, 13, nextPostamble);
							sheet.addCell(new Label(11,nextPostamble,ua,cellFormat2));
							nextPostamble++;
						} else if (extraInformationFlag && Main.observableResponse(node).equals("") == false) {
							sheet.addCell(new Label(11,nextPostamble,"O",headerFormat));
							sheet.addCell(new Label(12,nextPostamble,"",infoFormat));
							sheet.addCell(new Label(13,nextPostamble,"",infoFormat));
							sheet.addCell(new Label(14,nextPostamble,"",infoFormat));
							String or = Main.observableResponse(node);
							sheet.mergeCells(12, nextPostamble, 14, nextPostamble);
							sheet.addCell(new Label(12,nextPostamble,or,cellFormat2));
							nextPostamble++;
						}
					}
				}
				
				Integer largest = nextRow;
				if (nextPreamble > largest) {
					largest = nextPreamble;
				}
				if (nextPostamble > largest) {
					largest = nextPostamble;
				}
				
				if (largest > savedRow) {

					sheet.addCell(new Label(0,n,id + "",cellFormat));
					sheet.addCell(new Label(1,n,startBlock,cellFormat1));
					sheet.addCell(new Label(2,n,endBlock,cellFormat1));
					
					if (largest > nextPreamble) {
						sheet.addCell(new Label(3,nextPreamble,"",cellFormat2));
						sheet.mergeCells(3, nextPreamble, 6, largest-1);
						}
					if (largest > nextRow) {
						sheet.addCell(new Label(7,nextRow,"",cellFormat2));
						sheet.mergeCells(7, nextRow, 10, largest-1);
						nextRow = largest;
						}
					if (largest > nextPostamble) {
						sheet.addCell(new Label(11,nextPostamble,"",cellFormat2));
						sheet.mergeCells(11, nextPostamble, 14, largest-1);
						}
				
					sheet.mergeCells(0, n, 0, nextRow-1);
					sheet.mergeCells(1, n, 1, nextRow-1);
					sheet.mergeCells(2, n, 2, nextRow-1);
				
					sheet.addCell(new Label(15,n,"",cellFormat1));
					sheet.mergeCells(15, n, 15, nextRow-1);
				
				}
				
				id++;
				
			}

	        
	        // prepare to close
			SheetSettings setting = sheet.getSettings();  
			setting.setOrientation(PageOrientation.LANDSCAPE);  
			wb.write();
			wb.close(); 

			
	      } catch (Exception e) {

	      }
	    }
		
	}
	
	
	public SaveExcel (Boolean extra, Boolean documentation) {
		if (extra && !documentation) {
			this.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if (Main.testCasesGenerated()) {
						save(true);
					}
				}
			});
			this.setText("add extra information (debugging)");
		} else if (documentation) {
			this.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if (Main.testCasesGenerated()) {
						saveDocumentation();
					}
				}
			});
			this.setText("simplified format (documentation)");
		} else {
			this.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if (Main.testCasesGenerated()) {
						save(false);
					}
				}
			});
			this.setText("default format (human testing)");
		}
	}
	
}
