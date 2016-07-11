package excel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
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
import other.TestSegment;
import tree.Node;

public class SavePathExcel extends JMenuItem {

	private static final long serialVersionUID = 1L;
	

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
	    	
	    	WritableSheet sheet = wb.createSheet("Generated Test Plan", 0); 

			wb.setColourRGB(Colour.LIGHT_TURQUOISE2, 208, 234, 255);

			// Test generation info
			
			WritableCellFormat infoFormat=new WritableCellFormat();
			infoFormat.setAlignment(jxl.format.Alignment.JUSTIFY);
			infoFormat.setWrap(true);
			sheet.mergeCells(0, 0, 3, 0);
			Date date = new Date();
			String btPathName = Main.getBTFilePath();
			sheet.addCell(new Label(0,0,"Test plan generated from BT source: " + btPathName + ".\n" +
			"Generated on " + date.toString() + ".\n",infoFormat));
			
			sheet.setColumnView(0,10);
			sheet.setColumnView(1,15);
			sheet.setColumnView(2,50);
			sheet.setColumnView(3,30);
			
			// Table headings
			
			WritableFont headerFont=new WritableFont(WritableFont.ARIAL,10,WritableFont.BOLD);
			WritableCellFormat headerFormat=new WritableCellFormat(headerFont);

			headerFormat.setAlignment(jxl.format.Alignment.CENTRE);
			headerFormat.setVerticalAlignment(jxl.format.VerticalAlignment.CENTRE);
			headerFormat.setBorder(jxl.format.Border.ALL, jxl.format.BorderLineStyle.MEDIUM, jxl.format.Colour.BLACK);
			headerFormat.setBackground(Colour.LIGHT_TURQUOISE2);
			headerFormat.setWrap(false);

			sheet.addCell(new Label(0,1,"Test Case ID",headerFormat));
			sheet.addCell(new Label(1,1,"Actions and Observations",headerFormat));
			sheet.mergeCells(1, 1, 2, 1);
			sheet.addCell(new Label(3,1,"Comments",headerFormat));

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
			
			for (TestSegment segment: Main.getTestPath()) {
				
				TestCase tc = segment.getTestCase();
				ArrayList<Integer> preamble = segment.getPreamble();
				Integer id = tc.getIndex();
				
				Integer savedRow = nextRow;
				
				Boolean startFound = false;
				Boolean preambleFound = false;
				
				for (Integer i: preamble) {
					preambleFound = true;
					for (Node node: Main.getBlock(i).getNodes()) {
						if (!Main.userAction(node).equals("")) {
							String ua = Main.userAction(node);
							sheet.addCell(new Label(1,nextRow,"Action:",cellFormat2));
							sheet.addCell(new Label(2,nextRow,ua,cellFormat2));
							nextRow++;
						} else if (extraInformationFlag && !Main.observableResponse(node).equals("")) {
							String or = Main.observableResponse(node);
							sheet.addCell(new Label(1,nextRow,"Observation:",cellFormat2));
							sheet.addCell(new Label(2,nextRow,or,cellFormat2));
							nextRow++;
						}
					}
				}
				for (Integer i: (tc.getSteps())) {
					if (startFound || preambleFound) {
						for (Node node: Main.getBlock(i).getNodes()) {
								if (!Main.userAction(node).equals("")) {
									String ua = Main.userAction(node);
									sheet.addCell(new Label(1,nextRow,"Action:",cellFormat2));
									sheet.addCell(new Label(2,nextRow,ua,cellFormat2));
									nextRow++;
								} else if ((startFound || extraInformationFlag) && !Main.observableResponse(node).equals("")) {
									String or = Main.observableResponse(node);
									sheet.addCell(new Label(1,nextRow,"Observation:",cellFormat2));
									sheet.addCell(new Label(2,nextRow,or,cellFormat2));
									nextRow++;
								}
							}
						}
					startFound = true;
				}

				
				if (nextRow > savedRow) {

					sheet.mergeCells(0, savedRow, 0, nextRow-1);
					sheet.addCell(new Label(0,savedRow,id.toString(),cellFormat));
					sheet.mergeCells(3, savedRow, 3, nextRow-1);
					sheet.addCell(new Label(3, savedRow, "", cellFormat1));
				
				}
				
				
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
	

	public SavePathExcel (Boolean extra) {

		this.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (Main.testPathGenerated()) {
					save(extra);
				}
			}
		});
		if (extra) {
			this.setText("add extra information (debugging)");
		} else {
			this.setText("default format (human testing)");
		}
	}
	
	
}
