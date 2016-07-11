package com;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.net.UnknownHostException;

import other.Constants;

public class SBCLPipe {

  private String os = "";

  public SBCLPipe() {
    if (System.getProperty("os.name").startsWith("Windows")) {
      os = "Windows";
    } else if (System.getProperty("os.name").startsWith("Mac")) {
      // Mac differ a bit from other unix (there is no gksudo in Mac)
      // osascript is used by Mac instead of gksudo
      os = "Mac";
    } else {
      os = "Unix";
    }
  }

  private Socket client;
  private BufferedReader input;
  private PrintStream output;

  public String sendCommand(String command) {
	  System.out.println("BT Analyser command: " + command);
    output.println(command);
    output.flush();
    String result = "";
    Boolean end = false;
    long start = System.currentTimeMillis();
    try {
      while (System.currentTimeMillis() - start <= Constants.timeout) {
        if (input.ready()) {
          result = result.concat(input.readLine());
          end = result.contains("</result>");
        }
        if (end) {
        	System.out.println("BT Analyser result: " + result);
          return result;
        }
      }
      if (!end) {
        return "error|5|";
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
    return "error|5|";
  }

  public String connect(String address, int port) throws InterruptedException, IOException {
    if (client == null || client.isClosed()) {
      System.out.println("starting server");
      startServer();
      if (os == "Windows") {
    	  Thread.sleep(2000);
      } else {
    	  // Allow time for user to enter password
          Thread.sleep(10000);
      }
      System.out.println("connecting");
      return connectIOBuffers(address, port);
    } else {
      return "success";
    }
  }

  private void startServer() throws IOException {
	  if (os == "Windows") {
    	  Runtime.getRuntime().exec("cmd /c start cmd.exe /K \"cd BTAnalyser && start.cmd\"");
      } else if (os == "Mac") {
    	  Runtime.getRuntime().exec(new String[]{"osascript","-e","do shell script \"./start.sh &>/dev/null &\" with administrator privileges"});
      } else {
    	  /*
    	   * Assume it is a unix system with gksudo available.
    	   */
    	  Runtime.getRuntime().exec(new String[]{"bash","-c","gksudo ./start.sh"});
      }
  }


  private String connectIOBuffers(String address, int port) {
    try {
      client = new Socket(address, port);
      input = new BufferedReader(new InputStreamReader(client.getInputStream()));
      output = new PrintStream(client.getOutputStream());
      return "success";
    } catch (UnknownHostException e) {
      return "error|0|" + e.getMessage();
    } catch (IOException e) {
      return "error|1|" + e.getMessage();
    }
  }

  public void close() throws IOException {
	  if (client != null) {
		  output.println("(close-connection)");
		  output.flush();
		  input.close();
		  output.close();
		  client.close();
	  }
  }
}
