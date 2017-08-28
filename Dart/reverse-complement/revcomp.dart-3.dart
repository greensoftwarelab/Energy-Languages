/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Contributed by James Wendel
   Modified by Anders Johnsen
*/

import 'dart:io';

void main() {
  var src   = "CGATMKRYVBHD";
  var dst   = "GCTAKMYRBVDH";
  var tbl   = new List<int>(256);
  var seq   = new List<int>();
  
  // Set up lookup table
  for (int i = 0; i < tbl.length; i++)
    tbl[i] = i;
  
  for (int i = 0; i < src.length; i++) {
    tbl[src.codeUnitAt(i)]                = dst.codeUnitAt(i);
    tbl[src.toLowerCase().codeUnitAt(i)]  = dst.codeUnitAt(i);
  }

  var buffer = new List<int>(60);
  List<int> list = new List<int>();
  bool commentLine = false;
  StringBuffer sbuf = new StringBuffer();
   
  stdin.listen((List<int> dataList) {
    // Loop over all the contents of the buffer so far
    for (int data in dataList) {
      
      // Check if this is a comment line (and that we aren't already on a comment line)
      if (data == 62 && !commentLine) {
        int count = 0;
        
        // Print the reverse components for the last block 
        for (int g in list.reversed) {
          if (count == 60) {
            sbuf.writeln(new String.fromCharCodes(buffer));
            count=0;
          } 
          buffer[count++] = g;
        }
        // Print any stragling data
        if (count > 0) {
          sbuf.writeln(new String.fromCharCodes(buffer.getRange(0, count)));
        }
        // Reset the data for the begining of a block of data
        list.clear();
        commentLine = true;
      } 
        
      if (commentLine) {
        if (data == 10) {
          sbuf.write(new String.fromCharCodes(list));
          print(sbuf);
          sbuf = new StringBuffer();
          commentLine = false;
          list.clear();
        } else {
          list.add(data);
        }
      } else if (data != 10) {
          // Add the complement to the buffer
          list.add(tbl[data]);
      }
    }
  }).onDone(() {
    // Print out anything remaining in the buffers
    if (commentLine) {
      sbuf.write(new String.fromCharCodes(list));
    } else {
      int count = 0;
      for (int data in list.reversed) {
        if (count == 60) {
          sbuf.writeln(new String.fromCharCodes(buffer));
          count=0;
        } 
        buffer[count++] = data;
      }
      if (count > 0) {
        sbuf.write(new String.fromCharCodes(buffer.getRange(0, count)));
      }
    }
    print(sbuf);
  });
}