

import gnu.io.CommPort
import gnu.io.CommPortIdentifier
import gnu.io.SerialPort
import java.io._
import java.nio._
import java.io.InputStream
import java.io.OutputStream
import java.awt.image._
//import net.sourceforge.juint._

class TwoWaySerialComm {

  def connect(portName: String) {
    val portIdentifier = CommPortIdentifier.getPortIdentifier(portName);
    if (portIdentifier.isCurrentlyOwned()) {
      System.out.println("Error: Port is currently in use");
    } else {
      val commPort = portIdentifier.open(this.getClass().getName(), 2000);

      if (commPort.isInstanceOf[SerialPort]) {
        val serialPort = commPort.asInstanceOf[SerialPort];
        serialPort.setSerialPortParams(115200, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

        val in = serialPort.getInputStream();
        val out = serialPort.getOutputStream();

        (new Thread(new SerialReader(in))).start();
        (new Thread(new SerialWriter(out))).start();

      } else {
        System.out.println("Error: Only serial ports are handled by this example.");
      }
    }
  }
}

/** */
class SerialReader(val in: InputStream) extends Runnable {

  override def run() =
    {
      val buffer = new Array[Byte](1024)
      var len = 0;
      try {
        while (len > -1) {

          len = this.in.read(buffer)
          if (len > -1)
            System.out.print(new String(buffer, 0, len));
        }
      } catch {
        case e: IOException => e.printStackTrace();
      }
    }
}

class SerialWriter(val out: OutputStream) extends Runnable {
  override def run(): Unit = {
      try {
        var c: Int = 0;
        var stringBuffer = new StringBuffer
        while (c > -1) {
          c = System.in.read()
          
          if(c > -1 && c != 10) {
        	  stringBuffer.append(c.asInstanceOf[Char]);
          }
          
          if (c > -1 && c == 10) {
            val i = getValue(stringBuffer.toString)
            System.out.println(i);
            
            new Packet(i, 255, 0, 0, 255).write(out);
            new Packet(254, 0, 0, 0, 0).write(out);
            stringBuffer = new StringBuffer
          }
        }
      } catch {
        case e: IOException => e.printStackTrace();
      }
    }

  def getValue(s: String): Int = s match {
    case "inf" => Integer.MAX_VALUE
    case Int(x) => x
    case _ => error("not a number")
  }

  def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString

}

object Int {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.NumberFormatException => None
  }
}

object Driver extends Application {

  val en = CommPortIdentifier.getPortIdentifiers();
  while (en.hasMoreElements()) {
    var portId = en.nextElement().asInstanceOf[CommPortIdentifier];
    System.out.println(portId.getName());
  }

  (new TwoWaySerialComm()).connect("/dev/ttyUSB0");

}