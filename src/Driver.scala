import scala.collection.mutable

import gnu.io.CommPort
import gnu.io.CommPortIdentifier
import gnu.io.SerialPort

import java.util.concurrent._

import java.io._
import java.nio._

import java.io.InputStream
import java.io.OutputStream

import java.awt.Color._
import java.awt.{Color=>JColor}

class TwoWaySerialComm(val bq: BlockingQueue[Packet]) {

	var reader : Thread = new Thread
	var writer : Thread = new Thread
	
	def connect(portName: String) {
		val portIdentifier = CommPortIdentifier.getPortIdentifier(portName);
		if (portIdentifier.isCurrentlyOwned()) {
			System.out.println("Error: Port is currently in use");
		} else {
			val commPort = portIdentifier.open(this.getClass().getName(), 2000);

			if (commPort.isInstanceOf[SerialPort]) {
				val serialPort = commPort.asInstanceOf[SerialPort];
				serialPort.setSerialPortParams(4800, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

				val in = serialPort.getInputStream();
				val out = serialPort.getOutputStream();

				reader = (new Thread(new SerialReader(in))); reader.start
				writer = (new Thread(new SerialWriter(out, bq))); writer.start

			} else {
				System.out.println("Error: Only serial ports are handled by this example.");
			}
		}
	}
}

/** Just in case we hear something */
class SerialReader(val in: InputStream) extends Runnable {
	override def run() =
	{
		val buffer = new Array[Byte](1024)
		var len = 0;
		while (len > -1) {

			len = this.in.read(buffer)
			if (len > -1)
				System.out.print(new String(buffer, 0, len));
		}
	}
}

class SerialWriter(val out: OutputStream, val bq: BlockingQueue[Packet]) extends Runnable {
	override def run(): Unit = {
			while(true) {
				val p = bq.take();
				Thread.sleep(35);
				p.write(out);
				//System.out.println("Wrote a packet " + p)
				
			}
	}
}

object Int {
	def unapply(s: String): Option[Int] = try {
		Some(s.toInt)
	} catch {
	case _: java.lang.NumberFormatException => None
	}
}

object Driver {
	implicit def wrapColor(ac:JColor) = Color(ac)

	def getValue(s: String): Int = s match {
		case "inf" => Integer.MAX_VALUE
		case Int(x) => x
		case _ => error("not a number")
	}

	def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString

	def main(args: Array[String]) {

		val en = CommPortIdentifier.getPortIdentifiers();
		while (en.hasMoreElements()) {
			var portId = en.nextElement().asInstanceOf[CommPortIdentifier];
			System.out.println(portId.getName());
		}

		val bq = new LinkedBlockingQueue[Packet]
		val twsc = (new TwoWaySerialComm(bq)); twsc.connect("/dev/ttyUSB0");


		bq.offer(Packet(100, black)) // clear x 1
		bq.offer(Packet(101, black)) // clear x 2
		bq.offer(DrawPacket) // command "draw" packet        

		//readloop(bq);
		//rainbow(bq);
		do {
			showFonts(bq, "Testing123!?");
			Thread.sleep(3*1000);
		} while (false);
	}
	
	def showFonts(bq:BlockingQueue[Packet]) : Unit = showFonts(bq, Font.fontmap)
	
	def showFonts(bq:BlockingQueue[Packet], str:String) : Unit = { 
		var result = mutable.ArrayBuffer[FontCharacter]()
		
		for(letter <- str.toUpperCase) { 
			val matcher = Font.fontmap.find(fc => {fc.character == letter});
			if(matcher != None) {
				//System.out.println("Matcher " + matcher + " matched " + letter)
				result += matcher.get
			} else {
				result += Font.fontmap.find(fc => {fc.character == '?'}).get;
			}
			
		}
		
		showFonts(bq, result.toArray)
	}
	
	def showFonts(bq:BlockingQueue[Packet], fontLetters:Array[FontCharacter]) = {
		var colors = Color.rainbowSequence
		
		for(i <- 0 to fontLetters.length-1) {
			val fontLetter = fontLetters(i)
			val fontLetter_next = if(i+1 < fontLetters.length) Some(fontLetters(i+1)) else None
			
			//System.out.println("FontLetter="+fontLetter+",FontLetter_next="+fontLetter_next)
			
			// plus one so it scrolls off the screen
			for(j <- 10 to -5 by -1) {
				//System.out.println(j)
				bq.offer(Packet(100, black)) // clear x 1
				bq.offer(Packet(101, black)) // clear x 2

				
				val thisColor = colors(0); colors = colors.drop(1)
				val packs = fontLetter.toPacket(thisColor, j, 1)
				for(pack <- packs) {
					//System.out.println("Offer " + pack)
					bq.offer(pack)
				}
				
				if(false && j < 5 && !fontLetter_next.isEmpty) {
					val thisColor2 = colors(0); colors = colors.drop(1)
					val packs2 = fontLetter_next.get.toPacket(thisColor2, j+5, 1)
					for(pack2 <- packs2) {
						bq.offer(pack2)
					}
				}
				
				bq.offer(DrawPacket)
			}
		}
	}
	
	def rainbow(bq:BlockingQueue[Packet]) = {
		var colors = Color.rainbowSequence
		for(i <- 0 to 99) {
			val thisColor = colors(0); colors = colors.drop(1)
			bq.offer(Packet(i, thisColor))
		}
		bq.offer(DrawPacket); // command "draw" packet
	}
	
	def readloop(bq:BlockingQueue[Packet]) = {
		var c: Int = 0;
		var stringBuffer = new StringBuffer
		
		var colors = Color.rainbowSequence
		
		while (c > -1) {
			c = System.in.read()

			if(c > -1 && c != 10) {
				stringBuffer.append(c.asInstanceOf[Char]);
			}

			if (c > -1 && c == 10) {
				try {
					val i = getValue(stringBuffer.toString)
					System.out.println(i);
	
					for(x <- Packet.layout(i)) {
						val thisColor = colors(0)
						colors = colors.drop(1)
						
						bq.offer(Packet(x, thisColor))
					}
	
					bq.offer(Packet(254, 0, 0, 0, 0)); // command "draw" packet
				} catch {
					case _ => // do nothing
				}
				stringBuffer = new StringBuffer
			}

		}
	}
}