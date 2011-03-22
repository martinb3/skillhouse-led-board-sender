
import scala.collection.mutable


class FontCharacter(val character : Char, lines : Array[String]) {

	override def toString() : String = "font-"+character.toString
	
	def toPacket(c: Color, fromx: Int, fromy: Int) : Array[Packet] = {
		val on = mutable.ArrayBuffer[Packet]()
		//System.out.println("Writing packet " + toString())
		
		for(y <- 0 to 6) {
			for(x <- 0 to 4) {
				if(y < lines.length && x < lines(y).length && lines(y)(x) != ' ') {
					on += Packet(Packet.translate(x+fromx,y+fromy),c);
				}
			}
		}
		
		on.toArray
	}
}

object Font {

	val fontmap = readFonts()
	
	def readFonts() : Array[FontCharacter] = {
		val fontc ="ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!? ";
		
		var list = mutable.ArrayBuffer[FontCharacter]()
		
		val height = 7; val width = 5; val spacer = 1
		
		val lines = scala.io.Source.fromFile("5x7.font").getLines.toArray
		for(i <- 0 to (fontc.length-1)) {
			val offset = i*7;
			
			val letterLines = lines.slice(offset, offset+7)
			val newFontC = new FontCharacter(fontc(i), letterLines)
			list += newFontC
			//System.out.println("Adding " + newFontC + " as offset "+offset+":")
			//letterLines.foreach(System.out.println(_))
			//System.out.flush()
		}
		
		list.toArray
	}
}