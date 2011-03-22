import java.awt.{Color=>JColor}
import java.awt.Color._

class Color(val name:String, val r:Int, val g:Int, val b:Int) {
	
}

object Color {
	implicit def wrapColor(ac:JColor) = Color(ac)
	
	def apply(name:String, r:Int, g:Int, b:Int):Color = new Color(name, r, g, b);
	def apply(ac : JColor):Color = Color(ac.toString, ac.getRed, ac.getGreen, ac.getBlue)
	
	val Rainbow = Array[Color](red, orange, yellow, green, blue, Color("purple",75,0,130), Color("violet", 238,130,238))
	
	def rainbowSequence() : Stream[Color] = Stream.continually(Rainbow.toStream).flatten
}