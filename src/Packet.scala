/**
import java.io.OutputStream
 * This packet datastructure documents the possible commands you can send to the arduino. 
 * 
 * Every command is 4 bytes:
 *   index    red      green    blue     brightness
 *  (byte 1) (byte 2) (byte 3) (byte 4) (byte 5)
 * 
 * index: 0 -> 99 ( 2 strands x 50 bulbs )
 * 
 * 
 * for index values greater than 99 (byte goes up to 255):
 * 
 * index = 100 (bulb index 63 for strand 1 of 50, broadcast bulb)
 * index = 101 (bulb index 63 for strand 1 of 50, broadcast bulb)
 * 
 * You can send the color black for index 100 and 101 to clear the whole board.
 * 
 * For index = 254, this will write out the current buffer to the lights.
 * 
 *
 * Colors have 8 bits of range, so that means 16 colors (0->15)
 * 
 * @author martin@mbs3.org
 *
 */

import java.io.OutputStream

class Packet(bulb: Int, r: Int, g: Int, b: Int, a: Int) {
	
	def write(os: OutputStream) = {
		os.write(bulb);
		
		val r0 = downscale(r);
		val g0 = downscale(g);
		val b0 = downscale(b);
		val a0 = ((a/255.0)*204.0).asInstanceOf[Int] 
		
		os.write(if(r0 < 15) r else 15); // 1 byte each for 3 bytes = 64 colors
		os.write(if(g0 < 15) g else 15);
		os.write(if(b0 < 15) b else 15);
		os.write(if(a0 < 204) a else 204);
	}
	
	def downscale(c:Int) :Int = ((c/255.0)*16.0).asInstanceOf[Int] 
}
