/**
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
	
	override def toString : String = bulb.toString
}

object Packet {
	// Factory methods
	def apply(bulb: Int, r: Int, g: Int, b: Int, a: Int):Packet = new Packet(bulb, r, g, b, a)
	def apply(bulb: Int, r: Int, g: Int, b: Int):Packet = Packet(bulb, r, g, b, 255)
	def apply(bulb: Int, c:Color):Packet = Packet(bulb, c.r, c.g, c.b)
	

	/**
	 * For (x,y), the origin is (0,0), and x increments right, y increments down
	 * 
	 *    x ->
	 * 	y (0,0)(1,0)
	 *  | (0,1).......    (10,9)
	 *  v		  	(9,10)(10,10)
	 */
	val layout = Array (
					Array(	9	,	59	,	10	,	60	,	29	,	79	,	30	,	80	,	49	,	99	),
					Array(	8	,	58	,	11	,	61	,	28	,	78	,	31	,	81	,	48	,	98	),
					Array(	7	,	57	,	12	,	62	,	27	,	77	,	32	,	82	,	47	,	97	),
					Array(	6	,	56	,	13	,	63	,	26	,	76	,	33	,	83	,	46	,	96	),
					Array(	5	,	55	,	14	,	64	,	25	,	75	,	34	,	84	,	45	,	95	),
					Array(	4	,	54	,	15	,	65	,	24	,	74	,	35	,	85	,	44	,	94	),
					Array(	3	,	53	,	16	,	66	,	23	,	73	,	36	,	86	,	43	,	93	),
					Array(	2	,	52	,	17	,	67	,	22	,	72	,	37	,	87	,	42	,	92	),
					Array(	1	,	51	,	18	,	68	,	21	,	71	,	38	,	88	,	41	,	91	),
					Array(	0	,	50	,	19	,	69	,	20	,	70	,	39	,	89	,	40	,	90	)
						)
	
	def translate(x:Int,y:Int) = {
		layout(y % 10)(x % 10)
	}
}

object DrawPacket extends Packet(254,0,0,0,0) {}