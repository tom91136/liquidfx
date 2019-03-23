

val xs = -1 :: 0 :: 1 :: Nil

val ys = (for{
	z <- xs
	y <- xs
	x <- xs
} yield s"(char3)(+$x,+$y,+$z)").grouped(3).mkString(",\n")


val ns = "SWAP(1, 2)\n\tSWAP(0, 2)\n\tSWAP(0, 1)\n\tSWAP(4, 5)\n\tSWAP(3, 5)\n\tSWAP(3, 4)\n\tSWAP(0, 3)\n\tSWAP(1, 4)\n\tSWAP(2, 5)\n\tSWAP(2, 4)\n\tSWAP(1, 3)\n\tSWAP(2, 3)\n\tSWAP(7, 8)\n\tSWAP(6, 8)\n\tSWAP(6, 7)\n\tSWAP(9, 10)\n\tSWAP(11, 12)\n\tSWAP(9, 11)\n\tSWAP(10, 12)\n\tSWAP(10, 11)\n\tSWAP(6, 10)\n\tSWAP(6, 9)\n\tSWAP(7, 11)\n\tSWAP(8, 12)\n\tSWAP(8, 11)\n\tSWAP(7, 9)\n\tSWAP(8, 10)\n\tSWAP(8, 9)\n\tSWAP(0, 7)\n\tSWAP(0, 6)\n\tSWAP(1, 8)\n\tSWAP(2, 9)\n\tSWAP(2, 8)\n\tSWAP(1, 6)\n\tSWAP(2, 7)\n\tSWAP(2, 6)\n\tSWAP(3, 10)\n\tSWAP(4, 11)\n\tSWAP(5, 12)\n\tSWAP(5, 11)\n\tSWAP(4, 10)\n\tSWAP(5, 10)\n\tSWAP(3, 7)\n\tSWAP(3, 6)\n\tSWAP(4, 8)\n\tSWAP(5, 9)\n\tSWAP(5, 8)\n\tSWAP(4, 6)\n\tSWAP(5, 7)\n\tSWAP(5, 6)\n\tSWAP(14, 15)\n\tSWAP(13, 15)\n\tSWAP(13, 14)\n\tSWAP(16, 17)\n\tSWAP(18, 19)\n\tSWAP(16, 18)\n\tSWAP(17, 19)\n\tSWAP(17, 18)\n\tSWAP(13, 17)\n\tSWAP(13, 16)\n\tSWAP(14, 18)\n\tSWAP(15, 19)\n\tSWAP(15, 18)\n\tSWAP(14, 16)\n\tSWAP(15, 17)\n\tSWAP(15, 16)\n\tSWAP(21, 22)\n\tSWAP(20, 22)\n\tSWAP(20, 21)\n\tSWAP(23, 24)\n\tSWAP(25, 26)\n\tSWAP(23, 25)\n\tSWAP(24, 26)\n\tSWAP(24, 25)\n\tSWAP(20, 24)\n\tSWAP(20, 23)\n\tSWAP(21, 25)\n\tSWAP(22, 26)\n\tSWAP(22, 25)\n\tSWAP(21, 23)\n\tSWAP(22, 24)\n\tSWAP(22, 23)\n\tSWAP(13, 20)\n\tSWAP(14, 21)\n\tSWAP(15, 22)\n\tSWAP(15, 21)\n\tSWAP(14, 20)\n\tSWAP(15, 20)\n\tSWAP(16, 23)\n\tSWAP(17, 24)\n\tSWAP(17, 23)\n\tSWAP(18, 25)\n\tSWAP(19, 26)\n\tSWAP(19, 25)\n\tSWAP(18, 23)\n\tSWAP(19, 24)\n\tSWAP(19, 23)\n\tSWAP(16, 20)\n\tSWAP(17, 21)\n\tSWAP(17, 20)\n\tSWAP(18, 22)\n\tSWAP(19, 22)\n\tSWAP(18, 20)\n\tSWAP(19, 21)\n\tSWAP(19, 20)\n\tSWAP(0, 14)\n\tSWAP(0, 13)\n\tSWAP(1, 15)\n\tSWAP(2, 16)\n\tSWAP(2, 15)\n\tSWAP(1, 13)\n\tSWAP(2, 14)\n\tSWAP(2, 13)\n\tSWAP(3, 17)\n\tSWAP(4, 18)\n\tSWAP(5, 19)\n\tSWAP(5, 18)\n\tSWAP(4, 17)\n\tSWAP(5, 17)\n\tSWAP(3, 14)\n\tSWAP(3, 13)\n\tSWAP(4, 15)\n\tSWAP(5, 16)\n\tSWAP(5, 15)\n\tSWAP(4, 13)\n\tSWAP(5, 14)\n\tSWAP(5, 13)\n\tSWAP(6, 20)\n\tSWAP(7, 21)\n\tSWAP(8, 22)\n\tSWAP(8, 21)\n\tSWAP(7, 20)\n\tSWAP(8, 20)\n\tSWAP(9, 23)\n\tSWAP(10, 24)\n\tSWAP(10, 23)\n\tSWAP(11, 25)\n\tSWAP(12, 26)\n\tSWAP(12, 25)\n\tSWAP(11, 23)\n\tSWAP(12, 24)\n\tSWAP(12, 23)\n\tSWAP(9, 20)\n\tSWAP(10, 21)\n\tSWAP(10, 20)\n\tSWAP(11, 22)\n\tSWAP(12, 22)\n\tSWAP(11, 20)\n\tSWAP(12, 21)\n\tSWAP(12, 20)\n\tSWAP(6, 13)\n\tSWAP(7, 14)\n\tSWAP(8, 15)\n\tSWAP(8, 14)\n\tSWAP(7, 13)\n\tSWAP(8, 13)\n\tSWAP(9, 16)\n\tSWAP(10, 17)\n\tSWAP(10, 16)\n\tSWAP(11, 18)\n\tSWAP(12, 19)\n\tSWAP(12, 18)\n\tSWAP(11, 16)\n\tSWAP(12, 17)\n\tSWAP(12, 16)\n\tSWAP(9, 13)\n\tSWAP(10, 14)\n\tSWAP(10, 13)\n\tSWAP(11, 15)\n\tSWAP(12, 15)\n\tSWAP(11, 13)\n\tSWAP(12, 14)\n\tSWAP(12, 13)"

val ys2 = ns.split("\n\t")
	.grouped(6).map(xs => xs.mkString(" ")).mkString("\n")

println(ys2)
