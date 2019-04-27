
def   index3d( x : Int,  y : Int,  z : Int,
	 xMax : Int,  yMax : Int,  zMax : Int ) : Int = x * yMax * zMax + y * zMax + z


def  to3d( index : Int,  xMax : Int,  yMax : Int,  zMax : Int) : (Int, Int, Int) = {
	val  x = index / (yMax * zMax)
	val  y = (index - x * yMax * zMax) / zMax
	val  z = index - x * yMax * zMax - y * zMax
	(x, y, z)
}



val One = index3d(1, 1, 1, 13, 13, 13)


val Out = 13*13*13
to3d(Out, 13, 13, 13)



val Out2 = 12*12*12-1
to3d(Out2, 13, 13, 13)
to3d(Out2, 12, 12, 12)


to3d(2046, 13, 13, 13)



index3d(12, 12, 12, 13, 13, 13)

index3d(11, 11, 11, 13, 13, 13)




(Out - Out2)/12/12



//val Max = index3d(12, 12, 12, 13, 13, 13)
//val Max2 = index3d(12, 12, 12, 12, 12, 12)
//
//val Max3 = index3d(12, 12, 12, 13, 13, 13)
//
//
//
