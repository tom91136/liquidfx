package net.kurobako.liquidfx

import net.kurobako.liquidfx.Maths.Vec3


object Geom {


	//http://paulbourke.net/geometry/circlesphere/raysphere.c
	def lineSegmentIntersections(p1: Vec3, p2: Vec3,
								 sc: Vec3, r: Double, epsilon: Double = 0.00001): (Option[Vec3], Option[Vec3]) = {

		val dp = p2 - p1
		val a = dp.magnitudeSq
		val b = 2 * dp.dot(p1 - sc)
		val c = sc.magnitudeSq + p1.magnitudeSq - (2 * sc.dot(p1)) - (r * r)
		val bb4ac = b * b - 4 * a * c //discriminant
		if (a.abs < epsilon || bb4ac < 0) (None, None)
		else if (bb4ac.abs < epsilon) Some(p1 + dp * (-b / (2 * a))) -> None
		else {
			val x = -b + Math.sqrt(bb4ac).toFloat / 2 * a
			val y = -b - Math.sqrt(bb4ac).toFloat / 2 * a
			Some(p1 + dp * x) ->
			Some(p1 + dp * y)
		}
	}

}
