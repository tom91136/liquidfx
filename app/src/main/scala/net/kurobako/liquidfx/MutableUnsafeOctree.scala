package net.kurobako.liquidfx

import net.kurobako.liquidfx.SphSolver.Vec3

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


object MutableUnsafeOctree {

	private final val SideVecs = Array(
		Vec3(-1, -1, -1), Vec3(1, -1, -1),
		Vec3(-1, 1, -1), Vec3(1, 1, -1),
		Vec3(-1, -1, 1), Vec3(1, -1, 1),
		Vec3(-1, 1, 1), Vec3(1, 1, 1))

	def apply[A : ClassTag](c: Vec3, halfSize: Double )(f : A => Vec3) =
		new MutableUnsafeOctree[A](null, c, halfSize)(f)

}


// highly modified version of
// https://github.com/fishuyo/scalaAV/blob/master/src/main/scala/spatial/octree.scala
class MutableUnsafeOctree[A: ClassTag](parent: MutableUnsafeOctree[A], centre: Vec3, halfSize: Double)(f: A => Vec3) {

	private final val min = centre - halfSize
	private final val max = centre + halfSize

	private final var childrenCount                           = 0
	private final var children: Array[MutableUnsafeOctree[A]] = _
	private final var points  : ArrayBuffer[A]                = _

	private def contains(p: Vec3) = {
		if (p.x < min.x || p.x > max.x) false
		else if (p.y < min.y || p.y > max.y) false
		else if (p.z < min.z || p.z > max.z) false
		else true
	}

	private def intersectsSphere(centre: Vec3, radius: Double) = {
		val dx = if (centre.x < min.x) centre.x - min.x else if (centre.x > max.x) centre.x - max.x else 0d
		val dy = if (centre.y < min.y) centre.y - min.y else if (centre.y > max.y) centre.y - max.y else 0d
		val dz = if (centre.z < min.z) centre.z - min.z else if (centre.z > max.z) centre.z - max.z else 0d
		val d = dx * dx + dy * dy + dz * dz
		d <= radius * radius
	}

	def insertPoint(p: A): Boolean = {
		if (contains(f(p))) {
			if (halfSize <= 0.1d) {
				if (points == null) points = ArrayBuffer()
				points += p
			} else {
				if (children == null) children = Array.ofDim(8)
				val oct = octantId(f(p))
				if (children(oct) == null) {
					val size = halfSize * .5f
					val cen = MutableUnsafeOctree.SideVecs(oct) *+ (size, centre)
					children(oct) = new MutableUnsafeOctree(this, cen, size)(f)
					childrenCount += 1
				}
				return children(oct).insertPoint(p)
			}
		}
		false
	}

	private def octantId(p: Vec3): Int =
		(if (p.x >= centre.x) 1 else 0) +
		(if (p.y >= centre.y) 2 else 0) +
		(if (p.z >= centre.z) 4 else 0)

	private def pointsInSphere0(centre: Vec3, radius: Double): ArrayBuffer[A] = {
		var results: ArrayBuffer[A] = null
		val rSquared = radius * radius

		if (this.intersectsSphere(centre, radius)) {
			if (points != null) {
				results = points.filter(p => (centre - f(p)).magnitudeSq <= rSquared)
			} else if (childrenCount > 0) {
				for (i <- 0 until 8) {
					if (children(i) != null) {
						val points = children(i).pointsInSphere0(centre, radius)
						if (points != null) {
							if (results == null) results = ArrayBuffer()
							results ++= points
						}
					}
				}
			}
		}
		results
	}

	def pointsInSphere(centre: Vec3, radius: Double): Array[A] = {
		val x = pointsInSphere0(centre, radius)
		if (x == null) Array.empty else x.toArray
	}
}