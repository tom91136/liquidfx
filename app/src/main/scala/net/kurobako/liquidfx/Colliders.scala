package net.kurobako.liquidfx

import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.{Ray, Vec3}
import scalafx.scene.Node
import scalafx.scene.shape.{Box, Sphere, TriangleMesh}
import cats._
import cats.implicits._

object Colliders {

	def nodePos(n: Node) = Vec3(n.getTranslateX, n.getTranslateY, n.getTranslateZ)

	def concaveBoxCollider(b: Box): Ray => Vec3 = { r =>
		val origin = nodePos(b)
		val dim = Vec3(b.getWidth, b.getHeight, b.getDepth) / 2
		val min = origin - dim
		val max = origin + dim
		r.origin.clamp(
			min.x, max.x,
			min.y, max.y,
			min.z, max.z)
	}

	def convexBoxCollider(b: Box): Ray => Vec3 = {
		case Ray(prev, origin, vv) => if (b.isVisible) {
			val vel = vv.normalise
			val pos = nodePos(b)
			val dim = Vec3(b.getWidth, b.getHeight, b.getDepth) / 2
			val min = pos - dim
			val max = pos + dim

			//https://gamedev.stackexchange.com/a/103714/73429

			val t1 = (min.x - origin.x) / vel.x
			val t2 = (max.x - origin.x) / vel.x
			val t3 = (min.y - origin.y) / vel.y
			val t4 = (max.y - origin.y) / vel.y
			val t5 = (min.z - origin.z) / vel.z
			val t6 = (max.z - origin.z) / vel.z

			val aMin = if (t1 < t2) t1 else t2
			val bMin = if (t3 < t4) t3 else t4
			val cMin = if (t5 < t6) t5 else t6

			val aMax = if (t1 > t2) t1 else t2
			val bMax = if (t3 > t4) t3 else t4
			val cMax = if (t5 > t6) t5 else t6

			val fMax = if (aMin > bMin) aMin else bMin
			val fMin = if (aMax < bMax) aMax else bMax

			val t7 = if (fMax > cMin) fMax else cMin
			val t8 = if (fMin < cMax) fMin else cMax

			if (t8 < 0 || t7 > t8) {
				// no intersection
				origin
			} else {

				if (t7 < 0) prev
				else
					origin // - (origin distance prev)
			}
		} else origin
	}

	def convexMeshCollider(b: TriangleMesh): Ray => Vec3 = { r =>

		val Ray(_, orig, dir) = r

		def collideTriangle(triangle: Triangle): Option[Vec3] = {
			val Triangle(v0, v1, v2) = triangle
			// compute plane's normal
			val v0v1 = v1 - v0
			val v0v2 = v2 - v0
			// no need to normalize
			val N = v0v1 cross v0v2; // N
			val area2 = N.length

			val NdotRayDirection = N.dot(dir)

			if (NdotRayDirection.abs < 0.00001) // almost 0
				return None; // they are parallel so they don't intersect !

			// compute d parameter using equation 2
			val d = N dot v0

			// compute t (equation 3)
			val t = ((N dot orig) + d) / NdotRayDirection
			// check if the triangle is in behind the ray
			if (t < 0) return None // the triangle is behind

			// compute the intersection point using equation 1
			val P = orig + t *: dir

			// Step 2: inside-outside test
			// C is the vector perpendicular to triangle's plane

			// edge 0
			val edge0 = v1 - v0
			val vp0 = P - v0
			if (N.dot(edge0 cross vp0) < 0) return None // P is on the right side

			// edge 1
			val edge1 = v2 - v1
			val vp1 = P - v1
			if (N.dot(edge1 cross vp1) < 0) return None // P is on the right side

			// edge 2
			val edge2 = v0 - v2
			val vp2 = P - v2
			if (N.dot(edge2 cross vp2) < 0) return None // P is on the right side;

			Some(P) // this ray hits the triangle
		}


		val s = b.points
			.grouped(3).map(g => Vec3(g(0), g(1), g(2)))
			.grouped(3).map(v => Triangle(v(0), v(1), v(2)))
			.toList.collectFirstSome(collideTriangle(_))

		s.getOrElse(orig)
	}

	def convexSphereCollider(b: Sphere): Ray => Vec3 = {
		case Ray(prev, origin, vec) =>
			val centre = Vec3(b.getTranslateX, b.getTranslateY, b.getTranslateZ)
			val r = b.getRadius
			val radius2 = r * r


			val dir = vec.normalise

			val L = centre - origin
			val tca = L dot dir
			val d2 = (L dot L) - tca * tca

			if (d2 > radius2) { // no change
				origin
			} else {
				val thc = Math.sqrt(radius2 - d2)
				val t0 = tca - thc
				val t1 = tca + thc
				if (t0 < 0 && t1 < 0) {
					origin
				} else {
					val s = t0 min t1
					val b = t0 max t1
					val t = if (s < 0) b else s
					println(t)
					origin + (dir * (t / 500))
				}
			}
	}


}
