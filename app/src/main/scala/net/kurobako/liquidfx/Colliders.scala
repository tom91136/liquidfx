package net.kurobako.liquidfx

import java.util.{Timer, TimerTask}

import net.kurobako.liquidfx.SphSolver.{Ray, Response}
import scalafx.scene.Node
import scalafx.Includes._
import scalafx.scene.shape.{Box, MeshView, Sphere, TriangleMesh}
import cats._
import cats.implicits._
import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
import scalafx.application.Platform
import scalafx.scene.paint.{Color, PhongMaterial}

object Colliders {

	def nodePos(n: Node) = Vec3(n.getTranslateX, n.getTranslateY, n.getTranslateZ)

	def concaveBoxCollider(b: Box): Ray => Response = { r =>
		val origin = nodePos(b)
		val dim = Vec3(b.getWidth, b.getHeight, b.getDepth) / 2
		val min = origin - dim
		val max = origin + dim
		Response(r.origin.clamp(
			min.x, max.x,
			min.y, max.y,
			min.z, max.z), r.velocity)

	}

	def convexBoxCollider(b: Box): Ray => Response = {
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
				Response(origin, vv)
			} else {

				if (t7 < 0) Response(prev, vv)
				else
					Response(origin, vv) // - (origin distance prev)
			}
		} else Response(origin, vv)
	}

	var cache: Array[Triangle] = _


	val g     = new scalafx.scene.Group()
	val timer = new Timer()

	def convexMeshCollider(b: TriangleMesh, meshView: MeshView): Ray => Response = { r =>

		val Ray(prev, rayOrigin, rv) = r

		val rayVector = rv // .normalise


		//http://blackpawn.com/texts/pointinpoly/default.html
		def inTrig(triangle: Triangle, P: Vec3) = {
			val v0 = triangle.c - triangle.a
			val v1 = triangle.b - triangle.a
			val v2 = P - triangle.a

			// Compute dot products
			val dot00 = v0 dot v0
			val dot01 = v0 dot v1
			val dot02 = v0 dot v2
			val dot11 = v1 dot v1
			val dot12 = v1 dot v2

			// Compute barycentric coordinates
			val invDenom = 1.0 / (dot00 * dot11 - dot01 * dot01)
			val u = (dot11 * dot02 - dot01 * dot12) * invDenom
			val v = (dot00 * dot12 - dot01 * dot02) * invDenom

			// Check if point is in triangle
			(u >= 0) && (v >= 0) && (u + v < 1)
		}


		def collideTriangle2(unscaled: Triangle, p: Vec3, velocity: Vec3): Option[Response ] = {


			val triangle = Triangle(
				Vec3 (meshView.localToParent(unscaled.v0.x, unscaled.v0.y, unscaled.v0.z)),
				Vec3 (meshView.localToParent(unscaled.v1.x, unscaled.v1.y, unscaled.v1.z)),
				Vec3 (meshView.localToParent(unscaled.v2.x, unscaled.v2.y, unscaled.v2.z)),
			)




			//https://math.stackexchange.com/questions/588871/minimum-distance-between-point-and-face
			val n = (triangle.v1 - triangle.v0) cross (triangle.v2 - triangle.v0)
			val nn = n.normalise


			val t = (nn dot triangle.v0) - (nn dot p)
			// p0 = intersection
			val p0 = p +  (nn * t)


			val B_ = triangle.b - triangle.a
			val C_ = triangle.c - triangle.a
			val X_ = p - triangle.a


//			val side = Mat3(
//				B_.x, B_.y, B_.z,
//				C_.x, C_.y, C_.z,
//				X_.x, X_.y, X_.z,
//			).det

			if (inTrig(triangle, p0) && p.distance(p0) < 5) {

				//https://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector
				val r = velocity - (nn * 2 * (velocity dot nn))

//				println(s"R=$r V=$velocity")

				Some(Response(prev, r)  )
			} else {
				None
			}
		}

		def collideTriangle(vertex0: Vec3, vertex1: Vec3, vectex2: Vec3): Option[Vec3] = {
			val edge1 = vertex1 - vertex0
			val edge2 = vectex2 - vertex0


			val C = edge1.cross(edge2)


			// no need to normalize
			val h = rayVector cross edge2; // N
			val a = edge1 dot h

			if (a.abs < 0.00000001f) // almost 0
				return None; // they are parallel so they don't intersect !


			val f = 1f / a
			val s = rayOrigin - vertex0
			val u = f * (s dot h)
			if (u < 0f || u > 1f) return None

			val q = s cross edge1
			val v = f * (rayVector dot q)
			if (v < 0f || v > 1f) return None

			val t = f * (edge2 dot q)

			if (t < 0.00000001f) return None

			if (t > 2f) return None


			//			Some(prev)
			val vec = rayOrigin + (rayVector * t)
			//			println(vec.distance(rayOrigin) + " t = "+ t)
			//			Some(vec)
			Some(vec)

		}
		if (cache == null) {
			cache = b.points
				.grouped(3).map(g => Vec3(g(0), g(1), g(2)))
				.grouped(3).map(xs => Triangle(xs(0), xs(1), xs(2))).toArray
		}

		//		val s = cache.view
		//			 .map(v => collideTriangle(v.a, v.b, v.c))
		//			.find(_.isDefined).flatten

		val s = cache
			.map(v => collideTriangle2(v, rayOrigin, rayVector))
			.find(_.isDefined).flatten

		//		val s = cache
		//		 .find(v => inTrig(v.a,v.b, v.c, rayOrigin)).map(x => rayOrigin)


		//		println(s)


//		s.foreach { case (Response(v, rr), c) =>
//
//			val s = new Sphere(2) {
//				material = new PhongMaterial(c)
//				translateX = v.x
//				translateY = v.y
//				translateZ = v.z
//			}.delegate
//
//			val rs = new Sphere(2) {
//				material = new PhongMaterial(Color.Green)
//				translateX = v.x + rr.x * 6
//				translateY = v.y + rr.y * 6
//				translateZ = v.z + rr.z * 6
//			}.delegate
//			Platform.runLater {
//				g.children.add(s)
//				g.children.add(rs)
//			}
//
//			timer.schedule(new TimerTask {
//				override def run(): Unit = Platform.runLater {
//					g.children.remove(s)
//					g.children.remove(rs)
//				}
//			}, 1000)
//
//		}



		//		orig
		s.map(x => x )
			.getOrElse(Response(rayOrigin, rayVector))


	}

	//	def convexSphereCollider(b: Sphere): Ray => Vec3 = {
	//		case Ray(prev, origin, vec) =>
	//			val centre = Vec3(b.getTranslateX, b.getTranslateY, b.getTranslateZ)
	//			val r = b.getRadius
	//			val radius2 = r * r
	//
	//
	//			val dir = vec.normalise
	//
	//			val L = centre - origin
	//			val tca = L dot dir
	//			val d2 = (L dot L) - tca * tca
	//
	//			if (d2 > radius2) { // no change
	//				origin
	//			} else {
	//				val thc = Math.sqrt(radius2 - d2)
	//				val t0 = tca - thc
	//				val t1 = tca + thc
	//				if (t0 < 0 && t1 < 0) {
	//					origin
	//				} else {
	//					val s = t0 min t1
	//					val b = t0 max t1
	//					val t = if (s < 0) b else s
	//					println(t)
	//					origin + (dir * (t / 500))
	//				}
	//			}
	//	}


}
