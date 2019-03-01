package net.kurobako.liquidfx

import java.util.{Timer, TimerTask}

import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.{Ray, Response, Vec3}
import scalafx.scene.Node
import scalafx.scene.shape.{Box, Sphere, TriangleMesh}
import cats._
import cats.implicits._
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

	var cache: Array[Vec3] = _


	val g     = new scalafx.scene.Group()
	val timer = new Timer()

	def convexMeshCollider(b: TriangleMesh): Ray => Response = { r =>

		val Ray(prev, rayOrigin, rv) = r


		val rayVector = rv// .normalise


		def collideTriangle(vertex0: Vec3, vertex1: Vec3, vectex2: Vec3): Option[Vec3] = {
			// compute plane's normal
			val edge1 = vertex1 - vertex0
			val edge2 = vectex2 - vertex0
			// no need to normalize
			val h = rayVector cross edge2; // N
			val a = edge1 dot h

			if (a.abs < 0.000001) // almost 0
				return None; // they are parallel so they don't intersect !


			val f = 1.0 / a
			val s = rayOrigin - vertex0
			val u = f * (s dot h)
			if (u < 0.0 || u > 1.0) return None

			val q = s cross edge1
			val v = f * (rayVector dot q)
			if (v < 0.0 || v > 1.0) return None

			val t = f * (edge2 dot q)

			if (t < 0.000001) return None
			if(t > 5) return None


//			Some(prev)
			val vec = rayOrigin + (rayVector * (t))
			println(vec.distance(rayOrigin) + " t = "+ t)
			Some(prev)
//			Some(rayOrigin )

		}
		if (cache == null) {
			cache = b.points
				.grouped(3).map(g => Vec3(g(0), g(1), g(2))).toArray
		}

		val s = cache
			.grouped(3).map(v => collideTriangle(v(0), v(1), v(2)))
			.find(_.isDefined).flatten
		//		println(s)


		s.foreach { v =>

			val s = new Sphere(5) {
				material = new PhongMaterial(Color.Red)
				translateX = v.x
				translateY = v.y
				translateZ = v.z
			}.delegate
			Platform.runLater {
				g.children.add(s)
			}

			timer.schedule(new TimerTask {
				override def run(): Unit = Platform.runLater {
					g.children.remove(s)
				}
			}, 10000)

		}
		//		orig
		s.map(x => Response(x, rayVector))
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
