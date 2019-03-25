package net.kurobako.liquidfx

import java.util.Objects
import java.util.concurrent.ConcurrentHashMap

import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Response, Vec3}
import scalafx.geometry.Point3D

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.reflect.ClassTag

// based on https://nccastaff.bournemouth.ac.uk/jmacey/MastersProjects/MSc15/06Burak/BurakErtekinMScThesis.pdf
// and also http://mmacklin.com/pbf_sig_preprint.pdf
class SphSolver(val h: Double = 0.1, // Particle(smoothing kernel) size
				val scale: Double = 100,
			   ) {

	final val Vd = 0.49 // Velocity dampening
	final val Rho = 6378 // Reference density
	final val Epsilon    = 0.00000001
	final val CfmEpsilon = 600.0 // CFM propagation

	final val C                = 0.00001
	final val VorticityEpsilon = 0.0005
	final val CorrK            = 0.0001
	final val CorrN            = 4d
	final val CorrDeltaQ       = 0.3 * h

	private val hp2 = h * h
	private val h2  = h * 2d


	private val poly6Factor       = 315.0 / (64.0 * Math.PI * Math.pow(h, 9d))
	private val spikyKernelFactor = -(45d / (Math.PI * Math.pow(h, 6d)))
	private val p6DeltaQ          = poly6Kernel(CorrDeltaQ)

	private def poly6Kernel(r: Double): Double =
		if (r <= h) poly6Factor * Math.pow(hp2 - r * r, 3d) else 0.0

	// 4.9.2 Spiky Kernel(Desbrun and Gascuel (1996))
	private def spikyKernelGradient(x: Vec3, y: Vec3): Vec3 = {
		val r = x distance y
		if (r > h || r < Epsilon) Vec3.Zero
		else (x - y) * (spikyKernelFactor * (Math.pow(h - r, 2d) / r))
	}


	def advance[A](dt: Double = 0.0083,
				   iteration: Int,
				   constantForce: Particle[A] => Vec3 = { p: Particle[A] => Vec3(0d, p.mass * 9.8, 0d) })
				  (ps: Array[Particle[A]],
				   obs: Seq[Ray => Response]): Array[Particle[A]] = {


		class Atom(val particle: Particle[A],
				   var neighbours: Array[Atom] = null,
				   var now: Vec3,
				   var lambda: Double = 0,
				   var deltaP: Vec3 = Vec3.Zero,
				   var omega: Vec3 = Vec3.Zero,
				   var velocity: Vec3 = Vec3.Zero
				  )

		def applyForces(xs: Seq[Particle[A]]) = xs.par.map { p =>
			val f = constantForce(p)
			val that = p.copy(
				velocity = f *+ (dt, p.velocity) // apply external force (constant in this case)
			)
			new Atom(
				particle = that,
				velocity = that.velocity,
				now = that.velocity *+ (dt, that.position / scale)) // predict position
		}.seq


		val rg = List(0, -h2, h2, -h, h)
		val offsets = for {
			x <- rg
			y <- rg
			z <- rg
		} yield Vec3(x, y, z)


		def findNeighbour(xs: Seq[Atom]): Unit = {

			//			val bs = MutableUnsafeOctree[Atom](Vec3.Zero, 5)(_.now)
			//			xs.foreach(bs.insertPoint(_))

			val m = new ConcurrentHashMap[Any, ArrayBuffer[Atom]]()

			val VS = h2 / 2


			def sub(v: Vec3) = {
				val d = v / VS
				Vec3(d.x.round, d.y.round, d.z.round)
			}

			def mkHash(v: Vec3) = {
				val d = v / VS


				var res = 17
				res = res * 31 + d.x.round.toInt
				res = res * 31 + d.y.round.toInt
				res = res * 31 + d.z.round.toInt
				res

				//				(d.x.round, d.y.round, d.z.round)
			}


			xs.foreach { x =>

				offsets.map(_ + x.now).foreach { p =>
					m.computeIfAbsent(mkHash(p), { _ =>
						new ArrayBuffer[Atom]()
					}) += x
				}

				//				m.computeIfAbsent(mkHash(x.now), { _ =>
				//					new ArrayBuffer[Atom]()
				//				}) += x
			}


			case class Int2[A, B](key: Vec3 = null, value: B)

			def mkMyHash() = {


				val buckets = Array.fill[Int2[Vec3, Int]](xs.size * offsets.size)(Int2(value = 0))

				def findSlot(x: Vec3) = {
					var i = mkHash(x) % buckets.length
					while (buckets(i).key != null && buckets(i).key != sub(x)) {
						i = (i + 1) % buckets.length
					}
					i
				}


				def inccell(x: Vec3): Unit = {
					val slot = findSlot(x)
					val that = buckets(slot)
					if (that.key != null) {
						buckets(slot) = that.copy(value = that.value + 1)
					} else {
						buckets(slot) = Int2(sub(x), 1)
					}
				}

				xs.foreach { x =>

					offsets.map(_ + x.now).foreach { p =>
						inccell(p)
					}

				}


				val ls = xs.map { x =>
					buckets(findSlot(x.now)).value
				}.zipWithIndex


				//
				//				println("\tMy : \n " + ls.toList)
				//
				//				println("\tRef: \n " + xs.map(x => m.getOrDefault(mkHash(x.now), ArrayBuffer()).length).zipWithIndex.toList)
				//
				//
				//
				//				println("\tT = " + buckets.map(_.value).toList)

				println("\tKeys = " + xs.map { x =>
					mkHash(x.now) % buckets.length
				}.toSet.size)
				buckets.count(_ != 0)
			}


			//			val buckets = mkMyHash();


			import scala.collection.JavaConverters._

			val mm = mkMyHash()

			println("\t keyN=" + m.size() + " PN=" + xs.size + " My Buckets=" + mm)


			//			val rad = xs.map { a =>
			//				val those = m.getOrDefault(mkHash(a.now), new ArrayBuffer[Atom]())
			//				those.count { b => (a.now distance b.now) <= h2 }
			//			}.sum
			//
			//			val ann = xs.map { a =>
			//				val those = m.getOrDefault(mkHash(a.now), new ArrayBuffer[Atom]())
			//				those.length
			//			}.sum
			//
			//			println(s"\t rad:${rad} amm:${ann} ~~ ${ann.toFloat / rad}")


			xs.par.foreach { a =>


				val those = m.getOrDefault(mkHash(a.now), new ArrayBuffer[Atom]())


				//				val those = m.getOrDefault(mkHash(a.now),  new ArrayBuffer[Atom]())

				//				println(s" ${those.count { b => (a.now distance b.now) <= h2 }}  ${those.length}  ")

				//				val correct = bs.pointsInSphere(a.now, h2)

				//				println("\t" + (those.filter { b => (a.now distance b.now) <= h2 }.toSet == correct.toSet))
				a.neighbours = those.toArray //  those.filter { b => (a.now distanceSq b.now) <= (h2 * h2) }.toArray
				//				a.neighbours = correct
			}


			//			xs.par.foreach { a =>
			//				a.neighbours = bs.pointsInSphere(a.now, h2)
			//				 a.neighbours = xs.filter { b => (a.now distance b.now) <= h2 }
			//			}
		}


		// hotspot 60%
		def solveLambda(xs: Seq[Atom]): Unit = xs.par.foreach { a =>


			//			var v3i = Vec3.Zero
			//			var init = 0d
			//			var i = 0
			//			while(i < a.neighbours.length){
			//				val b = a.neighbours(i)
			//				init += b.particle.mass * poly6Kernel(a.now distance b.now)
			//				v3i = spikyKernelGradient(a.now, b.now) *+ (1.0 / Rho, v3i)
			//				i+=1
			//			}
			//			val norm2 = v3i.lengthSquared
			//			val rho = init


			// foldLeft 23%
			val rho = a.neighbours.foldLeft(0d)((acc, b) => acc + b.particle.mass * poly6Kernel(a.now distance b.now))

			// foldLeft 23%
			val norm2 = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
				spikyKernelGradient(a.now, b.now) *+ (1.0 / Rho, acc)
			}.lengthSquared


			val C = rho / Rho - 1.0

			//			println(a.particle.a + "->"+ C + " " + a.neighbours.map(_.particle.a).toList)
			a.lambda = -C / (norm2 + CfmEpsilon)
		}

		// hotspot 30%
		def solveDeltaP(xs: Seq[Atom]): Unit = xs.par.foreach { a =>
			a.deltaP = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
				// disable corr by setting it to 0
				val corr = -CorrK * Math.pow(poly6Kernel(a.now distance b.now) / p6DeltaQ, CorrN)
				val factor = (a.lambda + b.lambda + corr) / Rho
				spikyKernelGradient(a.now, b.now) *+ (factor, acc)
			}
		}

		def solveCollisionAndUpdate(xs: Seq[Atom]): Unit = xs.par.foreach { a =>
			val current = Response((a.now + a.deltaP) * scale, a.velocity)
			val res = obs.foldLeft(current) { case (Response(pos, vel), f) => f(Ray(a.particle.position, pos, vel)) }
			a.now = res.position / scale
			a.velocity = res.velocity

			//			println(s"\tP(${a.particle.a} ${a.lambda}  ${a.deltaP} ${a.now * scale} ${a.velocity* scale})")

		}


		def finalise(xs: Seq[Atom]) = {


			// TODO has no effect?
			def XSPHViscosityVelocity(a: Atom): Vec3 = {
				a.neighbours.foldLeft(a.velocity) { (acc, b) =>
					(b.velocity - a.velocity) *+
					(C * poly6Kernel(a.now distance b.now), acc)
				}
			}

			// FIXME
			def vorticityConfinementVelocity(a: Atom): Vec3 = {
				a.omega = xs.foldLeft(Vec3.Zero) { (acc, b) =>
					acc + (b.velocity - a.velocity cross
						   spikyKernelGradient(a.now, b.now))
				}
				val eta = xs.foldLeft(Vec3.Zero) { (acc, b) => spikyKernelGradient(a.now, b.now) *+ (a.omega.length, acc) }
				// normalise?
				val N = eta * (1.0 / eta.length)
				val v = (N cross a.omega) * VorticityEpsilon *+ (dt, a.velocity)
				v
			}

			xs.par.map { a =>


				val mc = a.neighbours.foldLeft(Vec3.Zero) { case (acc, b) => b.now + acc } / a.neighbours.size


				val deltaX = a.now - a.particle.position / scale
				// TODO  apply vorticity confinement and XSPH viscosity using the delta
				//  computed above
				a.particle.copy(
					position = a.now * scale,
					velocity = deltaX *+ (1.0 / dt, a.velocity) * Vd,
					surface = mc.distance(a.now) > 0.09

					//					neighbours = a.neighbours.map(_.particle)
				)
			}
		}

		val atoms = applyForces(ps)
		findNeighbour(atoms)
		// hotspot 90%
		for (i <- 0 until iteration) {
			println(s"@$i")
			solveLambda(atoms)
			solveDeltaP(atoms)
			solveCollisionAndUpdate(atoms)
		}
		finalise(atoms).toArray
	}

}

object SphSolver {

	case class Particle[A](a: A,
						   mass: Double = 1,
						   surface: Boolean = false,
						   position: Vec3,
						   velocity: Vec3 = Vec3.Zero,
						   neighbours: Seq[Particle[A]] = Nil,
						  )

	case class Ray(prev: Vec3, origin: Vec3, velocity: Vec3) {}

	case class Response(position: Vec3, velocity: Vec3)


	object Vec3 {
		def apply(v: Point3D): Vec3 = Vec3(v.x, v.y, v.z)

		def apply(v: Double): Vec3 = Vec3(v, v, v)
		final val NegativeOne = Vec3(-1d)
		final val One         = Vec3(1d)
		final val Zero        = Vec3(0d)
	}

	case class Mat3(
					   a: Double, b: Double, c: Double,
					   d: Double, e: Double, f: Double,
					   g: Double, h: Double, i: Double) {
		def det: Double = (a * e * i) + (b * f * g) + (c * d * h) -
						  (c * e * g) - (b * d * i) - (a * f * h)
	}



	case class Vec3(x: Double, y: Double, z: Double) {

		def p3d: Point3D = new Point3D(x, y, z)

		@inline def u: Double = x
		@inline def v: Double = y
		@inline def w: Double = z

		@inline def array[N: ClassTag](f: Double => N): Array[N] = Array(f(x), f(y), f(z))
		@inline def tuple: (Double, Double, Double) = (x, y, z)

		@inline def *+(t: Double, that: Vec3): Vec3 = Vec3(
			Math.fma(t, this.x, that.x),
			Math.fma(t, this.y, that.y),
			Math.fma(t, this.z, that.z))

		//		@inline def *+(t: Double, that: Vec3): Vec3 = (this * t) + that

		@inline def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
		@inline def negate: Vec3 = Vec3(-x, -y, -z)
		@inline def -(amount: Double): Vec3 = Vec3(x - amount, y - amount, z - amount)
		@inline def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
		@inline def +(amount: Double): Vec3 = Vec3(x + amount, y + amount, z + amount)
		@inline def *(that: Vec3): Vec3 = Vec3(x * that.x, y * that.y, z * that.z)
		@inline def *(factor: Double): Vec3 = Vec3(x * factor, y * factor, z * factor)
		@inline def *:(factor: Double): Vec3 = Vec3(x * factor, y * factor, z * factor)
		@inline def /(factor: Double): Vec3 = Vec3(x / factor, y / factor, z / factor)
		@inline def /:(factor: Double): Vec3 = Vec3(factor / x, factor / y, factor / z)
		@inline def cross(that: Vec3): Vec3 = Vec3(
			y * that.z - z * that.y,
			z * that.x - x * that.z,
			x * that.y - y * that.x)

		@inline def normalise: Vec3 = {
			val m = magnitude
			if (m == 0) Vec3.Zero
			else this / m
		}

		@inline def dot(that: Vec3): Double = x * that.x + y * that.y + z * that.z

		@inline def distance(that: Vec3): Double = Math.sqrt(distanceSq(that))

		@inline def distanceSq(that: Vec3): Double = {
			val a = x - that.x
			val b = y - that.y
			val c = z - that.z
			a * a + b * b + c * c
		}

		@inline def clamp(xMin: Double, xMax: Double,
						  yMin: Double, yMax: Double,
						  zMin: Double, zMax: Double): Vec3 = Vec3(
			clamp(xMin, xMax, x),
			clamp(yMin, yMax, y),
			clamp(zMin, zMax, z)
		)

		private def clamp(min: Double, max: Double, v: Double) = Math.max(min, Math.min(max, v))

		@inline def length: Double = Math.sqrt(lengthSquared)
		@inline def lengthSquared: Double = x * x + y * y + z * z
		@inline def magnitude: Double = length
		@inline def magnitudeSq: Double = lengthSquared

	}


}
