package net.kurobako.liquidfx

import java.util.concurrent.ConcurrentHashMap

import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Vec3}

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
				   obs: Seq[Ray => Vec3]): Array[Particle[A]] = {


		class Atom(val particle: Particle[A],
				   var neighbours: Array[Atom] = null,
				   var now: Vec3,
				   var lambda: Double = 0,
				   var deltaP: Vec3 = Vec3.Zero,
				   var omega: Vec3 = Vec3.Zero,
				  )

		def applyForces(xs: Seq[Particle[A]]) = xs.par.map { p =>
			val f = constantForce(p)
			val that = p.copy(
				force = f,
				velocity = f *+ (dt, p.velocity) // apply external force (constant in this case)
			)
			new Atom(
				particle = that,
				now = that.velocity *+ (dt, that.position / scale)) // predict position
		}.seq

		def findNeighbour(xs: Seq[Atom]): Unit = {
			val bs = MutableUnsafeOctree[Atom](Vec3.Zero, 5)(_.now)
			xs.foreach(bs.insertPoint(_))
			xs.par.foreach { a =>
				a.neighbours = bs.pointsInSphere(a.now, h2)
				// a.neighbours = xs.filter { b => (a.now distance b.now) <= h2 }
			}
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
			val rho =  a.neighbours.foldLeft(0d)((acc, b) => acc + b.particle.mass * poly6Kernel(a.now distance b.now))

			// foldLeft 23%
			val norm2 = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
				spikyKernelGradient(a.now, b.now) *+ (1.0 / Rho, acc)
			}.lengthSquared


			val C = rho / Rho - 1.0
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
			a.now = obs.foldLeft((a.now + a.deltaP) * scale)((acc, f) => f(Ray(a.particle.position, acc, a.particle.velocity))) / scale
		}


		def finalise(xs: Seq[Atom]) = {


			// TODO has no effect?
			def XSPHViscosityVelocity(a: Atom): Vec3 = {
				a.neighbours.foldLeft(a.particle.velocity) { (acc, b) =>
					(b.particle.velocity - a.particle.velocity) *+
					(C * poly6Kernel(a.now distance b.now), acc)
				}
			}

			// FIXME
			def vorticityConfinementVelocity(a: Atom): Vec3 = {
				a.omega = xs.foldLeft(Vec3.Zero) { (acc, b) =>
					acc + (b.particle.velocity - a.particle.velocity cross
						   spikyKernelGradient(a.now, b.now))
				}
				val eta = xs.foldLeft(Vec3.Zero) { (acc, b) => spikyKernelGradient(a.now, b.now) *+ (a.omega.length, acc) }
				// normalise?
				val N = eta * (1.0 / eta.length)
				val v = (N cross a.omega) * VorticityEpsilon *+ (dt, a.particle.velocity)
				v
			}

			xs.par.map { a =>
				val p = a.particle
				val deltaX = a.now - p.position / scale
				// TODO  apply vorticity confinement and XSPH viscosity using the delta
				//  computed above
				p.copy(
					position = a.now * scale,
					velocity = deltaX *+ (1.0 / dt, p.velocity) * Vd
				)
			}
		}

		val atoms = applyForces(ps)
		findNeighbour(atoms)
		// hotspot 90%
		for (_ <- 0 until iteration) {
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
						   position: Vec3,
						   force: Vec3 = Vec3.Zero,
						   velocity: Vec3 = Vec3.Zero,
						  )

	case class Ray(prev: Vec3, origin: Vec3, velocity: Vec3) {
	}


	object Vec3 {
		def apply(v: Double): Vec3 = Vec3(v, v, v)
		final val NegativeOne = Vec3(-1d)
		final val One         = Vec3(1d)
		final val Zero        = Vec3(0d)
	}
	case class Vec3(x: Double, y: Double, z: Double) {

		@inline def array[N: ClassTag](f: Double => N): Array[N] = Array(f(x), f(y), f(z))
		@inline def tuple: (Double, Double, Double) = (x, y, z)

		@inline def *+(t: Double, that: Vec3): Vec3 = Vec3(
			Math.fma(t, this.x, that.x),
			Math.fma(t, this.y, that.y),
			Math.fma(t, this.z, that.z))

		//		@inline def *+(t: Double, that: Vec3): Vec3 = (this * t) + that

		@inline def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
		@inline def -(amount: Double): Vec3 = Vec3(x - amount, y - amount, z - amount)
		@inline def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
		@inline def +(amount: Double): Vec3 = Vec3(x + amount, y + amount, z + amount)
		@inline def *(that: Vec3): Vec3 = Vec3(x * that.x, y * that.y, z * that.z)
		@inline def *(factor: Double): Vec3 = Vec3(x * factor, y * factor, z * factor)
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

		@inline def distance(that: Vec3): Double = {
			val a = x - that.x
			val b = y - that.y
			val c = z - that.z
			Math.sqrt(a * a + b * b + c * c)
		}

		def fastDistance(that: Vec3) = {

		}

		@inline def clamp(xMin: Double, xMax: Double,
						  yMin: Double, yMax: Double,
						  zMin: Double, zMax: Double): Vec3 = Vec3(
			clamp(xMin, xMax, x),
			clamp(yMin, yMax, y),
			clamp(zMin, zMax, z)
		)

		private def clamp(min: Double, max: Double, v: Double) = Math.max(min, Math.min(max, v))

		@inline def length: Double = Math.sqrt(x * x + y * y + z * z)
		@inline def lengthSquared: Double = length * length
		@inline def magnitude: Double = length
		@inline def magnitudeSquared: Double = lengthSquared

	}


}
