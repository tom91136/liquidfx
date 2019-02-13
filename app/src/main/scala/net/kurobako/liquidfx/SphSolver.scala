package net.kurobako.liquidfx

import java.util.concurrent.ConcurrentHashMap

import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Vec3}

// based on https://nccastaff.bournemouth.ac.uk/jmacey/MastersProjects/MSc15/06Burak/BurakErtekinMScThesis.pdf
// and also http://mmacklin.com/pbf_sig_preprint.pdf
class SphSolver(val h: Double = 0.1,
				val iteration: Int = 5,
				val scale: Double = 100,
			   ) {

	final val Rho = 6378

	final val Epsilon    = 0.00000001
	final val CfmEpsilon = 600.0

	final val Vd = 0.49


	final val C                = 0.00001
	final val VorticityEpsilon = 0.0005

	final val s_corrEnable          = true
	final val s_corr_k              = 0.0001
	final val s_corr_n              = 4d
	final val s_corr_deltaQ: Double = 0.3 * h

	private val hp2 = h * h
	private val hp6 = Math.pow(h, 6d)
	private val hp9 = Math.pow(h, 9d)
	private val h2  = h * 2d


	def advance[A](dt: Double = 0.0083 ,
				   constantForce: Particle[A] => Vec3 = { p: Particle[A] => Vec3(0d, p.mass * 9.8, 0d) })
				  (ps: Seq[Particle[A]],
				   obs: Seq[Ray => Vec3]): Array[Particle[A]] = {

		class Atom(var particle: Particle[A],
				   var neighbours: Seq[Atom] = Vector.empty,
				   var now: Vec3,
				   var lambda: Double = 0,
				   var deltaP: Vec3 = Vec3.Zero,
				   var omega: Vec3 = Vec3.Zero,
				   var vNew: Vec3 = Vec3.Zero,
				  )

		val POLY6_CONST = 315.0 / (64.0 * Math.PI * hp9)

		def poly6Kernel(r: Double): Double =
			if (r <= h) POLY6_CONST * Math.pow(hp2 - r * r, 3d) else 0.0

		// 4.9.2 Spiky Kernel(Desbrun and Gascuel (1996))
		def spikyKernelGradient(x: Vec3, y: Vec3): Vec3 = {
			val r = x distance y
			if (r > h || r < Epsilon) Vec3.Zero
			else {
				(x - y) * (-(45d / (Math.PI * hp6)) * (Math.pow(h - r, 2d) / r))
			}
		}

		def applyForce(xs: Seq[Particle[A]]) = xs.par.map { p =>

			val f = constantForce(p)


			val that = p.copy(
				force = f,
				velocity = f *+ (dt, p.velocity)
			)
			new Atom(
				particle = that,
				now = that.velocity *+ (dt, that.position / scale))
		}.toArray


		def fillNeighbour(xs: Seq[Atom]): Unit = {
			xs.par.foreach { a =>
				a.neighbours = xs.filter { b => (a.now distance b.now) <= h2 }
			}
		}

		val p6DeltaQ = poly6Kernel(s_corr_deltaQ)
		def solveDeltaP(xs: Seq[Atom]): Unit = {

			xs.par.foreach { a =>
				val rho = a.neighbours.map(b => b.particle.mass * poly6Kernel(a.now distance b.now)).sum
				val norm2 = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
					spikyKernelGradient(a.now, b.now) *+ (1.0 / Rho, acc)
				}.lengthSquared

				val C = rho / Rho - 1.0
				a.lambda = -C / (norm2 + CfmEpsilon)
			}

			xs.par.foreach { a =>
				a.deltaP = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
					val corr = if (s_corrEnable) {
						-s_corr_k * Math.pow(poly6Kernel(a.now distance b.now) / p6DeltaQ, s_corr_n)
					} else 0d
					val factor = (a.lambda + b.lambda + corr) / Rho
					spikyKernelGradient(a.now, b.now) *+ (factor, acc)
				}
			}

			xs.par.foreach { a =>
				a.now = obs.foldLeft((a.now + a.deltaP) * scale)((acc, f) => f(Ray(acc, a.particle.velocity))) / scale
			}
		}

		def delta(xs: Seq[Atom]): Unit = xs.par.foreach { p =>
			val deltaX = p.now - p.particle.position / scale
			p.particle = p.particle.copy(velocity = deltaX *+ (1.0 / dt, p.particle.velocity) * Vd)
		}

		def vorticityConfinement(xs: Seq[Atom]): Unit = {
			xs.par.foreach { a =>
				a.omega = xs.foldLeft(Vec3.Zero) { (acc, b) =>
					acc + (b.particle.velocity - a.particle.velocity).cross(spikyKernelGradient(a.now, b.now))
				}
			}
			xs.par.foreach { a =>
				val eta = xs.foldLeft(Vec3.Zero) { (acc, b) => spikyKernelGradient(a.now, b.now) *+ (a.omega.length, acc) }
				// normalise?
				val N = eta * (1.0 / eta.length)
				val v = N.cross(a.omega) * VorticityEpsilon *+ (dt, a.particle.velocity)
				a.particle = a.particle.copy(velocity = v)
			}
		}

		def viscositySmoothing(xs: Seq[Atom]) = {
			xs.par.foreach { a =>
				a.vNew = a.neighbours.foldLeft(a.particle.velocity) { (acc, b) =>
					(b.particle.velocity - a.particle.velocity) *+
					(C * poly6Kernel(a.now distance b.now), acc)
				}
			}
			xs.foreach(a => a.particle = a.particle.copy(velocity = a.vNew))
		}

		def finalise(xs: Seq[Atom]) = {
			xs.par.map(y => y.particle.copy(position = y.now * scale))
		}

		val atoms = applyForce(ps)
		fillNeighbour(atoms)
		for (_ <- 0 until iteration) solveDeltaP(atoms)
		delta(atoms)
		//		vorticityConfinement(atoms)
		//				viscositySmoothing(atoms)
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

	case class Ray(origin: Vec3, velocity: Vec3) {
	}


	object Vec3 {
		final val One  = Vec3(1, 1, 1)
		final val Zero = Vec3(0, 0, 0)
	}
	case class Vec3(x: Double, y: Double, z: Double) {

		@inline def *+(t: Double, that: Vec3): Vec3 = Vec3(
			Math.fma(t, this.x, that.x),
			Math.fma(t, this.y, that.y),
			Math.fma(t, this.z, that.z))

		//		@inline def *+(t: Double, that: Vec3): Vec3 = (this * t) + that

		@inline def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
		@inline def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
		@inline def *(that: Vec3): Vec3 = Vec3(x * that.x, y * that.y, z * that.z)
		@inline def *(factor: Double): Vec3 = Vec3(x * factor, y * factor, z * factor)
		@inline def /(factor: Double): Vec3 = Vec3(x / factor, y / factor, z / factor)
		@inline def cross(that: Vec3): Vec3 = Vec3(
			y * that.z - z * that.y,
			z * that.x - x * that.z,
			x * that.y - y * that.x)


		@inline def distance(that: Vec3): Double = {
			val a = x - that.x
			val b = y - that.y
			val c = z - that.z
			Math.sqrt(a * a + b * b + c * c)
		}

		@inline def clamp(xMin: Double, xMax: Double,
						  yMin: Double, yMax: Double,
						  zMin: Double, zMax: Double): Vec3 = Vec3(
			clamp(xMin, xMax, x),
			clamp(yMin, yMax, y),
			clamp(zMin, zMax, z)
		)

		private def clamp(min: Double, max: Double, v: Double) = Math.max(min, Math.min(max, v))

		@inline def magnitude: Double = Math.sqrt(x * x + y * y + z * z)
		@inline def length: Double = magnitude
		@inline def lengthSquared: Double = length * length

	}


}
