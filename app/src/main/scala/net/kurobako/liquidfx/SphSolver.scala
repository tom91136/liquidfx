package net.kurobako.liquidfx

import java.util.concurrent.ConcurrentHashMap

import net.kurobako.liquidfx.Maths.Vec3
import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Response}

import scala.collection.mutable.ArrayBuffer

// based on https://nccastaff.bournemouth.ac.uk/jmacey/MastersProjects/MSc15/06Burak/BurakErtekinMScThesis.pdf
// and also http://mmacklin.com/pbf_sig_preprint.pdf
class SphSolver(val h: Float = 0.1f, // Particle(smoothing kernel) size
				val scale: Float = 100f,
			   ) {

	import Maths._

	final val Vd = 0.49f // Velocity dampening
	final val Rho = 6378f // Reference density
	final val Epsilon    = 0.00000001f
	final val CfmEpsilon = 600.0f // CFM propagation

	final val C                = 0.00001f
	final val VorticityEpsilon = 0.0005f
	final val CorrK            = 0.0001f
	final val CorrN            = 4f
	final val CorrDeltaQ       = 0.3f * h

	private val hp2 = h * h
	private val h2  = h * 2f


	private val poly6Factor      : Float = 315.0f / (64.0f * Math.PI * (h ** 9d)).toFloat
	private val spikyKernelFactor: Float = -(45f / (Math.PI * (h ** 6d))).toFloat
	private val p6DeltaQ         : Float = poly6Kernel(CorrDeltaQ).toFloat

	private def poly6Kernel(r: Float): Float =
		if (r <= h) poly6Factor * ((hp2 - r * r) ** 3f) else 0f

	// 4.9.2 Spiky Kernel(Desbrun and Gascuel (1996))
	private def spikyKernelGradient(x: Vec3, y: Vec3): Vec3 = {
		val r = x distance y
		if (r > h || r < Epsilon) Vec3.Zero
		else (x - y) * (spikyKernelFactor * (((h - r) ** 2f) / r))
	}


	def advance[A](dt: Float = 0.0083f,
				   iteration: Int,
				   constantForce: Particle[A] => Vec3 = { p: Particle[A] => Vec3(0f, p.mass * 9.8f, 0f) })
				  (ps: Array[Particle[A]],
				   obs: Seq[Ray => Response]): Array[Particle[A]] = {


		class Atom(val particle: Particle[A],
				   var neighbours: Array[Atom] = null,
				   var now: Vec3,
				   var lambda: Float = 0,
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


		def findNeighbour(xs: Seq[Atom]): Unit = {

			val bs = MutableUnsafeOctree[Atom](Vec3.Zero, 5)(_.now)
			xs.foreach(bs.insertPoint(_))
			xs.par.foreach { a =>
				a.neighbours = bs.pointsInSphere(a.now, h2)
				a.neighbours = xs.filter { b => (a.now distance b.now) <= h2 }.toArray
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
			val rho = a.neighbours.foldLeft(0f)((acc, b) => acc + b.particle.mass * poly6Kernel(a.now distance b.now))

			// foldLeft 23%
			val norm2 = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
				spikyKernelGradient(a.now, b.now) *+ (1f / Rho, acc)
			}.lengthSquared


			val C = rho / Rho - 1f

			//			println(a.particle.a + "->"+ C + " " + a.neighbours.map(_.particle.a).toList)
			a.lambda = -C / (norm2 + CfmEpsilon)
		}

		// hotspot 30%
		def solveDeltaP(xs: Seq[Atom]): Unit = xs.par.foreach { a =>
			a.deltaP = a.neighbours.foldLeft(Vec3.Zero) { (acc, b) =>
				// disable corr by setting it to 0
				val corr = -CorrK * ((poly6Kernel(a.now distance b.now) / p6DeltaQ) ** CorrN)
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
				val N = eta * (1f / eta.length)
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
					velocity = deltaX *+ (1f / dt, a.velocity) * Vd,
					surface = mc.distance(a.now) > 0.09f

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
						   mass: Float = 1,
						   surface: Boolean = false,
						   position: Vec3,
						   velocity: Vec3 = Vec3.Zero,
						   neighbours: Seq[Particle[A]] = Nil,
						  )

	case class Ray(prev: Vec3, origin: Vec3, velocity: Vec3) {}

	case class Response(position: Vec3, velocity: Vec3)


}
