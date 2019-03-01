package net.kurobako.liquidfx

import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Response, Vec3}


object Check extends App {


	val ps = Array.tabulate(10)(i => Particle(i, position = Vec3(i * 10, i * 10, i * 10)))

	val cf = { p: Particle[Int] => Vec3(0, p.mass * 9.8, 0) }

	val solver = new SphSolver(0.1, 500)


	val solved = (0 until 5).foldLeft(ps) { (acc, _) =>
		solver.advance[Int](iteration = 5, constantForce = cf)(acc, { r: Ray => Response(r.origin.clamp(0, 500, 0, 500, 0, 500), r.velocity) } :: Nil)
	}

	def showVec(v : Vec3) = {
		s"glm::vec3(${v.x.formatted("%.3f")}, ${v.y.formatted("%.3f")}, ${v.z.formatted("%.3f")})"
	}

	println(solved.map(p => s"Particle(${p.a}, ${p.mass}, ${showVec(p.position)}, ${showVec(p.velocity)})").mkString("\n"))


}
