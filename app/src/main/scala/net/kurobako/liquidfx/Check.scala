package net.kurobako.liquidfx

import net.kurobako.liquidfx.SphSolver.Vec3


object Check extends App{


	import com.thoughtworks.compute.gpu._




	val as: Tensor = Tensor(Array(
		Vec3.One.array[Float](_.toFloat),
		Vec3.One.array[Float](_.toFloat),
		Vec3.One.array[Float](_.toFloat),
		Vec3.One.array[Float](_.toFloat),
	))


	val bs: Tensor = Tensor(Array(
		(Vec3.One * 2).array[Float](_.toFloat),
		(Vec3.One * 2).array[Float](_.toFloat),
		(Vec3.One * 2).array[Float](_.toFloat),
		(Vec3.One * 2).array[Float](_.toFloat),
	))





	val plus100 = as + bs// Tensor.fill(100.0f, Array(2, 3))

	println(plus100) // Output [[101.0,102.0,103.0],[104.0,105.0,106.0]]
	println(Tensor.min(as, bs)) // Output [[101.0,102.0,103.0],[104.0,105.0,106.0]]

}
