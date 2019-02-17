package net.kurobako.liquidfx


case class Surface(
					  latticeResolution: Double
				  )


case class Configuration(particles: Int,
						 gravity: Double,
						 dT: Double,
						 iteration: Int,

						) {

}
