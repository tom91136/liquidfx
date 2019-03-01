package net.kurobako.liquidfx

import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import java.time.Instant

import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.{Particle, Vec3}
import scalafx.Includes.handle
import scalafx.Includes._
import scalafx.animation.Animation
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{AmbientLight, Group, Scene}
import scalafx.scene.control.{Button, Label, Slider, ToggleButton}
import scalafx.scene.layout.{HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape._
import scalafx.stage.FileChooser

object MM extends JFXApp {


	val ambient = new AmbientLight(Color.LightYellow)


	def updateMesh(mesh: TriangleMesh, xs: Seq[Triangle]) = {
		mesh.points = xs.flatMap(_.points).map(_.toFloat).toArray
		mesh.texCoords = Array(0, 0)
		mesh.faces = Array.tabulate(xs.length * 3)(_ :: 0 :: Nil).flatten
	}

	val pcount            = 3000
	val xs: Array[Sphere] = Array.tabulate(pcount) { i => new Sphere(5) }

	new Thread({ () =>

		import java.nio.channels.FileChannel
		val file = Paths.get("/home/tom/libfluid/cmake-build-release/ipc.mmf")

		def time[R](name: String)(block: => R): R = {
			val t0 = System.nanoTime()
			val result = block
			val t1 = System.nanoTime()
			println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
			result
		}

		val filechannel = FileChannel.open(file, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_ONLY, 0, filechannel.size)
		buffer.order(ByteOrder.LITTLE_ENDIAN)
		var last: Long = 0
		while (buffer.isLoaded) {
			Thread.sleep(5)
			val start = buffer.getLong
			if (start > last) {
				last = start

				val ys = Array.ofDim[Particle[Long]](pcount)


				time("Read one") {
					//					println(s"Size=${buffer.capacity()}")
					for (i <- 0 until pcount) {
						val t = buffer.getLong
						val tpe = buffer.getInt
						val mass = buffer.getFloat
						val pos = Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat)
						val vel = Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat)

						ys(t.toInt) = Particle(t, mass, pos, vel)





						//						println(s"[$i] $t $tpe $mass $pos $vel")
					}

					def clamp(min: Double, max: Double, value: Double) = Math.max(min, Math.min(max, value))

					Platform.runLater {
						ys.foreach { y =>

							val p = xs(y.a.toInt)

							p.translateX = y.position.x
							p.translateY = y.position.y
							p.translateZ = y.position.z
							val v = clamp(120, 255, y.velocity.lengthSquared * 100).toInt
							p.material = new PhongMaterial(Color.rgb(v / 2, v / 2, v, 0.8))
						}


					}
				}

			} else {
			}


			buffer.clear()
		}

		()
	}).start()

	val subScene = SceneControl.mkScene(new Group(
		SceneControl.mkAxis(),

	) {
		children ++= xs.map(_.delegate)
	}, 500, 500)


	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(new VBox(

			new StackPane {
				children = Seq(
					subScene,
				)
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			},


		), 800, 800)


	}


}
