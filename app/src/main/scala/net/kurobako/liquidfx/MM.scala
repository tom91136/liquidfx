package net.kurobako.liquidfx

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Paths, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap

import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.{Particle, Vec3}
import scalafx.Includes.{handle, _}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape._
import scalafx.scene.{AmbientLight, Group, Scene}

object MM extends JFXApp {


	val ambient = new AmbientLight(Color.LightYellow)

	val mesh     = new TriangleMesh() {
		texCoords = Array(0, 0)
	}
	val meshView = new MeshView(mesh) {
		drawMode = DrawMode.Fill
		material = new PhongMaterial(Color.rgb(0, 119, 190))
		cullFace = CullFace.None
	}

	def updateMesh(mesh: TriangleMesh, xs: Seq[Triangle]) = {
		mesh.points = xs.flatMap(_.points).map(_.toFloat).toArray
		mesh.faces = Array.tabulate(xs.length * 3)(i => i -> 0).flatMap { case (l, r) => Array(l, r) }
	}

	val group = new Group(
		SceneControl.mkAxis(),
		meshView,
	)


	def time[R](name: String)(block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
		result
	}

	def readMmf(path: java.nio.file.Path)(f: ByteBuffer => Unit) = {
		import java.nio.channels.FileChannel


		val filechannel = FileChannel.open(path, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_ONLY, 0, filechannel.size)
		buffer.order(ByteOrder.LITTLE_ENDIAN)
		while (true) {
			try {
				buffer.load()
				f(buffer)
			} catch {
				case e: Throwable     => e.printStackTrace()
				case e: InternalError => println("VM : " + e.getMessage)
			} finally {
				buffer.clear()
			}
		}

	}
	def clamp(min: Double, max: Double, value: Double) = Math.max(min, Math.min(max, value))


	new Thread({ () =>
		var last = 0L

		val particles = new ConcurrentHashMap[Long, Sphere]()
		val S = Color.web("A76D34")
		val E = Color.White
		readMmf(Paths.get("/home/tom/libfluid/cmake-build-release/particles.mmf")) {
			buffer =>
				Thread.sleep(8)
				val start = buffer.getLong
				if (start != last) {
					last = start
					time("Particles.mmf") {
						val length = buffer.getLong.toInt
						val ys = Array.tabulate(length) { _ =>
							val t = buffer.getLong
							val tpe = buffer.getInt
							val mass = buffer.getFloat
							val pos = Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat)
							val vel = Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat)
							Particle(t, mass, pos, vel)
						}

						Platform.runLater {
							ys.foreach { y =>
								val sphere = particles.computeIfAbsent(y.a, { k =>
									new Sphere(10) {



										material = new PhongMaterial(S.interpolate(E, k.toFloat / length))
										group.children += this.delegate
									}
								})
								sphere.translateX = y.position.x
								sphere.translateY = y.position.y
								sphere.translateZ = y.position.z
							}
						}
					}
				}
		}
	}).start()

	new Thread({ () =>
		var last = 0L
		readMmf(Paths.get("/home/tom/libfluid/cmake-build-release/triangles.mmf")) {
			buffer =>
				Thread.sleep(8)
				val start = buffer.getLong
				if (start != last) {
					last = start
					time("triangles.mmf") {
						val length = buffer.getLong.toInt
						//						val ts = Array.tabulate(length) { _ =>
						//							Triangle(
						//								Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat),
						//								Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat),
						//								Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat))
						//						}

						val ts = Array.tabulate(length * 9) { _ => buffer.getFloat }
						def mkFaces(n: Int) = {
							val faces = Array.ofDim[Int](n * 2)
							for (i <- 0 until n) faces(i * 2) = i
							faces
						}
						val fs = mkFaces(ts.length / 3)

						Platform.runLater {
							mesh.points = ts
							mesh.faces = fs
						}
					}
				}
		}
	}).start()


	val subScene = SceneControl.mkScene(group, 500, 500)

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


		), 800, 800) {
			onKeyPressed = { e: KeyEvent =>
				e.code match {
					case KeyCode.S => meshView.visible = !meshView.visible.value
					case _         =>
				}
			}
		}


	}


}
