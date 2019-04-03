package net.kurobako.liquidfx

import java.nio.file.StandardOpenOption
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.ConcurrentHashMap

import better.files._
import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.Vec3
import net.kurobako.liquidfx.StructDefs._
import scalafx.Includes.{handle, _}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Point3D
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape._
import scalafx.scene.{AmbientLight, Group, Scene}


object MM extends JFXApp {

	private val BasePath = File("/home/tom/libfluid/cmake-build-release/samples/")

	def time[R](name: String)(block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
		result
	}


	private val showParticle = BooleanProperty(false)

	val ambient = new AmbientLight(Color.LightYellow)

	val mesh     = new TriangleMesh(VertexFormat.PointTexcoord) {
		texCoords = Array(1, 1)
	}
	val meshView = new MeshView(mesh) {
		drawMode = DrawMode.Fill
		material = new PhongMaterial(Color.rgb(0, 119, 190)) {
			specularColor = Color.rgb(190, 119, 190, 0.5)

		}
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


	def openMmf(path: File): ByteBuffer = {
		import java.nio.channels.FileChannel
		path.createFileIfNotExists()
		val maxMMF = 16 * 4 * 50000
		path.writeByteArray(Array.ofDim(maxMMF))
		println(s"collider = ${path.size.toFloat / 1024 / 1024}MB")
		val filechannel = FileChannel.open(path.path,
			StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_WRITE, 0, maxMMF)
		buffer.order(ByteOrder.LITTLE_ENDIAN)
		buffer.clear()
		buffer
	}


	def readMmf(path: File)(f: ByteBuffer => Unit) = {
		import java.nio.channels.FileChannel
		val filechannel = FileChannel.open(path.path, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_ONLY, 0, filechannel.size)
		buffer.order(ByteOrder.LITTLE_ENDIAN)
		while (true) {
			try {
				//				buffer.load()
				f(buffer)
			} catch {
				case e: Throwable     => e.printStackTrace()
				case e: InternalError => println("VM : " + e.getMessage)
			} finally {
				buffer.clear()
			}
		}

	}

	private def unoptimisedThreadedContinuousRead[A](file: File, f: StructCodec[Header |> A, A])
													(h: A => Unit): Unit = {
		new Thread({ () =>
			@volatile var last = 0L
			readMmf(file) {
				buffer =>
					Thread.sleep((1000.0 / 60 / 2).toLong)

					val (header, g) = f.read(buffer)
					val start = header.timestamp
					if (start != last) {
						last = start
						time(file.name) {
							val a = g()
							Platform.runLater(h(a))
						}
					}
			}
		}).start()
	}

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
					case KeyCode.P => showParticle.value = !showParticle.value
					case _         =>
				}
			}
		}
	}

	val run = for {
		headerDef <- StructDefs.readStructDef[Header](BasePath / "header.json")
		trianglesDef <- StructDefs.readStructDef[Triangles](BasePath / "triangle.json")
		particlesDef <- StructDefs.readStructDef[Particles](BasePath / "particle.json")
		headerCodec <- Header(headerDef)
		trianglesCodec <- Triangles(headerDef, trianglesDef)
		particlesCodec <- Particles(headerDef, particlesDef)
	} yield {




		Platform.runLater {


			val (node, view, trigs) = Bunny.makeBunny()

			view.scaleX = 4.8
			view.scaleY = 4.8
			view.scaleZ = 4.8
			view.translateY = 200
			view.translateX = -100
			view.translateZ = 200
			group.children += node

			val transformed = trigs.points.grouped(3).flatMap { g =>
				val p3 = view.localToParent(g(0), g(1), g(2))
				Array(p3.getX.toFloat, p3.getY.toFloat, p3.getZ.toFloat)
			}.toArray

			println(s"points: ${transformed.length} ${transformed.par.min} ${transformed.par.max}")
			val buffer = openMmf(BasePath / "colliders.mmf")
			trianglesCodec.write(Triangles(transformed), buffer)
		}


		val particles = new ConcurrentHashMap[Long, Sphere]()
		val S = Color.web("A76D34")
		val E = Color.White
		unoptimisedThreadedContinuousRead(BasePath / "particles.mmf", particlesCodec) {
			case Particles(xs) =>
				xs.foreach { x =>
					val sphere = particles.computeIfAbsent(x.id, { k =>
						new Sphere(10) {
							visible <== showParticle
							material = new PhongMaterial(S.interpolate(E, k.toFloat / xs.length))
							group.children += this.delegate
						}
					})
					sphere.translateX = x.position.x
					sphere.translateY = x.position.y
					sphere.translateZ = x.position.z
				}
		} -> unoptimisedThreadedContinuousRead(BasePath / "triangles.mmf", trianglesCodec) {
			case Triangles(xs) =>

				def mkFaces(n: Int) = {
					val faces = Array.ofDim[Int](n * 2)
					for (i <- 0 until n) {
						faces(i * 2 + 0) = i // vertex points
						//								faces(i * 2 + 1) = i / 3 // normals, so 1/3 of point length
					}
					faces
				}

				val fs = mkFaces(xs.length / 3)
				val sma = Array.tabulate[Int](fs.length)(x => 32)
				println("Trigs: " + xs.length)
				mesh.points = xs
				mesh.faces = fs
			//				mesh.getFaceSmoothingGroups.setAll(sma: _*)

		}

	}

	run match {
		case Left(e)  => throw e
		case Right(x) => println(x)
	}


}
