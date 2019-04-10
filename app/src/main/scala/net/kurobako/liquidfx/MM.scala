package net.kurobako.liquidfx

import java.nio.file.StandardOpenOption
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import better.files._
import com.google.common.base.StandardSystemProperty
import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
import net.kurobako.liquidfx.StructDefs._
import scalafx.Includes.{handle, _}
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.{Insets, Point3D, Pos}
import scalafx.scene.control.Label
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape._
import scalafx.scene.{AmbientLight, Group, PointLight, Scene, SubScene}


object MM extends JFXApp {

	private val BasePath = StandardSystemProperty.OS_NAME.value.toLowerCase match {
		case win if win.contains("win")   => File("C:\\Users\\Tom\\libfluid\\cmake-build-release\\samples\\")
		case unix if unix.contains("nix") ||
					 unix.contains("nux") ||
					 unix.contains("aix") => File("/home/tom/libfluid/cmake-build-release/samples/")
		case unknown                      => throw new Exception(s"Unknown os:$unknown")
	}


	def time[R](name: String)(block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
		result
	}

	private val showParticle = BooleanProperty(false)
	private val flatShading  = BooleanProperty(false)
	private val wireFrame    = BooleanProperty(false)
	val meshInvalidated    : AtomicBoolean = new AtomicBoolean(true)
	val particleInvalidated: AtomicBoolean = new AtomicBoolean(true)


	val mesh     = new TriangleMesh(VertexFormat.PointNormalTexcoord) {
		vertexFormat <== when(flatShading) choose VertexFormat.PointTexcoord otherwise VertexFormat.PointNormalTexcoord
		texCoords = Array(1, 1)
		vertexFormat.onChange {
			faces.clear()
			points.clear()
			meshInvalidated.set(true)
			Platform.requestNextPulse()
		}
	}
	val meshView = new MeshView(mesh) {
		cullFace = CullFace.None
		drawMode <== when(wireFrame) choose DrawMode.Line otherwise DrawMode.Fill
		material = new PhongMaterial(Color.rgb(0, 119, 190, 1)) {
			specularColor = Color.rgb(255, 255, 255, 1)
		}
	}


	def updateMesh(mesh: TriangleMesh, xs: Seq[Triangle]) = {
		mesh.points = xs.flatMap(_.points).toArray
		mesh.faces = Array.tabulate(xs.length * 3)(i => i -> 0).flatMap { case (l, r) => Array(l, r) }
	}

	val group = new Group(
		SceneControl.mkAxis(),
		new Box(10000, 1, 10000) {translateY = 2000},
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
							h(a)
						}
					}
			}
		}).start()
	}

	val subScene: SubScene = SceneControl.mkScene(group, 500, 500)
	val infoLabel          = new Label() {padding = Insets(8)}
	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(

			new StackPane {
				children = Seq(
					subScene,
					infoLabel
				)
				alignment = Pos.TopLeft
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			}, 800, 800) {
			onKeyPressed = { e: KeyEvent =>
				e.code match {
					case KeyCode.S => meshView.visible = !meshView.visible.value
					case KeyCode.P => showParticle.value = !showParticle.value
					case KeyCode.W => wireFrame.value = !wireFrame.value
					case KeyCode.F => flatShading.value = !flatShading.value
					case _         =>
				}
			}
		}
	}

	def mkFaces(n: Int, format: VertexFormat): Array[Int] = {
		format match {
			case VertexFormat.PointTexcoord       =>
				val faces = Array.ofDim[Int](n * 2)
				for (i <- 0 until n) faces(i * 2 + 0) = i // vertices
				faces
			case VertexFormat.PointNormalTexcoord =>
				val faces = Array.ofDim[Int](n * 3)
				for (i <- 0 until n) {
					faces(i * 3 + 0) = i // vertices
					faces(i * 3 + 1) = i // normals
				}
				faces
		}
	}

	val run = for {
		headerDef <- StructDefs.readStructDef[Header](BasePath / "header.json")
		meshTrianglesDef <- StructDefs.readStructDef[MeshTriangles](BasePath / "mesh_triangle.json")
		trianglesDef <- StructDefs.readStructDef[Triangles](BasePath / "triangle.json")
		particlesDef <- StructDefs.readStructDef[Particles](BasePath / "particle.json")
		trianglesCodec <- Triangles(headerDef, trianglesDef)
		meshTrianglesCodec <- MeshTriangles(headerDef, meshTrianglesDef)
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

			val buffer = openMmf(BasePath / "colliders.mmf")

			group.onMouseReleased = handle {group.onMouseDragged = null}
			group.onMouseClicked = { start: MouseEvent =>
				if (start.pickResult.intersectedNode.exists(_.delegate == view)) {
					val _start = Vec3(start.x, start.y, start.z)
					group.onMouseDragged = { drag: MouseEvent =>
						val delta = Vec3(drag.x, drag.y, drag.z) - _start
						view.translateX = delta.x
						view.translateY = delta.y

						val transformed = trigs.points.grouped(3).flatMap { g =>
							val p3 = view.localToParent(g(0), g(1), g(2))
							Array(p3.getX.toFloat, p3.getY.toFloat, p3.getZ.toFloat)
						}.toArray

						println(s"points: ${transformed.length} ${transformed.par.min} ${transformed.par.max}")
						buffer.clear()
						trianglesCodec.write(Triangles(transformed), buffer)

						drag.consume()
					}
					start.consume()
				}
			}



		}


		val particles = new ConcurrentHashMap[Long, Sphere]()
		val S = Color.web("A76D34")
		val E = Color.White


		@volatile var _points: Array[Float] = Array.empty
		@volatile var _normals: Array[Float] = Array.empty
		//		@volatile var _faces: Array[Int] = Array.empty

		@volatile var _particles: Array[Particle] = Array.empty

		@volatile var elapsed: Long = 0l
		val last: AtomicLong = new AtomicLong(System.currentTimeMillis())


		AnimationTimer { _ =>

			if (meshInvalidated.getAndSet(false)) {
				mesh.points = _points
				mesh.getNormals.setAll(_normals: _*)
				mesh.faces = mkFaces(_points.length / 3, mesh.getVertexFormat)
				//				_faces = Array.empty
				println("\t->Tick")
				infoLabel.text = s"${(1000.0 / elapsed).round}FPS (${elapsed}ms)" +
								 s"\nParticles : ${_particles.length}" +
								 s"\nTriangles : ${_points.length / 3}" +
								 s"\nVertices  : ${_points.length}"
			}
			if (showParticle.get() && particleInvalidated.getAndSet(false)) {
				_particles.foreach { x =>
					val sphere = particles.computeIfAbsent(x.id, { k =>
						new Sphere(10) {
							visible <== showParticle
							material = new PhongMaterial(S.interpolate(E, k.toFloat / _particles.length))
							group.children += this.delegate
						}
					})
					sphere.translateX = x.position.x
					sphere.translateY = x.position.y
					sphere.translateZ = x.position.z
				}
			}
		}.start()


		unoptimisedThreadedContinuousRead(BasePath / "particles.mmf", particlesCodec) {
			case Particles(xs) =>
				_particles = xs
				particleInvalidated.set(true)
		} -> unoptimisedThreadedContinuousRead(BasePath / "triangles.mmf", meshTrianglesCodec) {
			case MeshTriangles(vertices, normals) =>



				_points = vertices
				_normals = normals
				//				_faces = mkFaces(vertices.length / 3)
				meshInvalidated.set(true)
				val now = System.currentTimeMillis()
				elapsed = now - last.getAndSet(now)
				println(s"->Vertices: ${vertices.length} elasped=${elapsed}ms")
		}

	}

	run match {
		case Left(e)  => throw e
		case Right(x) => println(x)
	}


}
