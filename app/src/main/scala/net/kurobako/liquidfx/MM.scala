package net.kurobako.liquidfx

import java.nio.file.StandardOpenOption
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.concurrent.locks.ReentrantLock

import better.files._
import com.google.common.base.StandardSystemProperty
import javafx.scene.paint.Color
import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
import net.kurobako.liquidfx.StructDefs._
import scalafx.Includes.{handle, _}
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.binding.NumberExpression
import scalafx.beans.property._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{ColorPicker, Label, Slider}
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.PhongMaterial
import scalafx.scene.shape._
import scalafx.scene.{Group, Scene, SubScene}


object MM extends JFXApp {

	private val solverIter  = IntegerProperty(5)
	private val solverStep  = FloatProperty(1.2f)
	private val solverScale = FloatProperty(1000f)
	private val surfaceRes  = FloatProperty(1)
	private val gravity     = FloatProperty(9.8f)
	private val colour      = ObjectProperty[Color](Color.AQUA)

	private val showParticle = BooleanProperty(false)
	private val flatShading  = BooleanProperty(false)
	private val wireFrame    = BooleanProperty(false)

	private val BasePath = StandardSystemProperty.OS_NAME.value.toLowerCase match {
		case win if win.contains("win")   => File("C:\\Users\\Tom\\libfluid\\cmake-build-release\\samples\\")
		case unix if unix.contains("nix") ||
					 unix.contains("nux") ||
					 unix.contains("aix") => File("/home/tom/libfluid/cmake-build-release/samples/")
		case mac if mac.contains("mac")   => File("/Users/tom/libfluid/cmake-build-release/samples/")
		case unknown                      => throw new Exception(s"Unknown os:$unknown")
	}

	def time[R](name: String)(block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
		result
	}


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

		//		private val colour: Color = Color.rgb(0, 119, 190, 1)
		material = new PhongMaterial() {
			diffuseColor <== colour
			specularColor = Color.WHITE
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


	def openSink(path: File, size: Int): ByteBuffer = {
		import java.nio.channels.FileChannel
		path.createFileIfNotExists()
		path.writeByteArray(Array.ofDim(size))
		val filechannel = FileChannel.open(path.path,
			StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_WRITE, 0, size)
		buffer.order(ByteOrder.LITTLE_ENDIAN)
		buffer.clear()
		buffer
	}
	def openSource(path: File): ByteBuffer = {
		import java.nio.channels.FileChannel
		val filechannel = FileChannel.open(path.path, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_ONLY, 0, filechannel.size)
		buffer.order(ByteOrder.LITTLE_ENDIAN)
		buffer.clear()
		buffer
	}


	//	def readMmf(path: File)(f: ByteBuffer => Unit) = {
	//		import java.nio.channels.FileChannel
	//		val filechannel = FileChannel.open(path.path, StandardOpenOption.READ)
	//		val buffer = filechannel.map(FileChannel.MapMode.READ_ONLY, 0, filechannel.size)
	//		buffer.order(ByteOrder.LITTLE_ENDIAN)
	//		while (true) {
	//			try {
	//				f(buffer)
	//			} catch {
	//				case e: Throwable     => println("Warn:" + e.getClass + " -> " + e.getMessage)
	//				case e: InternalError => println("VM : " + e.getMessage)
	//			} finally {
	//				buffer.clear()
	//			}
	//		}
	//
	//	}

	private def makeScene(suspend: Boolean) = StructDefs.Scene(
		meta = SceneMeta(
			suspend = suspend, terminate = false,
			solverIter.value, solverStep.value, solverScale.value,
			surfaceRes.value, gravity.value),
		wells = Array(
			Well(Vec3(300), -1000f * 1000f * 50),
			Well(Vec3(1000, 1000, 1000), 1000f * 1000f * 10)
		),
		sources = Array(Source(Vec3(42), 42, 42), Source(Vec3(32), 32, 32))
	)

	private def doUpdate(codec: StructCodec[Header |> MeshTriangles, MeshTriangles],
						 sceneCodec: StructCodec[StructDefs.Scene, StructDefs.Scene])
						(action: MeshTriangles => Unit): Unit = {
		new Thread({ () =>
			@volatile var last = 0L
			val triangleSource = openSource(BasePath / "triangles.mmf")
			val sceneSink = openSink(BasePath / "scene.mmf", 8192) // FIXME compute from def
			while (true) {
				try {
					Thread.sleep((1000.0 / 60 / 2).toLong)
					val (header, readTrigs) = codec.read(triangleSource)
					val start = header.timestamp
					if (start != last && header.written == header.entries) {
						last = start
						sceneSink.clear()
						sceneCodec.write(makeScene(suspend = true), sceneSink)
						try {
							time("doUpdate") {
								action(readTrigs())
							}
						} finally {
							sceneSink.clear()
							sceneCodec.write(makeScene(suspend = false), sceneSink)
						}
					}
				} catch {
					case e: Throwable     => println("Warn:" + e.getClass + " -> " + e.getMessage)
					case e: InternalError => println("VM : " + e.getMessage)
				} finally {
					triangleSource.clear()
					sceneSink.clear()

				}
			}
		}).start()
	}

	val subScene: SubScene = SceneControl.mkScene(group, 500, 500)
	val infoLabel          = new Label() {padding = Insets(8)}

	def mkSlider[T: Numeric](source: Property[T, Number] with NumberExpression, min: Float, max: Float, name: String): HBox = {
		def valueWithName = f"$name(${source.floatValue}%.2f):"
		new HBox(
			new Label(valueWithName) {
				prefWidth = 200
				source.onChange {text = valueWithName}
			}, new Slider(min, max, source.floatValue()) {
				prefWidth = 450
				source <== value
			},
		)
	}

	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(

			new StackPane {
				children = Seq(
					subScene,
					new HBox(
						new VBox(
							mkSlider(solverIter, 0, 100, "Solver Iter"),
							mkSlider(solverScale, 100, 5000, "Solver scale"),
							mkSlider(solverStep, 0.1f, 10, "Solver step"),
							mkSlider(surfaceRes, 0.1f, 8, "Surface res"),
							mkSlider(gravity, -20, 20, "Graivty"),
							new ColorPicker(colour.value) {
								colour <== value
							}
						) {
							pickOnBounds = false
							hgrow = Priority.Always
						},
						infoLabel
					) {
						pickOnBounds = false
					}
				)
				alignment = Pos.TopLeft
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			}, 900, 900) {
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

		defs <- StructDefs.readStructDef(BasePath / "defs.json")

		headerDef <- defs.resolve[Header]("header")

		wellDef <- defs.resolve[Array[Well]]("well")
		sourceDef <- defs.resolve[Array[Source]]("source")
		sceneMetaDef <- defs.resolve[SceneMeta]("sceneMeta")

		trianglesDef <- defs.resolve[Triangles]("triangle")
		particlesDef <- defs.resolve[Particles]("particle")
		meshTrianglesDef <- defs.resolve[MeshTriangles]("meshTriangle")

		sceneCodec <- StructDefs.Scene(sceneMetaDef, headerDef, wellDef, sourceDef)
		particlesCodec <- Particles(headerDef, particlesDef)
		trianglesCodec <- Triangles(headerDef, trianglesDef)
		meshTrianglesCodec <- MeshTriangles(headerDef, meshTrianglesDef)

	} yield {


		Platform.runLater {


			val (node, view, trigs) = Bunny.makeMug()

			view.translateY = 200
			view.translateX = -100
			view.translateZ = 200
			group.children += node

			val buffer = openSink(BasePath / "colliders.mmf", size = 16 * 4 * 50000)

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
		val E = Color.WHITE


		@volatile var _points: Array[Float] = Array.empty
		@volatile var _normals: Array[Float] = Array.empty
		//		@volatile var _faces: Array[Int] = Array.empty

		@volatile var _particles: Array[Particle] = Array.empty

		@volatile var elapsed: Long = 0l
		val last: AtomicLong = new AtomicLong(System.currentTimeMillis())


		val lock = new ReentrantLock()

		AnimationTimer { _ =>

			if (meshInvalidated.getAndSet(false)) {

				try {
					lock.lock()

					mesh.points = _points
					mesh.getNormals.setAll(_normals: _*)
					mesh.faces = mkFaces(_points.length / 3, mesh.getVertexFormat)
				} finally {
					lock.unlock()
				}

				//				mesh.points = pointsCopy
				//				mesh.getNormals.setAll(normalsCopy: _*)
				//				mesh.faces = mkFaces(pointsCopy.length / 3, mesh.getVertexFormat)


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

		doUpdate(meshTrianglesCodec, sceneCodec) {
			case MeshTriangles(vertices, normals) =>
				try {
					lock.lock()
					_points = vertices
					_normals = normals
					meshInvalidated.set(true)
					val now = System.currentTimeMillis()
					elapsed = now - last.getAndSet(now)
				} finally lock.unlock()
		}

	}

	run match {
		case Left(e)  => throw e
		case Right(x) => println(x)
	}


}
