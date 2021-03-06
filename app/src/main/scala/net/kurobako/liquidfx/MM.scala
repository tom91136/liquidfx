package net.kurobako.liquidfx

import java.nio.file.StandardOpenOption
import java.nio.{ByteBuffer, ByteOrder}
import java.time.Instant
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
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos, Side}
import scalafx.scene.chart.{AreaChart, CategoryAxis, NumberAxis, XYChart}
import scalafx.scene.control.{Button, ButtonBar, ColorPicker, Label, ListCell, ListView, Slider}
import scalafx.scene.effect.DropShadow
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{BorderPane, HBox, Priority, Region, StackPane, VBox}
import scalafx.scene.paint.PhongMaterial
import scalafx.scene.shape._
import scalafx.scene.{DepthTest, Group, Node, Scene, SubScene}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


object MM extends JFXApp {

	private val solverIter  = IntegerProperty(5)
	private val solverStep  = FloatProperty(1.2f)
	private val solverScale = FloatProperty(300f)
	private val surfaceRes  = FloatProperty(1.5f)
	private val gravity     = FloatProperty(9.8f)
	private val colour      = ObjectProperty[Color](Color.rgb(0, 119, 190))

	private val showParticle     = BooleanProperty(false)
	private val showVertexColour = BooleanProperty(false)
	private val flatShading      = BooleanProperty(false)
	private val wireFrame        = BooleanProperty(false)

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

	val vertexColourGroup = new Group() {depthTest = DepthTest.Disabled}
	val particleGroup     = new Group() {depthTest = DepthTest.Enable}

	val sceneGroup = new Group(
		SceneControl.mkAxis(),
		//		new Box(10000, 1, 10000) {translateY = 2000},
		meshView,
		vertexColourGroup,
		particleGroup
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


	private def makeScene(suspend: Boolean) = {
		val xs = elementList.items.value.map(_.repr)
		StructDefs.Scene(
			meta = SceneMeta(
				suspend = suspend, terminate = false,
				solverIter = solverIter.value,
				solverStep = solverStep.value,
				solverScale = solverScale.value,
				surfaceRes = surfaceRes.value,
				gravity = gravity.value,
				minBound = Vec3(-500),
				maxBound = Vec3(500)
			),
			wells = xs.collect { case x: Well => x }.toArray,
			sources = xs.collect { case x: Source => x }.toArray,
			drains = xs.collect { case x: Drain => x }.toArray
		)
	}

	private def doUpdate(meshTriangle: StructCodec[Header |> MeshTriangles, MeshTriangles],
						 particles: StructCodec[Header |> Particles, Particles],
						 sceneCodec: StructCodec[StructDefs.Scene, StructDefs.Scene])
						(fMeshTriangles: MeshTriangles => Unit,
						 fParticles: Particles => Unit): Unit = {
		new Thread({ () =>

			val lastMeshTriangle = new AtomicLong(0)
			val lastParticles = new AtomicLong(0)

			val triangleSource = openSource(BasePath / "triangles.mmf")
			val particleSource = Try(openSource(BasePath / "particles.mmf")).toOption
			val sceneSink = openSink(BasePath / "scene.mmf", 8192) // FIXME compute from def

			def readMeshTriangles(last: AtomicLong): Unit = {
				val (header, readTrigs) = meshTriangle.read(triangleSource)
				val start = header.timestamp
				if (start != last.get() && header.written == header.entries) {
					last.set(start)
					time("read MeshTriangles") {fMeshTriangles(readTrigs())}
				}
			}

			def readParticles(last: AtomicLong): Unit = particleSource.foreach { source =>
				val (header, readParticles) = particles.read(source)
				val start = header.timestamp
				if (start != last.get() && header.written == header.entries) {
					last.set(start)
					time("read Particles") {fParticles(readParticles())}
				}
			}

			while (true) {
				try {
					Thread.sleep((1000.0 / 60 / 2).toLong)
					val scene = makeScene(suspend = true)
					try {
						sceneSink.clear()
						sceneCodec.write(scene, sceneSink)
						readMeshTriangles(lastMeshTriangle)
						readParticles(lastParticles)
					} finally {
						sceneSink.clear()
						sceneCodec.write(scene.copy(meta = scene.meta.copy(suspend = false)), sceneSink)
					}
				} catch {
					case e: Throwable     => println("Warn:" + e.getClass + " -> " + e.getMessage); e.printStackTrace()
					case e: InternalError => println("VM : " + e.getMessage)
				} finally {
					triangleSource.clear()
					particleSource.foreach(_.clear())
					sceneSink.clear()
				}
			}
		}).start()
	}

	val subScene: SubScene = SceneControl.mkScene(sceneGroup, 500, 500)
	val infoLabel          = new Label() {
		alignmentInParent = Pos.TopLeft
		textFill = Color.WHITE
		effect = new DropShadow(4, Color.BLACK)
		padding = Insets(8)
	}

	def mkSlider[T: Numeric](source: Property[T, Number] with NumberExpression,
							 min: Float, max: Float, name: String,
							 labelWidth: Double = Region.USE_COMPUTED_SIZE): HBox = {
		def valueWithName = f"$name(${source.floatValue}%.2f):"
		new HBox(
			new Label(valueWithName) {
				minWidth = labelWidth
				source.onChange {text = valueWithName}
			}, new Slider(min, max, source.floatValue()) {
				hgrow = Priority.Always
				source <== value
			},
		)
	}

	sealed trait Element[A] {
		def repr: A
		def gizmo: Node
	}

	def mkElementGizmo(color: Color, x: FloatProperty, y: FloatProperty, z: FloatProperty): Sphere =
		new Sphere(20) {
			material = new PhongMaterial(color)
			translateX <== x
			translateY <== y
			translateZ <== z
		}

	case class WellElement(x: FloatProperty = FloatProperty(0),
						   y: FloatProperty = FloatProperty(0),
						   z: FloatProperty = FloatProperty(0),
						   force: FloatProperty = FloatProperty(10),
						  ) extends Element[Well] {
		override val gizmo: Node = mkElementGizmo(Color.RED, x, y, z)
		override def repr: Well = Well(Vec3(x.value, y.value, z.value), force.value)
	}

	def packARGB(c: Color): Int = {
		val R = colour.value.getRed * 255
		val G = colour.value.getGreen * 255
		val B = colour.value.getBlue * 255
		val A = colour.value.getOpacity * 255
		((A.toInt & 0xFF) << 24) |
		((R.toInt & 0xFF) << 16) |
		((G.toInt & 0xFF) << 8) |
		((B.toInt & 0xFF) << 0)
	}

	def unpackARGB(x: Int): Color = Color.rgb(
		(x >> 16) & 0xFF,
		(x >> 8) & 0xFF,
		(x >> 0) & 0xFF,
		((x >> 24) & 0xFF).toFloat / 255)

	case class SourceElement(x: FloatProperty = FloatProperty(0),
							 y: FloatProperty = FloatProperty(0),
							 z: FloatProperty = FloatProperty(0),

							 vx: FloatProperty = FloatProperty(0),
							 vy: FloatProperty = FloatProperty(0),
							 vz: FloatProperty = FloatProperty(0),

							 rate: IntegerProperty = IntegerProperty(10),
							 tag: IntegerProperty = IntegerProperty(2),
							 colour: ObjectProperty[Color] = ObjectProperty[Color](Color.RED)
							) extends Element[Source] {
		override val gizmo: Node = mkElementGizmo(Color.BLUE, x, y, z)
		override def repr: Source = Source(
			centre = Vec3(x.value, y.value, z.value), velocity = Vec3(vx.value, vy.value, vz.value),
			rate = rate.value, tag = tag.value, colour = packARGB(colour.value))
	}

	case class DrainElement(x: FloatProperty = FloatProperty(0),
							y: FloatProperty = FloatProperty(0),
							z: FloatProperty = FloatProperty(0),
							width: FloatProperty = FloatProperty(100),
							depth: FloatProperty = FloatProperty(100)
						   ) extends Element[Drain] {
		override val gizmo: Node = mkElementGizmo(Color.rgb(0, 0, 0, 0.5), x, y, z)
		override def repr: Drain = Drain(Vec3(x.value, y.value, z.value), width.value, depth.value)
	}

	import javafx.scene.{chart => jfxsc}

	val InitMs                           = Instant.now().toEpochMilli
	val fpsSeries                        = ObservableBuffer[jfxsc.XYChart.Data[String, Number]]()
	val chart: AreaChart[String, Number] = new AreaChart(
		CategoryAxis("+T"),
		NumberAxis("Value"),
		ObservableBuffer(XYChart.Series[String, Number]("FPS", fpsSeries))
	) {
		maxHeight = 300

	}
	chart.legendSide = Side.Right
	chart.createSymbols = false
	chart.animated = false
	chart.opacity = 0.7


	val elementList = new ListView[Element[_]]() {
		prefWidth = 300
		vgrow = Priority.Always
		def mkRow(node: Node, remove: () => Unit) = {
			new HBox(node, new Button("X") {onAction = handle(remove())}) {
				alignment = Pos.CenterLeft
			}
		}
		cellFactory = { _ =>
			new ListCell[Element[_]] {
				item.onChange { (_, _, n) =>
					val deleteItem = { () =>
						items.value -= n
						sceneGroup.children -= n.gizmo
						()
					}
					graphic = n match {
						case null                                                  => null
						case WellElement(x, y, z, force)                           => mkRow(new VBox(
							new Label("Well"),
							mkSlider(x, -500, 500, "X", 80),
							mkSlider(y, -500, 500, "Y", 80),
							mkSlider(z, -500, 500, "Z", 80),
							mkSlider(force, -50000, 50000, "Force", 80),
						), deleteItem)
						case SourceElement(x, y, z, vx, vy, vz, rate, tag, colour) =>
							mkRow(new VBox(
								new Label("Source"),
								mkSlider(x, -500, 500, "X", 80),
								mkSlider(y, -500, 500, "Y", 80),
								mkSlider(z, -500, 500, "Z", 80),
								mkSlider(vx, -500, 500, "VX", 80),
								mkSlider(vy, -500, 500, "VY", 80),
								mkSlider(vz, -500, 500, "VZ", 80),
								mkSlider(rate, 0, 100, "Rate", 80),
								mkSlider(tag, 0, 1000, "Tag", 80),
								new ColorPicker(colour.value) {colour <== this.value}
							), deleteItem)
						case DrainElement(x, y, z, width, depth)                   => mkRow(new VBox(
							new Label("Drain"),
							mkSlider(x, -500, 500, "X", 80),
							mkSlider(y, -500, 500, "Y", 80),
							mkSlider(z, -500, 500, "Z", 80),
							mkSlider(width, 0, 1000, "Width", 80),
							mkSlider(depth, 0, 1000, "Depth", 80),
						), deleteItem)
					}


				}
			}
		}
	}

	val sceneControls = new HBox(
		new VBox(
			mkSlider(solverIter, 0, 100, "Solver Iter", 200),
			mkSlider(solverScale, 100, 5000, "Solver scale", 200),
			mkSlider(solverStep, 0.1f, 10, "Solver step", 200),
			mkSlider(surfaceRes, 0.1f, 8, "Surface res", 200),
			mkSlider(gravity, -20, 20, "Graivty", 200),

		) {
			pickOnBounds = false
			hgrow = Priority.Always
		},
		new ColorPicker(colour.value) {
			colour <== value
		}
	) {
		styleClass += "tool-bar"
		pickOnBounds = false
	}

	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(

			new StackPane {
				children = Seq(
					subScene,
					new BorderPane() {
						top = sceneControls
						def mkAddElementButton(name: String, default: () => Element[_]) = {
							new Button(name) {
								onAction = handle {
									val x = default()
									sceneGroup.children += x.gizmo
									elementList.items.value += x
								}
							}
						}
						bottom = chart
						center = infoLabel
						right = new VBox(
							new HBox(
								mkAddElementButton("+Well", () => WellElement()),
								mkAddElementButton("+Source", () => SourceElement()),
								mkAddElementButton("+Drain", () => DrainElement())
							) {
								styleClass += "tool-bar"
								spacing = 4
								padding = Insets(4)
							}, elementList
						)
						pickOnBounds = false
					}
				)
				alignment = Pos.TopLeft
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			}, 1200, 900) {
			onKeyPressed = { e: KeyEvent =>
				e.code match {
					case KeyCode.S => meshView.visible = !meshView.visible.value
					case KeyCode.P => showParticle.value = !showParticle.value
					case KeyCode.C => showVertexColour.value = !showVertexColour.value
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
		drainDef <- defs.resolve[Array[Drain]]("drain")
		sceneMetaDef <- defs.resolve[SceneMeta]("sceneMeta")

		vec3Def <- defs.resolve[Vec3]("vec3")


		trianglesDef <- defs.resolve[Triangles]("triangle")
		particlesDef <- defs.resolve[Particles]("particle")
		meshTrianglesDef <- defs.resolve[MeshTriangles]("meshTriangle")

		sceneCodec <- StructDefs.Scene(sceneMetaDef, headerDef, wellDef, sourceDef, drainDef)

		rigidBodyCodec <- RigidBody(headerDef, vec3Def)

		particlesCodec <- Particles(headerDef, particlesDef)
		trianglesCodec <- Triangles(headerDef, trianglesDef)
		meshTrianglesCodec <- MeshTriangles(headerDef, meshTrianglesDef)

	} yield {


		Platform.runLater {


			val (colliderNode, view, trigs) = Bunny.makeMug()
			view.translateY = 200
			view.translateX = -100
			view.translateZ = 200

			sceneGroup.children += colliderNode

			val colliderBuffer = openSink(BasePath / "colliders.mmf", size = 16 * 4 * 50000)
			val dBodyBuffer = openSink(BasePath / "dynamic_bodies.mmf", size = 16 * 4 * 50000)

			val bunnySampleGroup = new Group( ) {

			}
			sceneGroup.children+= bunnySampleGroup


			sceneGroup.onMouseReleased = handle {sceneGroup.onMouseDragged = null}
			sceneGroup.onMouseClicked = { start: MouseEvent =>
				if (start.pickResult.intersectedNode.exists(_.delegate == view)) {
					val _start = Vec3(start.x, start.y, start.z)
					sceneGroup.onMouseDragged = { drag: MouseEvent =>
						val delta = Vec3(drag.x, drag.y, drag.z) - _start
						view.translateX = delta.x
						view.translateY = delta.y

						val transformed = trigs.points.grouped(3).map { g =>
							val p3 = view.localToParent(g(0), g(1), g(2))
							Vec3(p3.getX.toFloat, p3.getY.toFloat, p3.getZ.toFloat)
						}.toArray




						val sampled = Bunny.samplePoints(transformed, (solverScale.value * (0.1 / 2)).toFloat).toArray


						bunnySampleGroup.children.clear()
						bunnySampleGroup.children = sampled.map(p => new Sphere(5, 1){
							translateX = p.x
							translateY = p.y
							translateZ = p.z
							material = new PhongMaterial(Color.RED)
						})


//						println(s"points: ${transformed.length} ${transformed.par.min} ${transformed.par.max}")
//						colliderBuffer.clear()
//						trianglesCodec.write(Triangles(transformed), colliderBuffer)


						dBodyBuffer.clear()
						rigidBodyCodec.write(RigidBody(sampled), dBodyBuffer)

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
		@volatile var _colours: Array[Int] = Array.empty
		@volatile var _particles: Array[Particle] = Array.empty

		@volatile var elapsed: Long = 0l
		val last: AtomicLong = new AtomicLong(System.currentTimeMillis())


		val lock = new ReentrantLock()

		val vertexColourNodes: ArrayBuffer[Sphere] = ArrayBuffer.empty
		val particleNodes: ArrayBuffer[Sphere] = ArrayBuffer.empty

		def fillOrRemove[A](xs: ArrayBuffer[A], n: Int)(f: => A): Unit = {
			(xs.length, n) match {
				case (existing, now) if existing < now =>
					xs ++= ArrayBuffer.fill(now - existing)(f)
				case (existing, now) if existing > now =>
					xs.remove(0, existing - now)
				case _                                 =>
				// great, nothing to do
			}
		}

		AnimationTimer { _ =>

			if (meshInvalidated.getAndSet(false)) {

				try {
					lock.lock()

					mesh.points = _points
					mesh.getNormals.setAll(_normals: _*)
					mesh.faces = mkFaces(_points.length / 3, mesh.getVertexFormat)

					if (showVertexColour.value) {
						fillOrRemove(vertexColourNodes, _colours.length) {
							new Sphere(5, 1)
						}
						vertexColourGroup.children.setAll(vertexColourNodes.map(_.delegate): _*)
						_colours.zipWithIndex.foreach { case (x, i) =>
							val c = unpackARGB(x)
							val point = vertexColourNodes(i)
							point.material = new PhongMaterial(c)
							point.translateX = _points(i * 3 + 0)
							point.translateY = _points(i * 3 + 1)
							point.translateZ = _points(i * 3 + 2)
						}
					} else vertexColourGroup.children.clear()

				} finally lock.unlock()

				val fps = 1000.0 / elapsed

				fpsSeries += XYChart.Data[String, Number]((Instant.now().toEpochMilli - InitMs) + "ms", fps)
				if (fpsSeries.length > 300) fpsSeries.remove(0, fpsSeries.length - 300)

				infoLabel.text = s"${fps.round}FPS (${elapsed}ms)" +
								 s"\nParticles : ${_particles.length}" +
								 s"\nTriangles : ${_points.length / 3}" +
								 s"\nVertices  : ${_points.length}"
			}
			if (particleInvalidated.getAndSet(false)) {

				try {
					lock.lock()
					if (showParticle.value) {
						fillOrRemove(particleNodes, _particles.length) {
							new Sphere(10, 3)
						}
						particleGroup.children.setAll(particleNodes.map(_.delegate): _*)
						_particles.zipWithIndex.foreach { case (x, i) =>
							val c = unpackARGB(x.colour)
							val point = particleNodes(i)
							point.material = new PhongMaterial(c)
							point.translateX = x.position.x
							point.translateY = x.position.y
							point.translateZ = x.position.z
						}
					} else particleGroup.children.clear()
				} finally lock.unlock()

			}
		}.start()

		doUpdate(meshTrianglesCodec, particlesCodec, sceneCodec)({
			case MeshTriangles(vertices, normals, colours) =>
				try {
					lock.lock()
					_points = vertices
					_normals = normals
					_colours = colours
					meshInvalidated.set(true)
					val now = System.currentTimeMillis()
					elapsed = now - last.getAndSet(now)
				} finally lock.unlock()
		}, { case Particles(xs) =>
			try {
				lock.lock()
				_particles = xs
				particleInvalidated.set(true)
			} finally lock.unlock()
		})
	}

	run match {
		case Left(e)  => throw e
		case Right(x) => println(x)
	}


}
