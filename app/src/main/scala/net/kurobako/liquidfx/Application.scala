package net.kurobako.liquidfx

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap

import cats.implicits._
import com.google.common.io.Resources
import com.javafx.experiments.importers.Importer3D
import javafx.animation.Interpolator
import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
import net.kurobako.liquidfx.Metaball
import net.kurobako.liquidfx.SphSolver.Particle
import scalafx.Includes._
import scalafx.animation._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.{BooleanProperty, DoubleProperty, FloatProperty, IntegerProperty, StringProperty}
import scalafx.scene._
import scalafx.scene.control.{Button, Label, Slider, ToggleButton}
import scalafx.scene.layout.{HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape._
import scalafx.stage.FileChooser
import scalafx.util.Duration
import Maths._
import scalafx.scene.input.{KeyCode, KeyEvent}

object Application extends JFXApp {

	val surface = BooleanProperty(true)
	val play    = BooleanProperty(true)
	val info    = StringProperty("")
	val iter    = IntegerProperty(1)
	val gravity = FloatProperty(9.8f)
	val deltaT  = FloatProperty(1f)

	val box   = new Box(100, 100, 100) {
		material = new PhongMaterial(Color.Red)
		translateY = 0
	}
	val plane = new Box(1000, 1, 1000) {
		material = new PhongMaterial(Color.White)
		translateY = 400
	}


	val slope     = new TriangleMesh() {}
	val slopeView = new MeshView(slope) {
		drawMode = DrawMode.Fill
		material = new PhongMaterial(Color.rgb(0, 100, 100, 0.5))
		depthTest = DepthTest.Enable
		cullFace = CullFace.None

	}

	val Yoff = -300
	val Zoff = -200
	val Xoff = 200

	updateMesh(slope, Triangle(
		Vec3(0 + Xoff, 100 + Yoff, 0 + Zoff),
		Vec3(0 + Xoff, 100 + Yoff, 300 + Zoff),
		Vec3(300 + Xoff, 700 + Yoff, 150 + Zoff),
	) :: Nil)

	def renderSurface(ts: Seq[Triangle]) = {
		Platform.runLater {
			updateMesh(mesh, ts)
		}
	}

	def renderParticles(xs: Seq[Particle[Sphere]]) = {

		def clamp(min: Double, max: Double, value: Double) = Math.max(min, Math.min(max, value))
		Platform.runLater {
			xs.foreach { p =>
				p.a.translateX = p.position.x
				p.a.translateY = p.position.y
				p.a.translateZ = p.position.z
				val v = clamp(120, 255, p.velocity.lengthSquared * 100).toInt

				val surface = if (p.tpe == SphSolver.Solid) {
					Color.Red
				} else {
					Color.rgb(v, v, v)
				}


				p.a.material = new PhongMaterial(surface)

			}
		}
	}


	val maxIter = 1000000
	val r       = 20 //28

	val wall  = 10
	val wallC = wall / 2
	val obs   = (for {
		x <- 0 to wall
		y <- 0 to wall
		z <- 0 to wall
		if x == 0 || x == wall ||
		   y == 0 || y == wall ||
		   z == 0 || z == wall ||
		   (x == wallC && y == wallC && z == wallC)

	} yield {
		val HS = 500f * (0.1f /2 )

		Particle(
			a = new Sphere(4, 5) {depthTest = DepthTest.Enable},
			position = Vec3(x.toFloat, y.toFloat, z.toFloat) * HS,
			mass = if ((x == wallC && y == wallC && z == wallC)) 1 else 1,
			tpe = SphSolver.Solid)
	}).toArray

	val xs: Array[Particle[Sphere]] = (for {
		x <- 0 to 200 by r
		y <- 0 to 200 by r
		z <- 0 to 500 by r

	} yield {
		Particle(
			a = new Sphere(r * 0.55, 5) {depthTest = DepthTest.Enable},
			position = Vec3(x.toFloat, y.toFloat, z.toFloat),
			mass = 1,
			tpe = SphSolver.Fluid)
	}
										  ).toArray ++ obs


	val ball = new Sphere(120) {
		drawMode = DrawMode.Line
		translateY = 120
	}

	val box2 = new Box(20, 320, 420) {
		drawMode = DrawMode.Line
		translateX = -120
		translateY = 0
		translateZ = -120
	}


	val box3 = new Box(120, 320, 420) {
		drawMode = DrawMode.Line
		translateX = 120
		translateY = 0
		translateZ = 100
	}

	val centre = new Sphere(20) {
		material = new PhongMaterial(Color.White)
	}

	val container = new Box(500, 500, 500) {
		drawMode = DrawMode.Line
		translateX = 0 //250
		translateY = 0
		translateZ = 0
		//		material = new PhongMaterial(Color.WhiteSmoke.opacity(0.2))
	}

	val mesh = new TriangleMesh()


	val bunny = Importer3D.load(Resources.getResource("l_bunny.obj").toExternalForm)
	//	bunny.scaleX = 100
	//	bunny.scaleY = 100
	//	bunny.scaleZ = 100
	println(bunny.getClass)


	val mv = bunny.asInstanceOf[javafx.scene.Group]
		.getChildren.get(0).asInstanceOf[javafx.scene.shape.MeshView]
	mv.scaleX = 2

	val bunnyMesh = mv
		.getMesh.asInstanceOf[javafx.scene.shape.TriangleMesh]

	mv.setDrawMode(DrawMode.Fill)
	mv.material = new PhongMaterial(Color.rgb(0, 100, 0, 0.3))
	mv.depthTest = DepthTest.Enable

	println(bunnyMesh)


	val quadDecel = new Interpolator {
		override def curve(input: Double) = 1.0f - Math.pow(1.0f - input, 4d)
	}


	val tl = new Timeline(30, KeyFrame(Duration(2000), values = Set(
		KeyValue(container.translateX, 0),
		KeyValue(container.translateX, 500),
		//					KeyValue(translateY, -200),
		//		KeyValue(container.translateZ, 200),
	))) {
		cycleCount = Timeline.Indefinite
		autoReverse = true

	}


	new Thread(() => {


		val solver = new SphSolver(scale = 500f)
		val obstacles = Array(
			//			Colliders.convexMeshCollider(bunnyMesh, mv),
			//			Colliders.convexMeshCollider(slope, slopeView),
			Colliders.concaveBoxCollider(container),
			//				convexSphereCollider(ball),
			//			Colliders.convexBoxCollider(box2),
			//			Colliders.convexBoxCollider(box3),
		)


		def time[R](name: String)(block: => R): R = {
			val t0 = System.nanoTime()
			val result = block
			val t1 = System.nanoTime()
			println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
			result
		}


		val gs = 8
		val xsc = 1000.0
		val ysc = 500.0
		val zsc = 500.0
		val diffX = (xsc / gs / 2).toInt
		val diffY = (ysc / gs / 2).toInt
		val diffZ = (zsc / gs / 2).toInt
		val lattice = time("lattice") {
			Metaball.mkLattice(gs.toInt)(
				xRange = -diffX to diffX,
				yRange = -diffY to diffY,
				zRange = -diffZ to diffZ)
		}

		renderParticles(xs)

		val G: Float = (6.674 * Math.pow(10, -11)).toFloat
		(0 to maxIter).foldLeft(xs) { (acc, frame) =>


			val now = Vec3(
				slopeView.getTranslateX,
				slopeView.getTranslateY,
				slopeView.getTranslateZ)
			Colliders.dv = (Colliders.prev - now)
			Colliders.prev = now
			println("\t" + Colliders.dv)

			val start = System.currentTimeMillis()
			val Gp = Vec3(
				centre.getTranslateX,
				centre.getTranslateY,
				centre.getTranslateZ)
			val that = time("solve") {
				solver.advance(
					dt = 0.0083f * deltaT.value,
					iteration = iter.value,
					constantForce = { p: Particle[Sphere] =>


						//						val distSq = Gp.distanceSq(p.position)
						//
						//						val rHat = (p.position - Gp) / math.sqrt(distSq).toFloat
						//
						//						val attractor = (rHat * (-100f * 1000f)) / distSq
						//
						//						//						1f / (Gp distance p.position)
						//						attractor.clamp(-10, 10, -10, 10, -10, 10) +
						Vec3(0f, p.mass * gravity.value, 0f)
					})(acc, obstacles)
			}.map {
				x =>
					x.tpe match {
						case SphSolver.Fluid => x
						case SphSolver.Solid => x.copy(position = x.position + Gp)
					}
			}

			val tCount = if (surface.value) {
				val ts = time("mc") {

					val bs = time("mc - octree") {
						val xx = MutableUnsafeOctree[Vec3](Vec3.Zero, 500)(identity)
						that.foreach(x => xx.insertPoint(x.position))
						xx
					}

					val rSq = 100 ** 2

					val cc = new ConcurrentHashMap[Vec3, Float]()

					def doIt(p: Vec3) = {
						cc.computeIfAbsent(p, p2 => {
							bs.pointsInSphere(p2, 25).foldLeft(0f) { (acc, x) => acc + (rSq / (x - p2).magnitudeSq) * 2 }
						})
						//					bs.pointsInSphere(p, 25).foldLeft(0.0) { (acc, x) => acc + (rSq / (x - p).magnitudeSq) }
					}
					val triangles = Metaball.parameterise(lattice, { p =>

						doIt(p)
					})
					println(s"triangles = ${triangles.length} cells = ${lattice.length}")
					triangles
				}
				renderSurface(ts)
				ts.length
			} else {
				renderSurface(Nil)
				0
			}


			val elapsedMs = System.currentTimeMillis() - start
			val text = s"[$frame] ${(1000.0 / elapsedMs).toInt}fps(${elapsedMs}ms) " +
					   s"@${that.length} particles " +
					   s"| ${tCount} triangles " +
					   s"| ${lattice.length} cell lattice"
			println(text)
			renderParticles(that)
			Platform.runLater(info.set(text))
			that
		}

	}).start()

	val ambient = new AmbientLight(Color.LightYellow)


	def updateMesh(mesh: TriangleMesh, xs: Seq[Triangle]) = {
		mesh.points = xs.flatMap(_.points).map(_.toFloat).toArray
		mesh.texCoords = Array(0, 0)
		mesh.faces = Array.tabulate(xs.length * 3)(_ :: 0 :: Nil).flatten
	}


	val subScene = SceneControl.mkScene(new Group(
		SceneControl.mkAxis(),
		container,
		//				ball,
		new MeshView(mesh) {
			drawMode = DrawMode.Fill
			material = new PhongMaterial(Color.rgb(0, 119, 190, 0.95))
			cullFace = CullFace.None

		},
		Colliders.g,
		box2,
		box3,
		centre,

		//				plane,
		//				box,
	) {
		Platform.runLater {
			children ++= xs.map(_.a.delegate)
			//			children += bunny
			children += slopeView
		}
	}, 500, 500)


	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(new VBox(


			new StackPane {
				children = Seq(
					subScene,
					new VBox(
						new Label("") {
							text <== iter.asString("Iter(%d):")
						}, new Slider(1, 30, 3) {
							prefWidth = 250
							iter <== value
						},
						new Label("") {
							text <== gravity.asString("Gravity(%.2f):")
						}, new Slider(-10, 100, 9.8) {
							prefWidth = 250
							gravity <== value
						},
						new Label("") {
							text <== deltaT.asString("ΔT(0.00083)×(%.2f):")
						}, new Slider(0.1, 10, 1) {
							prefWidth = 250
							deltaT <== value
						}

					) {pickOnBounds = false}
				)
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			},
			new HBox(new Label() {text <== info},
				new ToggleButton("Play/Pause") {
					play <== selected
				},
				new ToggleButton("Surface") {
					surface <== selected
				},
				new Button("Animate container") {
					onAction = handle {
						println(tl.getStatus)
						if (tl.getStatus != Animation.Status.Running.delegate) {
							tl.play()
						} else tl.pause()

					}
				},
				new ToggleButton("Toggle box") {
					box2.visible <== selected
					box3.visible <== selected
				}, new Button("Export Wavefront OBJ") {
					onAction = handle {
						Option(new FileChooser().showSaveDialog(stage)).foreach { f =>

							val vs = mesh.points
								.grouped(3).map(x => s"v ${x(0)} ${x(1)} ${x(2)}")
								//								.grouped(3).map(x => x.mkString("\n") + "\nf 1 2 3")
								.mkString("\n")
							val fs = (1 to mesh.points.size / 3).grouped(3).map(x => s"f ${x(0)} ${x(1)} ${x(2)}").mkString("\n")

							val path = f.toPath
							Files.deleteIfExists(path)
							Files.writeString(path,
								s"""g sph_water
								   |$vs
								   |$fs""".stripMargin, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)

						}
					}
				},
			) {
				styleClass += "tool-bar"
				style = "-fx-font-family: 'monospaced'"
			},

		), 800, 800) {
			onKeyPressed = { e: KeyEvent =>
				e.code match {
					case KeyCode.Up   => centre.translateY = centre.getTranslateY - 20
					case KeyCode.Down => centre.translateY = centre.getTranslateY + 20
					case KeyCode.A    => centre.translateX = centre.getTranslateX - 20
					case KeyCode.D    => centre.translateX = centre.getTranslateX + 20
					case KeyCode.W    => centre.translateZ = centre.getTranslateZ - 20
					case KeyCode.S    => centre.translateZ = centre.getTranslateZ + 20
					case _            =>
				}


				e.consume()
			}
		}


	}


}


