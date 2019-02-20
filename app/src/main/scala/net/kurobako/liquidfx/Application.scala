package net.kurobako.liquidfx

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap

import cats.implicits._
import javafx.animation.Interpolator
import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.{Particle, Vec3}
import scalafx.Includes._
import scalafx.animation._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.{BooleanProperty, DoubleProperty, IntegerProperty, StringProperty}
import scalafx.scene._
import scalafx.scene.control.{Button, Label, Slider, ToggleButton}
import scalafx.scene.layout.{HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape._
import scalafx.stage.FileChooser
import scalafx.util.Duration


object Application extends JFXApp {

	val surface = BooleanProperty(true)
	val play    = BooleanProperty(true)
	val info    = StringProperty("")
	val iter    = IntegerProperty(5)
	val gravity = DoubleProperty(9.8)
	val deltaT  = DoubleProperty(1)

	val box   = new Box(100, 100, 100) {
		material = new PhongMaterial(Color.Red)
		translateY = 0
	}
	val plane = new Box(1000, 1, 1000) {
		material = new PhongMaterial(Color.White)
		translateY = 400
	}



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
				p.a.material = new PhongMaterial(Color.rgb(v / 2, v / 2, v, 0.2))

			}
		}
	}


	val r   = 8
	val div = r / 2
	val xs  = (for {
		x <- 0 to 80 by r
		y <- 0 to 100 by r
		z <- 0 to 100 by r
	} yield Particle(a = new Sphere(r * 0.85) {
	}, position = Vec3(x.toFloat, y.toFloat - 200, z.toFloat))).toArray


	renderParticles(xs)

	val ball = new Sphere(120) {
		drawMode = DrawMode.Line
		translateY = 120
	}

	val box2 = new Box(20, 320, 420) {
		drawMode = DrawMode.Fill
		translateX = -120
		translateY = 100
		translateZ = -120
	}


	val box3 = new Box(120, 320, 420) {
		drawMode = DrawMode.Fill
		translateX = 120
		translateY = 100
		translateZ = 100
	}


	val container = new Box(800, 500, 370) {
		drawMode = DrawMode.Line
		//		material = new PhongMaterial(Color.WhiteSmoke.opacity(0.2))
	}

	val mesh = new TriangleMesh()


	val quadDecel = new Interpolator {
		override def curve(input: Double) = 1.0f - Math.pow(1.0f - input, 4d)
	}


	val tl = new Timeline(30, KeyFrame(Duration(2000), values = Set(
		KeyValue(container.translateX, 220),
		//					KeyValue(translateY, -200),
		//		KeyValue(container.translateZ, 200),
	))) {
		cycleCount = Timeline.Indefinite
		autoReverse = true

	}


	new Thread(() => {


		val solver = new SphSolver(scale = 550d)
		val obstacles = Array(
			Colliders.concaveBoxCollider(container),
			//				convexSphereCollider(ball),
			Colliders.convexBoxCollider(box2),
			Colliders.convexBoxCollider(box3),
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


		(0 to Int.MaxValue).foldLeft(xs) { (acc, frame) =>

			val start = System.currentTimeMillis()
			val that = time("solve") {
				solver.advance(
					dt = 0.0083 * deltaT.value,
					iteration = iter.value,
					constantForce = { p: Particle[Sphere] => Vec3(0d, p.mass * gravity.value, 0d) })(acc, obstacles)
			}

			val tCount = if(surface.value){
				val ts = time("mc") {

					val bs = time("mc - octree") {
						val xx = MutableUnsafeOctree[Vec3](Vec3.Zero, 500)(identity)
						that.foreach(x => xx.insertPoint(x.position))
						xx
					}

					val rSq = 100.0 * 100.0

					val cc = new ConcurrentHashMap[Vec3, Double]()

					def doIt(p: Vec3) = {
						cc.computeIfAbsent(p, p2 => {
							bs.pointsInSphere(p2, 25).foldLeft(0.0) { (acc, x) => acc + (rSq / (x - p2).magnitudeSq) * 2 }
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
			}else{
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
		box2,
		box3,
		//				plane,
		//				box,
	) {
		children ++= xs.map(_.a.delegate)
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
							val fs = (1 to mesh.points.size/3).grouped(3).map(x => s"f ${x(0)} ${x(1)} ${x(2)}").mkString("\n")

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

		), 800, 800)


	}


}


