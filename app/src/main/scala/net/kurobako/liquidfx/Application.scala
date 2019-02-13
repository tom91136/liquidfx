package net.kurobako.liquidfx

import cats.implicits._
import javafx.animation.Interpolator
import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Vec3}
import scalafx.Includes._
import scalafx.animation._
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{LongProperty, StringProperty}
import scalafx.scene._
import scalafx.scene.control.{Button, Label, Menu, MenuBar}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.{Box, DrawMode, Sphere}
import scalafx.util.Duration


object Application extends JFXApp {


	val info = StringProperty("")

	val box   = new Box(100, 100, 100) {
		material = new PhongMaterial(Color.Red)
		translateY = -50
	}
	val plane = new Box(1000, 1, 1000) {
		material = new PhongMaterial(Color.White)
		translateY = 400
	}


	def render(xs: Seq[Particle[Sphere]]) = {
		def clamp(min: Double, max: Double, value: Double) = Math.max(min, Math.min(max, value))
		Platform.runLater {
			xs.foreach { p =>
				p.a.translateX = p.position.x
				p.a.translateY = p.position.y
				p.a.translateZ = p.position.z
				val v = clamp(120, 255, p.velocity.lengthSquared * 100).toInt
				p.a.material = new PhongMaterial(Color.rgb(v / 2, v / 2, v, v.toFloat / 255))
			}
		}
	}


	val r   = 5
	val div = r / 2
	val xs  = (for {
		x <- 0 to 100 by r
		y <- 0 to 90 by r
		z <- 0 to 50 by r
	} yield Particle(a = new Sphere(r * 0.85) {
	}, position = Vec3(x.toFloat, 3 - z.toFloat, y.toFloat))).toArray


	render(xs)

	val ball      = new Sphere(10) {}
	val container = new Box(1000, 500, 500) {


		drawMode = DrawMode.Line
		//		material = new PhongMaterial(Color.WhiteSmoke.opacity(0.2))


	}

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


	def nodePos(n: Node) = Vec3(n.getTranslateX, n.getTranslateY, n.getTranslateZ)

	def concaveBoxCollider(b: Box): Ray => Vec3 = { r =>
		val origin = nodePos(b)
		val dim = Vec3(b.getWidth, b.getHeight, b.getDepth) / 2
		val s = origin - dim
		val e = origin + dim
		r.origin.clamp(
			s.x, e.x,
			s.y, e.y,
			s.z, e.z)
	}

	def convexSphereCollider(b: Sphere): Ray => Vec3 = {
		case Ray(origin, velocity) =>
			val r = b.getRadius
			val centre = Vec3(b.getTranslateX, b.getTranslateY, b.getTranslateZ)
			if ((origin - centre).magnitude < r * r) {
				origin - velocity
				// inside
				// TODO finish
			} else {
				origin
			}
	}

	new Thread(() => {

		val solver = new SphSolver(scale = 500d, iteration = 3)

		(0 to Int.MaxValue).foldLeft(xs) { (acc, n) =>
			val start = System.currentTimeMillis()
			val that = solver.advance()(acc, Seq(concaveBoxCollider(container)))
			val elapsedMs = System.currentTimeMillis() - start
			val text = s"Frame[$n] ${(1000.0 / elapsedMs).toInt}fps (${elapsedMs}ms) @${that.length} particles"
			println(text)
			render(that)
			Platform.runLater(info.set(text))
			that
		}

	}).start()

	val ambient  = new AmbientLight(Color.LightYellow)
	val subScene = SceneControl.mkScene(new Group(
		SceneControl.mkAxis(),
		container,
		//		ball,
		//				plane,
		//		box,
	) {
		children ++= xs.map(_.a.delegate)
	}, 500, 500)


	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(new VBox(

			new StackPane {
				children = subScene
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			},
			new HBox(new Label() {text <== info}, new Button("Animate") {
				onAction = handle {
					println(tl.getStatus)
					if (tl.getStatus == Animation.Status.Stopped.delegate) {
						tl.play()
					} else tl.stop()

				}
			}) {
				styleClass += "tool-bar"
				style = "-fx-font-family: 'monospaced'"
			}
		), 800, 800)


	}


}


