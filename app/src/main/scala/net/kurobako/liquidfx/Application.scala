package net.kurobako.liquidfx

import cats.implicits._
import javafx.animation.Interpolator
import net.kurobako.liquidfx.SphSolver.{Particle, Ray, Vec3}
import scalafx.Includes._
import scalafx.animation._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.StringProperty
import scalafx.scene._
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.{Box, DrawMode, Sphere}
import scalafx.util.Duration

import scala.collection.parallel.ForkJoinTaskSupport


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
		x <- 0 to 90 by r
		y <- 0 to 90 by r
		z <- 0 to 90 by r
	} yield Particle(a = new Sphere(r * 0.85) {
	}, position = Vec3(x.toFloat ,  y.toFloat - 200, z.toFloat))).toArray


	render(xs)

	val ball      = new Sphere(120) {
		drawMode = DrawMode.Line
		translateY = 120
	}

	val box2      = new Box(10, 320, 420) {
		drawMode = DrawMode.Fill
		translateX = -120
		translateY = 120
		translateZ = -120
	}


	val box3      = new Box(10, 320, 420) {
		drawMode = DrawMode.Fill
		translateX = 120
		translateY = 120
		translateZ = 120
	}

	val container = new Box(800, 500, 500) {
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
		val min = origin - dim
		val max = origin + dim
		r.origin.clamp(
			min.x, max.x,
			min.y, max.y,
			min.z, max.z)
	}

	def convexBoxCollider(b: Box): Ray => Vec3 = {
		case Ray(prev, origin, vv) =>
			val vel = vv .normalise
			val pos = nodePos(b)
			val dim = Vec3(b.getWidth, b.getHeight, b.getDepth) / 2
			val min = pos - dim
			val max = pos + dim

			//https://gamedev.stackexchange.com/a/103714/73429

			val t1 = (min.x - origin.x) / vel.x
			val t2 = (max.x - origin.x) / vel.x
			val t3 = (min.y - origin.y) / vel.y
			val t4 = (max.y - origin.y) / vel.y
			val t5 = (min.z - origin.z) / vel.z
			val t6 = (max.z - origin.z) / vel.z

			val aMin = if (t1 < t2) t1 else t2
			val bMin = if (t3 < t4) t3 else t4
			val cMin = if (t5 < t6) t5 else t6

			val aMax = if (t1 > t2) t1 else t2
			val bMax = if (t3 > t4) t3 else t4
			val cMax = if (t5 > t6) t5 else t6

			val fMax = if (aMin > bMin) aMin else bMin
			val fMin = if (aMax < bMax) aMax else bMax

			val t7 = if (fMax > cMin) fMax else cMin
			val t8 = if (fMin < cMax) fMin else cMax

			if (t8 < 0 || t7 > t8) {
				// no intersection
				origin
			} else {

				if(t7 < 0) prev
				else
				origin // - (origin distance prev)
			}
	}

	def convexSphereCollider(b: Sphere): Ray => Vec3 = {
		case Ray(prev, origin, vec) =>
			val centre = Vec3(b.getTranslateX, b.getTranslateY, b.getTranslateZ)
			val r = b.getRadius
			val radius2 = r * r


			val dir = vec.normalise

			val L = centre - origin
			val tca = L dot dir
			val d2 = (L dot L) - tca * tca

			if (d2 > radius2) { // no change
				origin
			} else {
				val thc = Math.sqrt(radius2 - d2)
				val t0 = tca - thc
				val t1 = tca + thc
				if (t0 < 0 && t1 < 0) {
					origin
				} else {
					val s = t0 min t1
					val b = t0 max t1
					val t = if (s < 0) b else s
					println(t)
					origin + (dir * (t / 500))
				}
			}
	}

	new Thread(() => {



		val solver = new SphSolver(scale = 250d, iteration = 2)
		val obstacles = Array(
			concaveBoxCollider(container),
			//				convexSphereCollider(ball),
			convexBoxCollider(box2),
			convexBoxCollider(box3),

		)
		(0 to Int.MaxValue).foldLeft(xs) { (acc, n) =>
			val start = System.currentTimeMillis()
			val that = solver.advance()(acc, obstacles)
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
		box2,
		box3,
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


