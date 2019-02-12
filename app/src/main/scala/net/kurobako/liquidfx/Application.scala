package net.kurobako.liquidfx

import cats.implicits._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Point2D
import scalafx.Includes._
import scalafx.scene.control.{Menu, MenuBar}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.VBox
import scalafx.scene._
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.{Box, Rectangle}


object Application extends JFXApp {


	def bindMouse(scene: Scene, root: Xform) = {

		val CONTROL_MULTIPLIER = 0.1
		val SHIFT_MULTIPLIER = 0.1
		val ALT_MULTIPLIER = 0.5
		val ROTATION_SPEED = 0.3
		val modifierFactor = 0.3

		def scenePos(event: MouseEvent) = new Point2D(event.sceneX, event.sceneY)


		scene.onMousePressed = { pe =>
			var start = scenePos(pe)
			var last = start
			scene.onMouseDragged = { de =>
				last = start
				start = scenePos(de)
				val delta = start.subtract(last)

				var modifier = 1.0

				if (de.isControlDown) modifier = CONTROL_MULTIPLIER
				if (de.isShiftDown) modifier = SHIFT_MULTIPLIER
				if (de.isPrimaryButtonDown) {
					root.rotate(
						root.rx.getAngle - delta.y * modifierFactor * modifier * ROTATION_SPEED,
						root.ry.getAngle + delta.x * modifierFactor * modifier * ROTATION_SPEED
					)
				}

				if (de.isMiddleButtonDown) {
										root2.t.setX(root2.t.getX + mouseDeltaX * MOUSE_SPEED * modifier * TRACK_SPEED)
										root2.t.setY(root2.t.getY + mouseDeltaY * MOUSE_SPEED * modifier * TRACK_SPEED)
				}

				//				else if (de.isSecondaryButtonDown) {
				//					val z = camera.getTranslateZ
				//					val newZ = z + mouseDeltaX * MOUSE_SPEED * modifier
				//					camera.setTranslateZ(newZ)
				//				}
				//				else if (de.isMiddleButtonDown) {
				//					root2.t.setX(root2.t.getX + mouseDeltaX * MOUSE_SPEED * modifier * TRACK_SPEED)
				//					root2.t.setY(root2.t.getY + mouseDeltaY * MOUSE_SPEED * modifier * TRACK_SPEED)
				//				}

			}

		}


	}

	stage = new PrimaryStage {

		private val box = new Box(100, 100, 100)
		box.material = new PhongMaterial(Color.Red)
		val root  = new Group(box)
		val xform = new Xform(root)
		xform.reset()
		private val subScene = new SubScene(root, 500, 500, false, SceneAntialiasing.Balanced)

		scene = new Scene(new VBox(
			new MenuBar() {
				menus = Seq(new Menu("A"))
			},
			subScene
		), 500, 500)

		bindMouse(scene.get, xform)

	}


}
