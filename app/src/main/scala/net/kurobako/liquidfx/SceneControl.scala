package net.kurobako.liquidfx

import scalafx.Includes._
import scalafx.geometry.Point2D
import scalafx.scene._
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.Box
import scalafx.scene.transform.Translate


object SceneControl {

	def mkAxis() = {
		val xAxis = new Box(240.0, 1, 1) {
			material = new PhongMaterial {
				diffuseColor = Color.DarkRed
				specularColor = Color.Red
			}
		}
		val yAxis = new Box(1, 240.0, 1) {
			material = new PhongMaterial {
				diffuseColor = Color.DarkGreen
				specularColor = Color.Green
			}
		}
		val zAxis = new Box(1, 1, 240.0) {
			material = new PhongMaterial {
				diffuseColor = Color.DarkBlue
				specularColor = Color.Blue
			}
		}
		new Group(xAxis, yAxis, zAxis)
	}

	def mkScene(that: Parent, width: Double, height: Double) = {


		val root = new Group(that)
		val xform: Xform = new Xform(that)
		val perspective: Camera = new PerspectiveCamera()

		val scene = new SubScene(root, width, height, true, SceneAntialiasing.Balanced) {
			camera = perspective
			pickOnBounds = true
			fill = Color.DarkGray
		}
		root.transforms += new Translate() {
			x <== scene.width / 2
			y <== scene.height / 2
		}.delegate


		perspective.translateZ = -100
		perspective.nearClip = 0

		val MOUSE_SPEED = 1.5
		val TRACK_SPEED = 1.0
		val ROTATION_SPEED = 0.3
		val modifierFactor = 0.85

		def rotate(delta: Point2D) = {
			xform.rotate(
				xform.rx.getAngle + delta.y * modifierFactor * ROTATION_SPEED,
				xform.ry.getAngle - delta.x * modifierFactor * ROTATION_SPEED
			)
		}

		def translate(delta: Point2D) = {
			xform.translate(
				xform.t.getX + delta.x * MOUSE_SPEED * TRACK_SPEED,
				xform.t.getY + delta.y * MOUSE_SPEED * TRACK_SPEED
			)
		}

		def zoom(factor: Double) = {
			perspective.translateZ = perspective.getTranslateZ + factor * -MOUSE_SPEED
		}

		def scenePos(event: MouseEvent) = new Point2D(event.sceneX, event.sceneY)

		scene.onMousePressed = { pe =>
			var start = scenePos(pe)
			var last = start


			scene.onMouseDragged = { de =>
				last = start
				start = scenePos(de)
				val delta = start.subtract(last)


				if (de.isPrimaryButtonDown) {
					if (de.isShiftDown) {
						translate(delta)
					} else if (de.isControlDown) {
						zoom((delta.x + delta.y) / 2)
					} else {
						rotate(delta)
					}
				}
			}
		}
		scene
	}


}
