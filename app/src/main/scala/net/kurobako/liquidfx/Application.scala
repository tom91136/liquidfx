package net.kurobako.liquidfx

import cats.implicits._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene._
import scalafx.scene.control.{Menu, MenuBar}
import scalafx.scene.layout.{Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.Box


object Application extends JFXApp {


	private def mkAxis() = {
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


	val box   = new Box(100, 100, 100) {
		material = new PhongMaterial(Color.Red)
		translateY = -50
	}
	val plane = new Box(300, 1, 300) {
		material = new PhongMaterial(Color.White)
	}


	val subScene = SceneControl.mkScene(new Group(
		mkAxis(),
		plane, box,
	), 500, 500)

	stage = new PrimaryStage {
		scene = new Scene(new VBox(
			new MenuBar() {
				menus = Seq(new Menu("A"))
			},
			new StackPane {
				children = subScene
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			}
		), 500, 500)


	}


}


