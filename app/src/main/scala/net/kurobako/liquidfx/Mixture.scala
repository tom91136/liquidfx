package net.kurobako.liquidfx

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.Includes._
import cats.implicits._
import javafx.scene.paint.Color
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{Button, ColorPicker}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.shape.Rectangle

import scala.collection.immutable

object Mixture extends JFXApp {


	def toCYMK(l: Color) = {
		val R = (l.red * 255).toInt
		val G = (l.green * 255).toInt
		val B = (l.blue * 255).toInt

		val cyan = 255 - R
		val magenta = 255 - G
		val yellow = 255 - B
		val black = math.min(math.min(cyan, magenta), yellow)
		(
			(cyan - black).toFloat / (255 - black),
			(magenta - black).toFloat / (255 - black),
			(yellow - black).toFloat / (255 - black),
			black.toFloat / 255,
			(l.opacity * 255).toFloat
		)
	}

	def toRGB(cmyk: (Float, Float, Float, Float, Float)) = {
		val (c, m, y, k, a) = cmyk

		val R = c * (1.0 - k) + k
		val G = m * (1.0 - k) + k
		val B = y * (1.0 - k) + k

		Color.rgb(
			((1.0 - R) * 255.0).round.toInt,
			((1.0 - G) * 255.0).round.toInt,
			((1.0 - B) * 255.0).round.toInt,
			a / 255)
	}

	def mix(cs: Seq[Color]): Color = {
		val fused = cs.toList.map(toCYMK(_)).combineAll
		val N = cs.length
		val normalised = fused match {
			case (c, m, y, k, a) => (
				c / N   ,
				m / N   ,
				y / N   ,
				k / N   ,
				a / N   )
		}
		toRGB(normalised)
	}

	val outcome = Rectangle(100, 100, Color.GREEN)

	def mkRects()  = {
		val c = ObjectProperty[Color](Color.RED)
		val cp = new ColorPicker(c.value)
		c <== cp.value
		c.onChange {
			outcome.fill = mix(inputs.map(_._2.value))
		}
		(cp, c)
	}

	val inputs: immutable.Seq[(ColorPicker, ObjectProperty[Color])] = List.fill(3)(mkRects())


	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(

			new VBox(
				new HBox(inputs.map(_._1): _*),
				outcome
			)

		)
	}

}
