package net.kurobako.liquidfx

import net.kurobako.liquidfx.Xform.RotateOrder
import net.kurobako.liquidfx.Xform.RotateOrder.ZYX
import scalafx.scene.Node
import scalafx.scene.transform.{Rotate, Scale, Translate}

class Xform(val node: Node) {


	val t          = new Translate
	val p          = new Translate
	val ip         = new Translate
	val rx: Rotate = new Rotate {axis = Rotate.XAxis;}
	val ry: Rotate = new Rotate {axis = Rotate.YAxis;}
	val rz: Rotate = new Rotate {axis = Rotate.ZAxis;}
	val s          = new Scale


	rotateOrder(ZYX)

	def rotateOrder(order: RotateOrder): Unit = {
		node.transforms = t :: p :: (order match {
			case RotateOrder.XYZ => rx :: ry :: rx :: Nil
			case RotateOrder.XZY => rx :: rz :: ry :: Nil
			case RotateOrder.YXZ => ry :: rx :: rz :: Nil
			case RotateOrder.YZX => ry :: rz :: rx :: Nil
			case RotateOrder.ZXY => rz :: rx :: ry :: Nil
			case RotateOrder.ZYX => rz :: ry :: rx :: Nil
		}) ::: s :: ip :: Nil
	}

	def translate(x: Double, y: Double, z: Double): Unit = {
		t.x = x
		t.y = y
		t.z = z
	}

	def translate(x: Double, y: Double): Unit = {
		t.x = x
		t.y = y
	}

	def tx(x: Double): Unit = t.x = x
	def ty(y: Double): Unit = t.y = y
	def tz(z: Double): Unit = t.z = z


	def scale(factor: Double): Unit = {
		s.x = factor
		s.y = factor
		s.z = factor
	}

	def pivot(x: Double, y: Double, z: Double): Unit = {
		p.x = x
		p.y = y
		p.z = z
		ip.x = -x
		ip.y = -y
		ip.z = -z
	}

	def rotate(x: Double, y: Double, z: Double): Unit = {
		rx.angle = x
		ry.angle = y
		rz.angle = z
	}

	def rotate(x: Double, y: Double): Unit = {
		rx.angle = x
		ry.angle = y
	}

	def reset(): Unit = {
		translate(0d, 0d, 0d)
		rotate(0d, 0d, 0d)
		scale(1d)
		pivot(0d, 0d, 0d)
	}
	override def toString: String =
		s"""t = (${t.getX}, ${t.getY}, ${t.getZ})
		   |r = (${rx.getAngle}, ${ry.getAngle}, ${rz.getAngle})
		   |s = (${s.getX}, ${s.getY}, ${s.getZ})
		   |p = (${p.getX}, ${p.getY}, ${p.getZ})
		   |ip = (${ip.getX}, ${ip.getY}, ${ip.getZ})""".stripMargin
}
object Xform {

	sealed trait RotateOrder
	object RotateOrder {
		case object XYZ extends RotateOrder
		case object XZY extends RotateOrder
		case object YXZ extends RotateOrder
		case object YZX extends RotateOrder
		case object ZXY extends RotateOrder
		case object ZYX extends RotateOrder
	}


}
