package net.kurobako.liquidfx

import scalafx.geometry.Point3D

import scala.reflect.ClassTag

object Maths {


	implicit class FloatMath(val a : Float) extends AnyVal {
		def ** (b : Double) : Float = Math.pow(a, b).toFloat
	}

	case class Mat3(a: Double, b: Double, c: Double,
					d: Double, e: Double, f: Double,
					g: Double, h: Double, i: Double) {
		def det: Double = (a * e * i) + (b * f * g) + (c * d * h) -
						  (c * e * g) - (b * d * i) - (a * f * h)
	}

	case class Triangle(v0: Vec3, v1: Vec3, v2: Vec3,
						n0: Vec3 = Vec3.Zero,
						n1: Vec3 = Vec3.Zero,
						n2: Vec3 = Vec3.Zero) {

		@inline def points: Array[Float] = Array(
			v0.x, v0.y, v0.z,
			v1.x, v1.y, v1.z,
			v2.x, v2.y, v2.z,
		)

		@inline def normals: Array[Float] = Array(
			n0.x, n0.y, n0.z,
			n1.x, n1.y, n1.z,
			n2.x, n2.y, n2.z,
		)

		@inline def a: Vec3 = v0
		@inline def b: Vec3 = v1
		@inline def c: Vec3 = v2

	}

	@inline def clampf(min: Float, max: Float, v: Float): Float = Math.max(min, Math.min(max, v))

	object Vec3 {
		def apply(v: Point3D): Vec3 = Vec3(v.x, v.y, v.z)

		def apply(x: Double, y: Double, z: Double): Vec3 = new Vec3(x.toFloat, y.toFloat, z.toFloat)

		def apply(v: Float): Vec3 = Vec3(v, v, v)
		final val NegativeOne = Vec3(-1f)
		final val One         = Vec3(1f)
		final val Zero        = Vec3(0f)
	}

	case class Vec3(x: Float, y: Float, z: Float) {

		def p3d: Point3D = new Point3D(x, y, z)

		@inline def u: Float = x
		@inline def v: Float = y
		@inline def w: Float = z

		@inline def array[N: ClassTag](f: Float => N): Array[N] = Array(f(x), f(y), f(z))
		@inline def tuple: (Float, Float, Float) = (x, y, z)

		@inline def *+(t: Float, that: Vec3): Vec3 = Vec3(
			Math.fma(t, this.x, that.x),
			Math.fma(t, this.y, that.y),
			Math.fma(t, this.z, that.z))

		//		@inline def *+(t: Float, that: Vec3): Vec3 = (this * t) + that

		@inline def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
		@inline def negate: Vec3 = Vec3(-x, -y, -z)
		@inline def -(amount: Float): Vec3 = Vec3(x - amount, y - amount, z - amount)
		@inline def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
		@inline def +(amount: Float): Vec3 = Vec3(x + amount, y + amount, z + amount)
		@inline def *(that: Vec3): Vec3 = Vec3(x * that.x, y * that.y, z * that.z)
		@inline def *(factor: Float): Vec3 = Vec3(x * factor, y * factor, z * factor)
		@inline def *:(factor: Float): Vec3 = Vec3(x * factor, y * factor, z * factor)
		@inline def /(factor: Float): Vec3 = Vec3(x / factor, y / factor, z / factor)
		@inline def /:(factor: Float): Vec3 = Vec3(factor / x, factor / y, factor / z)
		@inline def cross(that: Vec3): Vec3 = Vec3(
			y * that.z - z * that.y,
			z * that.x - x * that.z,
			x * that.y - y * that.x)

		@inline def normalise: Vec3 = {
			val m = magnitude
			if (m == 0) Vec3.Zero
			else this / m
		}

		@inline def dot(that: Vec3): Float = x * that.x + y * that.y + z * that.z

		@inline def distance(that: Vec3): Float = Math.sqrt(distanceSq(that)).toFloat

		@inline def distanceSq(that: Vec3): Float = {
			val a = x - that.x
			val b = y - that.y
			val c = z - that.z
			a * a + b * b + c * c
		}

		@inline def clamp(xMin: Float, xMax: Float,
						  yMin: Float, yMax: Float,
						  zMin: Float, zMax: Float): Vec3 = Vec3(
			clampf(xMin, xMax, x),
			clampf(yMin, yMax, y),
			clampf(zMin, zMax, z)
		)


		@inline def length: Float = Math.sqrt(lengthSquared).toFloat
		@inline def magnitude: Float = length

		@inline def lengthSquared: Float = x * x + y * y + z * z
		@inline def magnitudeSq: Float = lengthSquared

	}

}
