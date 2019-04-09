package net.kurobako.liquidfx

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import better.files.File
import net.kurobako.liquidfx.Maths.Vec3

import scala.annotation.switch
import scala.util.Try

object StructDefs {

	import upickle.default.{macroRW, ReadWriter => RW, _}


	@inline def readLongPromoted(buffer: ByteBuffer, size: Long): Long = {
		(size: @switch) match {
			case 1   => buffer.get.toLong
			case 2   => buffer.getShort.toLong
			case 4   => buffer.getInt.toLong
			case 8   => buffer.getLong
			case bad => throw new AssertionError(s"Unable to map length of $bad to any integral type")
		}
	}

	@inline def writeLongTruncated(buffer: ByteBuffer, size: Long, data: Long): Unit = {
		(size: @switch) match {
			case 1   => buffer.put(data.toByte)
			case 2   => buffer.putShort(data.toShort)
			case 4   => buffer.putInt(data.toInt)
			case 8   => buffer.putLong(data)
			case bad => throw new AssertionError(s"Unable to map length of $bad to any integral type")
		}
	}

	@inline def readIntTruncated(buffer: ByteBuffer, size: Long): Int = {
		(size: @switch) match {
			case 1   => buffer.get.toInt
			case 2   => buffer.getShort.toInt
			case 4   => buffer.getInt
			case 8   => buffer.getLong.toInt
			case bad => throw new AssertionError(s"Unable to map length of $bad to any integral type")
		}
	}

	@inline def writeIntTruncated(buffer: ByteBuffer, size: Long, data: Int): Unit = {
		(size: @switch) match {
			case 1   => buffer.put(data.toByte)
			case 2   => buffer.putShort(data.toShort)
			case 4   => buffer.putInt(data)
			case 8   => buffer.putLong(data.toLong)
			case bad => throw new AssertionError(s"Unable to map length of $bad to any integral type")
		}
	}

	@inline def readFloatTruncated(buffer: ByteBuffer, size: Long): Float = {
		(size: @switch) match {
			case 4   => buffer.getFloat
			case 8   => buffer.getDouble.toFloat
			case bad => throw new AssertionError(s"Unable to map length of $bad to any fractional type")
		}
	}

	@inline def writeFloatTruncated(buffer: ByteBuffer, size: Long, data: Float): Unit = {
		(size: @switch) match {
			case 4   => buffer.putFloat(data)
			case 8   => buffer.putDouble(data.toFloat)
			case bad => throw new AssertionError(s"Unable to map length of $bad to any fractional type")
		}
	}

	@inline def mkStagedReadFloatTruncated(sizes: Long*): (ByteBuffer, Long) => Float = {
		sizes.distinct.toList match {
			case 4 :: Nil => (b: ByteBuffer, _: Long) => b.getFloat
			case 8 :: Nil => (b: ByteBuffer, _: Long) => b.getDouble.toFloat
			case _        => (b: ByteBuffer, l: Long) => readFloatTruncated(b, l)
		}
	}

	@inline def mkStagedWriteFloatTruncated(sizes: Long*): (ByteBuffer, Long, Float) => Unit = {
		sizes.distinct.toList match {
			case 4 :: Nil => (b: ByteBuffer, _: Long, v: Float) => b.putFloat(v)
			case 8 :: Nil => (b: ByteBuffer, _: Long, v: Float) => b.putDouble(v)
			case _        => (b: ByteBuffer, l: Long, v: Float) => writeFloatTruncated(b, l, v)
		}
	}

	type |>[A, B] = (A, () => B)

	sealed trait Struct

	trait StructCodec[A, B] {
		def read(buffer: ByteBuffer): A
		def write(b: B, buffer: ByteBuffer): Unit
	}

	case class Header(timestamp: Long, entries: Int) extends Struct
	object Header {
		def apply(sdef: StructDef[Header]): Either[Throwable, StructCodec[Header, Header]] = for {
			timestamp <- sdef.resolveLength("timestamp", 0)
			entries <- sdef.resolveLength("entries", 1)
		} yield new StructCodec[Header, Header] {
			override def read(buffer: ByteBuffer): Header = Header(
				readLongPromoted(buffer, timestamp),
				readIntTruncated(buffer, entries))
			override def write(b: Header, buffer: ByteBuffer): Unit = {
				writeLongTruncated(buffer, size = timestamp, data = b.timestamp)
				writeIntTruncated(buffer, size = entries, data = b.entries)
			}
		}
	}


	case class Triangles(vertices: Array[Float]) extends Struct
	object Triangles {
		def apply(headerDef: StructDef[Header], sdef: StructDef[Triangles]):
		Either[Throwable, StructCodec[Header |> Triangles, Triangles]] = for {

			v0x <- sdef.resolveLength("v0.x", 0)
			v0y <- sdef.resolveLength("v0.y", 1)
			v0z <- sdef.resolveLength("v0.z", 2)
			v1x <- sdef.resolveLength("v1.x", 3)
			v1y <- sdef.resolveLength("v1.y", 4)
			v1z <- sdef.resolveLength("v1.z", 5)
			v2x <- sdef.resolveLength("v2.x", 6)
			v2y <- sdef.resolveLength("v2.y", 7)
			v2z <- sdef.resolveLength("v2.z", 8)
			headerFormatter <- Header(headerDef)
		} yield {
			val readStaged = mkStagedReadFloatTruncated(
				v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z,
			)
			val writeStaged = mkStagedWriteFloatTruncated(
				v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z,
			)
			val floatPerTrig = 3 * 3
			new StructCodec[Header |> Triangles, Triangles] {
				override def read(buffer: ByteBuffer): Header |> Triangles = {
					val header = headerFormatter.read(buffer)
					header -> { () =>
						val vertices = Array.ofDim[Float](header.entries * floatPerTrig)
						for (i <- 0 until header.entries) {
							vertices(i * floatPerTrig + 0) = readStaged(buffer, v0x)
							vertices(i * floatPerTrig + 1) = readStaged(buffer, v0y)
							vertices(i * floatPerTrig + 2) = readStaged(buffer, v0z)
							vertices(i * floatPerTrig + 3) = readStaged(buffer, v1x)
							vertices(i * floatPerTrig + 4) = readStaged(buffer, v1y)
							vertices(i * floatPerTrig + 5) = readStaged(buffer, v1z)
							vertices(i * floatPerTrig + 6) = readStaged(buffer, v2x)
							vertices(i * floatPerTrig + 7) = readStaged(buffer, v2y)
							vertices(i * floatPerTrig + 8) = readStaged(buffer, v2z)
						}
						Triangles(vertices)
					}
				}

				override def write(b: Triangles, buffer: ByteBuffer): Unit = {
					val entries = b.vertices.length / floatPerTrig
					val header = Header(System.currentTimeMillis(), entries)
					headerFormatter.write(header, buffer)
					println("Write header=" + header)
					for (i <- 0 until entries) {
						writeStaged(buffer, v0x, b.vertices(i * floatPerTrig + 0))
						writeStaged(buffer, v0y, b.vertices(i * floatPerTrig + 1))
						writeStaged(buffer, v0z, b.vertices(i * floatPerTrig + 2))
						writeStaged(buffer, v1x, b.vertices(i * floatPerTrig + 3))
						writeStaged(buffer, v1y, b.vertices(i * floatPerTrig + 4))
						writeStaged(buffer, v1z, b.vertices(i * floatPerTrig + 5))
						writeStaged(buffer, v2x, b.vertices(i * floatPerTrig + 6))
						writeStaged(buffer, v2y, b.vertices(i * floatPerTrig + 7))
						writeStaged(buffer, v2z, b.vertices(i * floatPerTrig + 8))
					}
				}
			}
		}
	}


	case class MeshTriangles(vertices: Array[Float], normals: Array[Float]) extends Struct
	object MeshTriangles {
		def apply(headerDef: StructDef[Header], sdef: StructDef[MeshTriangles]):
		Either[Throwable, StructCodec[Header |> MeshTriangles, MeshTriangles]] = for {

			v0x <- sdef.resolveLength("v0.x", 0)
			v0y <- sdef.resolveLength("v0.y", 1)
			v0z <- sdef.resolveLength("v0.z", 2)
			v1x <- sdef.resolveLength("v1.x", 3)
			v1y <- sdef.resolveLength("v1.y", 4)
			v1z <- sdef.resolveLength("v1.z", 5)
			v2x <- sdef.resolveLength("v2.x", 6)
			v2y <- sdef.resolveLength("v2.y", 7)
			v2z <- sdef.resolveLength("v2.z", 8)

			n0x <- sdef.resolveLength("n0.x", 9)
			n0y <- sdef.resolveLength("n0.y", 10)
			n0z <- sdef.resolveLength("n0.z", 11)
			n1x <- sdef.resolveLength("n1.x", 12)
			n1y <- sdef.resolveLength("n1.y", 13)
			n1z <- sdef.resolveLength("n1.z", 14)
			n2x <- sdef.resolveLength("n2.x", 15)
			n2y <- sdef.resolveLength("n2.y", 16)
			n2z <- sdef.resolveLength("n2.z", 17)

			headerFormatter <- Header(headerDef)
		} yield {
			val readStaged = mkStagedReadFloatTruncated(
				v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z,
				n0x, n0y, n0z, n1x, n1y, n1z, n2x, n2y, n2z,
			)
			val writeStaged = mkStagedWriteFloatTruncated(
				v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z,
				n0x, n0y, n0z, n1x, n1y, n1z, n2x, n2y, n2z
			)
			val floatPerTrig = 3 * 3
			new StructCodec[Header |> MeshTriangles, MeshTriangles] {
				override def read(buffer: ByteBuffer): Header |> MeshTriangles = {
					val header = headerFormatter.read(buffer)
					header -> { () =>
						val vertices = Array.ofDim[Float](header.entries * floatPerTrig)
						val normals = Array.ofDim[Float](header.entries * floatPerTrig)
						for (i <- 0 until header.entries) {
							vertices(i * floatPerTrig + 0) = readStaged(buffer, v0x)
							vertices(i * floatPerTrig + 1) = readStaged(buffer, v0y)
							vertices(i * floatPerTrig + 2) = readStaged(buffer, v0z)
							vertices(i * floatPerTrig + 3) = readStaged(buffer, v1x)
							vertices(i * floatPerTrig + 4) = readStaged(buffer, v1y)
							vertices(i * floatPerTrig + 5) = readStaged(buffer, v1z)
							vertices(i * floatPerTrig + 6) = readStaged(buffer, v2x)
							vertices(i * floatPerTrig + 7) = readStaged(buffer, v2y)
							vertices(i * floatPerTrig + 8) = readStaged(buffer, v2z)
							normals(i * floatPerTrig + 0) = readStaged(buffer, n0x)
							normals(i * floatPerTrig + 1) = readStaged(buffer, n0y)
							normals(i * floatPerTrig + 2) = readStaged(buffer, n0z)
							normals(i * floatPerTrig + 3) = readStaged(buffer, n1x)
							normals(i * floatPerTrig + 4) = readStaged(buffer, n1y)
							normals(i * floatPerTrig + 5) = readStaged(buffer, n1z)
							normals(i * floatPerTrig + 6) = readStaged(buffer, n2x)
							normals(i * floatPerTrig + 7) = readStaged(buffer, n2y)
							normals(i * floatPerTrig + 8) = readStaged(buffer, n2z)
						}
						MeshTriangles(vertices, normals)
					}
				}

				override def write(b: MeshTriangles, buffer: ByteBuffer): Unit = {
					val entries = b.vertices.length / floatPerTrig
					val header = Header(System.currentTimeMillis(), entries)
					headerFormatter.write(header, buffer)
					println("Write header=" + header)
					for (i <- 0 until entries) {
						writeStaged(buffer, v0x, b.vertices(i * floatPerTrig + 0))
						writeStaged(buffer, v0y, b.vertices(i * floatPerTrig + 1))
						writeStaged(buffer, v0z, b.vertices(i * floatPerTrig + 2))
						writeStaged(buffer, v1x, b.vertices(i * floatPerTrig + 3))
						writeStaged(buffer, v1y, b.vertices(i * floatPerTrig + 4))
						writeStaged(buffer, v1z, b.vertices(i * floatPerTrig + 5))
						writeStaged(buffer, v2x, b.vertices(i * floatPerTrig + 6))
						writeStaged(buffer, v2y, b.vertices(i * floatPerTrig + 7))
						writeStaged(buffer, v2z, b.vertices(i * floatPerTrig + 8))
						writeStaged(buffer, n0x, b.normals(i * floatPerTrig + 0))
						writeStaged(buffer, n0y, b.normals(i * floatPerTrig + 1))
						writeStaged(buffer, n0z, b.normals(i * floatPerTrig + 2))
						writeStaged(buffer, n1x, b.normals(i * floatPerTrig + 3))
						writeStaged(buffer, n1y, b.normals(i * floatPerTrig + 4))
						writeStaged(buffer, n1z, b.normals(i * floatPerTrig + 5))
						writeStaged(buffer, n2x, b.normals(i * floatPerTrig + 6))
						writeStaged(buffer, n2y, b.normals(i * floatPerTrig + 7))
						writeStaged(buffer, n2z, b.normals(i * floatPerTrig + 8))
					}
				}
			}
		}
	}

	case class Particle(id: Long, tpe: Int, mass: Float,
						position: Vec3, velocity: Vec3)

	case class Particles(xs: Array[Particle]) extends Struct
	object Particles {
		def apply(headerDef: StructDef[Header], sdef: StructDef[Particles]):
		Either[Throwable, StructCodec[Header |> Particles, Particles]] = for {
			id <- sdef.resolveLength("id", 0)
			tpe <- sdef.resolveLength("type", 1)
			mass <- sdef.resolveLength("mass", 2)
			positionX <- sdef.resolveLength("position.x", 3)
			positionY <- sdef.resolveLength("position.y", 4)
			positionZ <- sdef.resolveLength("position.z", 5)
			velocityX <- sdef.resolveLength("velocity.x", 6)
			velocityY <- sdef.resolveLength("velocity.y", 7)
			velocityZ <- sdef.resolveLength("velocity.z", 8)
			headerFormatter <- Header(headerDef)
		} yield {
			val readStaged = mkStagedReadFloatTruncated(
				positionX, positionY, positionZ,
				velocityX, velocityY, velocityZ)

			val writeStaged = mkStagedWriteFloatTruncated(
				positionX, positionY, positionZ,
				velocityX, velocityY, velocityZ)

			new StructCodec[Header |> Particles, Particles] {
				override def read(buffer: ByteBuffer): Header |> Particles = {
					val header = headerFormatter.read(buffer)
					header -> { () =>
						Particles(Array.fill(header.entries) {
							Particle(
								id = readLongPromoted(buffer, id),
								tpe = readIntTruncated(buffer, tpe),
								mass = readFloatTruncated(buffer, mass),
								position = Vec3(
									readStaged(buffer, positionX),
									readStaged(buffer, positionY),
									readStaged(buffer, positionZ)),
								velocity = Vec3(
									readStaged(buffer, velocityX),
									readStaged(buffer, velocityY),
									readStaged(buffer, velocityZ)
								)
							)
						})
					}
				}

				override def write(b: Particles, buffer: ByteBuffer): Unit = {
					headerFormatter.write(Header(System.currentTimeMillis(), b.xs.length), buffer)
					b.xs.foreach { x =>
						writeLongTruncated(buffer, id, x.id)
						writeIntTruncated(buffer, tpe, x.tpe)
						writeFloatTruncated(buffer, mass, x.mass)
						writeStaged(buffer, positionX, x.position.x)
						writeStaged(buffer, positionY, x.position.y)
						writeStaged(buffer, positionZ, x.position.z)
						writeStaged(buffer, velocityX, x.velocity.x)
						writeStaged(buffer, velocityY, x.velocity.y)
						writeStaged(buffer, velocityZ, x.velocity.z)
					}
				}
			}
		}
	}

	case class Entry(name: String, size: Long)
	case class StructDef[A <: Struct](fields: Vector[Entry]) {

		private lazy val lut: Map[(String, Int), Long] = fields
			.zipWithIndex
			.map { case (Entry(name, size), idx) => (name, idx) -> size }.toMap
		def resolveLength(key: String, index: Int): Either[Throwable, Long] = lut.get(key -> index) match {
			case Some(size) => Right(size)
			case None       => Left(new Exception(s"No key($key) at [$index], fields = ${fields.zipWithIndex}"))
		}
	}

	implicit val entryRw: RW[Entry] = macroRW
	implicit def structureRw[A <: Struct]: RW[StructDef[A]] = macroRW

	def readStructDef[A <: Struct](path: File): Either[Throwable, StructDef[A]] = {
		Try(read[StructDef[A]](path.contentAsString(StandardCharsets.UTF_8))).toEither
	}

}
