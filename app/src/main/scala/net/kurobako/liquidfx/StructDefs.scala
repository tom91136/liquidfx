package net.kurobako.liquidfx

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import better.files.File
import net.kurobako.liquidfx.Maths.Vec3

import scala.annotation.switch
import scala.util.Try

object StructDefs {

	import upickle.default.{macroRW, ReadWriter => RW, _}


	@inline def readBooleanTruncated(buffer: ByteBuffer, size: Long): Boolean = {
		readIntTruncated(buffer, size) == 0
	}

	@inline def writeBooleanTruncated(buffer: ByteBuffer, size: Long, data: Boolean): Unit = {
		writeIntTruncated(buffer, size, if (data) 1 else 0)
	}


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

	@inline def mkStagedReadIntTruncated(sizes: Long*): (ByteBuffer, Long) => Int = {
		sizes.distinct.toList match {
			case 1 :: Nil => (b: ByteBuffer, _: Long) => b.get.toInt
			case 2 :: Nil => (b: ByteBuffer, _: Long) => b.getShort.toInt
			case 4 :: Nil => (b: ByteBuffer, _: Long) => b.getInt
			case 8 :: Nil => (b: ByteBuffer, _: Long) => b.getLong.toInt
			case _ :: Nil => (b: ByteBuffer, l: Long) => readIntTruncated(b, l)
		}
	}


	@inline def mkStagedWriteIntTruncated(sizes: Long*): (ByteBuffer, Long, Int) => Unit = {
		sizes.distinct.toList match {
			case 1 :: Nil => (b: ByteBuffer, _: Long, v: Int) => b.put(v.toByte)
			case 2 :: Nil => (b: ByteBuffer, _: Long, v: Int) => b.putShort(v.toShort)
			case 4 :: Nil => (b: ByteBuffer, _: Long, v: Int) => b.putInt(v)
			case 8 :: Nil => (b: ByteBuffer, _: Long, v: Int) => b.putLong(v)
			case _ :: Nil => (b: ByteBuffer, l: Long, v: Int) => writeIntTruncated(b, l, v)
		}
	}

	type |>[A, B] = (A, () => B)

	sealed trait Struct

	trait StructCodec[A, B] {

		def read(buffer: ByteBuffer): A
		def write(b: B, buffer: ByteBuffer): Unit
	}

	case class Well(centre: Vec3, force: Float) extends Struct
	object Well {
		def apply(headerDef: StructDef[Header], sdef: StructDef[Array[Well]]):
		Either[Throwable, StructCodec[Array[Well], Array[Well]]] = for {
			wellx <- sdef.resolveLength("well.x", 0)
			welly <- sdef.resolveLength("well.y", 1)
			wellz <- sdef.resolveLength("well.z", 2)
			force <- sdef.resolveLength("force", 3)
			header <- Header(headerDef)
		} yield new StructCodec[Array[Well], Array[Well]] {
			override def read(buffer: ByteBuffer): Array[Well] =
				Array.fill(header.read(buffer).entries) {
					Well(Vec3(
						readFloatTruncated(buffer, wellx),
						readFloatTruncated(buffer, welly),
						readFloatTruncated(buffer, wellz)
					), readFloatTruncated(buffer, force))
				}
			override def write(wells: Array[Well], buffer: ByteBuffer): Unit = {
				header.write(Header(wells.length), buffer)
				wells.foreach { well =>
					writeFloatTruncated(buffer, wellx, well.centre.x)
					writeFloatTruncated(buffer, welly, well.centre.y)
					writeFloatTruncated(buffer, wellz, well.centre.z)
					writeFloatTruncated(buffer, force, well.force)
				}
			}
		}
	}

	case class Source(centre: Vec3, velocity: Vec3,
					  rate: Long, tag: Long, colour: Int) extends Struct
	object Source {
		def apply(headerDef: StructDef[Header], sdef: StructDef[Array[Source]]):
		Either[Throwable, StructCodec[Array[Source], Array[Source]]] = for {
			sourcex <- sdef.resolveLength("source.x", 0)
			sourcey <- sdef.resolveLength("source.y", 1)
			sourcez <- sdef.resolveLength("source.z", 2)
			source_velx <- sdef.resolveLength("source.vel.x", 3)
			source_vely <- sdef.resolveLength("source.vel.y", 4)
			source_velz <- sdef.resolveLength("source.vel.z", 5)
			rate <- sdef.resolveLength("rate", 6)
			tag <- sdef.resolveLength("tag", 7)
			colour <- sdef.resolveLength("colour", 8)
			header <- Header(headerDef)
		} yield new StructCodec[Array[Source], Array[Source]] {
			override def read(buffer: ByteBuffer): Array[Source] =
				Array.fill(header.read(buffer).entries) {
					Source(
						centre = Vec3(
							readFloatTruncated(buffer, sourcex),
							readFloatTruncated(buffer, sourcey),
							readFloatTruncated(buffer, sourcez)
						),
						velocity = Vec3(
							readFloatTruncated(buffer, source_velx),
							readFloatTruncated(buffer, source_vely),
							readFloatTruncated(buffer, source_velz)),
						rate = readLongPromoted(buffer, rate),
						tag = readLongPromoted(buffer, tag),
						colour = readIntTruncated(buffer, colour)
					)

				}
			override def write(sources: Array[Source], buffer: ByteBuffer): Unit = {
				header.write(Header(sources.length), buffer)
				sources.foreach { source =>
					writeFloatTruncated(buffer, sourcex, source.centre.x)
					writeFloatTruncated(buffer, sourcey, source.centre.y)
					writeFloatTruncated(buffer, sourcez, source.centre.z)
					writeFloatTruncated(buffer, source_velx, source.velocity.x)
					writeFloatTruncated(buffer, source_vely, source.velocity.y)
					writeFloatTruncated(buffer, source_velz, source.velocity.z)
					writeLongTruncated(buffer, rate, source.rate)
					writeLongTruncated(buffer, tag, source.tag)
					writeLongTruncated(buffer, colour, source.colour)
				}
			}
		}
	}

	case class Drain(centre: Vec3, width: Float, depth: Float) extends Struct
	object Drain {
		def apply(headerDef: StructDef[Header], sdef: StructDef[Array[Drain]]):
		Either[Throwable, StructCodec[Array[Drain], Array[Drain]]] = for {
			drainx <- sdef.resolveLength("drain.x", 0)
			drainy <- sdef.resolveLength("drain.y", 1)
			drainz <- sdef.resolveLength("drain.z", 2)
			width <- sdef.resolveLength("width", 3)
			depth <- sdef.resolveLength("depth", 4)
			header <- Header(headerDef)
		} yield new StructCodec[Array[Drain], Array[Drain]] {
			override def read(buffer: ByteBuffer): Array[Drain] =
				Array.fill(header.read(buffer).entries) {
					Drain(
						centre = Vec3(
							readFloatTruncated(buffer, drainx),
							readFloatTruncated(buffer, drainy),
							readFloatTruncated(buffer, drainz)
						),
						width = readFloatTruncated(buffer, width),
						depth = readFloatTruncated(buffer, depth))
				}
			override def write(sources: Array[Drain], buffer: ByteBuffer): Unit = {
				header.write(Header(sources.length), buffer)
				sources.foreach { drain =>
					writeFloatTruncated(buffer, drainx, drain.centre.x)
					writeFloatTruncated(buffer, drainy, drain.centre.y)
					writeFloatTruncated(buffer, drainz, drain.centre.z)
					writeFloatTruncated(buffer, width, drain.width)
					writeFloatTruncated(buffer, depth, drain.depth)
				}
			}
		}
	}


	case class SceneMeta(suspend: Boolean,
						 terminate: Boolean,
						 solverIter: Int,
						 solverStep: Float,
						 solverScale: Float,
						 surfaceRes: Float,
						 gravity: Float,
						 minBound: Vec3, maxBound: Vec3
						) extends Struct

	object SceneMeta {
		def apply(sdef: StructDef[SceneMeta]): Either[Throwable, StructCodec[SceneMeta, SceneMeta]] = for {
			suspend <- sdef.resolveLength("suspend", 0)
			terminate <- sdef.resolveLength("terminate", 1)
			solverIter <- sdef.resolveLength("solverIter", 2)
			solverStep <- sdef.resolveLength("solverStep", 3)
			solverScale <- sdef.resolveLength("solverScale", 4)
			surfaceRes <- sdef.resolveLength("surfaceRes", 5)
			gravity <- sdef.resolveLength("gravity", 6)

			minBoundx <- sdef.resolveLength("minBound.x", 7)
			minBoundy <- sdef.resolveLength("minBound.y", 8)
			minBoundz <- sdef.resolveLength("minBound.z", 9)

			maxBoundx <- sdef.resolveLength("maxBound.x", 10)
			maxBoundy <- sdef.resolveLength("maxBound.y", 11)
			maxBoundz <- sdef.resolveLength("maxBound.z", 12)

		} yield new StructCodec[SceneMeta, SceneMeta] {
			override def read(buffer: ByteBuffer): SceneMeta = SceneMeta(
				readBooleanTruncated(buffer, suspend),
				readBooleanTruncated(buffer, terminate),
				readIntTruncated(buffer, solverIter),
				readFloatTruncated(buffer, solverStep),
				readFloatTruncated(buffer, solverScale),
				readFloatTruncated(buffer, surfaceRes),
				readFloatTruncated(buffer, gravity),
				Vec3(
					readFloatTruncated(buffer, minBoundx),
					readFloatTruncated(buffer, minBoundy),
					readFloatTruncated(buffer, minBoundz)
				),
				Vec3(
					readFloatTruncated(buffer, maxBoundx),
					readFloatTruncated(buffer, maxBoundy),
					readFloatTruncated(buffer, maxBoundz)
				)
			)
			override def write(b: SceneMeta, buffer: ByteBuffer): Unit = {
				writeBooleanTruncated(buffer, suspend, b.suspend)
				writeBooleanTruncated(buffer, terminate, b.terminate)
				writeIntTruncated(buffer, solverIter, b.solverIter)
				writeFloatTruncated(buffer, solverStep, b.solverStep)
				writeFloatTruncated(buffer, solverScale, b.solverScale)
				writeFloatTruncated(buffer, surfaceRes, b.surfaceRes)
				writeFloatTruncated(buffer, gravity, b.gravity)

				writeFloatTruncated(buffer, minBoundx, b.minBound.x)
				writeFloatTruncated(buffer, minBoundy, b.minBound.y)
				writeFloatTruncated(buffer, minBoundz, b.minBound.z)

				writeFloatTruncated(buffer, maxBoundx, b.maxBound.x)
				writeFloatTruncated(buffer, maxBoundy, b.maxBound.y)
				writeFloatTruncated(buffer, maxBoundz, b.maxBound.z)
			}
		}
	}

	case class Scene(meta: SceneMeta,
					 wells: Array[Well],
					 sources: Array[Source],
					 drains: Array[Drain]) extends Struct

	object Scene {
		def apply(metaDef: StructDef[SceneMeta],
				  headerDef: StructDef[Header],
				  wellDef: StructDef[Array[Well]],
				  sourceDef: StructDef[Array[Source]],
				  drainDef: StructDef[Array[Drain]]
				 ): Either[Throwable, StructCodec[Scene, Scene]] = for {
			meta <- SceneMeta(metaDef)
			wellCodec <- Well(headerDef, wellDef)
			sourceCodec <- Source(headerDef, sourceDef)
			drainCodec <- Drain(headerDef, drainDef)
		} yield new StructCodec[Scene, Scene] {
			override def read(buffer: ByteBuffer): Scene = Scene(
				meta.read(buffer),
				wellCodec.read(buffer),
				sourceCodec.read(buffer),
				drainCodec.read(buffer)
			)
			override def write(scene: Scene, buffer: ByteBuffer): Unit = {
				meta.write(scene.meta, buffer)
				wellCodec.write(scene.wells, buffer)
				sourceCodec.write(scene.sources, buffer)
				drainCodec.write(scene.drains, buffer)
			}
		}
	}


	case class Header(timestamp: Long, entries: Int, written: Int) extends Struct
	object Header {
		def apply(n: Int): Header = new Header(System.currentTimeMillis(), n, n)
		def apply(sdef: StructDef[Header]): Either[Throwable, StructCodec[Header, Header]] = for {
			timestamp <- sdef.resolveLength("timestamp", 0)
			entries <- sdef.resolveLength("entries", 1)
			written <- sdef.resolveLength("written", 2)
		} yield new StructCodec[Header, Header] {
			override def read(buffer: ByteBuffer): Header = Header(
				readLongPromoted(buffer, timestamp),
				readIntTruncated(buffer, entries),
				readIntTruncated(buffer, written))
			override def write(b: Header, buffer: ByteBuffer): Unit = {
				writeLongTruncated(buffer, size = timestamp, data = b.timestamp)
				writeIntTruncated(buffer, size = entries, data = b.entries)
				writeIntTruncated(buffer, size = entries, data = b.written)
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
			headerCodec <- Header(headerDef)
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
					val header = headerCodec.read(buffer)
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
					headerCodec.write(Header(entries), buffer)
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


	case class MeshTriangles(vertices: Array[Float],
							 normals: Array[Float],
							 colours: Array[Int]) extends Struct
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

			cx <- sdef.resolveLength("c.x", 18)
			cy <- sdef.resolveLength("c.y", 19)
			cz <- sdef.resolveLength("c.z", 20)

			headerCodec <- Header(headerDef)
		} yield {
			val readStaged = mkStagedReadFloatTruncated(
				v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z,
				n0x, n0y, n0z, n1x, n1y, n1z, n2x, n2y, n2z,
			)
			val writeStaged = mkStagedWriteFloatTruncated(
				v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z,
				n0x, n0y, n0z, n1x, n1y, n1z, n2x, n2y, n2z
			)
			val readColourStaged = mkStagedReadIntTruncated(cx, cy, cz)
			val writeColourStaged = mkStagedWriteIntTruncated(cx, cy, cz)

			val v3fn = 3 * 3
			val s1in = 3

			new StructCodec[Header |> MeshTriangles, MeshTriangles] {
				override def read(buffer: ByteBuffer): Header |> MeshTriangles = {
					val header = headerCodec.read(buffer)
					header -> { () =>
						val vertices = Array.ofDim[Float](header.entries * v3fn)
						val normals = Array.ofDim[Float](header.entries * v3fn)
						val colours = Array.ofDim[Int](header.entries * s1in)
						for (i <- 0 until header.entries) {
							vertices(i * v3fn + 0) = readStaged(buffer, v0x)
							vertices(i * v3fn + 1) = readStaged(buffer, v0y)
							vertices(i * v3fn + 2) = readStaged(buffer, v0z)
							vertices(i * v3fn + 3) = readStaged(buffer, v1x)
							vertices(i * v3fn + 4) = readStaged(buffer, v1y)
							vertices(i * v3fn + 5) = readStaged(buffer, v1z)
							vertices(i * v3fn + 6) = readStaged(buffer, v2x)
							vertices(i * v3fn + 7) = readStaged(buffer, v2y)
							vertices(i * v3fn + 8) = readStaged(buffer, v2z)
							normals(i * v3fn + 0) = readStaged(buffer, n0x)
							normals(i * v3fn + 1) = readStaged(buffer, n0y)
							normals(i * v3fn + 2) = readStaged(buffer, n0z)
							normals(i * v3fn + 3) = readStaged(buffer, n1x)
							normals(i * v3fn + 4) = readStaged(buffer, n1y)
							normals(i * v3fn + 5) = readStaged(buffer, n1z)
							normals(i * v3fn + 6) = readStaged(buffer, n2x)
							normals(i * v3fn + 7) = readStaged(buffer, n2y)
							normals(i * v3fn + 8) = readStaged(buffer, n2z)
							colours(i * s1in + 0) = readColourStaged(buffer, cx)
							colours(i * s1in + 1) = readColourStaged(buffer, cy)
							colours(i * s1in + 2) = readColourStaged(buffer, cz)
						}
						MeshTriangles(vertices, normals, colours)
					}
				}

				override def write(b: MeshTriangles, buffer: ByteBuffer): Unit = {
					val entries = b.vertices.length / v3fn
					headerCodec.write(Header(entries), buffer)
					for (i <- 0 until entries) {
						writeStaged(buffer, v0x, b.vertices(i * v3fn + 0))
						writeStaged(buffer, v0y, b.vertices(i * v3fn + 1))
						writeStaged(buffer, v0z, b.vertices(i * v3fn + 2))
						writeStaged(buffer, v1x, b.vertices(i * v3fn + 3))
						writeStaged(buffer, v1y, b.vertices(i * v3fn + 4))
						writeStaged(buffer, v1z, b.vertices(i * v3fn + 5))
						writeStaged(buffer, v2x, b.vertices(i * v3fn + 6))
						writeStaged(buffer, v2y, b.vertices(i * v3fn + 7))
						writeStaged(buffer, v2z, b.vertices(i * v3fn + 8))
						writeStaged(buffer, n0x, b.normals(i * v3fn + 0))
						writeStaged(buffer, n0y, b.normals(i * v3fn + 1))
						writeStaged(buffer, n0z, b.normals(i * v3fn + 2))
						writeStaged(buffer, n1x, b.normals(i * v3fn + 3))
						writeStaged(buffer, n1y, b.normals(i * v3fn + 4))
						writeStaged(buffer, n1z, b.normals(i * v3fn + 5))
						writeStaged(buffer, n2x, b.normals(i * v3fn + 6))
						writeStaged(buffer, n2y, b.normals(i * v3fn + 7))
						writeStaged(buffer, n2z, b.normals(i * v3fn + 8))
						writeColourStaged(buffer, cx, b.colours(i * s1in + 0))
						writeColourStaged(buffer, cy, b.colours(i * s1in + 1))
						writeColourStaged(buffer, cz, b.colours(i * s1in + 2))
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
					headerFormatter.write(Header(System.currentTimeMillis(), b.xs.length, b.xs.length), buffer)
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
	case class StructDef[A](fields: Vector[Entry], size: Long) {

		if (fields.map(_.size).sum != size)
			throw new AssertionError(s"corrupt struct def ${this}")

		private lazy val lut: Map[(String, Int), Long] = fields
			.zipWithIndex
			.map { case (Entry(name, s), idx) => (name, idx) -> s }.toMap
		def resolveLength(key: String, index: Int): Either[Throwable, Long] = lut.get(key -> index) match {
			case Some(s) => Right(s)
			case None    => Left(new Exception(s"No key($key) at [$index], fields = ${fields.zipWithIndex}"))
		}
	}


	implicit val entryRw: RW[Entry] = macroRW
	implicit def structureRw[A]: RW[StructDef[A]] = macroRW

	trait MkDef {
		def resolve[A](key: String): Either[Throwable, StructDef[A]]
	}


	def readStructDef(path: File): Either[Throwable, MkDef] = {

		Try(read[Map[String, StructDef[Unit]]](path.contentAsString(StandardCharsets.UTF_8)))
			.map { map =>

				new MkDef {
					override def resolve[A](key: String) = map.get(key) match {
						case Some(sd) => Right(sd.asInstanceOf[StructDef[A]])
						case None     => Left(new Exception(s"$key not found in $map"))
					}
				}
			}.toEither
	}

}
