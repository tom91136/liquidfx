package net.kurobako.liquidfx

import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption
import java.time.{Duration, Instant}

import better.files.{Dispose, File}
import com.google.common.primitives.Floats
import net.kurobako.liquidfx.MM.BasePath
import net.kurobako.liquidfx.SphSolver.Vec3
import net.kurobako.liquidfx.StructDefs.{Entry, Header, Particle, Particles, StructDef, Triangles}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest._

class StructDefSpec extends FlatSpec with Matchers {


	val HeaderDef: StructDef[Header] = StructDef[Header](Vector(
		Entry("timestamp", 8),
		Entry("entries", 8)))

	val TrianglesDef: StructDef[Triangles] = StructDef[Triangles](Vector(
		Entry("v0.x", 4),
		Entry("v0.y", 4),
		Entry("v0.z", 4),
		Entry("v1.x", 4),
		Entry("v1.y", 4),
		Entry("v1.z", 4),
		Entry("v2.x", 4),
		Entry("v2.y", 4),
		Entry("v2.z", 4),
	))

	val ParticlesDef: StructDef[Particles] = StructDef[Particles](Vector(
		Entry("id", 8),
		Entry("type", 4),
		Entry("mass", 4),
		Entry("position.x", 4),
		Entry("position.y", 4),
		Entry("position.z", 4),
		Entry("velocity.x", 4),
		Entry("velocity.y", 4),
		Entry("velocity.z", 4)
	))

	implicit val vec3Eq: Equality[Vec3] = (a: Vec3, b: Any) => b match {
		case Vec3(x, y, z) =>
			(x === a.x +- 0.0000001f) &&
			(y === a.y +- 0.0000001f) &&
			(z === a.z +- 0.0000001f)
		case _             => false
	}

	implicit val particleEq: Equality[Particle] = (a: Particle, b: Any) => b match {
		case Particle(id, tpe, mass, pos, vel) =>
			id == a.id &&
			tpe == a.tpe &&
			(mass === a.mass +- 0.0000001f) &&
			(pos === a.position) && (vel === a.velocity)
		case _                                 => false
	}

	def openRwMmf(size: Long): Dispose[MappedByteBuffer] = {
		for {
			tmp <- File.temporaryFile()
			chan <- tmp.fileChannel(Seq(StandardOpenOption.READ, StandardOpenOption.WRITE))
		} yield chan.map(FileChannel.MapMode.READ_WRITE, 0, 4096)
	}

	"Particles" should "write and then read" in {
		openRwMmf(1024 * 4).foreach { buffer =>
			val expected = Particles(Array.tabulate(100) { i =>
				Particle(i, i * 2, 42, Vec3(43), Vec3(math.Pi))
			})
			val reader = Particles(HeaderDef, ParticlesDef).right.get
			buffer.clear()
			reader.write(expected, buffer)
			buffer.clear()
			val (header, f) = reader.read(buffer)
			val actual = f()
			(header.timestamp - System.currentTimeMillis()).toInt should be < 10
			header.entries should ===(expected.xs.length)
			actual.xs should contain theSameElementsInOrderAs expected.xs
		}
	}

	"Triangles" should "write and then read" in {
		openRwMmf(1024 * 16).foreach { buffer =>
			val floatPerTrig = 3 * 3
			val expected = Triangles(Array.tabulate(floatPerTrig * 100) { i => (math.Pi / (i + 1)).toFloat })
			val reader = Triangles(HeaderDef, TrianglesDef).right.get
			buffer.clear()
			reader.write(expected, buffer)
			buffer.clear()
			val (header, f) = reader.read(buffer)
			val actual = f()
			(header.timestamp - System.currentTimeMillis()).toInt should be < 10
			header.entries should ===(expected.ps.length / floatPerTrig)
			actual.ps should contain theSameElementsInOrderAs expected.ps
		}
	}


}
