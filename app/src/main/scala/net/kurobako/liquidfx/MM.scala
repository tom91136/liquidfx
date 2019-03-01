package net.kurobako.liquidfx

import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import java.time.Instant

import net.kurobako.liquidfx.SphSolver.Vec3

object MM {
	def main(args: Array[String]): Unit = {


		import java.io.RandomAccessFile
		import java.nio.MappedByteBuffer
		import java.nio.channels.FileChannel
		val file = Paths.get("/home/tom/Desktop/ipc")

		def time[R](name: String)(block: => R): R = {
			val t0 = System.nanoTime()
			val result = block
			val t1 = System.nanoTime()
			println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
			result
		}

		val filechannel = FileChannel.open(file, StandardOpenOption.READ)
		val buffer = filechannel.map(FileChannel.MapMode.READ_ONLY, 0, filechannel.size)

		var last: Long = 0;
		while (buffer.isLoaded) {
			Thread.onSpinWait()


			buffer.order(ByteOrder.LITTLE_ENDIAN)

			//			buffer.clear()


			var all = 0.0;
			val start = buffer.getLong

			//				println(s"Started=$start ~ ${Instant.ofEpochMilli(start)}")

			if (start > last) {
				last = start

				time("Read one") {
					println(s"Size=${buffer.capacity()}")
					for (i <- 0 until 10000) {
						val index = buffer.getLong
						val mass = buffer.getFloat
						val pos = Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat)
						val vel = Vec3(buffer.getFloat, buffer.getFloat, buffer.getFloat)
						//			all += (s"[$i] $index $mass $pos $vel".length)
						all += (pos distance vel)
					}
					println(all)
				}

			} else {
			}


			buffer.clear()
		}

	}
}
