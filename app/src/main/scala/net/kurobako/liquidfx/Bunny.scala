package net.kurobako.liquidfx

import com.google.common.io.Resources
import com.javafx.experiments.importers.Importer3D
import javafx.scene.{DepthTest, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{DrawMode, MeshView, TriangleMesh}
import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
import scalafx.scene.Group


object Bunny {

	def makeSlope(): (Node, MeshView, TriangleMesh) = {

		import net.kurobako.liquidfx.Application.updateMesh
		import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
		import scalafx.scene.DepthTest
		import scalafx.scene.paint.{Color, PhongMaterial}
		import scalafx.scene.shape.{CullFace, DrawMode, MeshView, TriangleMesh}

		val slope = new TriangleMesh() {}
		val slopeView = new MeshView(slope) {
			drawMode = DrawMode.Fill
			material = new PhongMaterial(Color.rgb(0, 100, 100, 0.5))
			depthTest = DepthTest.Enable
			cullFace = CullFace.None

		}

		val Yoff = -300
		val Zoff = -200
		val Xoff = 200

		updateMesh(slope, Triangle(
			Vec3(0 + Xoff, 100 + Yoff, 0 + Zoff),
			Vec3(0 + Xoff, 100 + Yoff, 800 + Zoff),
			Vec3(300 + Xoff, 700 + Yoff, 400 + Zoff),
		) :: Nil)

		(new Group(slopeView).delegate, slopeView.delegate, slope.delegate)
	}


	def makeBunny(): (Node, MeshView, TriangleMesh) = {
		val bunny: Node = Importer3D.load(Resources.getResource("l_bunny.obj").toExternalForm)
		val meshView: MeshView = bunny.asInstanceOf[javafx.scene.Group]
			.getChildren.get(0).asInstanceOf[javafx.scene.shape.MeshView]

		meshView.setDrawMode(DrawMode.FILL)
		meshView.setMaterial(new PhongMaterial(Color.rgb(200, 200, 200)) {
			setSpecularColor(Color.rgb(255, 255, 255))
		})
		meshView.setDepthTest(DepthTest.ENABLE)
		meshView.setScaleX(4.8)
		meshView.setScaleY(4.8)
		meshView.setScaleZ(4.8)
		val bunnyMesh = meshView
			.getMesh.asInstanceOf[javafx.scene.shape.TriangleMesh]
		(bunny, meshView, bunnyMesh)
	}


	def makeMug(): (Node, MeshView, TriangleMesh) = {
		val bunny: Node = Importer3D.load(Resources.getResource("Mug.obj").toExternalForm)
		val meshView: MeshView = bunny.asInstanceOf[javafx.scene.Group]
			.getChildren.get(0).asInstanceOf[javafx.scene.shape.MeshView]

		meshView.setDrawMode(DrawMode.FILL)
		meshView.setMaterial(new PhongMaterial(Color.rgb(200, 200, 200)) {
			setSpecularColor(Color.rgb(255, 255, 255))
		})
		meshView.setDepthTest(DepthTest.ENABLE)

		val bunnyMesh = meshView
			.getMesh.asInstanceOf[javafx.scene.shape.TriangleMesh]
		meshView.setScaleX(100 /2 )
		meshView.setScaleY(-100 /2 )
		meshView.setScaleZ(100 /2 )
		(bunny, meshView, bunnyMesh)
	}

	def time[R](name: String)(block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println(s"[$name] " + (t1 - t0).toDouble / 1000000 + "ms")
		result
	}

	def samplePoints(vs: Array[Vec3], a: Float) = {

		import Maths._

		val min = vs.par.foldLeft(Vec3.Zero) { case (l, r) => Vec3(l.x min r.x, l.y min r.y, l.z min r.z) }
		val max = vs.par.foldLeft(Vec3.Zero) { case (l, r) => Vec3(l.x max r.x, l.y max r.y, l.z max r.z) }


		val tree = MutableUnsafeOctree[Vec3](Vec3.Zero, min distance max)(identity)

		time("insert") {

			vs.foreach(tree.insertPoint(_))
		}


		val ps =
			time("PARAM") {
				for {
					x <- (min.x to max.x by a / 2).par
					y <- (min.y to max.y by a / 2)
					z <- (min.z to max.z by a / 2)
					p = Vec3(x.toFloat, y.toFloat, z.toFloat)
					if !tree.pointsInSphere(p, a / 2).isEmpty
				} yield p
			}
		println((min - max) + " vs=" + vs.length + " -> " + ps.length)

		ps.seq
	}


}
