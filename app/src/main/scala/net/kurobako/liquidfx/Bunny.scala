package net.kurobako.liquidfx

import com.google.common.io.Resources
import com.javafx.experiments.importers.Importer3D
import javafx.scene.{DepthTest, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{DrawMode, MeshView, TriangleMesh}
import scalafx.scene.Group


object Bunny {

	def makeSlope(): (Node, MeshView, TriangleMesh) = {

		import net.kurobako.liquidfx.Application.updateMesh
		import net.kurobako.liquidfx.Maths.{Triangle, Vec3}
		import scalafx.scene.DepthTest
		import scalafx.scene.paint.{Color, PhongMaterial}
		import scalafx.scene.shape.{CullFace, DrawMode, MeshView, TriangleMesh}

		val slope     = new TriangleMesh() {}
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
		meshView.setMaterial(new PhongMaterial(Color.rgb(200, 200, 200)){
			setSpecularColor(Color.rgb(255, 255, 255))
		})
		meshView.setDepthTest(DepthTest.ENABLE)
		meshView.setScaleX (4.8)
		meshView.setScaleY (4.8)
		meshView.setScaleZ (4.8)
		val bunnyMesh = meshView
			.getMesh.asInstanceOf[javafx.scene.shape.TriangleMesh]
		(bunny, meshView, bunnyMesh)
	}



	def makeMug(): (Node, MeshView, TriangleMesh) = {
		val bunny: Node = Importer3D.load(Resources.getResource("Mug.obj").toExternalForm)
		val meshView: MeshView = bunny.asInstanceOf[javafx.scene.Group]
			.getChildren.get(0).asInstanceOf[javafx.scene.shape.MeshView]

		meshView.setDrawMode(DrawMode.FILL)
		meshView.setMaterial(new PhongMaterial(Color.rgb(200, 200, 200)){
			setSpecularColor(Color.rgb(255, 255, 255))
		})
		meshView.setDepthTest(DepthTest.ENABLE)

		val bunnyMesh = meshView
			.getMesh.asInstanceOf[javafx.scene.shape.TriangleMesh]
		meshView.setScaleX(100)
		meshView.setScaleY(-100)
		meshView.setScaleZ(100)
		(bunny, meshView, bunnyMesh)
	}


}
