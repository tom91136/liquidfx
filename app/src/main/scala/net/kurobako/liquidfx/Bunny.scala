package net.kurobako.liquidfx

import com.google.common.io.Resources
import com.javafx.experiments.importers.Importer3D
import javafx.scene.{DepthTest, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{DrawMode, MeshView, TriangleMesh}

object Bunny {


	def makeBunny(): (Node, MeshView, TriangleMesh) = {
		val bunny: Node = Importer3D.load(Resources.getResource("l_bunny.obj").toExternalForm)
		val meshView: MeshView = bunny.asInstanceOf[javafx.scene.Group]
			.getChildren.get(0).asInstanceOf[javafx.scene.shape.MeshView]

		meshView.setDrawMode(DrawMode.FILL)
		meshView.setMaterial(new PhongMaterial(Color.rgb(200, 200, 200)){
			setSpecularColor(Color.rgb(255, 255, 255))
		})
		meshView.setDepthTest(DepthTest.ENABLE)

		val bunnyMesh = meshView
			.getMesh.asInstanceOf[javafx.scene.shape.TriangleMesh]
		(bunny, meshView, bunnyMesh)
	}


}
