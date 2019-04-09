package net.kurobako.liquidfx

import net.kurobako.liquidfx.MM.meshView
import net.kurobako.liquidfx.Metaball.Triangle
import net.kurobako.liquidfx.SphSolver.Vec3
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Point3D
import scalafx.scene.layout.{Priority, StackPane, VBox}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.{CullFace, DrawMode, MeshView, Sphere, TriangleMesh, VertexFormat}
import scalafx.scene.{Group, Scene}

object TrigCheck extends JFXApp {

	val Z = 200f

	val A = Triangle(
		Vec3(0, 0, 0),
		Vec3(0, 0, Z),
		Vec3(-300, 100, Z / 2))
	val B = Triangle(
		Vec3(300, 100, Z / 2),
		Vec3(0, 0, Z),
		Vec3(0, 0, 0))


	def splatX(xs: Vec3*) = {
		(for {
			x <- xs
			v <- x.array(_.toFloat)
		} yield v).toArray
	}

	def splat(xs: Triangle*) = {
		(for {
			x <- xs
			v <- x.arr
		} yield v.toFloat).toArray
	}
	val mesh = new TriangleMesh(VertexFormat.PointNormalTexcoord) {
		texCoords = Array(0, 2)


		val half: Vec3 = (A.normal + B.normal) / 2
		delegate.getNormals.setAll(splatX(
			half,
			half,
			half,

			half,
			half,
			half,
		): _*)
		points = splat(A, B)
		faceSmoothingGroups = Array(4, 4)
		faces = Array(
			0, 0, 0,
			1, 1, 0,
			2, 2, 0,

			3, 3, 0,
			4, 4, 0,
			5, 5, 0,
		)
	}

	val meshView = new MeshView(mesh) {
		drawMode = DrawMode.Fill
		material = new PhongMaterial(Color.rgb(0, 119, 190)) {
			specularColor = Color.rgb(190, 119, 190, 0.5)

		}
		cullFace = CullFace.None
	}

	val group = new Group(
		SceneControl.mkAxis(),
		meshView,
		new Sphere(100, 5)
	)


	val subScene = SceneControl.mkScene(group, 500, 500)


	stage = new PrimaryStage {
		title = "SPH simulation"
		scene = new Scene(new VBox(

			new StackPane {
				children = Seq(
					subScene,
				)
				subScene.width <== width
				subScene.height <== height
				vgrow = Priority.Always
			},


		), 800, 800) {}
	}

}
