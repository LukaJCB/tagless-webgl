package scalagl.programs

import cats.Monad
import cats.effect._
import cats.implicits._
import org.scalajs.dom.raw.WebGLRenderingContext.COLOR_BUFFER_BIT

import scalagl.algebra._
import scalagl.math.{Matrix4, Quaternion, Vector4}

object Program2Point5D {
  def simpleRace[F[_]: Monad](D: Dom[F], W: DrawImage[F]): F[Unit] = {

    val vertSrc =
      """
        |attribute vec4 a_position;
        |attribute vec2 a_texcoord;
        |
        |uniform mat4 u_matrix;
        |
        |varying vec2 v_texcoord;
        |
        |void main() {
        |   gl_Position = u_matrix * a_position;
        |   v_texcoord = a_texcoord;
        |}
      """.stripMargin

    val fragSrc =
      """
        |precision mediump float;
        |
        |varying vec2 v_texcoord;
        |
        |uniform sampler2D u_texture;
        |
        |void main() {
        |   gl_FragColor = texture2D(u_texture, v_texcoord);
        |   if(gl_FragColor.a < 0.5)
        |     discard;
        |}
      """.stripMargin


    val trackScale = Matrix4.forScale(Vector4(20, 20, 1, 1))

    val trackTranslation = Matrix4.forTranslation(Vector4(7f, 6.5f, 0, 1))

    val carScale = Matrix4.forScale(Vector4(0.5f, 0.42f, 1, 1))

    val carRotation = Matrix4.forRotation(Quaternion(-0.7f, 0, 0, 0.7f))

    val view = Matrix4.setLookAtM(0, -0.7f, -0.35f,
      0, 0, 0, 0, 1, 0)

    for {
      canvas <- W.createFullSizeCanvas()
      _ <- D.appendToBody(canvas)

      projection = Matrix4.forPerspective(90, canvas.width / canvas.height, 0.1f, 100f)
      projectionView = projection * view

      fragmentShader <- W.compileFragmentShader(fragSrc)
      vertexShader <- W.compileVertexShader(vertSrc)

      program <- (vertexShader, fragmentShader).mapN(W.createProgram).flatSequence

      trackImg <- D.createImageElement("racetrack.png")
      carImg <- D.createImageElement("car.png")

      track <- W.createTextureInfo(trackImg)
      car <- W.createTextureInfo(carImg)



      _ <- W.clearScreen(0, 0, 0, 1)

      _ <- program.toList.traverse { p =>
        W.drawImage(p, track, projectionView * trackTranslation * trackScale) *>
        W.drawImage(p, car, projectionView * carRotation * carScale)
      }

    } yield ()


  }
}
