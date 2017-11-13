package scalagl.interpreters

import cats.{Monad, Monoid, Order, Traverse}
import cats.implicits._
import cats.effect._
import org.scalajs.dom.ext.Color
import org.scalajs.dom.raw.{WebGLProgram, WebGLRenderingContext, WebGLTexture}

import scala.collection.immutable.SortedMap
import scala.collection.{Set, mutable}
import scala.language.higherKinds
import scalagl.algebra._
import scalagl.math.{Matrix4, Quaternion, Vector4}

case class InitializationOptions(backgroundColor: Color, textures: SortedMap[String, String])

sealed trait InitializationError
case class VertexShaderError(e: String) extends InitializationError
case class FragmentShaderError(e: String) extends InitializationError
case class ProgramLinkingError(e: String) extends InitializationError
object InitializationError {
  def vertex(e: String): InitializationError = VertexShaderError(e)
  def fragment(e: String): InitializationError = FragmentShaderError(e)
  def program(e: String): InitializationError = ProgramLinkingError(e)
}

case class RenderContext(program: WebGLProgram, textures: SortedMap[String, WebGLTexture], projection: Matrix4)

class RenderEngineInterpreterWebGL[F[_]: Monad](W: DrawImage[F], D: Dom[F])
  extends RenderEngine[F, InitializationOptions, InitializationError, RenderContext, WebGLTexture] {

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


  def initialize(options: InitializationOptions): F[Either[InitializationError, RenderContext]] = for {
    canvas <- W.createFullSizeCanvas()
    _ <- D.appendToBody(canvas)

    projection = Matrix4.forPerspective(90, canvas.width / canvas.height, 0.1f, 100f)

    fragmentShader <- W.compileFragmentShader(fragSrc)
    vertexShader <- W.compileVertexShader(vertSrc)

    program <- (vertexShader.leftMap(InitializationError.vertex), fragmentShader.leftMap(InitializationError.fragment))
      .mapN((v, f) => W.createProgram(v, f).map(_.leftMap(InitializationError.program)))
      .flatSequence

    images <- options.textures.traverse(D.createImageElement)

    textures <- images.traverse(W.createTextureInfo)

  } yield program.map(p => RenderContext(p, textures, projection))

  def renderLoop(c: RenderContext, seed: RenderOutput[WebGLTexture], f: (Camera, Set[Key]) => Camera): F[Unit] = {

    //TODO use ref here
    val keySet = mutable.Set.empty[Key]


    val loop = D.renderLoop(seed, { (previous: RenderOutput[WebGLTexture], time) =>
      val camera = f(previous.c, keySet)

      val view = Matrix4.setLookAtM(camera.camPos.x, camera.camPos.y, camera.camPos.z,
        camera.lookAt.x, camera.lookAt.y, camera.lookAt.z, 0, 0, 1)

      val projectionView = c.projection * view

      def updated = RenderOutput(camera, previous.objects.map(o => o.behaviour(o, keySet)))

      def draw(r: RenderObject[WebGLTexture]): F[Unit] = {
        val translate = Matrix4.forTranslation(Vector4(r.pos.x, r.pos.y, 0, 0))
        val rotation = Matrix4.setRotationRad(r.rotation, 0, 0, 1)

        val rotationX =
          if (r.modeSeven) Matrix4.forRotation(Quaternion(-0.7f, 0, 0, 0.7f))
          else Matrix4.identity

        val scale = Matrix4.forScale(Vector4(r.width, r.height, 1, 0))

        val model = translate * rotation * rotationX * scale

        val matrix = projectionView * model

        W.drawImage(c.program, r.tex, matrix)
      }

      for {
        _ <- W.clearScreen(0, 0, 0, 1)
        _ <- updated.objects.traverse_(draw)
      } yield updated
    })

    D.onKeyDown(e => Monad[F].pure(keySet add Key(e.keyCode))) *>
      D.onKeyUp(e => Monad[F].pure(keySet remove Key(e.keyCode))) *>
      loop
  }
}