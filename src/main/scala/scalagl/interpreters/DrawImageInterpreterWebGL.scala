package scalagl.interpreters

import cats._
import cats.implicits._
import org.scalajs.dom.raw.WebGLRenderingContext._
import org.scalajs.dom.raw.{HTMLImageElement, WebGLProgram, WebGLShader, WebGLTexture}

import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array
import scalagl.algebra._
import scalagl.math.Matrix4

class DrawImageInterpreterWebGL[F[_]: Monad](F: WebGL[F]) extends DrawImage[F] {

  def createFullSizeCanvas() = F.createFullSizeCanvas()

  def clearScreen(red: Double, green: Double, blue: Double, alpha: Double): F[Unit] =
    F.clearColor(red, green, blue, alpha) *> F.clear(COLOR_BUFFER_BIT)

  private def compileShader(src: String, shaderType: ShaderType): F[Either[String, WebGLShader]] = for {
    shader <- F.createShader(shaderType)
    _ <- F.shaderSource(shader, src)
    _ <- F.compileShader(shader)
    success <- F.getShaderParameter(shader)
    info <- F.getShaderInfoLog(shader)
  } yield if (success) Right(shader) else Left(s"Could not compile shader $shaderType: $info")

  def compileVertexShader(src: String): F[Either[String, VertexShader]] =
    compileShader(src, Vertex).map(_.map(VertexShader))

  def compileFragmentShader(src: String): F[Either[String, FragmentShader]] =
    compileShader(src, Fragment).map(_.map(FragmentShader))

  def createProgram(v: VertexShader, f: FragmentShader): F[Either[String, WebGLProgram]] = for {
    program <- F.createProgram()
    _ <- F.attachShader(program, v.value)
    _ <- F.attachShader(program, f.value)
    _ <- F.linkProgram(program)
    success <- F.getProgramParameter(program)
    info <- F.getProgramInfoLog(program)
  } yield if (success) Right(program) else Left("Could not link program: $info")


  def createTextureInfo(img: HTMLImageElement): F[WebGLTexture] = for {
    tex <- F.createTexture()
    _ <- F.bindTexture(Texture2D, tex)

    _ <- F.texParameteri(Texture2D, TextureWrapS, ClampToEdge)
    _ <- F.texParameteri(Texture2D, TextureWrapT, ClampToEdge)
    _ <- F.texParameteri(Texture2D, TextureMinFilter, Linear)

    _ <- F.bindTexture(Texture2D, tex)
    _ <- F.texImage2D(Texture2D, 0, Rgba, Rgba, UnsignedByteType, img)
  } yield tex

  def drawImage(program: WebGLProgram, tex: WebGLTexture, matrix: Matrix4): F[Unit] = {
    val positions = js.Array(
      0.5f, 0.5f,
      0.5f, -0.5f,
      -0.5f, 0.5f,
      -0.5f, -0.5f
    )

    val texCoords = js.Array(
      0.0f, 0.0f,
      0.0f, 1.0f,
      1.0f, 0.0f,
      1.0f, 1.0f
    )

    for {
      _ <- F.bindTexture(Texture2D, tex)
      _ <- F.useProgram(program)

      positionBuffer <- F.createBuffer()
      texBuffer <- F.createBuffer()

      positionLocation <- F.getAttribLocation(program, "a_position")
      texcoordLocation <- F.getAttribLocation(program, "a_texcoord")

      _ <- F.bindBuffer(ARRAY_BUFFER, positionBuffer)
      _ <- F.bufferData(ARRAY_BUFFER, new Float32Array(positions), STATIC_DRAW)
      _ <- F.enableVertexAttribArray(positionLocation)
      _ <- F.vertexAttribPointer(positionLocation, 2, FLOAT, false, 0, 0)

      _ <- F.bindBuffer(ARRAY_BUFFER, texBuffer)
      _ <- F.bufferData(ARRAY_BUFFER, new Float32Array(texCoords), STATIC_DRAW)
      _ <- F.enableVertexAttribArray(texcoordLocation)
      _ <- F.vertexAttribPointer(texcoordLocation, 2, FLOAT, false, 0, 0)

      matrixLocation <- F.getUniformLocation(program, "u_matrix")
      textureLocation <- F.getUniformLocation(program, "u_texture")

      _ <- F.uniformMatrix4fv(matrixLocation, false, matrix.toArray)
      _ <- F.uniform1i(textureLocation, 0)

      _ <- F.drawArrays(TriangleStrip, 0, 4)

    } yield ()

  }
}
