package scalagl.interpreters

import cats.effect._
import cats.implicits._
import monix.eval._
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw._
import org.scalajs.dom.raw.WebGLRenderingContext._

import scala.scalajs.js.Array
import scala.scalajs.js.typedarray.Float32Array
import scalagl.algebra._

object GLInterpreterIO extends WebGL[IO] {

  //TODO Memoize this in IO
  private lazy val can: Canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
  can.width = dom.window.innerWidth.toInt
  can.height = dom.window.innerHeight.toInt

  private lazy val context = can.getContext("webgl").asInstanceOf[dom.raw.WebGLRenderingContext]

  def createFullSizeCanvas(): IO[Canvas] = {
    IO(can)
  }
  val getContext: IO[WebGLRenderingContext] =
    IO(context)

  private def gl[B](f: WebGLRenderingContext => B): IO[B] =
    getContext >>= (g => IO(f(g)))

  def clearColor(red: Double, green: Double, blue: Double, alpha: Double): IO[Unit] =
    gl(_.clearColor(red, green, blue, alpha))

  def clear(flag: Int): IO[Unit] = gl(_.clear(flag))

  def createShader(shader: ShaderType): IO[WebGLShader] = gl(_.createShader(shader match {
    case Vertex => VERTEX_SHADER
    case Fragment => FRAGMENT_SHADER
  }))

  def shaderSource(shader: WebGLShader, src: String): IO[Unit] = gl(_.shaderSource(shader, src))

  def compileShader(shader: WebGLShader): IO[Unit] = gl(_.compileShader(shader))

  def getShaderParameter(shader: WebGLShader): IO[Boolean] =
    gl(_.getShaderParameter(shader, COMPILE_STATUS).asInstanceOf[Boolean])

  def getShaderInfoLog(shader: WebGLShader): IO[String] = gl(_.getShaderInfoLog(shader))

  def createProgram(): IO[WebGLProgram] = gl(_.createProgram())

  def attachShader(program: WebGLProgram, shader: WebGLShader): IO[Unit] = gl(_.attachShader(program, shader))

  def linkProgram(program: WebGLProgram): IO[Unit] = gl(_.linkProgram(program))

  def getProgramParameter(program: WebGLProgram): IO[Boolean] =
    gl(_.getProgramParameter(program, LINK_STATUS).asInstanceOf[Boolean])

  def getProgramInfoLog(program: WebGLProgram): IO[String] = gl(_.getProgramInfoLog(program))

  def useProgram(program: WebGLProgram): IO[Unit] = gl(_.useProgram(program))

  def createTexture(): IO[WebGLTexture] = gl(_.createTexture())

  def bindTexture(tex: TextureType, texture: WebGLTexture): IO[Unit] =
    gl(_.bindTexture(TextureType.convert(tex), texture))

  def texParameteri(tex: TextureType, param: TextureParam, value: TextureParamValue): IO[Unit] =
    gl(_.texParameteri(TextureType.convert(tex), TextureParam.convert(param), TextureParamValue.convert(value)))

  def texImage2D(tex: TextureType, level: Int, internalFormat: PixelFormat, format: PixelFormat, pixelType: PixelType, pixels: HTMLImageElement): IO[Unit] =
    gl(_.texImage2D(TextureType.convert(tex), level, PixelFormat.convert(internalFormat), PixelFormat.convert(format), PixelType.convert(pixelType), pixels))

  def createBuffer(): IO[WebGLBuffer] = gl(_.createBuffer)

  def getAttribLocation(program: WebGLProgram, name: String): IO[WebGLAttribLocation] =
    gl(_.getAttribLocation(program, name)).map(WebGLAttribLocation)

  def getUniformLocation(program: WebGLProgram, name: String): IO[WebGLUniformLocation] =
    gl(_.getUniformLocation(program, name))

  def bindBuffer(target: Int, buffer: WebGLBuffer): IO[Unit] = gl(_.bindBuffer(target, buffer))

  def bufferData(target: Int, data: Float32Array, usage: Int): IO[Unit] = gl(_.bufferData(target, data, usage))

  def enableVertexAttribArray(index: WebGLAttribLocation): IO[Unit] = gl(_.enableVertexAttribArray(index.value))

  def vertexAttribPointer(indx: WebGLAttribLocation, size: Int, `type`: Int, normalized: Boolean, stride: Int, offset: Int): IO[Unit] =
    gl(_.vertexAttribPointer(indx.value, size, `type`, normalized, stride, offset))

  def uniformMatrix4fv(location: WebGLUniformLocation, transpose: Boolean, value: Array[Double]): IO[Unit] =
    gl(_.uniformMatrix4fv(location, transpose, value))

  def uniform1i(location: WebGLUniformLocation, x: Int): IO[Unit] = gl(_.uniform1i(location, x))

  def drawArrays(mode: DrawMode, first: Int, count: Int): IO[Unit] = {
    gl(_.drawArrays(DrawMode.convert(mode), first, count))
  }
}
