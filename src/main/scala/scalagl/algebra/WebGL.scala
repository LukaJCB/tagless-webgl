package scalagl.algebra

import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.WebGLRenderingContext._
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array

trait WebGL[F[_]] {
  def clearColor(red: Double, green: Double, blue: Double, alpha: Double): F[Unit]
  def clear(flag: Int): F[Unit]

  def createShader(shader: ShaderType): F[WebGLShader]
  def shaderSource(shader: WebGLShader, src: String): F[Unit]
  def compileShader(shader: WebGLShader): F[Unit]
  def getShaderParameter(shader: WebGLShader): F[Boolean]
  def getShaderInfoLog(shader: WebGLShader): F[String]

  def createProgram(): F[WebGLProgram]
  def attachShader(program: WebGLProgram, shader: WebGLShader): F[Unit]
  def linkProgram(program: WebGLProgram): F[Unit]
  def getProgramParameter(program: WebGLProgram): F[Boolean]
  def getProgramInfoLog(program: WebGLProgram): F[String]
  def useProgram(program: WebGLProgram): F[Unit]

  def createTexture(): F[WebGLTexture]
  def bindTexture(tex: TextureType, texture: WebGLTexture): F[Unit]
  def texParameteri(tex: TextureType, param: TextureParam, value: TextureParamValue): F[Unit]
  def texImage2D(tex: TextureType, level: Int, internalFormat: PixelFormat, format: PixelFormat, pixelType: PixelType, pixels: HTMLImageElement): F[Unit]
  def createFullSizeCanvas(): F[Canvas]

  def createBuffer(): F[WebGLBuffer]
  def getAttribLocation(program: WebGLProgram, name: String): F[WebGLAttribLocation]
  def getUniformLocation(program: WebGLProgram, name: String): F[WebGLUniformLocation]
  def bindBuffer(target: Int, buffer: WebGLBuffer): F[Unit]
  def bufferData(target: Int, data: Float32Array, usage: Int): F[Unit]
  def enableVertexAttribArray(index: WebGLAttribLocation): F[Unit]
  def vertexAttribPointer(indx: WebGLAttribLocation, size: Int, `type`: Int, normalized: Boolean, stride: Int, offset: Int): F[Unit]
  def uniformMatrix4fv(location: WebGLUniformLocation, transpose: Boolean, value: js.Array[Double]): F[Unit]
  def uniform1i(location: WebGLUniformLocation, x: Int): F[Unit]
  def drawArrays(mode: DrawMode, first: Int, count: Int): F[Unit]
}















sealed trait TextureType
case object Texture2D extends TextureType
case object TextureCubeMap extends TextureType
object TextureType {
  val convert: TextureType => Int = {
    case Texture2D => TEXTURE_2D
    case TextureCubeMap => TEXTURE_CUBE_MAP
  }
}


sealed trait TextureParam
case object TextureWrapS extends TextureParam
case object TextureWrapT extends TextureParam
case object TextureMinFilter extends TextureParam
object TextureParam {
  val convert: TextureParam => Int = {
    case TextureWrapS => TEXTURE_WRAP_S
    case TextureWrapT => TEXTURE_WRAP_T
    case TextureMinFilter => TEXTURE_MIN_FILTER
  }
}

sealed trait TextureParamValue
case object ClampToEdge extends TextureParamValue
case object Linear extends TextureParamValue
object TextureParamValue {
  val convert: TextureParamValue => Int = {
    case ClampToEdge => CLAMP_TO_EDGE
    case Linear => LINEAR
  }
}

sealed trait PixelFormat
case object DepthComponent extends PixelFormat
case object Alpha extends PixelFormat
case object Rgb extends PixelFormat
case object Rgba extends PixelFormat
case object Luminance extends PixelFormat
case object LuminanceAlpha extends PixelFormat
object PixelFormat {
  val convert: PixelFormat => Int = {
    case DepthComponent => DEPTH_COMPONENT
    case Alpha => ALPHA
    case Rgb => RGB
    case Rgba => RGBA
    case Luminance => LUMINANCE
    case LuminanceAlpha => LUMINANCE_ALPHA
  }
}

sealed trait ShaderType
case object Fragment extends ShaderType
case object Vertex extends ShaderType


sealed trait DrawMode
case object Points extends DrawMode
case object Lines extends DrawMode
case object LineLoop extends DrawMode
case object LineStrip extends DrawMode
case object Triangles extends DrawMode
case object TriangleStrip extends DrawMode
case object TriangleFan extends DrawMode
object DrawMode {
  val convert: DrawMode => Int = {
    case Points => POINTS
    case Lines => LINES
    case LineLoop => LINE_LOOP
    case LineStrip => LINE_STRIP
    case Triangles => TRIANGLES
    case TriangleStrip => TRIANGLE_STRIP
    case TriangleFan => TRIANGLE_FAN
  }
}

sealed trait PixelType
case object ByteType extends PixelType
case object UnsignedByteType extends PixelType
case object ShortType extends PixelType
case object UnsignedShortType extends PixelType
case object IntType extends PixelType
case object UnsignedIntType extends PixelType
case object FloatType extends PixelType
object PixelType {
  val convert: PixelType => Int = {
    case ByteType => BYTE
    case UnsignedByteType => UNSIGNED_BYTE
    case ShortType => SHORT
    case UnsignedShortType => UNSIGNED_SHORT
    case IntType => INT
    case UnsignedIntType => UNSIGNED_INT
    case FloatType => FLOAT
  }
}

case class WebGLAttribLocation(value: Int) extends AnyVal
