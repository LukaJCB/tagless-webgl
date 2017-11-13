package scalagl.algebra

import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{HTMLImageElement, WebGLProgram, WebGLShader, WebGLTexture}

import scalagl.math.Matrix4


trait DrawImage[F[_]] {
  def createFullSizeCanvas(): F[Canvas]
  def clearScreen(red: Double, green: Double, blue: Double, alpha: Double): F[Unit]
  def compileVertexShader(src: String): F[Either[String, VertexShader]]
  def compileFragmentShader(src: String): F[Either[String, FragmentShader]]
  def createProgram(v: VertexShader, f: FragmentShader): F[Either[String, WebGLProgram]]
  def createTextureInfo(img: HTMLImageElement): F[WebGLTexture]
  def drawImage(program: WebGLProgram, tex: WebGLTexture, matrix: Matrix4): F[Unit]
}




case class VertexShader(value: WebGLShader) extends AnyVal
case class FragmentShader(value: WebGLShader) extends AnyVal
