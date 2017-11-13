package scalagl.algebra

import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

import scala.scalajs.js


trait Dom[F[_]] {
  def appendToBody(c: Canvas): F[Unit]
  def createImageElement(url: String): F[HTMLImageElement]
  def renderLoop[A](seed: A, cb: (A, Double) => F[A]): F[Unit]
  def onKeyDown(cb: KeyboardEvent => F[Unit]): F[Unit]
  def onKeyUp(cb: KeyboardEvent => F[Unit]): F[Unit]
}
