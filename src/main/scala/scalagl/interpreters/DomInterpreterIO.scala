package scalagl.interpreters

import cats.implicits._
import cats.effect._
import monix.eval._
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{Event, HTMLImageElement}
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom.KeyboardEvent

import scalagl.algebra.Dom

object DomInterpreterIO extends Dom[IO] {
  def appendToBody(c: Canvas): IO[Unit] = IO(dom.document.body.appendChild(c))

  def createImageElement(url: String): IO[HTMLImageElement] = IO.async { cb =>
    val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    element.onload = (_: dom.Event) => cb(Right(element))
    element.src = url

    element.addEventListener("error", (e: Event) => cb(Left(new Throwable(e.toString))))
  }

  def renderLoop[A](seed: A, cb: (A, Double) => IO[A]): IO[Unit] = {

    def inner(a: A)(time: Double): Unit = {
      val newA = cb(a, time).unsafeRunSync()
      dom.window.requestAnimationFrame(inner(newA))
    }
    IO(dom.window.requestAnimationFrame(inner(seed)))
  }


  def onKeyDown(cb: KeyboardEvent => IO[Unit]) =
    IO(dom.document.body.onkeydown = e => cb(e).unsafeRunSync())

  def onKeyUp(cb: KeyboardEvent => IO[Unit]) =
    IO(dom.document.body.onkeyup = e  => cb(e).unsafeRunSync())
}
