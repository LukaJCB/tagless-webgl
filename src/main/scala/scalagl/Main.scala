package scalagl

import cats.effect._
import cats.implicits._
import scalagl.programs.{EngineProgram, Program2Point5D}
import scalagl.interpreters._

object Main {

  def main(args: Array[String]): Unit = {
    EngineProgram
      .simpleRace(new RenderEngineInterpreterWebGL(new DrawImageInterpreterWebGL(GLInterpreterIO), DomInterpreterIO))
      .unsafeRunAsync {
        case Left(t) => println(t)
        case _ => ()
      }
  }

}
