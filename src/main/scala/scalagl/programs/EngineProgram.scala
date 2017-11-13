package scalagl.programs

import cats.Monad
import cats.implicits._
import org.scalajs.dom.ext.{Color, KeyCode, KeyValue}
import org.scalajs.dom.raw.WebGLTexture

import scala.collection.Set
import scala.collection.immutable.SortedMap
import scalagl.algebra._
import scalagl.interpreters.{InitializationOptions, RenderContext}
import scalagl.math.{Matrix4, Vector4}

object EngineProgram {

  val rotationSpeed = 0.06f
  val speed = 0.07f
  val offsetY = 0.85f
  val offsetZ = 0.4f

  def initial[Tex](textures: SortedMap[String, Tex]): RenderOutput[Tex] = {

    val eye = Matrix4.rotateAround(0, 0, 0, offsetY, offsetZ)
    val camera = Camera(eye, Vector4(0, 0, 0, 0))

    val track = RenderObject(Pos2D(10, -9), 30, 30, 0, false, textures("track"), (old: RenderObject[Tex], keys) => old)

    val car = RenderObject(Pos2D(0, 0), -0.42f, 0.5f, 0, true, textures("car"), { (old: RenderObject[Tex], keys) =>
      if (keys.contains(Key(KeyCode.Up))) {
        val newObj =
          if (keys.contains(Key(KeyCode.Left))) old.copy(rotation = old.rotation + rotationSpeed)
          else if (keys.contains(Key(KeyCode.Right))) old.copy(rotation = old.rotation - rotationSpeed)
          else old

        val dx = speed * Math.sin(newObj.rotation).toFloat
        val dy = speed * -Math.cos(newObj.rotation).toFloat

        newObj.copy(pos = newObj.pos.copy(x = newObj.pos.x + dx, y = newObj.pos.y + dy))

      } else if (keys.contains(Key(KeyCode.Down))) {
        old.copy(pos = old.pos.copy(y = old.pos.y - speed))
      } else if (keys.contains(Key(KeyCode.Left)))
        old.copy(rotation = old.rotation + rotationSpeed)
      else if (keys.contains(Key(KeyCode.Right)))
        old.copy(rotation = old.rotation - rotationSpeed)
      else old
    })

    RenderOutput(camera, List(track, car))
  }

  def updateCam(old: Camera, keys: Set[Key]): Camera = {
    if (keys.contains(Key(KeyCode.Up))) {

      val newCam =
        if (keys.contains(Key(KeyCode.Left))) {
          val eye = Matrix4.rotateAround(old.lookAt.x, old.lookAt.y, old.lookAt.w + rotationSpeed, offsetY, offsetZ)

          Camera(eye, Vector4(old.lookAt.x, old.lookAt.y, 0, old.lookAt.w + rotationSpeed))
        } else if (keys.contains(Key(KeyCode.Right))) {
          val eye = Matrix4.rotateAround(old.lookAt.x, old.lookAt.y, old.lookAt.w - rotationSpeed, offsetY, offsetZ)

          Camera(eye, Vector4(old.lookAt.x, old.lookAt.y, 0, old.lookAt.w - rotationSpeed))
        } else
          old

      val dx = speed * Math.sin(newCam.lookAt.w).toFloat
      val dy = speed * -Math.cos(newCam.lookAt.w).toFloat

      newCam.copy(lookAt = newCam.lookAt.copy(x = newCam.lookAt.x + dx, y = newCam.lookAt.y + dy),
        camPos = newCam.camPos.copy(x = newCam.camPos.x + dx, y = newCam.camPos.y + dy))

    } else if (keys.contains(Key(KeyCode.Down))) {
      Camera(old.camPos.copy(y = old.camPos.y - speed), old.lookAt.copy(y = old.lookAt.y - speed))
    } else if (keys.contains(Key(KeyCode.Left))) {
      val eye =
        Matrix4.rotateAround(old.lookAt.x, old.lookAt.y, old.lookAt.w + rotationSpeed, offsetY, offsetZ)

      Camera(eye, Vector4(old.lookAt.x, old.lookAt.y, 0, old.lookAt.w + rotationSpeed))
    } else if (keys.contains(Key(KeyCode.Right))) {
      val eye =
        Matrix4.rotateAround(old.lookAt.x, old.lookAt.y, old.lookAt.w - rotationSpeed, offsetY, offsetZ)

      Camera(eye, Vector4(old.lookAt.x, old.lookAt.y, 0, old.lookAt.w - rotationSpeed))
    } else
      old
  }

  def simpleRace[F[_]: Monad, E]
    (R: RenderEngine[F, InitializationOptions, E, RenderContext, WebGLTexture]): F[Unit] =
    for {
      context <- R.initialize(InitializationOptions(new Color(0, 0, 0), SortedMap("car" -> "car.png", "track" -> "racetrack.png")))
      _ <- context.traverse_(c => R.renderLoop(c, initial(c.textures), updateCam))
    } yield ()
}
