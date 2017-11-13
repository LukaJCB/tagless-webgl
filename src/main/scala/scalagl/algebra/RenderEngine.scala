package scalagl.algebra

import scala.collection.Set
import scalagl.math.Vector4

trait RenderEngine[F[_], Opt, E, Context, Tex] {
  def initialize(options: Opt): F[Either[E, Context]]
  def renderLoop(c: Context, seed: RenderOutput[Tex], f: (Camera, Set[Key]) => Camera): F[Unit]
}








case class Pos2D(x: Float, y: Float)

case class Camera(camPos: Vector4, lookAt: Vector4)

case class RenderObject[Tex](pos: Pos2D,
                             height: Float,
                             width: Float,
                             rotation: Float,
                             modeSeven: Boolean,
                             tex: Tex,
                             behaviour: (RenderObject[Tex], Set[Key]) => RenderObject[Tex])

case class RenderOutput[Tex](c: Camera, objects: List[RenderObject[Tex]])

case class Key(value: Int) extends AnyVal

case class RenderInput(time: Double, keys: Set[Key])
