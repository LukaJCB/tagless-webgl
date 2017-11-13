package scalagl.math

object Util {
  def clamp(value: Float, min: Float, max: Float): Float = {
    scala.math.min(scala.math.max(value, min), max)
  }

  def lerp(from: Float, to: Float, t: Float): Float = {
    from*(1-t) + to*t
  }
}
