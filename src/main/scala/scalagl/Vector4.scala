package scalagl

import scala.scalajs.js

object Vector4 {
  def fill(value: Float): Vector4 = Vector4(value, value, value, value)


  def fromBuffer(buffer: js.Array[Float]): Vector4 = {
    Vector4(buffer(0), buffer(1), buffer(2), buffer(3))
  }

}

case class Vector4(x: Float,
                    y: Float,
                    z: Float,
                    w: Float) {

  def apply(index: Int): Float = {
    index match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new ArrayIndexOutOfBoundsException(index)
    }
  }

  def magnitude: Float = {
    Math.sqrt(x * x + y * y + z * z + w * w).toFloat
  }

  def normalize: Vector4 = {
    val l = 1f / magnitude
    Vector4(x * l, y * l, z * l, w * l)
  }

  def unary_- : Vector4 = negate
  def negate: Vector4 = Vector4(-x, -y, -z, -w)

  def +(vec: Vector4): Vector4 = add(vec)
  def add(vec: Vector4): Vector4 = add(vec.x, vec.y, vec.z)
  def add(x: Float, y: Float, z: Float): Vector4 = {
    Vector4(this.x + x, this.y + y, this.z + z, this.w + w)
  }

  def -(vec: Vector4): Vector4 = subtract(vec)
  def subtract(vec: Vector4): Vector4 = subtract(vec.x, vec.y, vec.z, vec.w)
  def subtract(x: Float, y: Float, z: Float, w: Float): Vector4 = {
    Vector4(this.x - x, this.y - y, this.z - z, this.w - w)
  }

  def *(f: Float): Vector4 = scale(f)
  def /(f: Float): Vector4 = scale(1/f)

  def scale(f: Float): Vector4 = scale(f, f, f, f)
  def scale(vec: Vector4): Vector4 = scale(vec.x, vec.y, vec.z, vec.w)
  def scale(x: Float, y: Float, z: Float, w: Float): Vector4 = {
    Vector4(this.x * x, this.y * y, this.z * z, this.w * w)
  }

  def dotProduct(vec: Vector4): Float = {
    x * vec.x + y * vec.y + z * vec.z + w * vec.w
  }

  def lerp(vec: Vector4, t: Float): Vector4 = {
    Vector4(
      Util.lerp(x, vec.x, t),
      Util.lerp(y, vec.y, t),
      Util.lerp(z, vec.z, t),
      Util.lerp(w, vec.w, t)
    )
  }

  def distanceTo(vec: Vector4): Float = {
    (this - vec).magnitude
  }

  def clamp(min: Float, max: Float): Vector4 = {
    if (min > max) throw new IllegalArgumentException("min must not be greater than max")
    Vector4(
      Util.clamp(x, min, max),
      Util.clamp(y, min, max),
      Util.clamp(z, min, max),
      Util.clamp(w, min, max)
    )
  }

  def clamp(min: Vector4, max: Vector4): Vector4 = {
    Vector4(
      Util.clamp(x, min.x, max.x),
      Util.clamp(y, min.y, max.y),
      Util.clamp(z, min.z, max.z),
      Util.clamp(w, min.w, max.w)
    )
  }

  override def toString: String = {
    s"($x, $y, $z, $w)"
  }


}
