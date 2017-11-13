package scalagl.math

case class Quaternion(x: Float, y: Float, z: Float, w: Float) {
  def vector: Vector4 = Vector4(x, y, z, w)

  def *(rhs: Quaternion): Quaternion = multiply(rhs)
  def multiply(rhs: Quaternion): Quaternion = {
    Quaternion(
      this.w * rhs.x + this.x * rhs.w + this.y * rhs.z - this.z * rhs.y,
      this.w * rhs.y + this.y * rhs.w + this.z * rhs.x - this.x * rhs.z,
      this.w * rhs.z + this.z * rhs.w + this.x * rhs.y - this.y * rhs.x,
      this.w * rhs.w - this.x * rhs.x - this.y * rhs.y - this.z * rhs.z)
  }

}

