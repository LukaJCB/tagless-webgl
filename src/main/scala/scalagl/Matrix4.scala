package scalagl

import java.nio.FloatBuffer

import cats.instances.float

import scala.scalajs.js

object Matrix4 {

  def fromFloat(buffer: js.Array[Float]): Matrix4 = {
    Matrix4(
      buffer(0), buffer(1), buffer(2), buffer(3),
      buffer(4), buffer(5), buffer(6), buffer(7),
      buffer(8), buffer(9), buffer(10), buffer(11),
      buffer(12), buffer(13), buffer(14), buffer(15))
  }

  lazy val identity = Matrix4(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1)

  /**
    * Build a perspective transformation matrix.
    * @param fovRad Field of view, in radians.
    * @param aspect Width divided by height
    */
  def forPerspective(fovRad: Float, aspect: Float, near: Float, far: Float): Matrix4 = {
    val fov = 1 / scala.math.tan(fovRad / 2f).toFloat
    Matrix4(
      fov / aspect, 0, 0, 0,
      0, fov, 0, 0,
      0, 0, (far + near) / (near - far), -1,
      0, 0, (2 * far * near) / (near - far), 0)
  }

  /**
    * Build an orthographic transformation matrix
    */
  def forOrtho(left: Float, right: Float, bottom: Float, top: Float, near: Float, far: Float): Matrix4 = {
    val w = right - left
    val h = top - bottom
    val p = far - near

    val x = (right + left) / w
    val y = (top + bottom) / h
    val z = (far + near) / p

    Matrix4(
      2/w, 0,   0,    0,
      0,   2/h, 0,    0,
      0,   0,   -2/p, 0,
      -x,  -y,  -z,   1)
  }

  def forRotation(q: Quaternion): Matrix4 = {
    val Quaternion(x, y, z, w) = q
    val x2 = x + x
    val y2 = y + y
    val z2 = z + z

    val xx = x * x2
    val xy = x * y2
    val xz = x * z2

    val yy = y * y2
    val yz = y * z2
    val zz = z * z2

    val wx = w * x2
    val wy = w * y2
    val wz = w * z2

    Matrix4(
      1 - ( yy + zz ), xy + wz, xz - wy, 0,
      xy - wz, 1 - ( xx + zz ), yz + wx, 0,
      xz + wy, yz - wx, 1 - ( xx + yy ), 0,
      0, 0, 0, 1)
  }

  def forTranslation(translation: Vector4): Matrix4 = {
    Matrix4(
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      translation.x, translation.y, translation.z, 1)
  }

  def setRotateAround(x: Float, y: Float, rotation: Float) = {

    var camPos = Vector4(0, 0, 0, 1)
    var rotationMatrix = forRotation(Quaternion(rotation, 0, 0, rotation)) * forTranslation(Vector4(0, 0.8f, 0, 0))
    //multiply with the matrix

    val eyePos = rotationMatrix * camPos

    //translate to position of lookat-target
    eyePos.copy(x = eyePos.x + x, y = eyePos.y + y, z = 2 - 1.5f)


    setLookAtM(
      eyePos(0), eyePos(1), eyePos(2),
      x, y, 0,
      0, 0, 1)
  }

  def forScale(scale: Vector4): Matrix4 = {
    Matrix4(
      scale.x, 0, 0, 0,
      0, scale.y, 0, 0,
      0, 0, scale.z, 0,
      0, 0, 0, 1)
  }

  /**
    * Translates, rotates and then scales.
    */
  def forTranslationRotationScale(translation: Vector4, rotation: Quaternion, scale: Vector4): Matrix4 = {
    forTranslation(translation) * forRotation(rotation) * forScale(scale)
  }

  def length(x: Float, y: Float, z: Float): Float =
    Math.sqrt(x * x + y * y + z * z).toFloat

  def setLookAtM(
    eyeX: Float, eyeY: Float, eyeZ: Float,
    centerX: Float, centerY: Float, centerZ: Float, upX: Float, upY: Float,
    upZ: Float) = {


    var fx = centerX - eyeX
    var fy = centerY - eyeY
    var fz = centerZ - eyeZ


    var rlf = 1.0f / length(fx, fy, fz)
    fx *= rlf
    fy *= rlf
    fz *= rlf


    var sx = fy * upZ - fz * upY
    var sy = fz * upX - fx * upZ
    var sz = fx * upY - fy * upX


    var rls = 1.0f / length(sx, sy, sz)
    sx *= rls
    sy *= rls
    sz *= rls

    var ux = sy * fz - sz * fy
    var uy = sz * fx - sx * fz
    var uz = sx * fy - sy * fx

    val rm: js.Array[Float] = js.Array()

    rm(0) = sx
    rm(1) = ux
    rm(2) = -fx
    rm(3) = 0.0f

    rm(4) = sy
    rm(5) = uy
    rm(6) = -fy
    rm(7) = 0.0f

    rm(8) = sz
    rm(9) = uz
    rm(10) = -fz
    rm(11) = 0.0f

    rm(12) = 0.0f
    rm(13) = 0.0f
    rm(14) = 0.0f
    rm(15) = 1.0f

    fromFloat(rm).translate(Vector4(-eyeX, -eyeY, -eyeZ, 1))
  }
}

/**
  * Constructor takes values in column major order.  That is,  the first 4 values are the rows the first column.
  */
// Originally the matrix cells were put into fields in anticipation of making this all stack allocated (using AnyVal
// or similar).  That's not currently possible,  so this could potentially be transitioned to a simple Float array.
case class Matrix4(
                      c0r0: Float, c0r1: Float, c0r2: Float, c0r3: Float,
                      c1r0: Float, c1r1: Float, c1r2: Float, c1r3: Float,
                      c2r0: Float, c2r1: Float, c2r2: Float, c2r3: Float,
                      c3r0: Float, c3r1: Float, c3r2: Float, c3r3: Float) {

  def unary_- : Matrix4 = negate
  def negate: Matrix4 = {
    Matrix4(
      -c0r0, -c0r1, -c0r2, -c0r3,
      -c1r0, -c1r1, -c1r2, -c1r3,
      -c2r0, -c2r1, -c2r2, -c2r3,
      -c3r0, -c3r1, -c3r2, -c3r3)
  }

  def translate(translation: Vector4): Matrix4 = this * Matrix4.forTranslation(translation)

  def scale(scale: Vector4): Matrix4 = this * Matrix4.forScale(scale)

  def *(f: Float): Matrix4 = multiply(f)
  def multiply(f: Float): Matrix4 = {
    Matrix4(
      c0r0*f, c0r1*f, c0r2*f, c0r3*f,
      c1r0*f, c1r1*f, c1r2*f, c1r3*f,
      c2r0*f, c2r1*f, c2r2*f, c2r3*f,
      c3r0*f, c3r1*f, c3r2*f, c3r3*f)
  }

  def *(m: Matrix4): Matrix4 = multiply(m)
  def multiply(m: Matrix4): Matrix4 = {
    Matrix4(
      c0r0 * m.c0r0 + c1r0 * m.c0r1 + c2r0 * m.c0r2 + c3r0 * m.c0r3,  c0r1 * m.c0r0 + c1r1 * m.c0r1 + c2r1 * m.c0r2 + c3r1 * m.c0r3,  c0r2 * m.c0r0 + c1r2 * m.c0r1 + c2r2 * m.c0r2 + c3r2 * m.c0r3,  c0r3 * m.c0r0 + c1r3 * m.c0r1 + c2r3 * m.c0r2 + c3r3 * m.c0r3,
      c0r0 * m.c1r0 + c1r0 * m.c1r1 + c2r0 * m.c1r2 + c3r0 * m.c1r3,  c0r1 * m.c1r0 + c1r1 * m.c1r1 + c2r1 * m.c1r2 + c3r1 * m.c1r3,  c0r2 * m.c1r0 + c1r2 * m.c1r1 + c2r2 * m.c1r2 + c3r2 * m.c1r3,  c0r3 * m.c1r0 + c1r3 * m.c1r1 + c2r3 * m.c1r2 + c3r3 * m.c1r3,
      c0r0 * m.c2r0 + c1r0 * m.c2r1 + c2r0 * m.c2r2 + c3r0 * m.c2r3,  c0r1 * m.c2r0 + c1r1 * m.c2r1 + c2r1 * m.c2r2 + c3r1 * m.c2r3,  c0r2 * m.c2r0 + c1r2 * m.c2r1 + c2r2 * m.c2r2 + c3r2 * m.c2r3,  c0r3 * m.c2r0 + c1r3 * m.c2r1 + c2r3 * m.c2r2 + c3r3 * m.c2r3,
      c0r0 * m.c3r0 + c1r0 * m.c3r1 + c2r0 * m.c3r2 + c3r0 * m.c3r3,  c0r1 * m.c3r0 + c1r1 * m.c3r1 + c2r1 * m.c3r2 + c3r1 * m.c3r3,  c0r2 * m.c3r0 + c1r2 * m.c3r1 + c2r2 * m.c3r2 + c3r2 * m.c3r3,  c0r3 * m.c3r0 + c1r3 * m.c3r1 + c2r3 * m.c3r2 + c3r3 * m.c3r3)
  }

  /**
    * Computes the matrix product of this matrix with a column vector.
    */
  def *(vec: Vector4): Vector4 = multiply(vec)

  /**
    * Computes the matrix product of this matrix with a column vector.
    */
  def multiply(vec: Vector4): Vector4 = {
    Vector4(
      c0r0 * vec.x + c1r0 * vec.y + c2r0 * vec.z + c3r0 * vec.w,
      c0r1 * vec.x + c1r1 * vec.y + c2r1 * vec.z + c3r1 * vec.w,
      c0r2 * vec.x + c1r2 * vec.y + c2r2 * vec.z + c3r2 * vec.w,
      c0r3 * vec.x + c1r3 * vec.y + c2r3 * vec.z + c3r3 * vec.w)
  }

  def transpose: Matrix4 = {
    Matrix4(
      c0r0, c1r0, c2r0, c3r0,
      c0r1, c1r1, c2r1, c3r1,
      c0r2, c1r2, c2r2, c3r2,
      c0r3, c1r3, c2r3, c3r3
    )
  }

  def determinant: Float = {
    val a = c1r1 * c2r2 * c3r3 + c2r1 * c3r2 * c1r3 + c3r1 * c1r2 * c2r3 - c1r3 * c2r2 * c3r1 - c2r3 * c3r2 * c1r1 - c3r3 * c1r2 * c2r1
    val b = c0r1 * c2r2 * c3r3 + c2r1 * c3r2 * c0r3 + c3r1 * c0r2 * c2r3 - c0r3 * c2r2 * c3r1 - c2r3 * c3r2 * c0r1 - c3r3 * c0r2 * c2r1
    val c = c0r1 * c1r2 * c3r3 + c1r1 * c3r2 * c0r3 + c3r1 * c0r2 * c1r3 - c0r3 * c1r2 * c3r1 - c1r3 * c3r2 * c0r1 - c3r3 * c0r2 * c1r1
    val d = c0r1 * c1r2 * c2r3 + c1r1 * c2r2 * c0r3 + c2r1 * c0r2 * c1r3 - c0r3 * c1r2 * c2r1 - c1r3 * c2r2 * c0r1 - c2r3 * c0r2 * c1r1

    c0r0 * a - c1r0 * b + c2r0 * c - c3r0 * d
  }

  def inverse: Matrix4 = {
    // Compute inverse of a Matrix using minors, cofactors and adjugate
    // TODO: fix this.  the below approach is crazy
    val matrixOfCofactors = Matrix4(
      +(c1r1 * c2r2 * c3r3 + c2r1 * c3r2 * c1r3 + c3r1 * c1r2 * c2r3 - c1r3 * c2r2 * c3r1 - c2r3 * c3r2 * c1r1 - c3r3 * c1r2 * c2r1),
      -(c1r0 * c2r2 * c3r3 + c2r0 * c3r2 * c1r3 + c3r0 * c1r2 * c2r3 - c1r3 * c2r2 * c3r0 - c2r3 * c3r2 * c1r0 - c3r3 * c1r2 * c2r0),
      +(c1r0 * c2r1 * c3r3 + c2r0 * c3r1 * c1r3 + c3r0 * c1r1 * c2r3 - c1r3 * c2r1 * c3r0 - c2r3 * c3r1 * c1r0 - c3r3 * c1r1 * c2r0),
      -(c1r0 * c2r1 * c3r2 + c2r0 * c3r1 * c1r2 + c3r0 * c1r1 * c2r2 - c1r2 * c2r1 * c3r0 - c2r2 * c3r1 * c1r0 - c3r2 * c1r1 * c2r0),

      -(c0r1 * c2r2 * c3r3 + c2r1 * c3r2 * c0r3 + c3r1 * c0r2 * c2r3 - c0r3 * c2r2 * c3r1 - c2r3 * c3r2 * c0r1 - c3r3 * c0r2 * c2r1),
      +(c0r0 * c2r2 * c3r3 + c2r0 * c3r2 * c0r3 + c3r0 * c0r2 * c2r3 - c0r3 * c2r2 * c3r0 - c2r3 * c3r2 * c0r0 - c3r3 * c0r2 * c2r0),
      -(c0r0 * c2r1 * c3r3 + c2r0 * c3r1 * c0r3 + c3r0 * c0r1 * c2r3 - c0r3 * c2r1 * c3r0 - c2r3 * c3r1 * c0r0 - c3r3 * c0r1 * c2r0),
      +(c0r0 * c2r1 * c3r2 + c2r0 * c3r1 * c0r2 + c3r0 * c0r1 * c2r2 - c0r2 * c2r1 * c3r0 - c2r2 * c3r1 * c0r0 - c3r2 * c0r1 * c2r0),

      +(c0r1 * c1r2 * c3r3 + c1r1 * c3r2 * c0r3 + c3r1 * c0r2 * c1r3 - c0r3 * c1r2 * c3r1 - c1r3 * c3r2 * c0r1 - c3r3 * c0r2 * c1r1),
      -(c0r0 * c1r2 * c3r3 + c1r0 * c3r2 * c0r3 + c3r0 * c0r2 * c1r3 - c0r3 * c1r2 * c3r0 - c1r3 * c3r2 * c0r0 - c3r3 * c0r2 * c1r0),
      +(c0r0 * c1r1 * c3r3 + c1r0 * c3r1 * c0r3 + c3r0 * c0r1 * c1r3 - c0r3 * c1r1 * c3r0 - c1r3 * c3r1 * c0r0 - c3r3 * c0r1 * c1r0),
      -(c0r0 * c1r1 * c3r2 + c1r0 * c3r1 * c0r2 + c3r0 * c0r1 * c1r2 - c0r2 * c1r1 * c3r0 - c1r2 * c3r1 * c0r0 - c3r2 * c0r1 * c1r0),

      -(c0r1 * c1r2 * c2r3 + c1r1 * c2r2 * c0r3 + c2r1 * c0r2 * c1r3 - c0r3 * c1r2 * c2r1 - c1r3 * c2r2 * c0r1 - c2r3 * c0r2 * c1r1),
      +(c0r0 * c1r2 * c2r3 + c1r0 * c2r2 * c0r3 + c2r0 * c0r2 * c1r3 - c0r3 * c1r2 * c2r0 - c1r3 * c2r2 * c0r0 - c2r3 * c0r2 * c1r0),
      -(c0r0 * c1r1 * c2r3 + c1r0 * c2r1 * c0r3 + c2r0 * c0r1 * c1r3 - c0r3 * c1r1 * c2r0 - c1r3 * c2r1 * c0r0 - c2r3 * c0r1 * c1r0),
      +(c0r0 * c1r1 * c2r2 + c1r0 * c2r1 * c0r2 + c2r0 * c0r1 * c1r2 - c0r2 * c1r1 * c2r0 - c1r2 * c2r1 * c0r0 - c2r2 * c0r1 * c1r0)
    )
    // TODO: if determinant is 0, throw a meaningful exception
    matrixOfCofactors.transpose.multiply(1 / determinant)
  }

  def normalMatrix: Matrix4 = inverse.transpose

  def column(index: Int): Vector4 = {
    index match {
      case 0 => Vector4(c0r0, c0r1, c0r2, c0r3)
      case 1 => Vector4(c1r0, c1r1, c1r2, c1r3)
      case 2 => Vector4(c2r0, c2r1, c2r2, c2r3)
      case 3 => Vector4(c3r0, c3r1, c3r2, c3r3)
      case _ => throw new IllegalArgumentException("index is out of range.  Must be between 0 and 4")
    }
  }

  def row(index: Int): Vector4 = {
    index match {
      case 0 => Vector4(c0r0, c1r0, c2r0, c3r0)
      case 1 => Vector4(c0r1, c1r1, c2r1, c3r1)
      case 2 => Vector4(c0r2, c1r2, c2r2, c3r2)
      case 3 => Vector4(c0r3, c1r3, c2r3, c3r3)
      case _ => throw new IllegalArgumentException("index is out of range.  Must be between 0 and 4")
    }
  }

  def toArray: js.Array[Double] =
    js.Array(c0r0, c0r1, c0r2, c0r3, c1r0, c1r1, c1r2, c1r3, c2r0, c2r1, c2r2, c2r3, c3r0, c3r1, c3r2, c3r3)

  override def toString: String = {
    s"""[[$c0r0, $c0r1, $c0r2, $c0r3]
       | [$c1r0, $c1r1, $c1r2, $c1r3]
       | [$c2r0, $c2r1, $c2r2, $c2r3]
       | [$c3r0, $c3r1, $c3r2, $c3r3]]
     """.stripMargin
  }
}
