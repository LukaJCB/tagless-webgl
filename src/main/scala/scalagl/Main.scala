package scalagl

import org.scalajs.dom
import org.scalajs.dom.raw.WebGLRenderingContext._
import org.scalajs.dom.raw._
import org.scalajs.dom.raw.HTMLImageElement
import cats.effect._
import cats.implicits._
import scala.scalajs.js
import scala.scalajs.js.typedarray._

object Main {

  def draw(gl: WebGLRenderingContext): Unit = {
    gl.clear(COLOR_BUFFER_BIT)
  }

  case class RenderObject(tex: WebGLTexture, matrix: Matrix4)

  def renderLoop(gl: WebGLRenderingContext, program: WebGLProgram, objects: List[RenderObject])(time: Double): Unit = {
    gl.clear(COLOR_BUFFER_BIT)
    val translation = Matrix4.forTranslation(Vector4(0, -0.001f * time.toFloat, 0, 0))
    objects.zipWithIndex.foreach { case (o, n) =>
      drawImage(gl, program, o.tex, if (n == 1) o.matrix else translation * o.matrix)
    }
    dom.window.requestAnimationFrame(renderLoop(gl, program, objects))
  }

  def drawTriangles(gl: WebGLRenderingContext): Either[String, Unit] = {
    gl.clearColor(0.0, 0.0, 0.0, 0.0)
    gl.clear(COLOR_BUFFER_BIT)

    val vertText = "attribute vec2 position; void main() {gl_Position = vec4(position, 0, 1);}"
    val vertex = compileShader(gl, vertText, VERTEX_SHADER)

    val fragText = "precision highp float; uniform vec4 color; void main() {gl_FragColor = vec4(0, 1, 1, 1);}"
    val fragment = compileShader(gl, fragText, FRAGMENT_SHADER)

    val program = for {
      v <- vertex
      f <- fragment
      p <- createProgram(gl, v, f)
    } yield p

    program.map { p =>
      val vertices = new Float32Array(js.Array(
        -0.3f, -0.3f, 0.3f, -0.3f, 0.0f, 0.3f, 0.2f, 0.2f,
        0.6f, 0.6f, 0.4f, -0.4f))

      val buffer = gl.createBuffer()
      gl.bindBuffer(ARRAY_BUFFER, buffer)
      gl.bufferData(ARRAY_BUFFER, vertices, STATIC_DRAW)

      gl.useProgram(p)
      val progDyn = p.asInstanceOf[scalajs.js.Dynamic]
      progDyn.color = gl.getUniformLocation(p, "color")
      val temp2 = scalajs.js.Array[Double]()
      temp2.push(0f, 1f, 0.5f, 1.0f)
      gl.uniform4fv(progDyn.color.asInstanceOf[dom.raw.WebGLUniformLocation], temp2)

      progDyn.position = gl.getAttribLocation(p, "position")
      gl.enableVertexAttribArray(progDyn.position.asInstanceOf[Int])
      gl.vertexAttribPointer(progDyn.position.asInstanceOf[Int], 2, FLOAT, false, 0, 0)
      gl.drawArrays(TRIANGLES, 0, vertices.length / 2)
    }
  }


  def main(args: Array[String]): Unit = {
    val can: dom.html.Canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    dom.document.body.appendChild(can)
    can.width = dom.window.innerWidth.toInt
    can.height = dom.window.innerHeight.toInt

    val gl: WebGLRenderingContext = can.getContext("webgl").asInstanceOf[dom.raw.WebGLRenderingContext]

    gl.clearColor(0.0, 0.0, 0.0, 1.0)


    val vertSrc =
      """
        |attribute vec4 a_position;
        |attribute vec2 a_texcoord;
        |
        |uniform mat4 u_matrix;
        |
        |varying vec2 v_texcoord;
        |
        |void main() {
        |   gl_Position = u_matrix * a_position;
        |   v_texcoord = a_texcoord;
        |}
      """.stripMargin

    val fragSrc =
      """
        |precision mediump float;
        |
        |varying vec2 v_texcoord;
        |
        |uniform sampler2D u_texture;
        |
        |void main() {
        |   gl_FragColor = texture2D(u_texture, v_texcoord);
        |   if(gl_FragColor.a < 0.5)
        |     discard;
        |}
      """.stripMargin

    val program = for {
      vertex <- compileShader(gl, vertSrc, VERTEX_SHADER)
      fragment <- compileShader(gl, fragSrc, FRAGMENT_SHADER)
      program <- createProgram(gl, vertex, fragment)
    } yield program

    val trackScale = Matrix4.forScale(Vector4(10, 10, 1, 1))

    val trackTranslation = Matrix4.forTranslation(Vector4(3.8f, 4.5f, 0, 1))

    val carScale = Matrix4.forScale(Vector4(0.5f, 0.42f, 1, 1))

    val carRotation = Matrix4.forRotation(Quaternion(-0.7f, 0, 0, 0.7f))

    program.traverse { p =>
      for {
        track <- loadImage("racetrack.png").map(createTextureInfo(gl))
        car <- loadImage("car.png").map(createTextureInfo(gl))
      } yield dom.window.requestAnimationFrame(
            renderLoop(gl, p, List(
              RenderObject(track, trackTranslation * trackScale),
              RenderObject(car, carRotation * carScale)
            )))

    }.unsafeRunAsync(println)

  }


  def drawImage(gl: WebGLRenderingContext,
                program: WebGLProgram,
                tex: WebGLTexture,
                matrix: Matrix4) {
    gl.bindTexture(TEXTURE_2D, tex)

    // Tell WebGL to use our shader program pair
    gl.useProgram(program)

    val positionBuffer = gl.createBuffer()
    val texcoordBuffer = gl.createBuffer()

    val positionLocation = gl.getAttribLocation(program, "a_position")
    val texcoordLocation = gl.getAttribLocation(program, "a_texcoord")

    // Setup the attributes to pull data from our buffers
    gl.bindBuffer(ARRAY_BUFFER, positionBuffer)

    val positions = js.Array(
      0.5f, 0.5f,
      0.5f, -0.5f,
      -0.5f, 0.5f,
      -0.5f, -0.5f
    )

    gl.bufferData(ARRAY_BUFFER, new Float32Array(positions), STATIC_DRAW)

    gl.enableVertexAttribArray(positionLocation)
    gl.vertexAttribPointer(positionLocation, 2, FLOAT, normalized = false, 0, 0)



    gl.bindBuffer(ARRAY_BUFFER, texcoordBuffer)

    val texCoords = js.Array(
      0.0f, 0.0f,
      0.0f, 1.0f,
      1.0f, 0.0f,
      1.0f, 1.0f
    )

    gl.bufferData(ARRAY_BUFFER, new Float32Array(texCoords), STATIC_DRAW)

    gl.enableVertexAttribArray(texcoordLocation)
    gl.vertexAttribPointer(texcoordLocation, 2, FLOAT, normalized = false, 0, 0)



    // this matrix will convert from pixels to clip space
    val project = Matrix4.forPerspective(90, gl.canvas.width / gl.canvas.height, 0.1f, 100f)

    // this matrix will translate our quad to dstX, dstY
    //val translated = project.translate(Vector4(0, dstX, dstY, 0))

    val view = Matrix4.setLookAtM(0, -0.8f, -0.5f,
      0, 0, 0, 0, 1, 0)

    // this matrix will scale our 1 unit quad
    // from 1 unit to texWidth, texHeight units


    val projectionView = project * view

    val matrixLocation = gl.getUniformLocation(program, "u_matrix")
    val textureLocation = gl.getUniformLocation(program, "u_texture")

    // Set the matrix.
    gl.uniformMatrix4fv(matrixLocation, transpose = false, (projectionView * matrix).toArray)

    // Tell the shader to get the texture from texture unit 0
    gl.uniform1i(textureLocation, 0)

    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(TRIANGLE_STRIP, 0, 4)
  }

  def loadImage(url: String): IO[HTMLImageElement] = {
    IO.async { cb =>
      val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      element.onload = (e: dom.Event) => cb(Right(element))
      element.src = url

      element.addEventListener("error", (e: Event) => cb(Left(new Throwable(e.toString))))
    }
  }

  def createTextureInfo(gl: WebGLRenderingContext)(img: HTMLImageElement): WebGLTexture = {
    val tex = gl.createTexture()
    gl.bindTexture(TEXTURE_2D, tex)

    // let's assume all images are not a power of 2
    gl.texParameteri(TEXTURE_2D, TEXTURE_WRAP_S, CLAMP_TO_EDGE)
    gl.texParameteri(TEXTURE_2D, TEXTURE_WRAP_T, CLAMP_TO_EDGE)
    gl.texParameteri(TEXTURE_2D, TEXTURE_MIN_FILTER, LINEAR)

    gl.bindTexture(TEXTURE_2D, tex)
    gl.texImage2D(TEXTURE_2D, 0, RGBA, RGBA, UNSIGNED_BYTE, img)

    tex
  }

  def compileShader(gl: WebGLRenderingContext, src: String, shaderType: Int): Either[String, WebGLShader] = {
    val shader = gl.createShader(shaderType)

    // Set the shader source code.
    gl.shaderSource(shader, src)

    // Compile the shader
    gl.compileShader(shader)

    // Check if it compiled
    val success = gl.getShaderParameter(shader, COMPILE_STATUS).asInstanceOf[Boolean]
    if (!success) {
      // Something went wrong during compilation get the error
      Left("could not compile shader:" + gl.getShaderInfoLog(shader))
    }

    Right(shader)
  }

  def createProgram(gl: WebGLRenderingContext, vertex: WebGLShader, fragment: WebGLShader): Either[String, WebGLProgram] = {
    val program = gl.createProgram()

    // attach the shaders.
    gl.attachShader(program, vertex)
    gl.attachShader(program, fragment)

    // link the program.
    gl.linkProgram(program)

    // Check if it linked.
    val success = gl.getProgramParameter(program, LINK_STATUS).asInstanceOf[Boolean]
    if (!success) {
      // something went wrong with the link
      Left("program filed to link:" + gl.getProgramInfoLog (program))
    }

    Right(program)
  }

}
