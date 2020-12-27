import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

/* excerpt from: http://otfried.org/scala/image.html

The method getRGB(x: Int, y: Int): Int returns the color of the pixel at position (x, y),
the method setRGB(x: Int, y: Int, color: Int) sets the color of this pixel.

Colors are represented as Int objects. The three components red, green, and blue are "packed" together into one integer.
Each component has 8 bits, and therefore can have a value between 0 and 255.
The packed color is a 32-bit integer, whose bits look like this:

  tttt tttt rrrr rrrr gggg gggg bbbb bbbb

The top 8 bits are either zero, or represent the "transparency" of the pixel.
We will not use these transparency bits.
The next 8 bits represent red, the next 8 bits represent green, and the last 8 bits represent blue.
This is why this representation is called "RGB".
Given red, green, and blue components with values in the range 0 to 255, we can pack them together like this:

  val color = (red * 65536) + (green * 256) + blue

Given a packed integer color, we can extract the three components as follows:

  val red = (color & 0xff0000) / 65536
  val green = (color & 0xff00) / 256
  val blue = (color & 0xff)

(The & operator here is the bitwise-and operator. It makes sure that only the bits we are interested in are used.)
*/

object ImageProc extends App {

  val red   = 255 * 65536 //255 * 2^16
  val green = 255 * 256   //255 * 2^8
  val blue  = 255         //255 * 2^0

  def convertToColorBit(int: Int): Int = int & 0xffffff
  def zeroPixel(int: Int): Int = 0 * int
  def redPixel(int: Int): Int = red
  def greenPixel(int: Int): Int = green
  def bluePixel(int: Int): Int = blue
  def halfPixel(int: Int): Int = int / 2

  //Pass this into your HOF to Flip your image
  def mirrorMatrix(input: Array[Array[Int]]): Array[Array[Int]] = {
    val width = input(0).length
    val height = input.length
    val result = Array.ofDim[Int](height, width)
    for (i <- 0 until height)
      for (j <- 0 until width) {
        result(i)(j) = input(i)(width - j - 1)
      }
    result
  }

  //Pass this into your HOF to Mirror your image
  def flipMatrix(input: Array[Array[Int]]): Array[Array[Int]] = {
    val width = input(0).length
    val height = input.length
    val result = Array.ofDim[Int](height, width)
    for (i <- 0 until height) //for each col
      for (j <- 0 until width) { // for each row
        result(i)(j) = input(height - i - 1)(j)
      }
    result
  }

  def copyMatrixFromBufferedImage(img: BufferedImage): Array[Array[Int]] = {
    val res = Array.ofDim[Int](img.getHeight, img.getWidth) //swap
    for (i <- 0 until img.getHeight)
      for (j <- 0 until img.getWidth)
        res(i)(j) = img.getRGB(j,i)
    res
  }

  def copyMatrixToBufferedImage(mat: Array[Array[Int]]): BufferedImage = {
    val res = new BufferedImage(mat(0).length, mat.length, BufferedImage.TYPE_INT_RGB) //swap w and h
    for (i <- mat(0).indices)
      for (j <- mat.indices)
        res.setRGB(i, j, convertToColorBit(mat(j)(i))) //swap res i and j
    res
  }

  //Higher Order Function used for applying a pixel-atomic operation across an image
  def applyPixelTransformation(f: Int => Int, input: BufferedImage): BufferedImage = {
    copyMatrixToBufferedImage(copyMatrixFromBufferedImage(input).map(_.map(f(_))))
  }

  //Higher Order Function used for applying a 'matrix-atomic' operation across an image
  def applyImageTransformation(f: Array[Array[Int]] => Array[Array[Int]], input: BufferedImage): BufferedImage = {
    copyMatrixToBufferedImage(f(copyMatrixFromBufferedImage(input)))
  }

  def main() {
    val photo1 = ImageIO.read(new File("src/main/resources/jasonfox.png"))
    val photo2 = applyPixelTransformation(halfPixel, photo1) //Works perfectly, let it remain
    val photo3 = applyImageTransformation(mirrorMatrix, photo1) //Works perfectly
    val photo4 = applyImageTransformation(flipMatrix, photo1)

    ImageIO.write(photo2, "jpg", new File("src/main/resources/deepfry.jpg"))
  }
  main()
}

