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

object App extends App {

  def convertToColorBit(int: Int): Int = int & 0xffffff

  def zeroPixel(int: Int): Int = 0 * int
  def halfPixel(int: Int): Int = int / 2

  //Pass this into your HOF to Flip your image
  def flipMatrix(input: Array[Array[Int]]): Array[Array[Int]] = {
    val width = input(0).size
    val height = input.size
    val result = Array.ofDim[Int](width, height)
    for (i <- 0 until width)
      for (j <- 0 until height) {
        result(i)(j) = input(j)(width - i - 1)
      }
    result
  }

  //Pass this into your HOF to Mirror your image
  def mirrorMatrix(input: Array[Array[Int]]): Array[Array[Int]] = {
    val width = input(0).size
    val height = input.size
    val result = Array.ofDim[Int](width, height)
    for (i <- 0 until width) //for each col
      for (j <- 0 until height) { // for each row
        result(i)(j) = input(height - j - 1)(i)
      }
    result
  }

  //Higher Order Function used for applying a pixel-atomic operation across an image
  def applyPixelFunc(f: Int => Int, input: BufferedImage): BufferedImage = {

    //Convert BufferedImage into Array[Array[Int]]
    val mat = Array.ofDim[Int](input.getWidth, input.getHeight)
    for (i <- 0 until input.getWidth)
      for (j <- 0 until input.getHeight)
        mat(i)(j) = input.getRGB(i,j)

    val temp = mat.map(_.map(f(_)))
    //Convert Array[Array[Int]] back to Buffered Image
    val res = new BufferedImage(input.getWidth, input.getHeight, BufferedImage.TYPE_INT_RGB)
    for (i <- 0 until input.getWidth)
      for (j <- 0 until input.getHeight)
        res.setRGB(i, j, convertToColorBit(temp(i)(j)))

    res
  }

  //Higher Order Function used for applying a pixel-atomic operation across an image
  def applyTransformation(f: Array[Array[Int]] => Array[Array[Int]], input: BufferedImage): BufferedImage = {
    copyMatrixToBufferedImage(f(copyMatrixFromBufferedImage(input)))
  }

  def copyMatrixFromBufferedImage(img: BufferedImage): Array[Array[Int]] = {
    val res = Array.ofDim[Int](img.getWidth, img.getHeight)
    for (i <- 0 until img.getWidth)
      for (j <- 0 until img.getHeight)
        res(i)(j) = img.getRGB(i,j)
    res
  }

  def copyMatrixToBufferedImage(mat: Array[Array[Int]]): BufferedImage = {
    val res = new BufferedImage(mat(0).size, mat.size, BufferedImage.TYPE_INT_RGB)
    for (i <- 0 until mat(0).size)
      for (j <- 0 until mat.size)
        res.setRGB(i, j, convertToColorBit(mat(j)(i)))
    res
  }

  def main() {
    val photo1 = ImageIO.read(new File("src/main/resources/jasonfox.png"))

//    val photo2 = applyPixelFunc(halfPixel, photo1)
//    val photo2 = applyTransformation(flipMatrix, photo1)
    val photo2 = applyTransformation(mirrorMatrix, photo1)

    ImageIO.write(photo2, "jpg", new File("src/main/resources/test.jpg"))
  }
  main()
}

