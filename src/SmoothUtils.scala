import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.util.Random

object SmoothUtils {
  type Pixel = Int
  type Row = Array[Pixel]
  type MatrixImage = Array[Row]

  type SmoothFunction = MatrixImage => (Int, Int) => Pixel

  val dirs4: Array[(Int, Int)] = Array.apply((-1, 0), (1, 0), (0, -1), (0, 1))
  val dirs8: Array[(Int, Int)] = Array.apply((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  def smoothFunction1(image: MatrixImage)(x: Int, y: Int): Pixel = {
    val (n, m) = getDimenstions(image)
    val pixels = getPixels(image)(getListNeighbours8(n, m)(x, y))

    var (reds, greens, blues, alphas) = (0, 0, 0, 0)
    val p = pixels.length
    for (rgb <- pixels){
      val color = new Color(rgb)

      reds += color.getRed
      greens += color.getGreen
      blues += color.getBlue
      alphas += color.getAlpha
    }

    reds /= p
    greens /= p
    blues /= p
    alphas /= p

    new Color(reds, greens, blues, alphas).getRGB
  }

  def smoothImage(img: String, concurrent: Boolean = true): Unit ={
    val photoIn = ImageIO.read(new File("images\\" + img))

    val image = imageFromBufferedMatrixImage(photoIn)

    if (concurrent)
      new ConcurrentSmoothImage(image, smoothFunction1).run()
    else
      new IterativeSmoothImage(image, smoothFunction1).run()

    val photoOut = bufferedImageFromMatrixImage(image)
    ImageIO.write(photoOut, "jpg", new File("smooth-images\\" + "smooth-" + img))
  }

  def imageFromBufferedMatrixImage(photo: BufferedImage): MatrixImage = {
    val h = photo.getHeight
    val w = photo.getWidth
    val image = generateEmptyMatrixImage(h, w)

    for (j <- 0.until(w))
      for (i <- 0.until(h))
        image(i)(j) = photo.getRGB(j, i)

    image
  }

  def bufferedImageFromMatrixImage(image: MatrixImage): BufferedImage = {
    val w = getColumns(image)
    val h = getRows(image)
    val photoOut = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until w)
      for (y <- 0 until h)
        photoOut.setRGB(x, y, image(y)(x))

    photoOut
  }

  def isInBounds(N: Int, M: Int)(x: Int, y: Int): Boolean = {
    0 <= x && x < N && 0 <= y && y < M
  }

  def getListNeighbours4(N: Int, M: Int)(x: Int, y: Int): List[(Int, Int)] = {
    var list = List.empty[(Int, Int)]

    for ((dx, dy) <- dirs4)
      if (isInBounds(N, M)(x + dx, y + dy))
        list = (x + dx, y + dy) :: list

    list
  }

  def getListNeighbours8(N: Int, M: Int)(x: Int, y: Int): List[(Int, Int)] = {
    var list = List.empty[(Int, Int)]

    for ((dx, dy) <- dirs8)
      if (isInBounds(N, M)(x + dx, y + dy))
        list = (x + dx, y + dy) :: list

    list
  }

  def getPixels(image: MatrixImage)(list: List[(Int, Int)]): List[Pixel] = {
    var pixels = List.empty[Pixel]

    for ((x, y) <- list)
      pixels = image(x)(y) :: pixels

    pixels
  }

  def getRows(image: MatrixImage): Int = {
    image.length
  }

  def getColumns(image: MatrixImage): Int = {
    if (getRows(image) == 0)
      0
    else
      image(0).length
  }

  def getDimenstions(image: MatrixImage): (Int, Int) = {
    (getRows(image), getColumns(image))
  }

  def generateEmptyMatrixImage(n: Int, m: Int): MatrixImage = {
    Array.ofDim[Pixel](n, m)
  }

  def copy(source: MatrixImage, rowSource: Int, colSource: Int)(destination: MatrixImage, rowDest: Int, colDest: Int)
          (numRows: Int, numCols: Int): Unit = {
    for (i <- 0.until(numRows); j <- 0.until(numCols)){
      destination(rowDest + i)(colDest + j) = source(rowSource + i)(colSource + j)
    }
  }

  def copyImage(image: MatrixImage): MatrixImage = {
    val (n, m) = getDimenstions(image)
    val newImage = generateEmptyMatrixImage(n, m)
    copy(newImage, 0, 0)(image, 0, 0)(n, m)
    newImage
  }

  def makeLocalCopy(startRow: Int, numRows: Int, M: Int, image: MatrixImage): MatrixImage = {
    val localImage = generateEmptyMatrixImage(numRows, M)

    for (step <- 0.until(numRows))
      for (j <- 0.until(M))
        localImage(step)(j) = image(startRow + step)(j)

    localImage
  }

  def generateRandomImage(n: Int, m: Int): MatrixImage = {
    val image = generateEmptyMatrixImage(n, m)
    val random = new Random

    for (i <- 0.until(n); j <- 0.until(m)){
      image(i)(j) = random.nextInt()
    }

    image
  }

  def printImage(image: MatrixImage): Unit ={
    val (n, m) = getDimenstions(image)

    for (i <- 0.until(n)){
      for (j <- 0.until(m)){
        print(image(i)(j))
      }
      println()
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}