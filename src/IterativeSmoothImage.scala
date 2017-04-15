import SmoothUtils.SmoothFunction

class IterativeSmoothImage(image: SmoothUtils.MatrixImage, smoothFunction: SmoothFunction) extends Runnable{
  private val N = SmoothUtils.getRows(image)
  private val M = SmoothUtils.getColumns(image)

  private [this] val originalImage: SmoothUtils.MatrixImage = image

  var maxRounds = 50

  override def run(): Unit = {
    var (finished, stable) = (false, true)
    var rounds = 0

    while (!finished){
      val localImage = SmoothUtils.makeLocalCopy(0, N, M, originalImage) // local copy of the image region

      for (row <- 0.until(N); col <- 0.until(M)){
        val v = smoothFunction(image)(row, col)                          // READ neighbours from global image
        localImage(row)(col) = v                                         // update the local pixel
        stable = stable && (originalImage(row)(col) == v)
      }

      rounds += 1
      SmoothUtils.copy(localImage, 0, 0)(originalImage, 0, 0)(N, M) // publish image region

      finished = stable || rounds >= maxRounds                      // sync ends of WRITE phase
      stable = true
    }
  }
}