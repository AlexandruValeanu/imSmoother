import SmoothUtils.SmoothFunction
import io.threadcso._


class ConcurrentSmoothImage(image: SmoothUtils.MatrixImage, smoothFunction: SmoothFunction, WORKERS: Int = 8) extends Runnable{
  private val N = SmoothUtils.getRows(image)
  private val M = SmoothUtils.getColumns(image)

  private [this] val originalImage: SmoothUtils.MatrixImage = image

  var maxRounds = 50
  private [this] val computed = new Barrier(WORKERS)
  private [this] val updated = new CombiningBarrier[Boolean](WORKERS, true, _ && _)

  private def worker(startRow: Int, numRows: Int): PROC = proc{
    val localImage = SmoothUtils.makeLocalCopy(startRow, numRows, M, originalImage) // local copy of the image region
    var (finished, stable) = (false, true)
    var rounds = 0

    while (!finished){
      for (row <- startRow.until(startRow + numRows); col <- 0.until(M)){
        val v = smoothFunction(image)(row, col)                                   // READ neighbours from global image
        localImage(row - startRow)(col) = v                                       // update the local pixel
        stable = stable && (originalImage(row)(col) == v)
      }

      computed.sync() // sync ends of READ phase
      rounds += 1

      SmoothUtils.copy(localImage, 0, 0)(originalImage, startRow, 0)(numRows, M) // publish image region

      finished = updated.sync(stable || rounds >= maxRounds)         // sync ends of WRITE phase
      stable = true
    }
  }

  override def run(): Unit = {
    val slice = N / WORKERS
    var bigproc = worker(slice * (WORKERS - 1), N - slice * (WORKERS - 1))
    0.until(WORKERS - 1).foreach(i => bigproc = bigproc || worker(slice * i, slice))
    bigproc()
  }
}

