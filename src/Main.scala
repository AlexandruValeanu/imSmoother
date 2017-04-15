// Concurrent image smoothing application written in Scala

object Main {
  def main(args: Array[String]): Unit = {
    SmoothUtils.time(SmoothUtils.smoothImage("tom_jerry.png"))
  }
}
