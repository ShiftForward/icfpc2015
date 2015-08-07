package eu.shiftforward.icfpc2015

object MainApp extends App {

  case class Options(
    files: List[String] = List(),
    timeLimit: Option[Int] = None,
    memoryLimit: Option[Int] = None,
    coreNumber: Option[Int] = None,
    phrasesOfPower: List[String] = List())

  def nextOption(opts: Options, remArgs: List[String]): Options = {
    remArgs match {
      case Nil => opts
      case "-f" :: value :: tail =>
        nextOption(opts.copy(files = opts.files :+ value), tail)
      case "-t" :: value :: tail =>
        nextOption(opts.copy(timeLimit = Some(value.toInt)), tail)
      case "-m" :: value :: tail =>
        nextOption(opts.copy(memoryLimit = Some(value.toInt)), tail)
      case "-c" :: value :: tail =>
        nextOption(opts.copy(coreNumber = Some(value.toInt)), tail)
      case "-p" :: value :: tail =>
        nextOption(opts.copy(phrasesOfPower = opts.phrasesOfPower :+ value), tail)
      case _ => nextOption(opts, remArgs.tail)
    }
  }

  val options = nextOption(Options(), args.toList)
  println(options)
}
